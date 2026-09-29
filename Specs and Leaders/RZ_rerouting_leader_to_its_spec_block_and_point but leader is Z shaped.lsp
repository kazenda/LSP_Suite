;; POPRELEADER-Z - Re-draws the leader polyline for one SL_SPEC_LEADER group
;; as a double-elbow "Z" (horizontal-vertical-horizontal): exits the circle
;; left/right, jogs vertically at the horizontal midpoint between the anchor
;; edge and the SPEC landing point, then approaches the SPEC horizontally.
;; Select any one member of the trio (circle, leader, or SPEC); the other
;; two are found via the shared POPULATE pairID XData.
;;
;; No style detection - always draws the 4-point H-V-H route, regardless
;; of relative Y position. Use RR instead for the straight/L behavior.
;;
;; Collision with other geometry is NOT checked (handled manually for now).

(vl-load-com)

(defun PR:PairID (ent / xdata)
  (if (setq xdata (assoc -3 (entget ent '("POPULATE"))))
    (cdr (assoc 1000 (cdr (cadr xdata))))
  )
)

(defun PR:SetPairID (obj pairID / ent)
  (setq ent (vlax-vla-object->ename obj))
  (entmod
    (append
      (entget ent)
      (list (list -3 (list "POPULATE" (cons 1000 pairID))))
    )
  )
  (entupd ent)
)

;; Scan the whole drawing for the other two members sharing pairID.
;; Returns (circleObj leaderObj specObj) - any not found comes back nil.
(defun PR:FindGroup (pairID / ss i ent obj objName circleObj leaderObj specObj)
  (setq circleObj nil leaderObj nil specObj nil)
  (if (setq ss (ssget "_X"))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (= (PR:PairID ent) pairID)
          (progn
            (setq obj (vlax-ename->vla-object ent)
                  objName (vla-get-ObjectName obj))
            (cond
              ((= objName "AcDbCircle") (setq circleObj obj))
              ((= objName "AcDbPolyline") (setq leaderObj obj))
              ((and (= objName "AcDbBlockReference")
                    (= (strcase (vla-get-EffectiveName obj)) "SPEC"))
               (setq specObj obj))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (list circleObj leaderObj specObj)
)

;; Build the 4-point vertex list for the Z leader: horizontal out of the
;; anchor edge, vertical jog at the midpoint X between edge and landing,
;; horizontal into the SPEC.
(defun PR:RoutePointsZ (edgePt landingPt / midX elbow1 elbow2)
  (setq midX (/ (+ (car edgePt) (car landingPt)) 2.0))
  (setq elbow1 (list midX (cadr edgePt)))
  (setq elbow2 (list midX (cadr landingPt)))
  (list edgePt elbow1 elbow2 landingPt)
)

(defun PR:DrawLeader (ms pts pairID celtscaleVal / flatpts ptArray idx val prevCeltscale pline)
  (setq flatpts '())
  (foreach p pts
    (setq flatpts (append flatpts (list (car p) (cadr p))))
  )
  (setq ptArray (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flatpts)))))
  (setq idx 0)
  (foreach val flatpts
    (vlax-safearray-put-element ptArray idx val)
    (setq idx (1+ idx))
  )
  (setq prevCeltscale (getvar "CELTSCALE"))
  (setvar "CELTSCALE" celtscaleVal)
  (setvar "CELTYPE" "HIDDEN")
  (setq pline (vla-AddLightWeightPolyline ms ptArray))
  (setvar "CELTSCALE" prevCeltscale)
  (setvar "CELTYPE" "BYLAYER")
  (PR:SetPairID pline pairID)
  pline
)

(defun c:RZ (/ doc ms sel ent pairID group circleObj leaderObj specObj
                        center radius edgePt landingPt pts newPline oldCeltscale)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  (setq sel (entsel "\nSelect circle, leader, or SPEC block to re-leader (Z): "))
  (if (not sel)
    (princ "\nNothing selected.")
    (progn
      (setq ent (car sel)
            pairID (PR:PairID ent))
      (if (not pairID)
        (princ "\nSelected entity has no POPULATE pairID.")
        (progn
          (setq group (PR:FindGroup pairID)
                circleObj (nth 0 group)
                leaderObj (nth 1 group)
                specObj   (nth 2 group))
          (cond
            ((not circleObj) (princ "\nCould not find the circle anchor for this group."))
            ((not specObj)   (princ "\nCould not find the SPEC block for this group."))
            (T
              ;; Anchor edge point: always left or right, whichever faces
              ;; the SPEC. No level/not-level branching - RZ always draws
              ;; the H-V-H route.
              (setq center (vlax-safearray->list
                             (vlax-variant-value (vla-get-Center circleObj)))
                    radius (vla-get-Radius circleObj)
                    landingPt (vlax-safearray->list
                                (vlax-variant-value (vla-get-InsertionPoint specObj))))
              (setq edgePt
                (list (+ (car center)
                         (if (>= (car landingPt) (car center)) radius (- radius)))
                      (cadr center))
              )

              (setq pts (PR:RoutePointsZ edgePt landingPt))

              ;; Preserve the existing leader's linetype scale (group code 48)
              ;; so re-leadering doesn't reset dashes back to CELTSCALE 1.
              (setq oldCeltscale
                (if leaderObj
                  (if (assoc 48 (entget (vlax-vla-object->ename leaderObj)))
                    (cdr (assoc 48 (entget (vlax-vla-object->ename leaderObj))))
                    1.0
                  )
                  1.0
                )
              )

              ;; Swap in the new polyline before deleting the old one.
              (setq newPline (PR:DrawLeader ms pts pairID oldCeltscale))
              (if leaderObj
                (vla-Delete leaderObj)
              )

              (princ
                (strcat "\nPOPRELEADER-Z complete. Style: Z | POPULATE ID: " pairID))
            )
          )
        )
      )
    )
  )
  (princ)
)
