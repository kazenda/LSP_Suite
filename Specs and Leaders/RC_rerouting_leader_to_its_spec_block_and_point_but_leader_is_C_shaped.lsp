;; POPRELEADER-C - Re-draws the leader polyline for one SL_SPEC_LEADER group
;; as a "C": it exits the circle on the side OPPOSITE the SPEC (left edge if
;; the SPEC is to the right, right edge if the SPEC is to the left), runs
;; outward by a set distance, jogs vertically to the SPEC's Y level, then
;; doubles back horizontally all the way into the SPEC landing point.
;; Select any one member of the trio (circle, leader, or SPEC); the other
;; two are found via the shared POPULATE pairID XData.
;;
;; Route is always 4 points: edge -> outward jog -> vertical -> landing.
;;
;; Notes:
;;  - The outward jog distance is 10x the circle radius, so it scales with
;;    the bubble. Change jogDist in c:RC to adjust it.
;;  - If the SPEC's Y is within the circle's vertical extent, the return run
;;    would cut across the circle; the leader is still drawn, with a warning.
;;  - If the SPEC is at exactly the same Y as the circle center, a C can't be
;;    formed (the run would double back on itself) - nothing is changed.
;;  - Collision with other geometry is NOT checked (handled manually for now).
;;
;; Helper functions are the same as in the RZ file, so loading both is fine.

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

;; Build the 4-point vertex list for the C leader.
;; outDir is -1 (jog goes left of the edge) or +1 (jog goes right of it).
(defun PR:RoutePointsC (edgePt landingPt outDir jogDist / jogX elbow1 elbow2)
  (setq jogX   (+ (car edgePt) (* outDir jogDist)))
  (setq elbow1 (list jogX (cadr edgePt)))
  (setq elbow2 (list jogX (cadr landingPt)))
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

(defun c:RC (/ doc ms sel ent pairID group circleObj leaderObj specObj
                        center radius edgePt landingPt outDir jogDist dy
                        pts newPline oldCeltscale)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  (setq sel (entsel "\nSelect circle, leader, or SPEC block to re-leader (C): "))
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
              (setq center (vlax-safearray->list
                             (vlax-variant-value (vla-get-Center circleObj)))
                    radius (vla-get-Radius circleObj)
                    landingPt (vlax-safearray->list
                                (vlax-variant-value (vla-get-InsertionPoint specObj))))

              ;; C = start on the side AWAY from the SPEC, then double back.
              ;; SPEC to the right -> exit the left edge, jog further left.
              ;; SPEC to the left  -> exit the right edge, jog further right.
              (setq outDir (if (>= (car landingPt) (car center)) -1 1))
              (setq edgePt (list (+ (car center) (* outDir radius)) (cadr center)))

              ;; How far the C sticks out past the circle edge.
              (setq jogDist (* radius 10.0))

              (setq dy (- (cadr landingPt) (cadr center)))

              (if (< (abs dy) 1e-6)
                (princ "\nSPEC is level with the circle center - a C can't be formed. Use RZ or RR instead.")
                (progn
                  (setq pts (PR:RoutePointsC edgePt landingPt outDir jogDist))

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

                  (if (< (abs dy) radius)
                    (princ "\nWarning: SPEC Y is inside the circle's height - the return run crosses the circle.")
                  )
                  (princ
                    (strcat "\nPOPRELEADER-C complete. Style: C | POPULATE ID: " pairID))
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)
