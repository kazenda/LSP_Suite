; SPECLEADER - Draws a circle at object, polyline leader from circle edge, SPEC block at end
; Looks up spec description from TOC blocks by PAGECODE match
; Scale input drives block size, circle size, and linetype scale (base: 1:10)

(vl-load-com)

;; POPULATE association data.  Every circle, leader, and SPEC made by SL
;; receives the same ID so POPULATE can later find the correct trio.
(defun POP:EnsureRegApp (doc appName / apps)
  (if (not (tblsearch "APPID" appName))
    (progn
      (setq apps (vla-get-RegisteredApplications doc))
      (vla-Add apps appName)
    )
  )
)

(defun POP:SetPairID (obj pairID / ent)
  (setq ent (vlax-vla-object->ename obj))
  (entmod
    (append
      (entget ent)
      (list
        (list -3 (list "POPULATE" (cons 1000 pairID)))
      )
    )
  )
  (entupd ent)
)

;; Remembered scale denominator, shared across c:SL calls for this session.
;; nil until SLSCALE sets it, or until SL asks for it the first time.
(setq *SL:ScaleDenom* nil)

(defun c:SLSCALE (/ input)
  (setq input (getstring "\nEnter scale denominator (e.g. 15 for 1:15): "))
  (setq *SL:ScaleDenom* (atof input))
  (princ (strcat "\nSL scale set to 1:" input " for subsequent SL placements."))
  (princ)
)

(defun c:SL (/ doc ms scaleDenom scaleFactor ltNew
               specCode specDesc found
               tocAtts tocAtt tocTag
               centerPt secondPt ang radius edgePt
               pts pt flatpts ptArray pline
               circleObj blkObj attribs att
               pairID xScale
               prevCeltscale idx)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))
  (POP:EnsureRegApp doc "POPULATE")

  ; Scale input - reuse the remembered scale if SLSCALE has set one;
  ; otherwise ask here, same as before, and remember it for next time.
  (if *SL:ScaleDenom*
    (setq scaleDenom *SL:ScaleDenom*)
    (progn
      (setq scaleDenom (atof (getstring "\nEnter scale denominator (e.g. 15 for 1:15): ")))
      (setq *SL:ScaleDenom* scaleDenom)
    )
  )
  (setq scaleFactor (/ scaleDenom 10.0))
  (setq ltNew       (* 100.0 scaleFactor))
  (setq radius      (* 5.0 scaleFactor))

  ; Spec code lookup in TOC blocks
  (setq specCode (strcase (getstring "\nEnter spec code (e.g. P-01): ")))
  (setq specDesc "")
  (setq found nil)

  (vlax-for obj ms
    (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
             (= (strcase (vla-get-Name obj)) "TOC")
             (not found))
      (progn
        (setq tocAtts (vlax-invoke obj 'GetAttributes))
        (foreach tocAtt tocAtts
          (if (and (= (strcase (vlax-get tocAtt 'TagString)) "PAGECODE")
                   (= (strcase (vlax-get tocAtt 'TextString)) specCode))
            (setq found T)
          )
        )
        (if found
          (foreach tocAtt tocAtts
            (if (= (strcase (vlax-get tocAtt 'TagString)) "PAGETITLE")
              (setq specDesc (vlax-get tocAtt 'TextString))
            )
          )
        )
      )
    )
  )

  (if (not found)
    (princ (strcat "\nCode " specCode " not found in TOC. Proceeding with empty description."))
  )

  ; Pick center point on object
  (setq centerPt (getpoint "\nClick point on object (circle center): "))

  ; Pick second point to determine direction
  (setq secondPt (getpoint centerPt "\nPick direction point (first leader point): "))

  ; Angle from center to second point
  (setq ang (atan
    (- (cadr secondPt) (cadr centerPt))
    (- (car  secondPt) (car  centerPt))
  ))

  ; Edge point on circle circumference toward second point
  (setq edgePt (list
    (+ (car  centerPt) (* radius (cos ang)))
    (+ (cadr centerPt) (* radius (sin ang)))
  ))

  ; Draw circle
  (setq circleObj (vla-AddCircle ms (vlax-3d-point centerPt) radius))
  (setq pairID
    (cdr (assoc 5 (entget (vlax-vla-object->ename circleObj))))
  )

  ; Collect all polyline points starting from edge
  (setq pts (list edgePt secondPt))
  (setq pt (getpoint secondPt "\nNext point (Enter to finish): "))
  (while pt
    (setq pts (append pts (list pt)))
    (setq pt (getpoint (last pts) "\nNext point (Enter to finish): "))
  )

  ; Build safearray with XY only (no Z) for lightweight polyline
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

  ; Draw lightweight polyline with CELTSCALE for dashed linetype
  (setq prevCeltscale (getvar "CELTSCALE"))
  (setvar "CELTSCALE" ltNew)
  (setvar "CELTYPE" "HIDDEN")
  (setq pline (vla-AddLightWeightPolyline ms ptArray))
  (setvar "CELTSCALE" prevCeltscale)
  (setvar "CELTYPE" "BYLAYER")

  ;; A SPEC ending left of its circle is mirrored about its local Y axis.
  ;; Its multiline attribute remains readable in this block definition.
  (if (< (car (last pts)) (car centerPt))
    (setq xScale (- scaleFactor))
    (setq xScale scaleFactor)
  )

  ; Place SPEC block at last point.
  (setq blkObj
    (vla-InsertBlock ms
      (vlax-3d-point (last pts))
      "SPEC"
      xScale scaleFactor scaleFactor 0.0)
  )

  ; Fill attributes
  (setq attribs (vlax-invoke blkObj 'GetAttributes))
  (foreach att attribs
    (if (= (strcase (vlax-get att 'TagString)) "PAGECODE")
      (vlax-put att 'TextString specCode)
    )
    (if (= (strcase (vlax-get att 'TagString)) "SPECDESC")
      (vlax-put att 'TextString specDesc)
    )
  )
  (vlax-invoke blkObj 'Update)

  ;; Store the same ID on every member of this SL group.
  (POP:SetPairID circleObj pairID)
  (POP:SetPairID pline pairID)
  (POP:SetPairID blkObj pairID)

  (princ (strcat "\nPlaced: " specCode " - " specDesc
                 " | POPULATE ID: " pairID))
  (princ)
)
