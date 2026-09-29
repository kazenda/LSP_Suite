;; EE - Erase an entire SL_SPEC_LEADER group (circle, leader, SPEC block)
;; in one shot. Select any one member of the trio; the other two are found
;; via the shared POPULATE pairID XData and all found members are deleted.
;; Silent - no confirmation prompt, no Y/N. Ctrl+Z to undo.
;;
;; Self-contained: includes its own copies of PR:PairID / PR:FindGroup so
;; this file can be loaded on its own without RR/RZ. Merge later if
;; keeping one shared copy is preferred - identical definitions, so no
;; conflict either way.

(vl-load-com)

(defun PR:PairID (ent / xdata)
  (if (setq xdata (assoc -3 (entget ent '("POPULATE"))))
    (cdr (assoc 1000 (cdr (cadr xdata))))
  )
)

;; Scan the whole drawing for members sharing pairID.
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

(defun c:EE (/ sel ent pairID group circleObj leaderObj specObj count)
  (setq sel (entsel "\nSelect circle, leader, or SPEC block to erase (whole group): "))
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
                specObj   (nth 2 group)
                count 0)
          (if circleObj (progn (vla-Delete circleObj) (setq count (1+ count))))
          (if leaderObj (progn (vla-Delete leaderObj) (setq count (1+ count))))
          (if specObj   (progn (vla-Delete specObj)   (setq count (1+ count))))
          (princ (strcat "\nEE complete. " (itoa count) " object(s) erased."))
        )
      )
    )
  )
  (princ)
)
