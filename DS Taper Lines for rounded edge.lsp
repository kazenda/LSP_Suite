;;; DS -- draw an exponentially spaced array of lines parallel to a
;;; selected reference line.
;;; The reference line itself is left untouched (not regenerated).
;;; n new lines are offset perpendicular to it, with cumulative offsets
;;; growing geometrically by "ratio". "maxDist" is the signed offset of
;;; the last line from the reference line: its sign sets the direction
;;; (positive/negative = one perpendicular side or the other), same as
;;; ARRAY's distance-with-sign convention.

(defun dt:line (p1 p2)
  (entmakex
    (list '(0 . "LINE")
          (cons 10 p1)
          (cons 11 p2)))
)

(defun dt:pow-sum (r n / s i)
  ;; Sum r^0 ... r^(n-1).
  (setq s 0.0 i 0)
  (repeat n
    (setq s (+ s (expt r i)))
    (setq i (1+ i))
  )
  s
)

(defun dt:pick-line (/ ent edata p1 p2)
  ;; Select an existing LINE entity and pull its endpoints directly
  ;; from its DXF data (group codes 10/11).
  (setq ent (entsel "\nSelect reference line: "))
  (if ent
    (progn
      (setq edata (entget (car ent)))
      (if (= (cdr (assoc 0 edata)) "LINE")
        (progn
          (setq p1 (cdr (assoc 10 edata))
                p2 (cdr (assoc 11 edata)))
          (list p1 p2)
        )
        (progn
          (princ "\nSelected object is not a LINE.")
          nil
        )
      )
    )
  )
)

(defun c:DS (/ ref p1 p2 ang perpAng ratio n maxDist
                       gsum g1 offset i lp1 lp2)
  (vl-load-com)

  ;; Defaults are remembered between runs.
  (if (not *DT_RATIO*)   (setq *DT_RATIO* 1.35))
  (if (not *DT_N*)       (setq *DT_N* 8))
  (if (not *DT_MAXDIST*) (setq *DT_MAXDIST* 100.0))

  (setq ref (dt:pick-line))
  (if ref
    (progn
      (setq p1 (car ref)
            p2 (cadr ref))
      (setq ang (angle p1 p2)
            perpAng (+ ang (/ pi 2.0)))

      (setq n (getint (strcat "\nNumber of lines <" (itoa *DT_N*) ">: ")))
      (if (not n) (setq n *DT_N*))
      (if (< n 1) (setq n 1))
      (setq *DT_N* n)

      (setq ratio
            (getreal
              (strcat "\nOffset growth ratio <"
                      (rtos *DT_RATIO* 2 2) ">: ")))
      (if (not ratio) (setq ratio *DT_RATIO*))
      (if (<= ratio 1.0) (setq ratio 1.01))
      (setq *DT_RATIO* ratio)

      (setq maxDist
            (getreal
              (strcat "\nDistance to last line (+/- sets direction) <"
                      (rtos *DT_MAXDIST* 2 2) ">: ")))
      (if (not maxDist) (setq maxDist *DT_MAXDIST*))
      (if (zerop maxDist) (setq maxDist *DT_MAXDIST*))
      (setq *DT_MAXDIST* maxDist)

      ;; Solve g1 so the n-th cumulative offset lands exactly on maxDist.
      (setq gsum (dt:pow-sum ratio n))
      (setq g1 (/ maxDist gsum))

      (setq i 0
            offset 0.0)

      (repeat n
        (setq offset (+ offset (* g1 (expt ratio i))))
        (setq lp1 (polar p1 perpAng offset))
        (setq lp2 (polar p2 perpAng offset))
        (dt:line lp1 lp2)
        (setq i (1+ i))
      )

      (princ
        (strcat
          "\nDS: " (itoa n)
          " parallel offset lines from reference line"
          "; ratio=" (rtos ratio 2 2)
          "; last offset=" (rtos maxDist 2 2)
        )
      )
    )
    (princ "\nNo reference line selected.")
  )
  (princ)
)

(princ "\nDS loaded. Type DS to run.")
(princ)
