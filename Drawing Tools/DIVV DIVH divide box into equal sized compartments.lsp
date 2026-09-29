;;; DIVV / DIVH -- evenly divide an axis-aligned rectangular cabinet outline.
;;; DIVV adds vertical dividers (equal-width compartments).
;;; DIVH adds horizontal shelves (equal-height compartments).

(defun div:line (p1 p2)
  ;; entmakex keeps the points in WCS, unlike COMMAND which uses the current UCS.
  (entmakex (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
)

(defun div:settings (/ n thick)
  ;; Return (number-of-compartments divider-thickness), or nil on invalid input.
  (setq n (getint "\nNumber of compartments <3>: "))
  (if (null n) (setq n 3))
  (setq thick (getreal "\nDivider thickness <20>: "))
  (if (null thick) (setq thick 20.0))
  (cond
    ((< n 2)
      (princ "\nAt least 2 compartments are required.")
      nil
    )
    ((< thick 0.0)
      (princ "\nDivider thickness cannot be negative.")
      nil
    )
    (T (list n thick))
  )
)

(defun div:bounds (/ ent obj minpt maxpt)
  (setq ent (entsel "\nSelect rectangular cabinet outline: "))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object (car ent)))
      (vla-getboundingbox obj 'minpt 'maxpt)
      (list (vlax-safearray->list minpt)
            (vlax-safearray->list maxpt))
    )
  )
)

(defun c:DIVV (/ settings bounds minpt maxpt n thick total opening gap i x z)
  (vl-load-com)
  (setq settings (div:settings))
  (if settings
    (progn
      (setq bounds (div:bounds))
      (if bounds
        (progn
          (setq minpt (car bounds)
                maxpt (cadr bounds)
                n      (car settings)
                thick  (cadr settings)
                total  (- (car maxpt) (car minpt))
                opening (- total (* (1- n) thick)))
          (if (<= opening 0.0)
            (princ "\nThe dividers are too thick for this cabinet width.")
            (progn
              ;; Every clear compartment is the same width.
              (setq gap (/ opening n)
                    i   1
                    z   (caddr minpt))
              (repeat (1- n)
                (setq x (+ (car minpt) (* i gap) (* (1- i) thick)))
                (div:line (list x (cadr minpt) z)
                          (list x (cadr maxpt) z))
                (div:line (list (+ x thick) (cadr minpt) z)
                          (list (+ x thick) (cadr maxpt) z))
                (setq i (1+ i))
              )
            )
          )
        )
        (princ "\nNothing selected.")
      )
    )
  )
  (princ)
)

(defun c:DIVH (/ settings bounds minpt maxpt n thick total opening gap i y z)
  (vl-load-com)
  (setq settings (div:settings))
  (if settings
    (progn
      (setq bounds (div:bounds))
      (if bounds
        (progn
          (setq minpt (car bounds)
                maxpt (cadr bounds)
                n      (car settings)
                thick  (cadr settings)
                total  (- (cadr maxpt) (cadr minpt))
                opening (- total (* (1- n) thick)))
          (if (<= opening 0.0)
            (princ "\nThe shelves are too thick for this cabinet height.")
            (progn
              ;; Every clear compartment is the same height.
              (setq gap (/ opening n)
                    i   1
                    z   (caddr minpt))
              (repeat (1- n)
                (setq y (+ (cadr minpt) (* i gap) (* (1- i) thick)))
                (div:line (list (car minpt) y z)
                          (list (car maxpt) y z))
                (div:line (list (car minpt) (+ y thick) z)
                          (list (car maxpt) (+ y thick) z))
                (setq i (1+ i))
              )
            )
          )
        )
        (princ "\nNothing selected.")
      )
    )
  )
  (princ)
)

(princ "\nDIVV loaded: vertical dividers. DIVH loaded: horizontal shelves.")
(princ)
