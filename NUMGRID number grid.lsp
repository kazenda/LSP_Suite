(defun c:NUMGRID (/ start step rows cols spacing-x spacing-y origin val pt-x pt-y)
  ;; Prompt for series parameters
  (setq start    (getreal "\nStart number: "))
  (setq step     (getreal "Step value: "))
  (setq cols     (getint  "Number of columns: "))
  (setq rows     (getint  "Number of rows: "))
  (setq spacing-x (getreal "X spacing between numbers: "))
  (setq spacing-y (getreal "Y spacing between rows: "))
  (setq origin   (getpoint "\nPick origin point: "))

  ;; Generate grid
  (setq val start)
  (repeat rows
    (setq pt-y (cadr origin))
    (setq col-count 0)
    (repeat cols
      (setq pt-x (+ (car origin) (* col-count spacing-x)))
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 8 (getvar "CLAYER"))
          (cons 10 (list pt-x pt-y 0.0))
          (cons 40 (getvar "TEXTSIZE"))
          (cons 1 (rtos val 2 (if (zerop (- val (fix val))) 0 4)))
          (cons 7 (getvar "TEXTSTYLE"))
        )
      )
      (setq val (+ val step))
      (setq col-count (1+ col-count))
    )
    (setq origin (list (car origin) (- (cadr origin) spacing-y) 0.0))
  )
  (princ)
)