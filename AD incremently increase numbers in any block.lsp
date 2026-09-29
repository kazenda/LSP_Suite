; INCATTSEQ - Click a source block, then click targets one by one.
; Each click increments ALL numbers in EVERY attribute by (step * click count),
; based purely on click order - never reads the target's own current value.
; Matches CP.lsp style (entnext/entget/entmod attribute walking).

; Tags listed here are never modified, no matter what block they appear on.
; Add more tag names (uppercase) separated by spaces if needed, e.g. '("SKALA" "NOJUDUL")
(setq *incattseq-skip-tags* '("SKALA"))

(defun c:AD (/ step src sattribs base-vals target tattribs att tagname
                      baseval newval clickcount)

  (setq step (getreal "\nStep value to add per click (e.g. 1, -1, 5): "))
  (if (not step) (setq step 1.0))

  (princ "\nPick source block (the base/first value): ")
  (setq src (car (entsel)))
  (if (null src) (exit))

  ; Walk source attributes and store tag + value pairs
  (setq sattribs (entnext src))
  (setq base-vals nil) ; list of (tagname . value)

  (while (and sattribs (= (cdr (assoc 0 (entget sattribs))) "ATTRIB"))
    (setq tagname (cdr (assoc 2 (entget sattribs))))
    (setq baseval (cdr (assoc 1 (entget sattribs))))
    (if (not (member (strcase tagname) *incattseq-skip-tags*))
      (setq base-vals (cons (cons tagname baseval) base-vals))
    )
    (setq sattribs (entnext sattribs))
  )

  (if (null base-vals)
    (progn (princ "\nNo attributes found on source block.") (exit))
  )

  (princ "\nSource base values:")
  (foreach pair base-vals
    (princ (strcat "\n  " (car pair) " = " (cdr pair)))
  )

  (princ "\nPick target blocks in order (Enter to finish): ")
  (setq clickcount 0)

  (while (setq target (car (entsel "\nPick target: ")))
    (setq clickcount (1+ clickcount))

    (setq tattribs (entnext target))
    (while (and tattribs (= (cdr (assoc 0 (entget tattribs))) "ATTRIB"))
      (setq tagname (cdr (assoc 2 (entget tattribs))))
      (setq baseval (cdr (assoc tagname base-vals)))

      (if (and baseval (not (member (strcase tagname) *incattseq-skip-tags*)))
        (progn
          ; always increment from the SOURCE base value by (step * clickcount)
          (setq newval (increment-all-numbers baseval (* step clickcount)))
          (setq att (entget tattribs))
          (setq att (subst (cons 1 newval) (assoc 1 att) att))
          (entmod att)
          (entupd tattribs)
        )
      )
      (setq tattribs (entnext tattribs))
    )
    (princ (strcat "\nClick #" (itoa clickcount) " set."))
  )
  (princ "\nDone.")
  (princ)
)

; Increments every run of digits in str by the given step (can be negative/decimal;
; decimal step is truncated to integer for the addition).
; Preserves leading zeros per number run and any surrounding text.
(defun increment-all-numbers (str step / result i c num numstr collecting padLen newnum newstr)
  (setq result ""
        i 0
        collecting nil
        numstr "")
  (while (< i (strlen str))
    (setq i (1+ i))
    (setq c (substr str i 1))
    (if (and (>= (ascii c) 48) (<= (ascii c) 57))
      (progn
        (setq numstr (strcat numstr c))
        (setq collecting T)
      )
      (progn
        (if collecting
          (progn
            (setq padLen (strlen numstr))
            (setq num (atoi numstr))
            (setq newnum (fix (+ num step)))
            (setq newstr (itoa newnum))
            (while (< (strlen newstr) padLen)
              (setq newstr (strcat "0" newstr))
            )
            (setq result (strcat result newstr))
            (setq numstr "")
            (setq collecting nil)
          )
        )
        (setq result (strcat result c))
      )
    )
  )
  (if collecting
    (progn
      (setq padLen (strlen numstr))
      (setq num (atoi numstr))
      (setq newnum (fix (+ num step)))
      (setq newstr (itoa newnum))
      (while (< (strlen newstr) padLen)
        (setq newstr (strcat "0" newstr))
      )
      (setq result (strcat result newstr))
    )
  )
  result
)