(defun c:CW (/ src attribs srcval target tattribs att newval num numstr)
  (setq src (car (entsel "\nPick source TOC block: ")))
  (if (null src) (exit))

  (setq attribs (entnext src))
  (setq srcval nil)

  (while (and attribs (= (cdr (assoc 0 (entget attribs))) "ATTRIB"))
    (if (= (cdr (assoc 2 (entget attribs))) "PAGECODE")
      (setq srcval (cdr (assoc 1 (entget attribs))))
    )
    (setq attribs (entnext attribs))
  )

  (if (null srcval)
    (progn (princ "\nPAGECODE attribute not found.") (exit))
  )

  (princ (strcat "\nCopied: " srcval))
  (princ "\nPick target TOC blocks (Enter to finish): ")

  (setq newval srcval)

  (while (setq target (car (entsel "\nPick target: ")))
    (setq newval (CW:increment-all-numbers newval))

    (setq tattribs (entnext target))
    (while (and tattribs (= (cdr (assoc 0 (entget tattribs))) "ATTRIB"))
      (if (= (cdr (assoc 2 (entget tattribs))) "PAGECODE")
        (progn
          (setq att (entget tattribs))
          (setq att (subst (cons 1 newval) (assoc 1 att) att))
          (entmod att)
          (entupd tattribs)
        )
      )
      (setq tattribs (entnext tattribs))
    )
    (princ (strcat "\nSet: " newval))
  )
  (princ "\nDone.")
)

; Adds 1 to a digit string and keeps its zero padding, e.g. "01" -> "02", "09" -> "10"
(defun CW:bump-padded (numstr / newstr)
  (setq newstr (itoa (1+ (atoi numstr))))
  (while (< (strlen newstr) (strlen numstr))
    (setq newstr (strcat "0" newstr))
  )
  newstr
)

(defun CW:increment-all-numbers (str / result i c numstr collecting)
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
            (setq result (strcat result (CW:bump-padded numstr)))
            (setq numstr "")
            (setq collecting nil)
          )
        )
        (setq result (strcat result c))
      )
    )
  )
  (if collecting
    (setq result (strcat result (CW:bump-padded numstr)))
  )
  result
)
