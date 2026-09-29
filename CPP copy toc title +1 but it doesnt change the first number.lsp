(defun c:CPP (/ src attribs srcval target tattribs att newval)
  (setq src (car (entsel "\nPick source TOC block: ")))
  (if (null src) (exit))

  (setq attribs (entnext src))
  (setq srcval nil)

  (while (and attribs (= (cdr (assoc 0 (entget attribs))) "ATTRIB"))
    (if (= (cdr (assoc 2 (entget attribs))) "PAGETITLE")
      (setq srcval (cdr (assoc 1 (entget attribs))))
    )
    (setq attribs (entnext attribs))
  )

  (if (null srcval)
    (progn (princ "\nPAGETITLE attribute not found.") (exit))
  )

  (princ (strcat "\nCopied: " srcval))
  (princ "\nPick target TOC blocks (Enter to finish): ")

  (setq newval srcval)

  (while (setq target (car (entsel "\nPick target: ")))
    (setq newval (increment-skip-first newval))

    (setq tattribs (entnext target))
    (while (and tattribs (= (cdr (assoc 0 (entget tattribs))) "ATTRIB"))
      (if (= (cdr (assoc 2 (entget tattribs))) "PAGETITLE")
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

(defun increment-skip-first (str / result i c numstr collecting firstdone num)
  (setq result ""
        i 0
        collecting nil
        firstdone nil
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
            (if firstdone
              (progn
                (setq num (atoi numstr))
                (setq result (strcat result (itoa (1+ num))))
              )
              (progn
                (setq result (strcat result numstr))
                (setq firstdone T)
              )
            )
            (setq numstr "")
            (setq collecting nil)
          )
        )
        (setq result (strcat result c))
      )
    )
  )
  (if collecting
    (if firstdone
      (progn
        (setq num (atoi numstr))
        (setq result (strcat result (itoa (1+ num))))
      )
      (setq result (strcat result numstr))
    )
  )
  result
)