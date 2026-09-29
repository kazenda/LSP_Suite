(defun c:CS (/ src attribs srcobj srcval target tattribs tobj)
  (setq src (car (entsel "\nPick source SPEC block: ")))
  (if (null src) (exit))

  (setq attribs (entnext src))
  (setq srcval nil)

  (while (and attribs (= (cdr (assoc 0 (entget attribs))) "ATTRIB"))
    (if (= (cdr (assoc 2 (entget attribs))) "SPECDESC")
      (progn
        (setq srcobj (vlax-ename->vla-object attribs))
        (setq srcval (vla-get-textstring srcobj))
      )
    )
    (setq attribs (entnext attribs))
  )

  (if (null srcval)
    (progn (princ "\nSPECDESC not found.") (exit))
  )

  (princ (strcat "\nCopied: " srcval))
  (princ "\nPick target SPEC blocks (Enter to finish): ")

  (while (setq target (car (entsel "\nPick target: ")))
    (setq tattribs (entnext target))
    (while (and tattribs (= (cdr (assoc 0 (entget tattribs))) "ATTRIB"))
      (if (= (cdr (assoc 2 (entget tattribs))) "SPECDESC")
        (progn
          (setq tobj (vlax-ename->vla-object tattribs))
          (vla-put-textstring tobj srcval)
          (entupd tattribs)
        )
      )
      (setq tattribs (entnext tattribs))
    )
  )
  (princ "\nDone.")
)