;;; CT_bump_toc_number.lsp
;;; Command: CTB  (Change Title - Bump)
;;;
;;; Select multiple TOC blocks (window/crossing), specify an increment
;;; (default 1, can be negative), and this will find the PAGECODE
;;; attribute on each selected block, split it into a prefix + trailing
;;; number (e.g. "SDF-12" -> prefix "SDF-" + number "12"), add the
;;; increment, and write the result back with the same zero-padding
;;; width as the original number.
;;;
;;; Examples:
;;;   SDF-12  + 1  -> SDF-13
;;;   SDF-05  + 1  -> SDF-06   (padding preserved)
;;;   SDF-99  + 1  -> SDF-100  (grows past padding width naturally)
;;;   SDF-12  + -1 -> SDF-11
;;;
;;; Blocks with no PAGECODE attribute, or a PAGECODE that doesn't end
;;; in digits, are skipped with a warning printed to the command line so
;;; one bad block doesn't stop the batch.

(defun CTB:get-pagecode-attrib (blkent / attribs found)
  ;; returns the ename of the PAGECODE ATTRIB sub-entity of blkent, or nil
  (setq attribs (entnext blkent))
  (setq found nil)
  (while (and attribs (not found)
              (= (cdr (assoc 0 (entget attribs))) "ATTRIB"))
    (if (= (cdr (assoc 2 (entget attribs))) "PAGECODE")
      (setq found attribs)
    )
    (setq attribs (entnext attribs))
  )
  found
)

(defun CTB:split-trailing-number (str / len i c numstart)
  ;; returns (prefix . numberstring) by peeling digits off the end of str
  ;; returns nil if str has no trailing digits
  (setq len (strlen str))
  (setq i len)
  (setq numstart nil)
  (while (and (> i 0)
              (wcmatch (substr str i 1) "[0-9]"))
    (setq numstart i)
    (setq i (1- i))
  )
  (if numstart
    (cons (substr str 1 (1- numstart)) (substr str numstart))
    nil
  )
)

(defun CTB:bump-numstring (numstr inc / n width newn newstr padneeded)
  ;; increments numstr by inc, preserving zero-padding width when possible
  (setq width (strlen numstr))
  (setq n (atoi numstr))
  (setq newn (+ n inc))
  (if (< newn 0)
    (setq newn 0) ; don't go negative; adjust manually if this happens
  )
  (setq newstr (itoa newn))
  (setq padneeded (- width (strlen newstr)))
  (while (> padneeded 0)
    (setq newstr (strcat "0" newstr))
    (setq padneeded (1- padneeded))
  )
  newstr
)

(defun c:CTB (/ ss n i ent attrib val parts prefix numpart newnum newval inc skipped done)

  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss)
    (progn (princ "\nNo blocks selected.") (exit))
  )

  (setq inc (getint "\nAmount to add to number (can be negative) <1>: "))
  (if (null inc) (setq inc 1))

  (setq n (sslength ss))
  (setq i 0)
  (setq skipped 0)
  (setq done 0)

  (while (< i n)
    (setq ent (ssname ss i))
    (setq attrib (CTB:get-pagecode-attrib ent))

    (cond
      ((null attrib)
       (princ (strcat "\nSkipped (no PAGECODE attribute) on entity: " (vl-princ-to-string ent)))
       (setq skipped (1+ skipped))
      )
      (t
       (setq val (cdr (assoc 1 (entget attrib))))
       (setq parts (CTB:split-trailing-number val))
       (cond
         ((null parts)
          (princ (strcat "\nSkipped (no trailing number found in \"" val "\")"))
          (setq skipped (1+ skipped))
         )
         (t
          (setq prefix (car parts))
          (setq numpart (cdr parts))
          (setq newnum (CTB:bump-numstring numpart inc))
          (setq newval (strcat prefix newnum))
          (entmod (subst (cons 1 newval) (assoc 1 (entget attrib)) (entget attrib)))
          (entupd attrib)
          (princ (strcat "\n" val " -> " newval))
          (setq done (1+ done))
         )
       )
      )
    )
    (setq i (1+ i))
  )

  (princ (strcat "\nDone. Updated " (itoa done) " block(s), skipped " (itoa skipped) "."))
  (princ)
)

(princ "\nCTB loaded. Type CTB to bulk-renumber PAGECODE attributes.")
(princ)
