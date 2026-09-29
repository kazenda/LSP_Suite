; WS - Sets scale and syncs drawing title from TOC block
; Looks up current layout name in TOC block's PAGECODE attribute
; and writes matching PAGETITLE to KOPTITLE on current layout

(defun c:WS (/ ss ssms ent ed att att-list blk blk-ed ctab i
               vp-obj vp-found custom-scale denom scale-str
               attribs srcVal kopEnt
               blk-obj pageCode pageTitle foundCode foundTitle)

  (setq ctab (getvar "CTAB"))

  ; ─────────────────────────────────────────
  ; PART 1: SET SCALE
  ; ─────────────────────────────────────────

  (setq ss (ssget "X" '((0 . "VIEWPORT"))))
  (setq vp-found nil)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (if (and (= (cdr (assoc 410 ed)) ctab)
                 (> (cdr (assoc 69 ed)) 1))
          (setq vp-found ent)
        )
        (setq i (1+ i))
      )
    )
  )

  (if vp-found
    (progn
      (setq vp-obj (vlax-ename->vla-object vp-found))
      (setq custom-scale (vla-get-customscale vp-obj))
      (if (and custom-scale (> custom-scale 0))
        (progn
          (setq denom (fix (+ 0.5 (/ 1.0 custom-scale))))
          (setq scale-str (strcat "1:" (itoa denom)))
        )
        (setq scale-str "1:?")
      )
      (princ (strcat "\nScale detected: " scale-str))

      (setq ss (ssget "X" '((0 . "INSERT") (2 . "kop angleina1"))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq blk (ssname ss i))
            (setq blk-ed (entget blk))
            (if (= (cdr (assoc 410 blk-ed)) ctab)
              (progn
                (setq att (entnext blk))
                (while att
                  (if (and (= (cdr (assoc 0 (entget att))) "ATTRIB")
                           (= (cdr (assoc 2 (entget att))) "SCALE"))
                    (progn
                      (setq att-list (entget att))
                      (setq att-list
                        (subst (cons 1 scale-str)
                               (assoc 1 att-list)
                               att-list))
                      (entmod att-list)
                      (entupd att)
                    )
                  )
                  (setq att (entnext att))
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
    (princ "\nNo viewport found on this sheet!")
  )

  ; ─────────────────────────────────────────
  ; PART 2: SYNC DRAWING TITLE FROM TOC
  ; ─────────────────────────────────────────

  (setq srcVal nil kopEnt nil)

  ; Search modelspace for all TOC blocks
  (setq ssms (ssget "X" (list (cons 0 "INSERT") (cons 2 "TOC") (cons 410 "Model"))))
  (if ssms
    (progn
      (setq i 0)
      (while (< i (sslength ssms))
        (setq blk-obj (vlax-ename->vla-object (ssname ssms i)))
        (setq attribs (vlax-invoke blk-obj 'GetAttributes))
        (setq foundCode nil foundTitle nil)
        (foreach att attribs
          (if (= (strcase (vlax-get att 'TagString)) "PAGECODE")
            (setq foundCode (vlax-get att 'TextString))
          )
          (if (= (strcase (vlax-get att 'TagString)) "PAGETITLE")
            (setq foundTitle (vlax-get att 'TextString))
          )
        )
        ; If this TOC block's PAGECODE matches current layout name, grab the title
        (if (= (strcase foundCode) (strcase ctab))
          (setq srcVal foundTitle)
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo TOC blocks found in modelspace.")
  )

  ; Search paperspace for KOPTITLE
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 410 ctab))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq attribs (vlax-invoke (vlax-ename->vla-object (ssname ss i)) 'GetAttributes))
        (foreach att attribs
          (if (= (strcase (vlax-get att 'TagString)) "KOPTITLE")
            (setq kopEnt att)
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (if (and srcVal kopEnt)
    (progn
      (vlax-put kopEnt 'TextString srcVal)
      (vlax-invoke kopEnt 'Update)
      (princ (strcat "\nTitle synced: " srcVal))
    )
    (progn
      (if (null srcVal) (princ (strcat "\nNo TOC entry found for layout: " ctab)))
      (if (null kopEnt) (princ "\nKOPTITLE not found on current layout."))
    )
  )

  (princ "\nDone!")
  (princ)
)
