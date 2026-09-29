; SYNSPEC - Replace spec descriptions in SPEC blocks by looking up codes from TOC
; Prompts for old code and new code, looks both up in TOC, replaces matching SPECDESC attributes

(defun c:SY (/ doc ms oldCode newCode oldDesc newDesc
                    tocAtts tocAtt tocCode
                    specAtts specAtt
                    foundOld foundNew
                    updated total)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  (setq oldCode (strcase (getstring "\nEnter code to replace (e.g. P-01): ")))
  (setq newCode (strcase (getstring "\nEnter new code (e.g. P-02): ")))

  ; Look up both codes in TOC blocks
  (setq oldDesc "")
  (setq newDesc "")
  (setq foundOld nil)
  (setq foundNew nil)

  (vlax-for obj ms
    (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
             (= (strcase (vla-get-Name obj)) "TOC"))
      (progn
        (setq tocAtts (vlax-invoke obj 'GetAttributes))
        (setq tocCode "")
        ; Read code from this TOC block
        (foreach tocAtt tocAtts
          (if (= (strcase (vlax-get tocAtt 'TagString)) "PAGECODE")
            (setq tocCode (strcase (vlax-get tocAtt 'TextString)))
          )
        )
        ; If matches old or new code, grab the description
        (foreach tocAtt tocAtts
          (if (= (strcase (vlax-get tocAtt 'TagString)) "PAGETITLE")
            (progn
              (if (= tocCode oldCode)
                (progn (setq oldDesc (vlax-get tocAtt 'TextString)) (setq foundOld T))
              )
              (if (= tocCode newCode)
                (progn (setq newDesc (vlax-get tocAtt 'TextString)) (setq foundNew T))
              )
            )
          )
        )
      )
    )
  )

  ; Abort if either code not found
  (if (not foundOld)
    (progn (princ (strcat "\nCode " oldCode " not found in TOC.")) (exit))
  )
  (if (not foundNew)
    (progn (princ (strcat "\nCode " newCode " not found in TOC.")) (exit))
  )

  (princ (strcat "\nReplacing: " oldDesc))
  (princ (strcat "\nWith: "      newDesc))

  ; Scan SPEC blocks, match SPECDESC against oldDesc, replace with newDesc
  (setq updated 0)
  (setq total   0)

  (vlax-for obj ms
    (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
             (= (strcase (vla-get-Name obj)) "SPEC"))
      (progn
        (setq total (1+ total))
        (setq specAtts (vlax-invoke obj 'GetAttributes))
        (foreach specAtt specAtts
          (if (and (= (strcase (vlax-get specAtt 'TagString)) "SPECDESC")
                   (= (vlax-get specAtt 'TextString) oldDesc))
            (progn
              (vlax-put specAtt 'TextString newDesc)
              (vlax-invoke obj 'Update)
              (setq updated (1+ updated))
            )
          )
        )
      )
    )
  )

  (princ (strcat "\nDone. " (itoa updated) " of " (itoa total) " SPEC blocks updated."))
  (princ)
)
