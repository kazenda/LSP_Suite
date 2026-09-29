; PLACETOC - Auto places TOC blocks with sequential page codes
; Asks for prefix, start number, end number and placement point
; Places blocks in a column, PAGETITLE left blank

(defun c:PT (/ prefix startNum endNum i codeStr pt blk blkObj attribs att spacing doc ms)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  ; Get user input
  (setq prefix   (strcase (getstring "\nEnter page code prefix (e.g. FURN): ")))
  (setq startNum (getint "\nStart number: "))
  (setq endNum   (getint "\nEnd number: "))
  (setq pt       (getpoint "\nClick placement point for first block: "))
  (setq spacing  140) ; vertical spacing in drawing units, adjust as needed

  (if (and prefix startNum endNum pt)
    (progn
      (setq i startNum)
      (while (<= i endNum)

        ; Format code with leading zero e.g. FURN-01
        (if (< i 10)
          (setq codeStr (strcat prefix "-0" (itoa i)))
          (setq codeStr (strcat prefix "-" (itoa i)))
        )

        ; Insert block using vla
        (setq blkObj
          (vla-InsertBlock ms
            (vlax-3d-point pt)
            "TOC"
            1.0 1.0 1.0 0.0)
        )

        ; Write attributes
        (setq attribs (vlax-invoke blkObj 'GetAttributes))
        (foreach att attribs
          (if (= (strcase (vlax-get att 'TagString)) "PAGECODE")
            (vlax-put att 'TextString codeStr)
          )
          (if (= (strcase (vlax-get att 'TagString)) "PAGETITLE")
            (vlax-put att 'TextString "")
          )
        )
        (vlax-invoke blkObj 'Update)

        (princ (strcat "\nPlaced: " codeStr))

        ; Move down for next block
        (setq pt (list (car pt) (- (cadr pt) spacing) 0.0))
        (setq i (1+ i))
      )
      (princ "\nAll TOC blocks placed!")
    )
    (princ "\nCancelled.")
  )
  (princ)
)
