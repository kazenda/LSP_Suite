; PLACEFURN - Auto places FURNLIST blocks with sequential FURNCODE and FURNTITLE
; Asks for code prefix, title base, start number, end number and placement point
; Places blocks in a column

(defun c:PF (/ prefix titleBase startNum endNum i codeStr titleStr pt blkObj attribs att spacing doc ms)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  ; Get user input
  (setq prefix    (strcase (getstring "\nEnter furniture code prefix (e.g. CA, T): ")))
  (setq titleBase (getstring T "\nEnter title base (e.g. cabinet, table): "))
  (setq startNum  (getint "\nStart number: "))
  (setq endNum    (getint "\nEnd number: "))
  (setq pt        (getpoint "\nClick placement point for first block: "))
  (setq spacing   140) ; vertical spacing in drawing units, adjust as needed

  (if (and prefix titleBase startNum endNum pt)
    (progn
      (setq i startNum)
      (while (<= i endNum)

        ; Format code e.g. CA1, T1 (no dash, no leading zero)
        (setq codeStr (strcat prefix (itoa i)))

        ; Format title e.g. "cabinet 1", "table 1"
        (setq titleStr (strcat titleBase " " (itoa i)))

        ; Insert FURNLIST block
        (setq blkObj
          (vla-InsertBlock ms
            (vlax-3d-point pt)
            "FURNLIST"
            1.0 1.0 1.0 0.0)
        )

        ; Write FURNCODE and FURNTITLE attributes
        (setq attribs (vlax-invoke blkObj 'GetAttributes))
        (foreach att attribs
          (if (= (strcase (vlax-get att 'TagString)) "FURNCODE")
            (vlax-put att 'TextString codeStr)
          )
          (if (= (strcase (vlax-get att 'TagString)) "FURNTITLE")
            (vlax-put att 'TextString titleStr)
          )
        )
        (vlax-invoke blkObj 'Update)

        (princ (strcat "\nPlaced: " codeStr " - " titleStr))

        ; Move down for next block
        (setq pt (list (car pt) (- (cadr pt) spacing) 0.0))
        (setq i (1+ i))
      )
      (princ "\nAll FURNLIST blocks placed!")
    )
    (princ "\nCancelled.")
  )
  (princ)
)