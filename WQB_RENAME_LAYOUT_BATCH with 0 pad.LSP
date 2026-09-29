(defun pad2 (n / s)
  (setq s (itoa n))
  (if (< (strlen s) 2)
    (setq s (strcat "0" s))
  )
  s
)

(defun c:WQB (/ prefix start-num layouts i name doc lo-collection sorted-layouts layout-count)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq lo-collection (vla-get-layouts doc))
  (setq layout-count (vla-get-count lo-collection))
  
  ; Build list with tab order
  (setq sorted-layouts '())
  (setq i 0)
  (while (< i layout-count)
    (setq lay (vla-item lo-collection i))
    (if (not (= (vla-get-name lay) "Model"))
      (setq sorted-layouts 
        (cons (list (vla-get-taborder lay) (vla-get-name lay) lay) 
              sorted-layouts))
    )
    (setq i (1+ i))
  )
  
  ; Sort by tab order
  (setq sorted-layouts 
    (vl-sort sorted-layouts '(lambda (a b) (< (car a) (car b)))))
  
  (setq prefix (getstring T "\nEnter prefix (or leave blank for just numbers): "))
  (setq start-num (atoi (getstring "\nStart from number: ")))
  
  (setq i 0)
  (foreach lay sorted-layouts
    (setq name (strcat prefix (pad2 (+ start-num i))))
    (vla-put-name (caddr lay) name)
    (princ (strcat "\n" (cadr lay) " → " name))
    (setq i (1+ i))
  )
  (princ "\nDone!")
  (princ)
)