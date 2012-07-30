(defun myprint (x)
  (message x))


(defun call-myprint (a)
  (myprint a))

(progn
  (flet ((myprint (x) t))
    (call-myprint "hallo")
    )
  (sit-for 1)
  (call-myprint "hallo")
  (sit-for 1)
  (flet ((myprint (x) (message "bong!")))
    (call-myprint "hallo"))
)
    



(setq x nil)
(setq this
      (cond ((eq window-system 'mac)
             (if (> (display-pixel-height) 1000)
                 60
               50))
            ((eq window-system 'w32)
             8)
            (t 9)))

(print this)

             (append default-frame-alist 
                     '((font . "-apple-monaco-medium-r-*--12-*-*-*-*-*-mac-roman"))))
