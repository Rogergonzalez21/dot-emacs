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
