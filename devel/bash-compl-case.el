(defun bash-completion-send--debug-around (orig-fun &rest args)
  "print what is sent to bash-completion-send."
  
  (message (format "---bcs-car: %s" (car args)))
  (apply orig-fun args))
(advice-add 'bash-completion-send :around #'bash-completion-send--debug-around)
