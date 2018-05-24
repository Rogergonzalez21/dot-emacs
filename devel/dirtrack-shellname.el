(setq shell-dirstack-query "cygpath -m `dirs`")

(defun xcd (str)
  (interactive "DEnter dir")
  (shell-process-cd str))

 '(cygwin-mount-table
   (quote
    (("C:\\\\" . "/c")
     ("C:\\\\cygwin\\\\bin" . "/usr/bin")
     ("C:\\\\cygwin\\\\lib" . "/usr/lib"))))
