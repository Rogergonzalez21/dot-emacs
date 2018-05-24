(defun x ()
  (interactive)
  (delete-region 1 9)
  (self-insert-command
  (insert "1234567890")
)
