(defun leo-dired-kill-subdir-go-up (&optional remember-marks)
  "Remove all lines of current subdirectory and go to line of that subdirectory.
Lower levels are unaffected."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive)
  (let* ((beg (dired-subdir-min))
	 (end (dired-subdir-max))
	 (modflag (buffer-modified-p))
	 (cur-dir (dired-current-directory))
	 (cons (assoc-string cur-dir dired-switches-alist))
	 buffer-read-only)
    (if (equal cur-dir default-directory)
	(error "Attempt to kill top level directory"))
    (prog1
	(if remember-marks (dired-remember-marks beg end))
      (delete-region beg end)
      (if (eobp)			; don't leave final blank line
	  (delete-char -1))
      (dired-unsubdir cur-dir)
      (dired-goto-file cur-dir)        ; leo: goto that directory  line
      (when cons
	(setq dired-switches-alist (delete cons dired-switches-alist)))
      (restore-buffer-modified-p modflag))))
