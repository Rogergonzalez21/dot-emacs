(defun ask-user-about-supersession-threat-orig (fn)
  "Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called."
  (discard-input)
  (save-window-excursion
    (let ((prompt
	   (format "%s changed on disk; \
rrrrrrrrrrrrrrrrrrrrrrrrrrrreally edit the buffer? (y, n, r or C-h) "
		   (file-name-nondirectory fn)))
	  (choices '(?y ?n ?r ?? ?\C-h))
	  answer)
      (while (null answer)
	(setq answer (read-char-choice prompt choices))
	(cond ((memq answer '(?? ?\C-h))
	       (ask-user-about-supersession-help)
	       (setq answer nil))
	      ((eq answer ?r)
	       ;; Ask for confirmation if buffer modified
	       (revert-buffer nil (not (buffer-modified-p)))
	       (signal 'file-supersession
		       (list "File reverted" fn)))
	      ((eq answer ?n)
	       (signal 'file-supersession
		       (list "File changed on disk" fn)))))
      (message
       "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))

(defun ask-user-about-supersession-threat-new (fn)
  "Overwrite without question"
  (discard-input)
  (save-window-excursion
      (message
       "File on disk noooooooooooooooow will become a backup file if you save these changes.")
      (setq buffer-backed-up nil)))
