(defun leo-locate (search-string &optional filter arg)
  "Same as command `locate', but it doesn't switch to other window."
  (interactive
   (list
    (locate-prompt-for-search-string)
    nil
    current-prefix-arg))

  (if (equal search-string "")
      (error "Please specify a filename to search for"))
  (let* ((locate-cmd-list (funcall locate-make-command-line search-string))
	 (locate-cmd (car locate-cmd-list))
	 (locate-cmd-args (cdr locate-cmd-list))
	 (run-locate-command
	  (or (and arg (not locate-prompt-for-command))
	      (and (not arg) locate-prompt-for-command))))

    ;; Find the Locate buffer
    (save-window-excursion
      (set-buffer (get-buffer-create locate-buffer-name))
      (locate-mode)
      (let ((inhibit-read-only t)
	    (buffer-undo-list t))
	(erase-buffer)

	(setq locate-current-filter filter)
	(set (make-local-variable 'locate-local-search) search-string)
	(set (make-local-variable 'locate-local-filter) filter)
	(set (make-local-variable 'locate-local-prompt) run-locate-command)

	(if run-locate-command
	    (shell-command search-string locate-buffer-name)
	  (apply 'call-process locate-cmd nil t nil locate-cmd-args))

        (leo-locate-convert-output)

	(and filter
	     (locate-filter-output filter))
        
	(locate-do-setup search-string run-locate-command)
	))
    (and (not (string-equal (buffer-name) locate-buffer-name))
	(switch-to-buffer locate-buffer-name))

    (let ((leo-dired-dont-omit t))
      (run-hooks 'dired-mode-hook))
    (dired-next-line 3)			;move to first matching file.
    (run-hooks 'locate-post-command-hook)))
