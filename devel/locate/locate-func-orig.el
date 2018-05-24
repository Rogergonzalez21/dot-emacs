(defun locate (search-string &optional filter arg)
  "Run the program `locate', putting results in `*Locate*' buffer.
Pass it SEARCH-STRING as argument.  Interactively, prompt for SEARCH-STRING.
With prefix arg ARG, prompt for the exact shell command to run instead.

This program searches for those file names in a database that match
SEARCH-STRING and normally outputs all matching absolute file names,
one per line.  The database normally consists of all files on your
system, or of all files that you have access to.  Consult the
documentation of the program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)

You can specify another program for this command to run by customizing
the variables `locate-command' or `locate-make-command-line'.

The main use of FILTER is to implement `locate-with-filter'.  See
the docstring of that function for its meaning.

After preparing the results buffer, this runs `dired-mode-hook' and
then `locate-post-command-hook'."
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

	(and filter
	     (locate-filter-output filter))

	(locate-do-setup search-string)))
    (and (not (string-equal (buffer-name) locate-buffer-name))
	(pop-to-buffer locate-buffer-name))

    (run-hooks 'dired-mode-hook)
    (dired-next-line 3)			;move to first matching file.
    (run-hooks 'locate-post-command-hook)))
