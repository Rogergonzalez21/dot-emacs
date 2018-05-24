(defun locate-do-setup (search-string)
  (goto-char (point-min))
  (save-excursion

    ;; Nothing returned from locate command?
    (and (eobp)
	 (progn
	   (kill-buffer locate-buffer-name)
	   (if locate-current-filter
	       (error "Locate: no match for %s in database using filter %s"
		      search-string locate-current-filter)
	     (error "Locate: no match for %s in database" search-string))))

    (locate-insert-header search-string)

    (while (not (eobp))
      (insert-char ?\s locate-filename-indentation t)
      (locate-set-properties)
      (forward-line 1)))
  (goto-char (point-min)))
