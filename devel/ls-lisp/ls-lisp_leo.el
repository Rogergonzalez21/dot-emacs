(defun x-ls-lisp-handle-switches (file-alist switches)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  Default sorting is alphabetic."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (setq my-file-alist (copy-sequence file-alist))
  (or (memq ?U switches)		; unsorted
      ;; Catch and ignore unexpected sorting errors
      (condition-case err
	  (setq file-alist
		(let (index)
		  ;; Copy file-alist in case of error
		  (sort (copy-sequence file-alist) ; modifies its argument!
			(cond ((memq ?S switches)
			       (lambda (x y) ; sorted on size
				 ;; 7th file attribute is file size
				 ;; Make largest file come first
				 (< (nth 7 (cdr y))
				    (nth 7 (cdr x)))))
			      ((setq index (ls-lisp-time-index switches))
			       (lambda (x y) ; sorted on time
				 (ls-lisp-time-lessp (nth index (cdr y))
						     (nth index (cdr x)))))
			      ((memq ?X switches)
			       (lambda (x y) ; sorted on extension
				 (leo-ls-lisp-string-lessp
				  (ls-lisp-extension (car x))
				  (ls-lisp-extension (car y)))))
			      (t
			       (lambda (x y) ; sorted alphabetically
				 (leo-ls-lisp-string-lessp (car x) (car y))))))))
	(error (message "Unsorted (ls-lisp sorting error) - %s"
			(error-message-string err))
	       (ding) (sit-for 2))))	; to show user the message!
  (if (memq ?F switches)		; classify switch
      (setq file-alist (mapcar 'ls-lisp-classify file-alist)))

  (if leo-ls-lisp-dotdirs-first
  ;; Re-sort directories for the special dirs '.' and '..'
      (setq file-alist (leo-move-dotdirs-to-front file-alist (memq ?U switches))))

  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (eq (cadr (setq el (car file-alist))) t) ; directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)
		))))

  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(not (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (not (memq ?U switches))	; unsorted order is reversed
	      (not (memq ?r switches)))	; reversed sort order requested
          ls-lisp-dirs-first)	        ; already reversed
      (nreverse file-alist)
    file-alist))
