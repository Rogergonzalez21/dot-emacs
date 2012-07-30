;;
;; locate
;;
(require 'locate)

(defcustom leo-locate-dbdir 
  (cond ((eq system-type 'windows-nt)
         "c:/cygwin/usr/local/var/")
        ((eq system-type 'cygwin)
         "/usr/local/var/")
        ((eq system-type 'darwin)
         "/opt/local/var/")
        (t 
         "/usr/var"))
  "*directory with the locate databases"
  :type 'string
  :group 'leos
  :group 'locate)

(defcustom leo-locate-basename-only nil
  "*locate is called with the \"--basename\" option.
this restricts the search to the basename part of the filename"
  :type 'boolean
  :group 'leos
  :group 'locate)

(defcustom leo-locate-custom-args "--max-database-age 1"
  "*custom arguments for locate"
  :type 'boolean
  :group 'leos
  :group 'locate)

(setq locate-filename-indentation 2)

(setq locate-command
      (cond ((eq system-type 'darwin)
             "glocate")
            (t
             "locate")))

(defface locate-header
  '((t (:inherit font-lock-function-name-face)))
  "Face used to highlight the locate header."
  :group 'locate
  :version "22.1")
(defvar locate-header-face 'locate-header
  "Face used to highlight the locate header.")

(defun leo-locate-make-command-line (arg)
  ;;3/01/2008: reversed back to standard way: test case (on pc) was "bronte storms"
  (let ((l (list arg))) ;; standard way: no quoting
;;  (let ((l (list (format "\042%s\042" arg)))) ;; quoting with double quotes (doesn't work on win32/cygwin)
    (if case-fold-search
        (push "-i" l))
    (if leo-locate-basename-only
        (push "--basename" l))
    (if locate-fcodes-file
        (push (concat "--database=" locate-fcodes-file) l))
    (if leo-locate-custom-args
        (setq l (append (split-string leo-locate-custom-args) l)))
    (push locate-command l)
    l))

(setq locate-make-command-line 'leo-locate-make-command-line)

;;
;; Correct dired-move-to-filename-regexp for locate-mode
;;
(defadvice locate-mode (after correct-dired-move-var activate)
  "Correct dired-move-to-filename-regexp for locate-mode"
  (setq dired-move-to-filename-regexp
	(concat "^."
		(make-string (1- locate-filename-indentation) ?\ )))
  (if (eq system-type 'windows-nt)
      (setq default-directory "c:/"))
  (setq revert-buffer-function nil))

(defadvice locate-mode (after correct-dired-move-var activate)
  "Correct dired-move-to-filename-regexp for locate-mode and change key bindings."
  (setq dired-move-to-filename-regexp
 	(concat "^."
 		(make-string (1- locate-filename-indentation) ?\ )))
  (if (eq system-type 'windows-nt)
      (setq default-directory "c:/"))
  (setq revert-buffer-function nil)

  ;; special key bindings
  (define-key  locate-mode-map [return] 'dired-advertised-find-file)
  (define-key  locate-mode-map [S-return] 'leo-dired-advertised-find-file)
  (define-key  locate-mode-map "\C-c\C-d"  'leo-locate-choose-db)
  (define-key  locate-mode-map "\C-c\C-b"  'leo-locate-toggle-basename-only)
  (define-key  locate-mode-map "l"  'leo-locate-with-filter)
  (define-key  locate-mode-map "L"  'leo-locate)
  ;; don't use leo-dired-find-container: doesn't work properly
  ;; so use build-in locate-find-directory instead
  (define-key locate-mode-map "c"       'locate-find-directory)
  ;; and bind oher-window function
  (define-key locate-mode-map "\C-x4c" 'locate-find-directory-other-window)
  ;; leo-dired-do-shell-command doesn't work right because it reverts the buffer
  ;; so use build-in dired-do-shell-command
  (define-key locate-mode-map "!"       'dired-do-shell-command))


;;
;; overwrite locate-do-setup for better not found msg & correct default dir
;;
(defun locate-do-setup (search-string &optional run-locate-command)
  (goto-char (point-min))
  (save-excursion

    ;; Nothing returned from locate command?
    (let (db-err-text)

      (and (eobp)
           (progn
;;	   (kill-buffer locate-buffer-name)
             (setq db-err-text (if locate-fcodes-file
                               (format "database %s" locate-fcodes-file)
                             "default database"))
             (setq db-err-text (if locate-current-filter
                                   (format "no match in %s using filter %s"
                                           db-err-text locate-current-filter)
                                 (format "no match in %s" db-err-text)))))

      (locate-insert-header (concat search-string))
      (when (eobp)
        (insert-char ?\  locate-filename-indentation t)
        (insert db-err-text))

      (while (not (eobp))
        (insert-char ?\  locate-filename-indentation t)
        (locate-set-properties)
        (forward-line 1))
      (goto-char (point-min))
      (forward-char 2)
      (delete-backward-char 1)      
      (if (eq system-type 'windows-nt)
          (insert "c:"))
      (goto-char (point-min))
      (forward-line 1)

      (if run-locate-command
          (insert " ")
        (if leo-locate-basename-only
            (insert " Basename")
          (insert " Wholename")))
      ))    
  (goto-char (point-min)))

(defun leo-locate-convert-output ()
  "Convert output from the locate command to system dependend paths"
  (goto-char (point-min))
  (if (eq system-type 'windows-nt)
      (while (re-search-forward "/c/" nil t)
        (replace-match "C:/" nil nil))))

;;
;; Same as command `locate', but it doesn't switch to other window.
;;
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
	      (and (not arg) locate-prompt-for-command)))
	 )
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
    (run-hooks 'locate-post-command-hook)
    )
  )
;;
;; Same as command `locate-with-filter', but it doesn't switch to other window.
;;
(defun leo-locate-with-filter (search-string filter &optional arg)
  "Same as command `locate-with-filter', but it doesn't switch to other window."
  (interactive
   (list
    (locate-prompt-for-search-string)
    (read-from-minibuffer "Filter: " nil nil
			  nil 'locate-grep-history-list)
    current-prefix-arg))
  (leo-locate search-string filter arg))

;;
;; stuff for selecting other locate databases
;;
(defun leo-locate-mod-dbdir-nt (dbfile)
  (let ((case-fold-search t))
    (if (string-match (regexp-quote "c:/cyg/") dbfile)
        (replace-match "/" nil nil dbfile)
      dbfile)))

(defun leo-locate-modify-dbdir (dbfile)
      (cond ((eq system-type 'windows-nt)
             (leo-locate-mod-dbdir-nt dbfile))
            (t dbfile)))

(defun leo-locate-choose-db (dbfile)
  "choose the db-file for locate"
  (interactive 
   (let* ((insert-default-directory nil)
          (answer (read-file-name "Database file [empty=default]: " leo-locate-dbdir nil t nil))
          (dbfile (if (not (equal answer ""))
           (leo-locate-modify-dbdir 
                       (expand-file-name answer leo-locate-dbdir)))))
     (setq leo-locate-db-prev answer)
     (list dbfile)))
  (setq locate-fcodes-file dbfile))

(defun leo-locate-toggle-basename-only (&optional arg)
  "Toggle whether to match the locate string only against the base name of the file or 
against the full path with filename.
With prefix argument ARG, mtach the basename only if ARG is positive,
otherwise match the full path."
  (interactive "P")
  (setq leo-locate-basename-only
	(if (null arg)
	    (not leo-locate-basename-only)
	  (> (prefix-numeric-value arg) 0)))
  (message "basename-only matching for locate %s"
	   (if leo-locate-basename-only "enabled" "disabled")))
  
