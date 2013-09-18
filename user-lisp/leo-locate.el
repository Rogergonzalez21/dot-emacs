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
         "/usr/local/var/locate/")
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
  "Correct dired-move-to-filename-regexp for locate-mode 
and change key bindings."

  (setq dired-move-to-filename-regexp
 	(concat "^."
 		(make-string (1- locate-filename-indentation) ?\ )))
  (if (eq system-type 'windows-nt)
      (setq default-directory "c:/"))
  (setq revert-buffer-function nil)

  ;; special key bindings
  (define-key  locate-mode-map [return] 'dired-find-file)
  (define-key  locate-mode-map [S-return] 'leo-dired-find-file)
  (define-key  locate-mode-map "\C-c\C-d"  'leo-locate-choose-db)
  (define-key  locate-mode-map "\C-c\C-b"  'leo-locate-toggle-basename-only)
  (define-key  locate-mode-map "L"  'leo-locate-with-filter)
  (define-key  locate-mode-map "l"  'leo-locate)
  (define-key  locate-mode-map "k"  'leo-locate-keep-filter)
  (define-key  locate-mode-map "f"  'leo-locate-flush-filter)
  ;; don't use leo-dired-find-container: doesn't work properly
  ;; so use build-in locate-find-directory instead
  (define-key locate-mode-map "c"       'locate-find-directory)
  ;; and bind oher-window function
  (define-key locate-mode-map "\C-x4c" 'locate-find-directory-other-window)
  ;; leo-dired-do-shell-command doesn't work right because it reverts the buffer
  ;; so use build-in dired-do-shell-command
  (define-key locate-mode-map "!"       'dired-do-shell-command))

;;
;; Convert output
;; (gets called in locate-do-setup replacement)
;;
(defun leo-locate-convert-output-system-dependend ()
  "Convert output from the locate command to system dependend paths"
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (if (eq system-type 'windows-nt)
        (while (re-search-forward "/c/" nil t)
          (replace-match "c:/" nil nil)))))

;;
;; goto first filename line
;;
(defun leo-locate-goto-first-filename-line ()
  (interactive)
  (goto-char (point-min))
  (goto-char
   (next-single-property-change (point) 'dired-filename nil nil)))
    
;;
;; overwrite locate-do-setup for better not found msg & correct default dir
;;
(defun leo-locate-convert-output-and-do-setup (search-string)
  "Replacement for locate-do-setup with better not-found msg and
correct default dir.
Does converting of output to system dependend paths as well."
  (leo-locate-convert-output-system-dependend)

  (goto-char (point-min))
  (save-excursion
    (let* ((db-err-text (if locate-fcodes-file
                           (format "database %s" locate-fcodes-file)
                         "default database")))
      ;; Nothing returned from locate command?
      (and (eobp)
           (progn
             ;; don't kill buffer keep content
             ;; (kill-buffer locate-buffer-name)
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

      (if (and (boundp 'run-locate-command)
               run-locate-command)
          (insert " ")
        (if leo-locate-basename-only
            (insert " Basename")
          (insert " Wholename")))
      ))
  (goto-char (point-min)))

(defun leo-locate-dired-dont-omit ()
  ;; empty --> omit mode does not get switched on
  )

;;
;; Same as command `locate', but customised
;;
(defun leo-locate (search-string &optional filter arg)
  "Same as command `locate', but customised."
  (interactive
   (list
    (locate-prompt-for-search-string)
    nil
    current-prefix-arg))

  (flet ((pop-to-buffer (BUFFER) 
                        (switch-to-buffer BUFFER))
         (locate-do-setup (search-string) 
                          (leo-locate-convert-output-and-do-setup search-string))
         (leo-dired-manage-omit-mode() 
                                    (leo-locate-dired-dont-omit)))
    (locate search-string filter arg)))

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
;; in buffer filter commands
;;
(defun leo-locate-keep-filter (filter &optional arg)
  "Filters content of locate buffer KEEPING the lines matching a regex"
     (interactive
      (list
       (read-from-minibuffer "Lines to keep: " nil nil
                             nil 'locate-grep-history-list)
       current-prefix-arg))

     (let ((inhibit-read-only t))
       (leo-locate-goto-first-filename-line)
       (keep-lines filter)))     

(defun leo-locate-flush-filter (filter &optional arg)
  "Filters content of locate buffer FLUSHING the lines matching a regex"
     (interactive
      (list
       (read-from-minibuffer "Lines to flush: " nil nil
                             nil 'locate-grep-history-list)
       current-prefix-arg))

     (let ((inhibit-read-only t))
       (goto-char (point-min))
       (flush-lines filter)))     

;;
;; stuff for selecting other locate databases
;;
(defun leo-locate-mod-dbdir-nt (dbfile)
  (let ((case-fold-search t))
    (if (string-match (regexp-quote leo-cygwin-root) dbfile)
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
          (answer 
           (read-file-name "Database file [empty=default]: " 
                                 leo-locate-dbdir nil t nil))
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
  
