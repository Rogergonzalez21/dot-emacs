;;
;; dired functions (are loaded in dired hook)
;;
;; ---------------------------------------------------------------------------

(defun leo-dired-find-file-other-frame ()
  "In dired, visit this file or directory in another frame. 
(like other `...-other-window' funcs)"
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename) t)))

(defun leo-dired-maybe-insert-subdir-ff (dirname &optional switches)
  (dired-maybe-insert-subdir dirname switches)
  (current-buffer))

(defun leo-dired-find-file ()
  "In Dired, visit the file or directory named on this line.
Directories are inserted in the same buffer (like `dired-maybe-insert-subdir')"
  (interactive)
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-directory-functions 'leo-dired-maybe-insert-subdir-ff))
    (dired-find-file)))

;;
;; omitting
;;
;; (if (eq system-type 'darwin) <-- omit them opn windows as well for ipod.
(setq dired-omit-files (concat dired-omit-files 
                               "\\|^\\.DS_Store$\\|^\\."))
(if (not (eq system-type 'darwin))
    (setq dired-omit-files (concat dired-omit-files 
                                   "\\|^CVS$\\|^Root$\\|^Repository$\\|^Entries.Old$\\|^Entries.Log$\\|^Entries.Extra.Old$\\|^Entries.Extra$\\|^Entries$\\|^Template$")))

(defun leo-dired-omit-unmarked-files  (&optional arg)
  "In Dired omit all unmarked files.  With an prefix arg omit the marked files.

This works via marking the files with `dired-omit-marker-char'. So be aware that reverting the buffer will loose this manual omitting."
  (interactive "P")
  (if (not arg)
      (dired-toggle-marks))
  (dired-change-marks dired-marker-char dired-omit-marker-char)
  (if (not arg)
      (dired-toggle-marks))
  (if dired-omit-mode
      (progn
        (dired-omit-mode 0)
        (dired-omit-mode 1))
    (dired-omit-mode 1)))


;;
;; sort circle with unsort
;;
;; ---------------------------------------------------------------------------
(setq dired-ls-sorting-switches 
      (let (case-fold-search)
        (dired-replace-in-string "S" ""
         (dired-replace-in-string "U" "" dired-ls-sorting-switches))))


;; ---------------------------------------------------------------------------
(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name/unsort.  Reverts the buffer.
  (setq dired-actual-switches
	(let (case-fold-search)
	  (if (string-match " " dired-actual-switches)
	      ;; New toggle scheme: add/remove a trailing " -t"
	      (if (string-match " -t\\'" dired-actual-switches)
		  (dired-replace-in-string " -t\\'" "" dired-actual-switches)
		(concat dired-actual-switches " -t"))
	    ;; old toggle scheme: look for some 't' switch and add/remove it
	    (concat
	     "-l"
	     (dired-replace-in-string (concat "[-ltUS"
					      dired-ls-sorting-switches "]")
				      ""
				      dired-actual-switches)
	     (if (string-match 
                  (concat "[t" dired-ls-sorting-switches "]")
                  dired-actual-switches)
		 "S" ;; if it was "t" then "S"
               (if (string-match 
                    (concat "[S" dired-ls-sorting-switches "]")                    
                    dired-actual-switches)
                   "U" ;; if it was "S" then "U"
                 (if (string-match 
                      (concat "[U" dired-ls-sorting-switches "]")
                      dired-actual-switches)
                     "" ;; if it was "U" then ""
                 "t")))))))
  (dired-sort-set-modeline)
  (revert-buffer))

;;
;; replacement for dired-copy-filename: works on the current file, not on the marked files
;;
(defun leo-dired-copy-filename-as-kill (&optional arg)
  "Similarly to `dired-copy-filename-as-kill' copy the filename into the kill ring, 
but use the current file, not the mark file(s).
Invoked with any prefix arg, use the absolute file name filenaem including its path.

You can then feed the file name to other commands with \\[yank]."
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat (function identity)
                        (if arg
                            (dired-get-marked-files nil 1);; wrong! i need something else for "t"
                          (dired-get-marked-files 'no-dir 1))
                        " "))))
    (kill-new string)
    (message "%s" string)))
;;
;; open containing directory to current dired entry
;;
(defun leo-dired-find-container ()
  "In dired, visit the directory which contains the file on this line."
  (interactive)
  (let ((file-name (file-name-directory (dired-get-filename)))
	;; bind it so that the command works on directories too,
	;; independent of the user's setting
	(find-file-run-dired t))
    (if (file-exists-p file-name)
        (if (not (equal file-name list-buffers-directory))
            (find-file file-name)
          (message "Current buffer displays container %s." file-name))
      (if (file-symlink-p file-name)
	  (error "Container %s is a symlink to a nonexistent target." file-name)
	(error "Container %s no longer exists." file-name)))))
;;
;; like dired-up-directory but kills the old dired buffer
(defun leo-dired-up-directory-kill-last-buffer (&optional other-window)
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer. If the 
directory is found in another buffer, the child buffer we came from
gets deleted!
Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	;; Only try dired-goto-subdir if buffer has more than one dir.
	(and (cdr dired-subdir-alist)
	     (dired-goto-subdir up))
	(progn
	  (if other-window
	      (dired-other-window up)
            (progn
              (if (cdr dired-subdir-alist)
                  (if (yes-or-no-p (format "Attempt to kill %s, but has subdirectories; kill anyway? " dir))
                      (kill-buffer (current-buffer)))
                (kill-buffer (current-buffer)))
              (dired up)))
	  (dired-goto-file dir)))))

;;
;; like dired-kill-subdir but go up to directory line as well
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

;;
;;
(defun leo-dired-jump-in-shell ()
  "Jump to dired buffer corresponding to current buffer.
If in a file, find ...
If in dired already, pop up a level and ..."
  (interactive "P")
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (if (eq major-mode 'dired-mode)
        (progn
          (setq dir (dired-current-directory))
          (dired-up-directory other-window)
          (or (dired-goto-file dir)
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory dir))
                (dired-goto-file dir))))
      (if other-window
          (dired-other-window dir)
        (dired dir)))))

(defvar leo-dired-confirmer 'yes-or-no-p) ; or y-or-n-p?
  
(defun leo-shellex-dired-on-marked-objects ()
  "Invoke the Explorer on all dired-marked entries with confirmation - not sure whether this works!"
  (interactive)
  (let ((dmf-list (dired-get-marked-files t current-prefix-arg))
        (index 0))
    ;; canonicalize file list for pop up
    (if (dired-mark-pop-up
	 " *ShellExing*" 'shellex dmf-list leo-dired-confirmer
	 (format "Delete %s " (dired-mark-prompt arg dmf-list)))
	(while (< index (length dmf-list))
;;	  (w32-shellex-on-file (nth index dmf-list))
	  (message (nth index dmf-list))
	  (setq index (1+ index))
	  )
      )
    )
  )

;;
;; ediff in dired
;;
;; ----------------------------------------------------------------------------
(defun leo-dired-ediff (file)
  "Compare file at point with file FILE using `ediff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for file is the first file given to `ediff'."
  (interactive
   (let ((default (if (mark t)
		      (save-excursion (goto-char (mark t))
				      (dired-get-filename t t)))))
     (require 'ediff)
     (list (read-file-name (format "Diff %s with: %s"
				   (dired-get-filename t)
				   (if default
				       (concat "(default " default ") ")
				     ""))
			   (dired-current-directory) default t)
           )))
  (ediff file (dired-get-filename t)))

;;
;; we have to define it here for dired override
;;
(defun dired (dirname &optional switches)
  "--- Changed by Leo: ---
Use `leo-dired-switches' as switches if leo-dired-switches is defined.
Necessary for `leo-ido-dired' because it calls internally via ido-dired 
the function `dired'.
--- End of `Changed'---

  \"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the rest as an explicit
list of files to make directory entries for.
\\<dired-mode-map>\
You can move around in it with the usual commands.
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh."
  (interactive (dired-read-dir-and-switches ""))
  (let ((sw (if (boundp 'leo-dired-switches)
                leo-dired-switches 
              switches)))
    (switch-to-buffer (dired-noselect dirname sw))))

(defun dired-summary ()
  "--- Changed by Leo: ---
display leo's additions.
--- End of `Changed'---
Summarize basic Dired commands and show recent dired errors."
  (interactive)
  (dired-why)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "e/E/F: shellex file/marked/folder, k: kill subdir, c: container"))

;;
;; customize dired-do-shell-command
;;
(load "leo-dired-do-shell")
