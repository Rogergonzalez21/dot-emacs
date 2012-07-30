;;
;; CVS
;;
(eval-when-compile (require 'cl))
(require 'pcvs)

(setq cvs-program "c:/Software/cvsnt/cvs.exe")

;;
;; do commands on cvs files
;;
(defun-cvs-mode (leo-cvs-mode-shell-command . SIMPLE) ()
  "Run shell-command on all selected files."
  (interactive)
  (let ((marked (cvs-get-marked (cvs-ignore-marks-p "byte-compile")))
        fis command)
    (dolist (fi marked)
      (push (cvs-fileinfo->full-path fi) fis))
    (setq command (read-string (format "shell command on %d cvs files: " (length fis)) 
                               nil 'dired-shell-command-history nil nil))
    (dired-do-shell-command command nil fis)))

(defun leo-check-regexp-in-file (regexp fn)
  "Check in file fn whether it matches the regexp regexp."
  (when (and fn (file-readable-p fn)
             (not (file-directory-p fn)))
    (message "Checking %s" fn)
    ;; For now we do it inside emacs. Grep might be better for a lot of files
    (with-temp-buffer
      (insert-file-contents fn)
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun-cvs-mode leo-cvs-mode-mark-files-containing-regexp (regex)
  "Mark all files matching REGEX."
  (interactive "sMark files contents matches: ")
  (ewoc-map (lambda (cookie)
	      (when (and (not (eq (cvs-fileinfo->type cookie) 'DIRCHANGE))
			 (leo-check-regexp-in-file 
                          regex (cvs-fileinfo->full-path cookie)))
		(setf (cvs-fileinfo->marked cookie) t)))
	    cvs-cookies))

(define-key cvs-mode-map "*" nil)
(define-key cvs-mode-map "*!" 'cvs-mode-unmark-all-files)
(define-key cvs-mode-map "*%" 'cvs-mode-mark-matching-files)
(define-key cvs-mode-map "%" nil)
(define-key cvs-mode-map "%m" 'cvs-mode-mark-matching-files)
(define-key cvs-mode-map "%g" 'leo-cvs-mode-mark-files-containing-regexp)
