;;
;; dired functions for dired-do-shell-command etc
;;
;;
;; enhancments for dired-do-shell-command
;;
(setq dired-guess-shell-alist-user
   (list 
    (list "\\(^\\|/\\)NUL$" "nuldel")
    (list "\\.jpe?g$" "printimgtags" "exiftool -s -c \"%.6f\"")
    (list "\\.pdf$" "showpdf")))
    

;;(setq dired-guess-shell-alist-default nil)

;;
;; my own guess UI
;;
(defvar leo-dired-guess-default-list  nil
  "Holds the the defaults for `leo-dired-guess-yank'")

(defvar leo-minibuffer-local-dired-guess-map minibuffer-local-map
  "Local keymap for minibuffer input when doing dired guessing")


(defun leo-dired-guess-shell-command (prompt files)
  "Ask user with PROMPT for a shell command, guessing a default from FILES."

  (let ((default (dired-guess-default files))
        default-list old-history val (failed t))

    (if (null default)
        ;; Nothing to guess
        (read-from-minibuffer prompt nil leo-minibuffer-local-dired-guess-map nil 'dired-shell-command-history)

      ;; Save current history list
      (setq old-history dired-shell-command-history)

      (if (listp default)

          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))

        ;; Just one guess
        (setq default-list (list default)))
      
      (setq leo-dired-guess-default-list default-list)

      ;; Push all guesses onto history so that they can be retrieved with M-p
      ;; and put the first guess in the prompt but not in the initial value.
      (setq dired-shell-command-history
            ;;(append default-list dired-shell-command-history)
            dired-shell-command-history ;; ,-- leo: don't push guesses onto history!
            prompt (concat prompt (format "[%s] " default)))

      ;; The unwind-protect returns VAL, and we too.
      (unwind-protect
          ;; BODYFORM
          (progn
            (setq val (read-from-minibuffer prompt nil leo-minibuffer-local-dired-guess-map nil
                                            'dired-shell-command-history)
                  failed nil)
            ;; If we got a return, then use default.
            (if (equal val "")
                (setq val default))
            val)

        ;; UNWINDFORMS
        ;; Undo pushing onto the history list so that an aborted
        ;; command doesn't get the default in the next command.
        (setq dired-shell-command-history old-history)
        (if (not failed)
            (or (equal val (car-safe dired-shell-command-history))
                (setq dired-shell-command-history
                      (cons val dired-shell-command-history))))))))


;;; REDEFINE.
;;; Redefine dired-aux.el's version:
(defun dired-read-shell-command (prompt arg files)
  "Read a dired shell command prompting with PROMPT (using read-string).
ARG is the prefix arg and may be used to indicate in the prompt which
  files are affected.
This is an extra function so that you can redefine it."
  (dired-mark-pop-up
   nil 'shell files
   'leo-dired-guess-shell-command
   (format prompt (dired-mark-prompt arg files)) ; PROMPT
   files))                                       ; FILES

(defun leo-dired-guess-yank ()
  (interactive)
  (if leo-dired-guess-default-list
      (progn
        (goto-char (point-max))
        (delete-minibuffer-contents)
        (insert (car leo-dired-guess-default-list))
        (setq leo-dired-guess-default-list 
              (append (cdr leo-dired-guess-default-list) 
                      (list (car leo-dired-guess-default-list))))
        (goto-char (or minibuffer-temporary-goal-position (point-max)))
        )
    (message "no guesses found")))

(define-key leo-minibuffer-local-dired-guess-map [?\M-\C-g] 'leo-dired-guess-yank)

;;
;; dired-do-shell-command replacement
;;
(defun leo-dired-do-shell-command (command &optional arg file-list)
  "like `dired-do-shell-command' but it doesn't raise the mini-buffer and 
redisplays the dired buffer."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command (concat "! on "
					"%s: ")
				current-prefix-arg
				files)
      current-prefix-arg
      files)))
  (let* ((resize-mini-windows nil)
         (on-each (not (string-match dired-star-subst-regexp command)))
	 (subst (not (string-match dired-quark-subst-regexp command)))
	 (star (not (string-match "\\*" command)))
	 (qmark (not (string-match "\\?" command))))
    ;; Get confirmation for wildcards that may have been meant
    ;; to control substitution of a file name or the file name list.
    (if (cond ((not (or on-each subst))
	       (error "You can not combine `*' and `?' substitution marks"))
	      ((and star (not on-each))
	       (y-or-n-p "Confirm--do you mean to use `*' as a wildcard? "))
	      ((and qmark (not subst))
	       (y-or-n-p "Confirm--do you mean to use `?' as a wildcard? "))
	      (t))
        (progn
          (if on-each
              (dired-bunch-files
               (- 10000 (length command))
               (function (lambda (&rest files)
                           (dired-run-shell-command
                            (dired-shell-stuff-it command files t arg))))
               nil
               file-list)
            ;; execute the shell command
            (dired-run-shell-command
             (dired-shell-stuff-it command file-list nil arg)))
          (let ((msg (current-message)))
            (dired-revert)
            (message msg))))))
