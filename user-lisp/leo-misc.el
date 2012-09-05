;;
;; customization group
;;
;; ---------------------------------------------------------------------------
(defgroup leos nil
  "leo's miscellanous customization"
  :group 'tools)

;;
;; general helper funcs
;;
;; ---------------------------------------------------------------------------
(defun leo-fill-string-right (string len)
  "Fill STRING on the right side with spaces to the length of LEN."
  (let* ((str-len (length string))
        (add-len (- len str-len)))        
    (if (> add-len 0)
        (concat string (make-string add-len ? ))
      (substring string 0 len))))
;;
;; some working functions for windows
;;
(when (eq system-type 'windows-nt) 
  (defun leo-w32-restore-frame ()
    "Restore a minimized frame"
    (interactive)
    (w32-send-sys-command 61728))
  
  (defun  leo-w32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488))
)  

;;
;; MY version... ;-)
;;
(defun leo-other-window-backward()
  (interactive)
  (other-window -1))

;;
;; MY version... ;-)
;;
(defun leo-switch-to-recent-buffer ()
  "Select a buffer in the current window with out prompting."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;
;; buffer swapping (uses buffer-move.el)
;;

(require 'buffer-move)

(defun leo-buf-move-down-or-up ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (buf-move-up)
      (buf-move-down))))

;;
;; isearch improvements
;;
(defun leo-forward-symbol (&optional arg)
  "Move point forward arg symbols (= words with only \"symbol constituents\" inbetween."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((opoint (point)))
    (while (< arg 0)
      (forward-word -1)  
      (skip-syntax-backward "_w")
      (setq arg (1+ arg)))
    (while (> arg 0)
      (forward-word 1)  
      (skip-syntax-forward "_w")
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))

(defun leo-isearch-yank-symbol ()
  "Pull next symbol (word or more) buffer into search string.

This function is derived from `isearch-yank-word'."
  (interactive)
  (isearch-yank-internal (lambda () (leo-forward-symbol 1) (point))))

(defun leo-isearch-yank-whole-word ()
  "Pull whole word around point from buffer into search string.

This function is derived from isearch-yank-line."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
	  (goto-char isearch-other-end))
     (buffer-substring-no-properties
      (progn (forward-word -1) (point))
      (progn (forward-word 1) (point))))))

(define-key isearch-mode-map "\C-w" 'isearch-yank-word)
(define-key isearch-mode-map [?\C-\S-w] 'leo-isearch-yank-whole-word)
(define-key isearch-mode-map "\C-v" 'leo-isearch-yank-symbol)

;;
;; info mode commands
;;
(defun leo-Info-up-restricted ()
  "Go to the superior node of this node but do not leave this file.
- helpful to stay in the scope of one manual, eg. elisp."
  (interactive)
  (Info-up t))

(add-hook 'Info-mode-hook
	  (lambda ()
            (define-key Info-mode-map "u" 'leo-Info-up-restricted)
            (define-key Info-mode-map "U" 'Info-up)))

;;
;; gnus
;;
(defun leo-start-with-gnus ()
  (interactive)
  (leo-appearance-gnus)
  (gnus))

;;
;; use delphi backups for diff-backup
;; (adds an entry in file-name-handler-alist!)
;;
(defun leo-make-delphi-backup-file-name (file)
  "turns a deplhi file (file.pas) into its backup file (file.~pas)"
  (if (string-match "\\.\\(...\\)$" file)
      (replace-match ".~\\1" t nil file))
  )  

(defun leo-diff-delphi-backup-file-handler (operation &rest args)
  ;; First check for the specific operation that we handle specially.
  (save-match-data
    (if (eq operation 'diff-latest-backup-file) 
        (let* ((fn (car args)))
          (leo-make-delphi-backup-file-name fn))
          ;;(leo-make-delphi-backup-file-name fn))
      ;; Handle any operation we don't know about.
      (let ((inhibit-file-name-handlers
             (cons 'leo-diff-delphi-backup-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

(setq leo-diff-delphi-backup-file-name-handler-entry
      (cons '"\\.pas$\\|\\.dpr$" 'leo-diff-delphi-backup-file-handler))

(push leo-diff-delphi-backup-file-name-handler-entry file-name-handler-alist)

;;
;; ediff
;;
(require 'ediff)

(defvar leo-ediff-first-setup-p nil)

(defun leo-ediff-before-setup ()
  "customize ediff"
  (setq leo-ediff-first-setup-p t))

(defun leo-ediff-after-setup-windows ()
  "customize ediff"
  (when leo-ediff-first-setup-p
    (setq leo-ediff-first-setup-p nil)    
    ;; toggle things
    (if (eq ediff-split-window-function 'split-window-vertically)
        (ediff-toggle-split))
    (if (not ediff-wide-display-p)
        (ediff-toggle-wide-display))))

(defun leo-ediff-quit ()
  "customize ediff"
  (if ediff-wide-display-p
      (ediff-toggle-wide-display)))

(add-hook 'ediff-before-setup-hook 'leo-ediff-before-setup)
(add-hook 'ediff-after-setup-windows-hook 'leo-ediff-after-setup-windows)
(add-hook 'ediff-quit-hook 'leo-ediff-quit)
(add-hook 'ediff-suspend-hook 'leo-ediff-quit)

(defun ediff-display-pixel-width ()
  "--- Changed by Leo: ---
on mac osx: return a smaller display width than 1920 (main montior window width) so that in dual montor mode the emacs frame doesn't stretch over both displays. 
--- End of `Changed'---

Return the width of display's screen in pixels."
  (if (eq system-type 'darwin)
      (min 1850 (display-pixel-width))
    (display-pixel-width)))

;;
;; search function for my emacs files
;;
(defcustom leo-my-emacsfiles-form 
      '(append (list (concat leo-emacs-userroot-path ".emacs") 
                     (concat leo-emacs-userroot-path ".custom"))
               (file-expand-wildcards (concat leo-emacs-userroot-path "user-lisp/*.el"))               
               )
      "*doc"
      :type 'string
      :group 'leos)

(defun leo-search-my-emacsfiles (regexp)
  "Search through emacs user file (as defined in `leo-emacs-userfiles-form' for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch emacs user files (regexp): ")
  (tags-search regexp leo-my-emacsfiles-form))

;;
;; things for note creation
;;
(defcustom leo-notes-directory
  (cond ((or (eq system-type 'windows-nt) 
             (eq system-type 'cygwin) 
             (eq system-type 'darwin))
         "~/Dropbox/snippets")
        (t 
         "~"))
  "*directory where notes are stored"
  :type 'string
  :group 'leos)

(defcustom leo-notes-filename 
  (cond ((or (eq system-type 'windows-nt) (eq system-type 'x))
         "notes-work.txt")
        (t 
         "notes-home.txt"))
  "*generic name for notes"
  :type 'string
  :group 'leos)

(defun leo-notes-find-note ()
  "*open leo's notes file"
  (interactive)
  (find-file (concat leo-notes-directory "/" leo-notes-filename)))
  
(defun leo-switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))
  
(defun leo-switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))
  
(defun leo-notes-flush-percent-lines ()
  "this function deletes all lines ending with percent (\"%\"). it does this by replacing each block of those consecutive lines with the string \"---\"."
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "\\(^.*%$\\)+" "%@%")
  (beginning-of-buffer)
  (replace-regexp "\\(^%@%\\\n\\)+" "---\n")
)
