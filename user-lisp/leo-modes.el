;;
;; general mode things
;;

(setq kill-whole-line t)
(delete-selection-mode 1)
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard t))

(fset 'yes-or-no-p 'y-or-n-p)
(modify-syntax-entry ?' ".   " text-mode-syntax-table)

;;; add these lines if you like color-based syntax highlighting
(global-font-lock-mode t)

;;
;; stuff for truncate-lines
;;
(setq-default truncate-lines nil)

;; like toggle-truncate-lines just without message
(defun leo-toggle-truncate-lines-silent (&optional arg)
  "Toggle whether to fold or truncate long lines for the current buffer.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise don't truncate them.  Note that in side-by-side
windows, this command has no effect if `truncate-partial-width-windows'
is non-nil.

Note: like `toggle-truncate-lines' just without message."
  (interactive "P")
  (setq truncate-lines
	(if (null arg)
	    (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
		      (if (eq buffer (window-buffer window))
			  (set-window-hscroll window 0)))
		    nil t))))


;;
;; special modes
;;

;; magit
 (setq magit-last-seen-setup-instructions "1.4.0")
