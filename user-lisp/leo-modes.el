;;
;; general mode things
;;
(setq default-major-mode 'text-mode)
(setq kill-whole-line t)
(transient-mark-mode t)
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
;; text-mode
;;
(setq-default ispell-program-name "aspell")
(eval-after-load "flyspell"
    '(progn
       (delq (assoc 'mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (delq (assoc 'down-mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(defun leo-text-mode-hook-func ()
  (set-input-method "german-prefix")
  (column-number-mode 1)
  (flyspell-mode 1))
 
(add-hook 'text-mode-hook
           'leo-text-mode-hook-func)
  
(add-hook 'help-mode-hook
           '(lambda () 
              (column-number-mode 1)))

;;
;; special stuff for Day One doentry files
;;
(defun leo-set-dayone-files-modes ()
  (nxml-mode)
  (flyspell-prog-mode))
(add-to-list 'auto-mode-alist '("\\.doentry$" . leo-set-dayone-files-modes))

;;
;; general stuff for programming modes
;;
(defun leo-general-programming-stuff ()
  (define-key c-mode-map [?\M-\C-\\] 'c-indent-line-or-region)
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'leo-general-programming-stuff)

;;
;; ruby mode
;;
(if (and (boundp 'emacs-major-version) ; nil if prior to 19.23
	   (> emacs-major-version 23))     ; nil if prior to 20.0.0
    (progn
      (require 'inf-ruby)

      (push '("simple-ruby" . "irb --prompt simple") 
            inf-ruby-implementations)
      ;; stay with inf mode ruby for history
      (setq inf-ruby-default-implementation "ruby")

      (defun leo-inf-ruby-preoutput-filter (output)
        (if (equal major-mode 'inf-ruby-mode)
            (let ((leo-ret-line-pattern "^\n           *$"))
              (if (string-match leo-ret-line-pattern output)
                  ;; output a marker for output filter if only new prompt
                  "l$pre#o" 
                ;; normal output
                output))
          output))

      (add-hook 'comint-preoutput-filter-functions 'leo-inf-ruby-preoutput-filter)

      (defun leo-inf-ruby-output-filter (output)
        (if (equal major-mode 'inf-ruby-mode)
            (if (string= "l$pre#o" output)
                (delete-backward-char 8))))

      (add-hook 'comint-output-filter-functions 'leo-inf-ruby-output-filter)
      ))

;;
;; lua mode
;;
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
