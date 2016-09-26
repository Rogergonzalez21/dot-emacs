;;
;; things for document modes
;;
(require `real-auto-save)

;;
;; text-mode
;;
;; (quail-define-package
;;  "leo-german-prefix-dq" "German" "DX>" t
;;  "German (Deutsch) input method with prefix modifiers
;; Key translation rules are:
;;  :A -> Ä ->   :O -> Ö   :U -> Ü   :s -> ß
;; Customisation: \"\" -> \"
;; " nil t nil nil nil nil nil nil nil nil t)

;; (quail-define-rules
;;  (":A" ?Ä)
;;  (":O" ?Ö)
;;  (":U" ?Ü)
;;  (":a" ?ä)
;;  (":o" ?ö)
;;  (":u" ?ü)
;;  (":s" ?ß)
;; )

(defun leo-text-mode-hook-func ()
  ;;(set-input-method "german-prefix")
  (column-number-mode 1)
  (flyspell-mode 1)
  (abbrev-mode 1)
  ;; smart tab
  (require 'smart-tab)
  (smart-tab-mode 1))

 
(add-hook 'text-mode-hook
          'leo-text-mode-hook-func)

(add-hook 'lisp-interaction-mode-hook
          'leo-text-mode-hook-func)

(add-hook 'help-mode-hook
          '(lambda () 
             (column-number-mode 1)))

;;
;; markdown
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq markdown-open-command
      (cond ((eq system-type 'darwin)
             "marked")
            (t
             nil)))

(defun leo-markdown-mode-customisation ()
  "modify local keymap for compile commands"
  (leo-local-compile-keys)
  (local-set-key (kbd "C-c C-c v") 'leo-markdown-view)
  (local-set-key (kbd "C-c C-c s") 'leo-markdown-copy-snippet-other-window))

(add-hook 'markdown-mode-hook
          'leo-markdown-mode-customisation) ;; defined in progmodes.el!

(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;;
;; preview stuff
;;
(defun leo-markdown-preview ()
  ;; mainly for windows where theer is no Marked
  "Run `markdown-command' on the current buffer and view output in browser.

Like `markdown-preview', just with title taken from md buffer name."
  (interactive)
  (let ((title (or (buffer-name) markdown-output-buffer-name)))
    (message (format "Opening %s..." title))
    (browse-url-of-buffer
     (leo-markdown-standalone markdown-output-buffer-name title))))

(defun leo-markdown-standalone (&optional output-buffer-name title)
  "Special function to provide standalone HTML output.

Like `markdown-standalone', just separate title parameter."
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (unless (markdown-output-standalone-p)
      (markdown-add-xhtml-header-and-footer title))
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)

(defun leo-markdown-view ()
  "Call either `markdown-open' or  `(leo-)markdown-preview', depending
whether `markdown-open-command' command is set."
  (interactive)
  (if markdown-open-command
      (markdown-open)
    (leo-markdown-preview)))

;;
;; make snippet (for pasting into stackoverflow etc)
;;
(defun leo-markdown-clean-snippet ()
  "Clean the the html code in the current buffer.

Does currently remove a <p>...</p> pair at the beginning of the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward "\\`\\s-*<p>\\(.*?\\)</p>" nil t)
        (replace-match "\\1")))))
        
(defun leo-markdown-make-snippet (output-buffer-name &optional noclean nocopy)
  "Special function to provide html snippet output"
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (goto-char (point-min))
    (html-mode)
    (when (not noclean)
      (leo-markdown-clean-snippet))
    (when (not nocopy)
      (kill-new (buffer-string))
      (message (format "Saved text of %s to kill ring" output-buffer-name))))
  output-buffer-name)

(defun leo-markdown-copy-snippet-other-window (&optional arg)
  "Run `markdown' on current buffer, clean (html) output up, display it 
in other window and copy buffer to clipboard

With  optional argument C-u do NOT call clean-up function 
`leo-markdown-clean-snippet'."
  (interactive "P")
  (let ((resize-mini-windows nil) ;;; no output in echo area!
        (noclean arg)) ;; no clean-up with C-u argument
    (display-buffer (leo-markdown-make-snippet nil noclean))))

(defun leo-markdown-timestamp ()
   (interactive)
   (insert (format-time-string "%Y-%m-%d %H:%M:%S +0000")))

(defun leo-markdown-set-footnote-counter (cnt)
  (interactive "nSet footnote counter to: ")
  (setq markdown-footnote-counter cnt))

;; flyspell in markdown: \... constructs are not spellchecked
(defun flyspell-ignore-tex-commands ()
  "Function used for `flyspell-generic-check-word-predicate' to ignore stuff starting with \"http\" or \"https\"."
  (save-excursion
    (forward-whitespace -1)
    (when (looking-at " ")  ;; needs more work:
      (forward-char))       ;; does not do the right thing if expression is at the beginning of a line..
    (not (or
          (save-excursion (looking-at "@[a-z]*\\b"))
          (save-excursion (looking-at ".*\\\\[a-z].*\\b"))
             ))
    ))

(put 'markdown-mode 'flyspell-mode-predicate 'flyspell-ignore-tex-commands)
