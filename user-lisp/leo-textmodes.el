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

(defun leo-markdown-mode-customisation ()
  "modify local keymap for compile commands"
  (leo-local-compile-keys))

(add-hook 'markdown-mode-hook
          'leo-markdown-mode-customisation) ;; defined in progmodes.el!

(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

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

(eval-after-load 'markdown-mode
    '(progn
       (define-key markdown-mode-map "\C-c\C-cs" 
         'leo-markdown-copy-snippet-other-window)))

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

;;
;; special stuff for Day One doentry files
;; Attention: They are in nxml mode, which is a PROG mode!!!
;; Note: This can be deleted at some stage, because Day one V2 doesn't use doentry files anymore
(defun leo-set-dayone-files-modes ()
  (nxml-mode))

(add-to-list 'auto-mode-alist '("\\.doentry$" . leo-set-dayone-files-modes))

