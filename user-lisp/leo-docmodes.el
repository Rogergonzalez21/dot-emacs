;;
;; things for document modes
;;

;;
;; text-mode
;;
(quail-define-package
 "leo-german-prefix-dq" "German" "DX>" t
 "German (Deutsch) input method with prefix modifiers
Key translation rules are:
 \"A -> Ä ->   \"O -> Ö   \"U -> Ü   \"s -> ß
Customisation: \"\" -> \"
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"A" ?Ä)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"o" ?ö)
 ("\"u" ?ü)
 ("\"s" ?ß)
 ("\"\"" ?\")
)

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
;; markdown
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(dolist (e '("\\.Rmd$" "\\.md$" "\\.markdown$" "\\.txt$")) 
  (add-to-list 'auto-mode-alist (cons e 'markdown-mode)))

(defun leo-markdown-snippet (&optional output-buffer-name)
  "special function to provide html snippet output"
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)

(defun leo-markdown-snippet-other-window (&optional output-buffer-name)
  " Run `markdown' on current buffer and display in other window"
  (interactive)
  (display-buffer (leo-markdown-snippet output-buffer-name)))

(eval-after-load "markdown"
    '(progn
       (define-key markdown-mode-map "\C-c\C-cs" 'leo-markdown-snippet-other-window)))

(defun leo-markdown-timestamp ()
   (interactive)
   (insert (format-time-string "%Y-%m-%d %H:%M:%S +0000")))

;;
;; special stuff for Day One doentry files
;;
(defun leo-set-dayone-files-modes ()
  (nxml-mode)
  (flyspell-prog-mode))
(add-to-list 'auto-mode-alist '("\\.doentry$" . leo-set-dayone-files-modes))

