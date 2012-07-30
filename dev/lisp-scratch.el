(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (modify-syntax-entry ?- "w")       ; now '-' is not considered a word-delimiter
             ))