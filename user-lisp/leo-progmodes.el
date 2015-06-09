;;
;; general stuff for programming modes
;;
(defun leo-prog-mode-hook-func ()
  ;;(column-number-mode 1)
  (flyspell-prog-mode)
  ;; smart tab
  (require 'smart-tab)
  (smart-tab-mode 1))

(add-hook 'prog-mode-hook 'leo-prog-mode-hook-func)

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

;;
;; makefile mode
;;
(defun leo-local-compile-keys ()
  "modify local keymap for compile commands"  
  (local-set-key (kbd "C-c C-p") 'compile)  
  (local-set-key (kbd "C-c p") 'recompile)  
  )
(add-hook 'makefile-mode-hook 'leo-local-compile-keys)

;; set auto0mode list: first delete old stuff, then add new stuff
(dolist (val '('makefile-gmake-mode 'makefile-bsdmake-mode))
  (rassq-delete-all val auto-mode-alist))

(dolist (mapping '(("\\.mk\\'" . makefile-gmake-mode)
               ("\\.make\\'" . makefile-gmake-mode)
               ( "GNUmakefile\\'" . makefile-gmake-mode)
               ( "[Mm]akefile\\'" . makefile-gmake-mode)
               ( "make.*\\.incl$" . makefile-gmake-mode)))               
  (add-to-list 'auto-mode-alist mapping))
  
