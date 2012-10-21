;;
;; shell and comint stuff
;;
(setq explicit-bash-args '("--noediting" "-i"))
(setq explicit-fakecygpty-args '("bash" "--noediting"))
 
;; a few things to do on windows (cos default shell is "cmdproxy")
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  ;;(setq shell-file-name "fakecygpty")
  ;;(setq shell-file-name "c:/MinGW/msys/1.0/bin/bash.exe")
  (setq shell-file-name "bash") ;; this should be the cygwin bash
  (setenv "SHELL" shell-file-name)
  (setq shell-completion-execonly nil)) ;; so that shellscript without .exe work

(defun leo-shell-mode-hook ()
  "cursor keys get mapped"
  ;; (local-set-key '[C-up] (lookup-key global-map [up]))
  ;; (local-set-key '[C-down] (lookup-key global-map [down]))
  ;; (local-set-key '[up] 'comint-previous-input)
  ;; (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-previous-matching-input-from-input))

(add-hook 'shell-mode-hook 'leo-shell-mode-hook)

;;
;; eshell stuff
;;

(setq eshell-directory-name 
      (concat leo-emacs-shareddata-path "eshell"))
;; eshell-prompt-function and eshell-prompt-regexp are set in .custom
