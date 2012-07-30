;;
;; shell and comint stuff
;;
(setq comint-process-echoes t)

(setq explicit-bash-args '("--noediting" "-i"))
 
;; a few things to do on windows (cos default shell is "cmdproxy")
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setq shell-completion-execonly nil)) ;; so that shellscript without .exe work

(defun leo-shell-mode-hook ()
  "cursor keys get mapped"
  (local-set-key '[C-up] (lookup-key global-map [up]))
  (local-set-key '[C-down] (lookup-key global-map [down]))
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-previous-matching-input-from-input))

(add-hook 'shell-mode-hook 'leo-shell-mode-hook)
