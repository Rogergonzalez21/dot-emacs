;;
;; shell and comint stuff
;;
(setq explicit-bash-args '("--noediting" "-i"))
(setq explicit-fakecygpty-args '("bash" "--noediting"))
 
;; a few things to do on windows (cos default shell is "cmdproxy")
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (add-to-list 'explicit-bash-args '"+m" t) 
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m)
  (setq shell-file-name "bash") ;; this should be the cygwin bash
  (setenv "SHELL" shell-file-name)
  (setq shell-completion-execonly nil) ;; so that shellscript without .exe work
  ;;(setq shell-file-name "fakecygpty")
  ;;(setq shell-file-name "c:/MinGW/msys/1.0/bin/bash.exe")
)

(defun leo-shell-mode-hook ()
  "cursor keys get mapped"
  (local-set-key '[C-up] (lookup-key global-map [up]))
  (local-set-key '[C-down] (lookup-key global-map [down]))
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-previous-matching-input-from-input))

(add-hook 'shell-mode-hook 'leo-shell-mode-hook)

(defun leo-comint-init-without-echo () 
  (setq comint-process-echoes t)) 
(add-hook 'comint-mode-hook 'leo-comint-init-without-echo) 

;;
;; functions for create new shell and switch to shell
;;
(defun leo-shell-new-shell (&optional dir)
  "Run an inferior shell with directory DIR in a new buffer"
  (interactive
      (list
       (expand-file-name
        (read-directory-name
         "New shell in directory: " default-directory default-directory
         nil nil))))
  (let ((buffer (generate-new-buffer-name "*shell*")))
    (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                         (comint-check-proc (current-buffer)))
                     (get-buffer-create (or buffer "*shell*"))
                   ;; If the current buffer is a dead shell buffer, use it.
                   (current-buffer)))

    ;; set the local vars in the shell buffer buffer
    (with-current-buffer buffer
      (set (make-local-variable 'explicit-shell-file-name)
           (file-remote-p
            (expand-file-name  "/bin/bash")
            'localname))
      (setq default-directory dir))
    ;; create the shell via the normal shell command
    (shell buffer)))

(defun leo-shell-switch-shell()
  (interactive)
  (let* ((bl (save-excursion
              (delq
               nil
               (mapcar (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (if (eq major-mode 'shell-mode)
                                 (buffer-name buf)))))
                       (buffer-list)))))
         (bname (if (= (length bl) 1)
                     (car bl)
                   (ido-completing-read "Shell:" bl nil t nil nil ))))
      (switch-to-buffer bname)))


(defun leo-shell (&optional new)
  "Switch to shell or with argument create new one."
  (interactive "P")
  (if new
      (call-interactively 'leo-shell-new-shell)
    (leo-shell-switch-shell)))


;;
;; eshell stuff
;;

(setq eshell-directory-name 
      (concat leo-emacs-shareddata-path "eshell"))
;; eshell-prompt-function and eshell-prompt-regexp are set in .custom
