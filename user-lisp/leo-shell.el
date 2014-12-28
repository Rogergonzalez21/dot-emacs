;;
;; shell and comint stuff
;;
(setq explicit-bash-args (list "--login" "--noediting" "-i" ))
 
;; a few things to do on windows (cos default shell is "cmdproxy")
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (add-to-list 'explicit-bash-args '"+m" t) ;; no job conrol under windows!
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m)
  (setq shell-file-name "bash") ;; this should be the cygwin bash
  (setenv "SHELL" shell-file-name)
  (setq shell-completion-execonly nil) ;; so that shellscript without .exe work
)

;;  bash-completion
(if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
    ;; cygwin
    (progn
      ;;(require 'bash-completion)
      ;;(setq bash-completion-prog "C:/cygwin/bin/bash") ;; path to cygwin bash
      ;;(bash-completion-setup)
      nil)
  ;; normal
  (require 'bash-completion)
  (bash-completion-setup))


(defun leo-shell-mode-hook ()
  "cursor keys get mapped"
  (local-set-key '[C-up] (lookup-key global-map [up]))
  (local-set-key '[C-down] (lookup-key global-map [down]))
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-previous-matching-input-from-input))
(add-hook 'shell-mode-hook 'leo-shell-mode-hook)

;;(defun leo-comint-init-without-echo () 
;;  (setq comint-process-echoes t)) 
;;(add-hook 'comint-mode-hook 'leo-comint-init-without-echo) 

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
  (let ((buffer (generate-new-buffer-name (format "*shell*[%s]" dir))))
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
      (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
          (setenv "LEOSEMACSSHELLSTARTDIR" dir)) ;; only need for cygwin!
      (setq default-directory dir))
    ;; create the shell via the normal shell command
    (shell buffer)))

(defcustom leo-shell-first-shell-default-directory
  "~/tmp/"
  "*default directory the first shell is created in"
  :type 'string
  :group 'leos
  :group 'shell)

(defun leo-shell-new-shell-maybe-first (&optional not-first)
  "Run an inferior shell in a new buffer in directory `leo-shell-first-shell-default-directory' or in interactivly provided direcory when called with not-first not nil."
  (interactive "P")
  (let ((default-directory 
          (cond (not-first ;; when call with argument
                 default-directory)
                ((null leo-shell-first-shell-default-directory) 
                 ;; when fisrt shell default not set
                 default-directory)
                (t ;; normally
                 leo-shell-first-shell-default-directory))))
    ;; do the work
    (call-interactively 'leo-shell-new-shell)))

(defun leo-shell-make-dwim-buffer-list ()
  "make the list of shell buffer to choose from.

be smart: if current window shows a shell buffer don't list that one."
  (interactive)
  (let* ((bl (save-excursion
               (delq nil (mapcar (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (if (eq major-mode 'shell-mode)
                                           (buffer-name buf)))))
                                 (buffer-list)))))
         (cb (window-buffer)))
    ;; current displayed buffer shouldn't be first choice
    (if (and bl
             (eq (get-buffer (car bl)) cb))
        ;;(nconc (cdr bl) (list (car bl)))
        (cdr bl)
      bl)))            
    

(defun leo-shell (&optional new)
  "Switch to shell or create new one when called with argument."
  (interactive "P")
  (let* ((blist (leo-shell-make-dwim-buffer-list))
         (bname (cond (new ;; when called with argument
                       nil)
                      ((null blist)          ;; when no shell buffer is there
                       nil)
                      ((= (length blist) 1) ;; when one shell buffer is there
                       (car blist))
                      (t                 ;; else (when multipleshell buffers are there)
                       (ido-completing-read "Shell:" blist nil t nil nil )))))
    (if bname
        (switch-to-buffer bname)
      (leo-shell-new-shell-maybe-first new))))


;;
;; eshell stuff
;;

(setq eshell-directory-name 
      (concat leo-emacs-shareddata-path "eshell"))
;; eshell-prompt-function and eshell-prompt-regexp are set in .custom
