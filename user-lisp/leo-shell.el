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


(defun leo-shell-mode-hook ()
  "cursor keys get mapped"
  ;; don't change the cursor up/down behaviour anymore
  ;;(local-set-key '[C-up] (lookup-key global-map [up])) 
  ;;(local-set-key '[C-down] (lookup-key global-map [down]))
  ;;(local-set-key '[up] 'comint-previous-input)
  ;;(local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-previous-matching-input-from-input)
  (local-set-key (kbd "M-RET") 'leo-shell-resync-dirs))

(add-hook 'shell-mode-hook 'leo-shell-mode-hook)

;;(defun leo-comint-init-without-echo () 
;;  (setq comint-process-echoes t)) 
;;(add-hook 'comint-mode-hook 'leo-comint-init-without-echo) 

;; function for re-reading history file

(defun leo-shell-load-history ()
  "Reload history (from file) when in shell buffer.

Note: Please use this function only, when the shell is set to saving history after each command.
In bash the expression `export PROMPT_COMMAND=\'history -a\'' does this."
  (interactive)
  (if (not (eq major-mode 'shell-mode))
      (error "Not in shell buffer"))
  (if (not (file-readable-p comint-input-ring-file-name))
      (error "Cannot read history file %s"
		      comint-input-ring-file-name))
  (comint-read-input-ring)
  (message "(Re-)loaded history"))

                                        ;
;;
;; hack for better handling of directories with spaces for directory syncing
;; Note: this hack looses the abilty to track pushd/popd directories...
;;

;; dirs should only print the latest directory
(setq shell-dirstack-query "command dirs +0")

;; like shell-resync-dirs but parses the whole line as one directory
(defun leo-shell-resync-dirs ()
  "Like `shell-resync-dirs' but parses the whole line as one directory.

Works in conjunction with a `shell-dirstack-query' value of \"dirs +0\").

"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc))
	 (started-at-pmark (= (point) (marker-position pmark))))
    (save-excursion
      (goto-char pmark)
      ;; If the process echoes commands, don't insert a fake command in
      ;; the buffer or it will appear twice.
      (unless comint-process-echoes
	(insert shell-dirstack-query) (insert "\n"))
      (sit-for 0)			; force redisplay
      (comint-send-string proc shell-dirstack-query)
      (comint-send-string proc "\n")
      (set-marker pmark (point))
      (let ((pt (point))
	    (regexp
	     (concat
	      (if comint-process-echoes
		  ;; Skip command echo if the process echoes
		  (concat "\\(" (regexp-quote shell-dirstack-query) "\n\\)")
		"\\(\\)")
	      "\\(.+\n\\)")))
	;; This extra newline prevents the user's pending input from spoofing us.
	(insert "\n") (backward-char 1)
	;; Wait for one line.
	(while (not (looking-at regexp))
	  (accept-process-output proc)
	  (goto-char pt)))
      (goto-char pmark) (delete-char 1) ; remove the extra newline
      ;; That's the dirlist. grab it & parse it.
      (let* ((dl (buffer-substring (match-beginning 2) (1- (match-end 2))))
	     (dl-len (length dl))
	     (ds '())			; new dir stack
	     (i 0))
	(while (< i dl-len)
	  ;; regexp = optional whitespace, (non-whitespace), optional whitespace
	  ;; (string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
          (string-match "\\(.*\\)" dl i) ; pick off the whole line as directory
	  (setq ds (cons (concat comint-file-name-prefix
				 (substring dl (match-beginning 1)
					    (match-end 1)))
			 ds))
	  (setq i (match-end 0)))
	(let ((ds (nreverse ds)))
	  (with-demoted-errors "Couldn't cd: %s"
	    (shell-cd (car ds))
	    (setq shell-dirstack (cdr ds)
		  shell-last-dir (car shell-dirstack))
	    (shell-dirstack-message)))))
    (if started-at-pmark (goto-char (marker-position pmark)))))

;; For your typing convenience:
(defalias 'dirs 'leo-shell-resync-dirs)

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
                 ;; when first shell default not set
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
                      (t                 ;; else (when multiple shell buffers are there)
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
