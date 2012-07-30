;;
;; buffer and file selection helper
;;
(require 'ido)

(setq ido-show-dot-for-dired t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-confirm-unique-completion t)
(setq ido-save-directory-list-file
      (concat leo-emacs-userdata-path 
              (cond ((eq system-type 'windows-nt)
                     ".ido.last_nt")
                    (t
                     ".ido.last"))))

(defun leo-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map [down] 'ido-next-work-directory)
  (define-key ido-completion-map [up]   'ido-prev-work-directory))
(add-hook 'ido-setup-hook 'leo-ido-keys)

(defun leo-ido-switch-to-recent-buffer ()
  "Select the most recent buffer window without prompting the ido way."
  (interactive)
  (let ((ido-process-ignore-lists t)
        ido-ignored-list)
    (switch-to-buffer (car (ido-make-buffer-list nil)))))

(defun leo-unmapped-write-file (filename &optional confirm)
  "Write current buffer into file FILENAME.

This just calls the unmapped write-file function in files.el."
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file: "
				 nil nil nil nil)
	   (read-file-name "Write file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))
  (write-file filename confirm))


;;
;; ido-dired with prefix arg
;;
(defun leo-ido-dired ()
  "Call dired the ido way and allow for changing the listing switches via 
prefix arg. (original implementation of ido-dired doesn't obey the prefix 
arg.)
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (let ((leo-dired-switches (if current-prefix-arg      
                      (read-string "Dired listing switches: "
                                   dired-listing-switches)
                    nil)))
    (ido-dired)))

;; switch it on
(ido-mode t)

