(defun leo-ido-file-try ()
  "Change to next working file name in list."
  (interactive)
  (let ((name "hallo"))
    (when name
      (ido-set-current-directory "~/")
      (setq ido-text-init ".emacs")
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun leo-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map " " 'leo-ido-file-try))
(add-hook 'ido-setup-hook 'leo-ido-keys)
