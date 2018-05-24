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
  (let ((bl (save-excursion
              (delq
               nil
               (mapcar (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (if (eq major-mode 'shell-mode)
                                 (buffer-name buf)))))
                       (buffer-list))))))
    (switch-to-buffer
     (ido-completing-read "Shell:" bl nil nil nil nil ))))

(defun leo-shell (&optional new)
  "switch to shell or with argument create new one."
  (interactive "P")
  (if new
      (call-interactively 'leo-shell-new-shell)
    (leo-shell-switch-shell)))

(global-set-key "\ez" 'leo-shell)
