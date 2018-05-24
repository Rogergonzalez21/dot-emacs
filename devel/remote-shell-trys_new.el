
(setq uniquify-list-buffers-directory-modes '(dired-mode cvs-mode vc-dir-mode shell-mode))

(defun leo-shell-new-shell (&optional buffer)
  "Run an inferior shell, but customised."
  (interactive
   (list
    (prog1
        (read-buffer "Shell buffer: "
                     (generate-new-buffer-name "*shell*"))
        (setq default-directory
              (expand-file-name
               (read-directory-name
                "Default directory: " default-directory default-directory
                nil nil))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  (with-current-buffer buffer
    (set (make-local-variable 'explicit-shell-file-name)
         (file-remote-p
          (expand-file-name  "/bin/bash")
          'localname)))
  
  (shell buffer))

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
     (ido-completing-read "Shell:" bl nil nil nil nil))))


(defun leo-shell
