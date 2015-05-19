(defadvice shell-quote-argument (after swap-pathdelimiter activate)
  "swap pathdelimiter for windows cmdline tools used in shell-do-command"
  (if leo-dired-shell-swap-pathdelimiters
      (progn        
        (while (string-match "/" ad-return-value)            
            (setq ad-return-value (replace-match "\\\\" t t ad-return-value))))))

;;
;; functions for w32 commands
;;
(defun leo-w32-swap-pathdelimiters (file-name)
  (print "leo-w32...")
  (if leo-dired-shell-swap-pathdelimiters
      (progn        
        (while (string-match "/" file-name)            
            (setq file-name (replace-match "\\" t t file-name)))))
  file-name)

(defun w32-integ-dired-execute(verb &optional show-flag)
  "Call ShellExecute with current file from dired."
  (let ((file-name (leo-w32-swap-pathdelimiters (dired-get-filename))))
    (w32-shell-execute-with-msg verb file-name nil show-flag)))

