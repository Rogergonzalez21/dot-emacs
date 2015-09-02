;;
;; python things
;;

;; the basics: mode defintion etc
(require 'python)

(eval-after-load 'python
  '(progn
     (define-key python-mode-map "\C-c\C-c" 'leo-python-shell-send-buffer-with-args)
     ))

(setq python-shell-interpreter "python3")

;;  redefine run-python to normally not ask for anything (only with c-u c-u)
;;
(defun run-python (cmd &optional dedicated show)
  "Run an inferior Python process.

--- Changed by leo ---
Input and output via buffer named after
`python-shell-buffer-name'.  If there is a process already
running in that buffer, just switch to it.

With argument 16 (C-u C-u, allows you to define CMD so you can edit the
command used to call the interpreter and define DEDICATED, so a
dedicated process for the current buffer is open.  When numeric
prefix arg is other than 0 or 4 do not SHOW.

Runs the hook `inferior-python-mode-hook' (after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if (equal current-prefix-arg (list 16))
       (list
        (read-string "Run Python: " (python-shell-parse-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (python-shell-parse-command) t t)))
  (python-shell-make-comint
   cmd (python-shell-get-process-name dedicated) show)
  dedicated)


(defcustom leo-python-args-to-send ""
  "arguments for current buffer used by `python-shell-send-buffer-with-args'.")
(make-variable-buffer-local 'leo-python-args-to-send)
(put 'leo-python-args-to-send 'safe-local-variable #'stringp)

(defun leo-python-shell--save-temp-file-with-args (string &optional args)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write 'utf-8))
    (with-temp-file temp-file-name
      (insert "# -*- coding: utf-8 -*-\n") ;Not needed for Python-3.
      (insert string)
      (delete-trailing-whitespace)
      ;; now, AFTER inserting buffer content, insert command line magic
      (goto-char (point-min))
      (re-search-forward "^\\s-*import\\>"  nil t)
      (backward-char 6)
      ;; command line arguments go into argv!
      (insert "import sys,shlex; sys.argv=shlex.split('''" args "''')\n"))
    temp-file-name))

(defun leo-python-shell-send-region-with-args (start end nomain &optional args)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (let* ((python--use-fake-loc
          (or python--use-fake-loc (not buffer-file-name)))
         (string (python-shell-buffer-substring start end nomain))
         (process (python-shell-get-or-create-process))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    (let* ((temp-file-name (leo-python-shell--save-temp-file-with-args string args))
           (file-name (or (buffer-file-name) temp-file-name)))
      (python-shell-send-file file-name process temp-file-name t)
      (unless python--use-fake-loc
        (with-current-buffer (process-buffer process)
          (compilation-fake-loc (copy-marker start) temp-file-name
                                3)) ;; Not 1, because of the added lines coding+args)
        ))))


(defun leo-python-shell-send-buffer-with-args (&optional args)
  "Send the entire buffer to inferior Python process with the 
args of string `leo-python-args-to-send'.  

A \"if __name__ == '__main__':\" block is executed.

With prefix ARG allows to set the args string before sending the buffer."
  (interactive 
   (if current-prefix-arg 
       (list
        (read-string "Python arguments: " leo-python-args-to-send))
     (list leo-python-args-to-send)))

  ;; show the python buffer
  (if (python-shell-get-process)
      (display-buffer (process-buffer (python-shell-get-or-create-process)) t))

  ; save the (possibly new) arguments to `leo-python-args-to-send'.
  (setq leo-python-args-to-send args)
  (let ((buf-name (buffer-name))
        (largs (concat (buffer-name) " " args)))
    (with-temp-message (format "Run \"%s\" with args \"%s\"..." buf-name args)
      (save-restriction
        (widen)
        (leo-python-shell-send-region-with-args (point-min) (point-max) nil largs)))))

(defun leo-python-shell-setup-mode-vars ()
  "Setup variables for python shell mode, so that the point is alwayas at the bottom."
  (interactive)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-show-maximum-output t))
