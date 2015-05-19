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
;;(setq python-shell-interpreter "python")

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
     (list (python-shell-parse-command) nil t)))
  (python-shell-make-comint
   cmd (python-shell-get-process-name dedicated) show)
  dedicated)


(defcustom leo-python-args-to-send ""
  "arguments for current buffer used by `python-shell-send-buffer-with-args'.")
(make-variable-buffer-local 'leo-python-args-to-send)
(put 'leo-python-args-to-send 'safe-local-variable #'stringp)

(defun leo-python-shell-send-buffer-with-args (&optional args)
  "Send the entire buffer to inferior Python process with the 
args of string `leo-python-args-to-send'.

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
        (largs (concat (buffer-name) " " args)) 
        (source-buffer (current-buffer))
        (interpreter python-shell-interpreter))
    (with-temp-buffer
      ; copy everything need from the script buffer
      (insert-buffer-substring source-buffer)
      (setq-local python-shell-interpreter interpreter)
      (goto-char (point-min))
      (re-search-forward "^\\s-*import\\>"  nil t)
      (backward-char 6)
      (indent-rigidly (point) (point-max) python-indent-offset)
      (insert "import sys,shlex; sys.argv=shlex.split('''" largs "''')\n")
      (insert "try:\n")
      (goto-char (point-max))
      (insert "\nexcept SystemExit as e:\n    if \"%s\" % e != '': print(\"Terminated with exit code %s\" % e)\n")
      (with-temp-message (format "Run \"%s\" with args \"%s\"..." buf-name args)
        (python-shell-send-buffer 1)))))

(defun leo-python-shell-setup-mode-vars ()
  "Setup variables for python shell mode, so that the point is alwayas at the bottom."
  (interactive)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-show-maximum-output t))
