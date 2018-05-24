(define-derived-mode xxx-shell-mode comint-mode "Shell"
  "Major mode for interacting with an inferior shell.\\<shell-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from
    the end of process to the end of the current line.
\\[comint-send-input] before end of process output copies the current line minus the prompt to
    the end of the buffer and sends it (\\[comint-copy-old-input] just copies the current line).
\\[send-invisible] reads a line of text without echoing it, and sends it to
    the shell.  This is useful for entering passwords.  Or, add the function
    `comint-watch-for-password-prompt' to `comint-output-filter-functions'.

If you want to make multiple shell buffers, rename the `*shell*' buffer
using \\[rename-buffer] or \\[rename-uniquely] and start a new shell.

If you want to make shell buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

`cd', `pushd' and `popd' commands given to the shell are watched by Emacs to
keep this buffer's default directory the same as the shell's working directory.
While directory tracking is enabled, the shell's working directory is displayed
by \\[list-buffers] or \\[mouse-buffer-menu] in the `File' field.
\\[dirs] queries the shell and resyncs Emacs's idea of what the current
    directory stack is.
\\[shell-dirtrack-mode] turns directory tracking on and off.
\(The `dirtrack' package provides an alternative implementation of this
feature - see the function `dirtrack-mode'.)

\\{shell-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`shell-mode-hook' (in that order).  Before each input, the hooks on
`comint-input-filter-functions' are run.  After each shell output, the hooks
on `comint-output-filter-functions' are run.

Variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp'
and `shell-popd-regexp' are used to match their respective commands,
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Variables `comint-completion-autolist', `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore' control the
behavior of file name, command name and variable name completion.  Variable
`shell-completion-execonly' controls the behavior of command name completion.
Variable `shell-completion-fignore' is used to initialize the value of
`comint-completion-fignore'.

Variables `comint-input-ring-file-name' and `comint-input-autoexpand' control
the initialization of the input ring history, and history expansion.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and `comint-scroll-to-bottom-on-output'
control whether input and output cause the window to scroll to the end of the
buffer."
  (setq comint-prompt-regexp shell-prompt-pattern)
  (shell-completion-vars)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'font-lock-defaults) '(shell-font-lock-keywords t))
  (set (make-local-variable 'shell-dirstack) nil)
  (set (make-local-variable 'shell-last-dir) nil)
  (shell-dirtrack-mode 1)

  ;; By default, ansi-color applies faces using overlays.  This is
  ;; very inefficient in Shell buffers (e.g. Bug#10835).  We use a
  ;; custom `ansi-color-apply-face-function' to convert color escape
  ;; sequences into `font-lock-face' properties.
  (setq-local ansi-color-apply-face-function #'shell-apply-ansi-color)
  (shell-reapply-ansi-color)

  ;; This is not really correct, since the shell buffer does not really
  ;; edit this directory.  But it is useful in the buffer list and menus.
  (setq list-buffers-directory (expand-file-name default-directory))
  ;; shell-dependent assignments.
  (when (ring-empty-p comint-input-ring)
    (let ((shell (file-name-nondirectory (car
		   (process-command (get-buffer-process (current-buffer))))))
	  (hsize (getenv "HISTSIZE")))
      (message (format "HISTSIZE = \"%s\"" hsize))
      (and (stringp hsize)
	   (integerp (setq hsize (string-to-number hsize)))
	   (> hsize 0)
	   (set (make-local-variable 'comint-input-ring-size) hsize))
      (setq comint-input-ring-file-name
	    (or (getenv "HISTFILE")
		(cond ((string-equal shell "bash") "~/.bash_history")
		      ((string-equal shell "ksh") "~/.sh_history")
		      (t "~/.history"))))
      (if (or (equal comint-input-ring-file-name "")
	      (equal (file-truename comint-input-ring-file-name)
		     (file-truename "/dev/null")))
	  (setq comint-input-ring-file-name nil))
      ;; Arrange to write out the input ring on exit, if the shell doesn't
      ;; do this itself.
      (if (and comint-input-ring-file-name
	       (string-match shell-dumb-shell-regexp shell))
	  (set-process-sentinel (get-buffer-process (current-buffer))
				#'shell-write-history-on-exit))
      (setq shell-dirstack-query
	    (cond ((string-equal shell "sh") "pwd")
		  ((string-equal shell "ksh") "echo $PWD ~-")
		  ;; Bypass any aliases.  TODO all shells could use this.
		  ((string-equal shell "bash") "command dirs")
		  (t "dirs")))
      ;; Bypass a bug in certain versions of bash.
      (when (string-equal shell "bash")
        (add-hook 'comint-preoutput-filter-functions
                  'shell-filter-ctrl-a-ctrl-b nil t)))
    (comint-read-input-ring t)))


(defun xxx-shell (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (prog1
	     (read-buffer "Shell buffer: "
			  ;; If the current buffer is an inactive
			  ;; shell buffer, use it as the default.
			  (if (and (eq major-mode 'xxx-shell-mode)
				   (null (get-buffer-process (current-buffer))))
			      (buffer-name)
			    (generate-new-buffer-name "*shell*")))
	   (if (file-remote-p default-directory)
	       ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
	       (setq default-directory
		     (expand-file-name
		      (read-directory-name
		       "Default directory: " default-directory default-directory
		       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'xxx-shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
	   (file-remote-p default-directory)
	   (null explicit-shell-file-name)
	   (null (getenv "ESHELL")))
      (with-current-buffer buffer
	(set (make-local-variable 'explicit-shell-file-name)
	     (file-remote-p
	      (expand-file-name
	       (read-file-name
		"Remote shell path: " default-directory shell-file-name
		t shell-file-name))
	      'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (pop-to-buffer-same-window buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
		     (getenv "ESHELL") shell-file-name))
	   (name (file-name-nondirectory prog))
	   (startfile (concat "~/.emacs_" name))
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
	(setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
	     (if (file-exists-p startfile) startfile)
	     (if (and xargs-name (boundp xargs-name))
		 (symbol-value xargs-name)
	       '("-i")))
      (xxx-shell-mode)))
  buffer)
