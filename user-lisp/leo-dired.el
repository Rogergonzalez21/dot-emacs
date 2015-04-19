;;
;; dired manager
;;

;;
;; my own dwim command
;;
(defvar leo-dired-dwim-directory  nil
  "holds the dwim directory for `leo-dired-dwim-yank'")

(defadvice dired-dwim-target-directory (before add-dwim-to-history activate)
  "stores dwim directory for special command `leo-dired-dwim-yank'.
works best if `dired-dwim-target' is NOT set."
  (let* ((other-buf (window-buffer (next-window)))
         (other-dir (save-excursion
                      (set-buffer other-buf)
                      (and (eq major-mode 'dired-mode)
                           (dired-current-directory)))))
    (setq leo-dired-dwim-directory other-dir)))

(defun leo-dired-dwim-yank ()
  "insert the stored dwim directory `leo-dired-dwim-directory'."
  (interactive)
  (if leo-dired-dwim-directory
      (progn
       (goto-char (point-max))
       (delete-minibuffer-contents)
       (insert leo-dired-dwim-directory)
       (goto-char (or minibuffer-temporary-goal-position (point-max)))
       )
    (message "yank DWIM: no directory found")))

(define-key minibuffer-local-filename-completion-map [up] 'previous-history-element)
(define-key minibuffer-local-filename-completion-map [?\M-\C-g] 'leo-dired-dwim-yank)

;;
;; functions using the find command
;;
(when (eq system-type 'windows-nt) 
 (setq find-ls-option (quote ("-exec ls -lGd {} \\;" . "-lGd")))
)

;;;###autoload
(defun find-iname-dired (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The command run (after changing into DIR) is

    find . -iname 'PATTERN' -ls"
  (interactive
   "DFind-name (directory): \nsFind-iname (filename wildcard): ")
  (find-dired dir (concat "-iname '" pattern "'")))

;; finds all items from the last 24 hours except dot files
(defun leo-find-since-yesterday (dir)
  "Search DIR recursively for files changed in the last 24 hours and run 
dired on those files.
The command run (after changing into DIR) is

    find . -ctime -1 -a ! -regex \"\\./\\..*\""
  (interactive
   "DFind-name (directory): ")
  (find-dired dir (concat "-ctime -1 -a ! -regex \"\\./\\..*\"")))

;;
;; decoration and font stuff
;;
(defvar leo-dired-match-date-regexp
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
	 (l-or-quote "\\([A-Za-z']\\|[^\0-\177]\\)")
	 ;; In some locales, month abbreviations are as short as 2 letters,
	 ;; and they can be followed by ".".
	 ;; In Breton, a month name  can include a quote character.
	 (month (concat l-or-quote l-or-quote "+\\.?"))
	 (s " ")
	 (yyyy "[0-9][0-9][0-9][0-9]")
	 (dd "[ 0-3][0-9]")
	 (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
	 (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
	 (zone "[-+][0-2][0-9][0-5][0-9]")
	 (iso-mm-dd "[01][0-9]-[0-3][0-9]")
	 (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
	 (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
		      "\\|" yyyy "-" iso-mm-dd "\\)"))
	 (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
			  s "+"
			  "\\(" HH:MM "\\|" yyyy "\\)"))
	 (western-comma (concat month s "+" dd "," s "+" yyyy))
	 ;; Japanese MS-Windows ls-lisp has one-digit months, and
	 ;; omits the Kanji characters after month and day-of-month.
	 (mm "[ 0-1]?[0-9]")
	 (japanese
	  (concat mm l "?" s dd l "?" s "+"
		  "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
	 ;; The "[0-9]" below requires the previous column to end in a digit.
	 ;; This avoids recognizing `1 may 1997' as a date in the line:
	 ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README
	 ;; The "[kKMGTPEZY]?" below supports "ls -alh" output.
	 ;; The ".*" below finds the last match if there are multiple matches.
	 ;; This avoids recognizing `jservice  10  1024' as a date in the line:
	 ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host
    (concat ".*[0-9][kKMGTPEZY]?" s
	    "\\(\\(" western "\\|" western-comma "\\|" japanese "\\|" iso "\\)" s "\\)"
	    s "*"))
  "Regular expression to the date part in a directory listing.
It inherits from `dired-move-to-filename-regexp' and  does just the grouping for the font-lock-highlighting appropriately.")

(defvar leo-dired-match-size-regexp
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
	 (l-or-quote "\\([A-Za-z']\\|[^\0-\177]\\)")
	 ;; In some locales, month abbreviations are as short as 2 letters,
	 ;; and they can be followed by ".".
	 ;; In Breton, a month name  can include a quote character.
	 (month (concat l-or-quote l-or-quote "+\\.?"))
	 (s " ")
	 (yyyy "[0-9][0-9][0-9][0-9]")
	 (dd "[ 0-3][0-9]")
	 (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
	 (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
	 (zone "[-+][0-2][0-9][0-5][0-9]")
	 (iso-mm-dd "[01][0-9]-[0-3][0-9]")
	 (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
	 (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
		      "\\|" yyyy "-" iso-mm-dd "\\)"))
	 (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
			  s "+"
			  "\\(" HH:MM "\\|" yyyy "\\)"))
	 (western-comma (concat month s "+" dd "," s "+" yyyy))
	 ;; Japanese MS-Windows ls-lisp has one-digit months, and
	 ;; omits the Kanji characters after month and day-of-month.
	 (mm "[ 0-1]?[0-9]")
	 (japanese
	  (concat mm l "?" s dd l "?" s "+"
		  "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
	 ;; The "[0-9]" below requires the previous column to end in a digit.
	 ;; This avoids recognizing `1 may 1997' as a date in the line:
	 ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README
	 ;; The "[kKMGTPEZY]?" below supports "ls -alh" output.
	 ;; The ".*" below finds the last match if there are multiple matches.
	 ;; This avoids recognizing `jservice  10  1024' as a date in the line:
	 ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host
    (concat ".*\\( [0-9]+[kKMGTPEZY]?" s "\\)"
	    "\\(" western "\\|" western-comma "\\|" japanese "\\|" iso "\\)"
	    s "+"))
  "Regular expression to the size in a directory listing.
It inherits from `dired-move-to-filename-regexp' and  does just the grouping for the font-lock-highlighting appropriately.")

(defface leo-dired-decorated
  '((t (:inherit italic)))
  "Face used for decorating ls columns."
  :group 'dired-faces)
(defvar leo-dired-decorated-face 'leo-dired-decorated
  "Face name used for decorating ls columns.")

(font-lock-add-keywords
 'dired-mode
 `((,leo-dired-match-size-regexp 1 leo-dired-decorated-face t)))

;;
;; copy directory
;;
(defun leo-copy-directory (&optional arg)
  "Copy current default directory to kill ring."
  (interactive "p")
  (let (dir)
    (if (eq major-mode 'dired-mode)
        (setq dir (dired-current-directory))
      (setq dir default-directory))
    (kill-new dir)
    (if arg
        (message "Directory %s" dir))))

(defun leo-copy-directory-for-os ()
  "Copy current default directory to kill ring, but for operation system. (i.e. with mangled path delimiters)."
  (interactive nil)
  (leo-copy-directory)
  (leo-path-convert-kill-for-os)
  (message "Directory %s" (current-kill 0)))

;;
;; system-open for files
;;
(when (eq system-type 'windows-nt)
  (require 'w32-integ)
)
;;
;; Open current directory by calling Windows Explorer
;;
(defun leo-w32-integ-current-dir-explorer-open()
    "Open current directory by calling Windows Explorer."
    (interactive)
    (let ((file-name (if (eq major-mode 'dired-mode)
                         (dired-current-directory)
                       default-directory)))
      (w32-shell-execute-with-msg "open" (expand-file-name file-name) nil)))


;;
;; things to do at dired startup
;;
(add-hook 'dired-load-hook
	  (lambda ()
            (require 'dired-sort-menu)
            (require 'dired-x)
            (load "leo-dired-in-hook.el")

            (define-key dired-mode-map [return] 'leo-dired-find-file)
            (define-key dired-mode-map [S-return] 'dired-find-file)
            (define-key dired-mode-map "v" 'dired-display-file)

	    (define-key dired-mode-map "\C-o" 'dired-omit-mode)
	    (define-key dired-mode-map "\C-t" 'leo-dired-omit-unmarked-files)
	    (define-key dired-mode-map "\C-p" 'leo-toggle-ls-lisp-verbosity)
            (define-key dired-mode-map [?\C-\S-r] 'dired-sort-menu-toggle-recursive)
            (define-key dired-mode-map "w" 'leo-dired-copy-filename-as-kill)
            (define-key dired-mode-map "W" 'dired-copy-filename-as-kill)
            (define-key dired-mode-map "k" 'leo-dired-kill-subdir-go-up)
            (define-key dired-mode-map "K" 'dired-do-kill-lines)
            (define-key dired-mode-map "\ek" 'dired-kill-tree)
            (define-key dired-mode-map "e" 'w32-integ-dired-explorer-open)
            (define-key dired-mode-map (kbd "\C-x 5 \C-m") 'leo-dired-find-file-other-frame) ;; delete it?
            (define-key dired-mode-map (kbd "C-x 5 o") 'leo-dired-find-file-other-frame)
	    (define-key dired-mode-map "c" 'leo-dired-find-container)
	    (define-key dired-mode-map "^" 'leo-dired-up-directory-kill-last-buffer)
	    (define-key dired-mode-map "!" 'leo-dired-do-shell-command)
	    ))

;;
;; presetting omit
;; note: used in leo-locate as well
;;
(defun leo-dired-manage-omit-mode ()
  (if (not (and (boundp 'leo-dired-dont-omit)
                leo-dired-dont-omit))
      (dired-omit-mode 1)
    (dired-omit-mode 0)))
    
(add-hook 'dired-mode-hook 'leo-dired-manage-omit-mode)
