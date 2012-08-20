;; leos .emacs 
;;
;; path settings
;;
(cd "~")

(setq leo-emacs-userroot-path
      (file-name-directory (or load-file-name buffer-file-name)))

(setq leo-emacs-userdata-path 
      "~/.emacsdata/")

(setq leo-emacs-localdata-path
      "~/archive/emacs/")
;; custom file for easy customization settings
;; attention: this file has to be _loaded_ as well (at the end of this file)
(setq custom-file 
      (concat leo-emacs-userroot-path ".custom"))

;; load-path
(push (expand-file-name leo-emacs-userroot-path)
      load-path)
(push (expand-file-name (concat leo-emacs-userroot-path "site-lisp"))
      load-path)
(push (expand-file-name (concat leo-emacs-userroot-path "user-lisp"))
      load-path)

;; info-path
(when (eq system-type 'windows-nt)
  (push "c:/cygwin/usr/share/info"
	Info-default-directory-list))
(push (expand-file-name (concat leo-emacs-userroot-path "site-info/"))
      Info-default-directory-list)

;;
;; auto save and backup config
;;
(setq leo-emacs-backup-dir 
      (concat leo-emacs-localdata-path "backup/"))

(setq auto-save-file-name-transforms 
      (list
       (list "\\`/[^/]*:\\(.+/\\)*\\(.*\\)" 
             (concat leo-emacs-backup-dir "\\2") 
             t)
       (list "\\(.+/\\)*\\(.*\\)" 
             (concat leo-emacs-backup-dir "\\2")
             t)
       ))
(setq auto-save-list-file-prefix (concat leo-emacs-backup-dir "auto-save/.saves-"))

(setq backup-directory-alist 
      `(("." . ,leo-emacs-backup-dir)))


;;
;; my functions + info + ediff
;;
(load "leo-misc.el")

;;
;; ls-lisp stuff
;; 
(load "leo-ls-lisp")

;;
;; files&coding stuff (sets omit extensions for dired as well!)
;; 
(load "leo-files")

;;
;; dired stuff
;; 
(load "leo-dired")

;;
;; ido stuff
;;
(load "leo-ido.el")

;;
;; locate stuff
;;
(load "leo-locate.el")

;;
;; goto last change
;;
(require 'goto-chg)


;;
;; global key bindings
;;
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key "\eg" 'goto-line)
(global-set-key "\er" 'revert-buffer)
(global-set-key [?\M-\C-r] 'replace-regexp)
(global-set-key [?\C-\S-s] 'search-forward)
(global-set-key [?\M-\C-\S-s] 'search-forward-regexp)

;;
(global-set-key "\C-x\C-t" 'toggle-truncate-lines)
(global-set-key "\C-\\" 'ido-switch-buffer)
(global-set-key "\C-xd" 'leo-ido-dired)
(global-set-key "\C-x\C-w" 'leo-unmapped-write-file)
(global-set-key "\C-x\C-a" 'ffap)

(global-set-key "\C-^" 'enlarge-window)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key [?\C-x ?\C-n] 'leo-notes-find-note)
(global-set-key [?\C-x ?\C-\S-N] 'leo-switch-to-scratch-buffer)
(global-set-key [?\C-x ?\M-n] 'leo-switch-to-messages-buffer)

(global-set-key [?\C-x ?\M-l] 'leo-locate)
(global-set-key [?\C-x ?\M-L] 'leo-locate-with-filter)
(global-set-key [?\C-x ?\M-d] 'leo-copy-directory)
(global-set-key [?\C-x ?\M-p] 'leo-copy-directory-for-os)
(global-set-key [?\C-x ?\M-j] 'leo-w32-integ-current-dir-explorer-open)


;;
;;  ffap
;;
(defvar leo-ffap-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'find-file-at-point)
    (define-key map "4f" 'ffap-other-window)
    (define-key map "5f" 'ffap-other-frame)
    (define-key map "d" 'dired-at-point)
    (define-key map "4d" 'ffap-dired-other-window)
    (define-key map "5d" 'ffap-dired-other-frame)
    map)
  "Keymap for find-file-at-point subcommands. (default bounds to C-x a.)")
(fset 'leo-ffap-prefix-map leo-ffap-prefix-map)
(define-key global-map "\C-xa" 'leo-ffap-prefix-map)


;;
;; packages and related key bindings
;;
(defun leo-nop ()
  "no operation (might be assigned to some key strokes)"
  (interactive)
  )

;;
;; cua-lite and things like that
;;
(when (eq system-type 'windows-nt)
  (setq w32-alt-is-meta nil))

(require 'cua-lite)
(defun leo-cua-lite-keys ()
  "Bunch of stuff to run for cua-lite when keys are bound."
  (global-set-key "\C-s" 'isearch-forward)
  (global-set-key "\C-w" 'kill-region)
  (global-set-key "\C-x\C-s" 'save-buffer)
  (cua-lite-bind-both-motion-keys "C-<up>" 'scroll-down)
  (cua-lite-bind-both-motion-keys "C-<down>" 'scroll-up)
  (when (or (eq system-type 'darwin) (eq system-type 'windows-nt))
    (cua-lite-bind-both-motion-keys "<A-left>" 'beginning-of-line)
    (cua-lite-bind-both-motion-keys "<A-right>" 'end-of-line)  
    (cua-lite-bind-both-motion-keys "<A-up>" 'beginning-of-buffer)
    (cua-lite-bind-both-motion-keys "<A-down>" 'end-of-buffer))
  (when (eq system-type 'cygwin)
    (cua-lite-bind-both-motion-keys "<H-left>" 'beginning-of-line)
    (cua-lite-bind-both-motion-keys "<H-right>" 'end-of-line)  
    (cua-lite-bind-both-motion-keys "<H-up>" 'beginning-of-buffer)
    (cua-lite-bind-both-motion-keys "<H-down>" 'end-of-buffer))

  (when (eq system-type 'darwin)
    (global-set-key [kp-delete] 'delete-char)
    (global-set-key [S-kp-delete] 'kill-region)
    (global-set-key [S-kp-insert] 'yank)
    (global-set-key [C-kp-insert] 'kill-ring-save)
    (global-set-key "\M-c" 'leo-nop)
    (global-set-key "\M-v" 'leo-nop)))

(setq cua-lite-bind-keys-hook '(leo-cua-lite-keys))
(cua-lite 1)

;;
;; setting C-tab and C-S-tab (with special case for minibuffer)
;;
(global-set-key [C-tab] 'other-window)
(global-set-key [?\C-=] 'other-window)

(define-key minibuffer-local-completion-map [S-tab] 
  (lookup-key minibuffer-local-completion-map [C-tab]))
(define-key minibuffer-local-completion-map [C-tab] 'other-window)

(define-key minibuffer-local-map [S-tab] 
  (lookup-key minibuffer-local-map [C-tab]))
(define-key minibuffer-local-map [C-tab] 'other-window)

(define-key minibuffer-local-must-match-map [S-tab] 
  (lookup-key minibuffer-local-must-match-map [C-tab]))
(define-key minibuffer-local-must-match-map [C-tab] 'other-window)

(global-set-key [C-S-tab] 'leo-other-window-backward)
(define-key minibuffer-local-completion-map [C-tab] 'leo-other-window-backward)
(define-key minibuffer-local-map [C-tab] 'leo-other-window-backward)
(define-key minibuffer-local-must-match-map [C-tab] 'leo-other-window-backward)

(global-set-key [?\C-`] (if (functionp 'leo-ido-switch-to-recent-buffer)
    'leo-ido-switch-to-recent-buffer
  'leo-switch-to-recent-buffer))
(global-set-key [?\C-~] 'leo-buf-move-down-or-up)

(define-key help-map "C" 'describe-current-coding-system)


;;
;; frames
;; 
(global-set-key "\M-`" 'other-frame)

;; mac aqua stuff
(when (eq window-system 'ns)
  (setq ns-command-modifier 'alt)
  (global-set-key [?\A-`] 'other-frame)
  (global-set-key [?\A-w] 'delete-frame))  

;; windows system menu stuff
(when (eq system-type 'windows-nt)
  (defvar leo-system-menu-prefix-map
    (let ((map (make-sparse-keymap)))
      (define-key map "r" 'leo-w32-restore-frame)
      (define-key map "x" 'leo-w32-maximize-frame)
      (define-key map "n" 'iconify-or-deiconify-frame)
      (define-key map "c" 'delete-frame)
      map)
    "Keymap for system menu actions. (default bounds to A-SPC)")
  (fset 'leo-system-menu-prefix-map leo-system-menu-prefix-map)
  (define-key global-map  [?\A- ]  'leo-system-menu-prefix-map)
  (global-set-key [?\A-`] 'other-frame)
  (global-set-key [?\A-w] 'delete-frame))  

;; cygwin stuff
(when (eq system-type 'cygwin)
  (global-set-key [?\H-`] 'other-frame))
  
;;
;; frame appearance (and info!)
;;

(when (not noninteractive)
   (load "leo-frames.el")
   (define-key ctl-x-5-map "k" 'leo-kill-this-buffer-and-frame)
   (define-key ctl-x-5-map "p" 'leo-set-frame-position-from-index))

;;
;; recentf
;;
(load "leo-recentf.el")
(global-set-key "\C-x\C-o" 'recentf-open-files)
(global-set-key "\C-x\C-r" 'recentf-open-files-compl)
(define-key ctl-x-5-map "\C-o" 'leo-recentf-open-files-other-frame)

(when (eq system-type 'darwin) 
  (define-key widget-keymap "\r" 'widget-button-press))


;;(require 'dired-efap)
;;(define-key dired-mode-map [f2] 'dired-efap)

;;
;; key bindings for some general commands
;;
(defvar leo-general-command-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'calculator)
    (define-key map "d" 'ediff-buffers)
    (define-key map "t" 'twit)
    (define-key map "s" 'leo-search-my-emacsfiles)
    (define-key map "m" 'man)
    (define-key map "r" 'run-ruby)
    map)
  "Keymap for mode switching subcommands. (default bounds to C-x g.)")
(fset 'leo-general-command-prefix-map leo-general-command-prefix-map)
(define-key global-map "\C-xg" 'leo-general-command-prefix-map)

;;
;; key bindings for  mode switching
;;
(defvar leo-mode-switch-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'normal-mode)
    (define-key map "h" 'html-helper-mode)
    (define-key map "p" 'php-mode)
    (define-key map "t" 'text-mode)
    map)
  "Keymap for mode switching subcommands. (default bounds to C-x m.)")
(fset 'leo-mode-switch-prefix-map leo-mode-switch-prefix-map)
(define-key global-map "\C-xm" 'leo-mode-switch-prefix-map)

;;
;; general mode things
;;
(load "leo-modes.el")


;;
;; web (html/php/javascript) things
;;
(load "leo-html.el")

;;
;; lisp mode
;;
(find-function-setup-keys)

;;
;; python
;;
(load "leo-python.el")

;;
;; twitter
;;
(load "leo-twitter.el")

;;
;; shell
;;
(load "leo-shell.el")
(global-set-key "\ez" 'shell)

;;
;; server

;;
(require 'server)
;;does not work under cygwin
(when (eq system-type 'windows-nt)
  (setq server-auth-dir
        (concat leo-emacs-userdata-path "server/")))
(server-start)

;;
;; java, cedet and jdee
;;
;;(load "leo-java.el")

;;
;; history stuff
;;
(require 'savehist)
(setq savehist-file
      (concat leo-emacs-userdata-path ".emacs-history"))
(savehist-load)

;; load temporary definitions if they exist.
(load ".emacs-temp" t) 
;; load customize stuff
(load-file custom-file)

;;
;; sessions
;;
;;(desktop-load-default)
;;(desktop-read)
;;(setq desktop-files-not-to-save "^/[^/:]*:\\|^//")