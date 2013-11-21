;;
;; leos .emacs 
;;
;; path settings
;;
(cd "~")

(setq leo-emacs-userroot-path
      (file-name-directory (or load-file-name buffer-file-name)))

(setq leo-emacs-shareddata-path 
      (concat leo-emacs-userroot-path "shared-data/"))

(setq leo-emacs-userdata-path 
      "~/.emacsdata/")
;; even tually we might try to get ride off leo-emacs-userdata-path completely
;; and replace it with user-emacs-directory... 
(setq user-emacs-directory
      leo-emacs-userdata-path)

(setq leo-emacs-archivedata-path
      "~/archive/emacs/")

;; custom file for easy customization settings
;; attention: this file has to be _loaded_ as well (at the end of this file)
(setq custom-file 
      (concat leo-emacs-userroot-path ".custom"))

;; abbrevs file
(setq abbrev-file-name
      (concat leo-emacs-userroot-path "abbrev_defs"))

;; load-path
(push (expand-file-name leo-emacs-userroot-path)
      load-path)
(push (expand-file-name (concat leo-emacs-userroot-path "site-lisp"))
      load-path)
(push (expand-file-name (concat leo-emacs-userroot-path "user-lisp"))
      load-path)

;;
;; Info path stuff
;;
(setq leo-Info-main-directory 
      (expand-file-name (concat leo-emacs-userroot-path "site-info/")))

(when (eq system-type 'windows-nt)
  (push "c:/cygwin/usr/share/info"
	Info-default-directory-list))
(push leo-Info-main-directory 
      Info-default-directory-list)

;; packages are going where they are seen on all systems
(setq package-user-dir 
      (expand-file-name (concat leo-emacs-userroot-path "site-lisp/elpa")))
;;
;; auto save and backup config
;;
(setq leo-emacs-backup-dir 
      (concat leo-emacs-archivedata-path "backup/"))

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
;; general helper (used in package management)
;;
(defun leo-add-to-front-of-list (list-var element)
  "Add ELEMENT to the front of LIST after removing all other occurances of ELEMENT in LIST"
  (let ((cleaned (delete element (symbol-value list-var))))
    (set list-var (cons element cleaned))))

;;
;; package stuff
;;
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-hook 'after-init-hook 'leo-after-init-hook)
(defun leo-after-init-hook ()
  "After package initialisation."
  (leo-add-to-front-of-list 
   'Info-directory-list  leo-Info-main-directory)
  ;;(ido-at-point-mode)
  )


;;
;; git stuff
;;
(require 'magit)

;;
;; small things fopr windows et al + info + ediff + deft
;;
(load "leo-misc")

;;
;; ls-lisp and cygwin stuff
;; 
(load "leo-cygwin")

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
(load "leo-ido")

;;
;; locate stuff
;;
(load "leo-locate")

;;
;; goto last change
;;
(require 'goto-chg)

;;
;; global key bindings
;;
(global-set-key (kbd "C-/") 'set-mark-command)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key [C-space] 'completion-at-point)

(global-set-key (kbd "C-z") 'undo-only)
(global-set-key [?\C-\S-z] 'goto-last-change)

(global-set-key "\eg" 'goto-line)
(global-set-key "\er" 'revert-buffer)

(global-set-key (kbd "C-S-r") 'replace-string)
(global-set-key (kbd "M-C-r") 'replace-regexp)
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

(global-set-key [?\C-x ?\C-n] 'leo-deft-switch-and-filter-clear)
(global-set-key [?\C-x ?\C-\S-N] 'leo-notes-find-note)
(global-set-key [?\C-x ?\M-n] 'leo-switch-to-scratch-buffer)
(global-set-key [?\C-x ?\M-m] 'leo-switch-to-messages-buffer)

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
;; modifier stuff
;;
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil
      w32-lwindow-modifier 'hyper) ; Left Windows key
  (setq w32-alt-is-meta nil))

;;
;; global navigation keys
;;
(global-set-key (kbd "<C-up>") 'scroll-down-command)
(global-set-key (kbd "<C-down>") 'scroll-up-command)

(when (or (eq system-type 'darwin) (eq system-type 'windows-nt))
  (global-set-key (kbd "<A-left>") 'beginning-of-line)
  (global-set-key (kbd "<A-right>") 'end-of-line)  
  (global-set-key (kbd "<A-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<A-down>") 'end-of-buffer))
(when (eq system-type 'cygwin)
  (global-set-key (kbd "<H-left>") 'beginning-of-line)
  (global-set-key (kbd "<H-right>") 'end-of-line)  
  (global-set-key (kbd "<H-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<H-down>") 'end-of-buffer))


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
   (load "leo-frames")
   (define-key ctl-x-5-map "k" 'leo-kill-this-buffer-and-frame)
   (define-key ctl-x-5-map "p" 'leo-set-frame-from-index))

;;
;; recentf
;;
(load "leo-recentf")
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
    (define-key map "p" 'recompile)
    (define-key map "d" 'ediff-current-file)
    (define-key map "D" 'ediff-buffers)
    (define-key map "g" 'magit-status)
    (define-key map "e" 'leo-search-my-emacsfiles)
    (define-key map "s" 'synonyms-no-read)
    (define-key map "m" 'man)
    (define-key map "f" 'leo-spell-switch-mode)    
    (define-key map "r" 'rename-uniquely)    
    (define-key map "n" 'deft-new-file-named)
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
;; natural languge stuff
;;
(load "leo-spell")

(require 'google-this)
(google-this-mode)

;;
;; general mode things
;;
(load "leo-modes")

;;
;; general mode things
;;
(load "leo-docmodes")

;;
;; web (html/php/javascript) things
;;
(load "leo-html")

;;
;; lisp mode
;;
(find-function-setup-keys)

;;
;; python
;;
(load "leo-python")

;;
;; shell
;;
(load "leo-shell")
(global-set-key "\ez" 'leo-shell)

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
;;(load "leo-java")

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
