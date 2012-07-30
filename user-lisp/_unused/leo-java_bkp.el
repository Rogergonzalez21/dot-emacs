(setq abbrev-file-name
       (concat leo-emacs-userroot-path ".abbrev_defs"))
(setq srecode-map-save-file
      (concat leo-emacs-userdata-path ".srecode-map"))

(push (expand-file-name (concat leo-emacs-userroot-path "site-lisp/elib"))
      load-path)
(push (expand-file-name (concat leo-emacs-userroot-path "site-lisp/cedet/common"))
      load-path)
(load "cedet.el")

(push (expand-file-name (concat leo-emacs-userroot-path "site-lisp/jdee/lisp"))
      load-path)

;; Set the debug option to enable a backtrace when a
;; problem occurs.
;;(setq debug-on-error t)


;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde nil)
;; to read:
;;
;;  (setq defer-loading-jde t)
;;

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))

(defun leo-jde-save-compile-run  ()
  "save current buffer, compile it and run it."
  (interactive)
  (save-buffer)
  (flet ((save-some-buffers (&optional arg pred) t))
    (jde-compile))
  (sleep-for 0 100)
  (jde-run 1))


(setq jde-global-classpath (quote ("." "~/dev/java/lib/")))
(setq jde-sourcepath (quote ("." "~/dev/java/lib/")))

(setq jde-jdk-registry 
      (cond ((eq system-type 'windows-nt)
             (quote (("1.6.0" . "c:/Program Files/Java/jdk1.6.0_18"))))
            ((eq system-type 'darwin)
             (quote (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/"))))
      ))

(setq jde-jdk (quote ("1.6.0")))

;; re-do the indent binding from c-common...
(defun leo-jde-mode-hook ()
  (define-key jde-mode-map [?\M-\C-\\] 'c-indent-line-or-region))

(add-hook 'jde-mode-hook 'leo-jde-mode-hook)
