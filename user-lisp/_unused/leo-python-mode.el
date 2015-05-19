;;
;; python things
;;

;; the basics: mode defintion etc
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)

;; custom funcs
(defun leo-py-execute-buffer (&optional async)
  "like py-execute-buffer but read local vars upfront."
  (interactive "P")
  (hack-local-variables)
  (let ((w (selected-window)))
    (py-execute-buffer async)
    (select-window w)))

(defun leo-py-shell (&optional async)
  "like py-shell but unset local py-which-shell."
  (interactive "P")
  (kill-local-variable 'py-which-shell)
  (py-shell))

(put 'py-which-shell 'safe-local-variable 'stringp)

(add-hook 'python-mode-hook
	  (lambda ()
            (define-key py-mode-map "\C-c\C-c"  'leo-py-execute-buffer)
            ))


