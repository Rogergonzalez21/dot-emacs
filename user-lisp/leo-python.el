;;
;; python things
;;

;; the basics: mode defintion etc
(require 'python)

(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

(defun leo-python-shell-setup-mode-vars ()
  "Setup variables for python shell mode, so that the point is alwayas at the bottom."
  (interactive)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-show-maximum-output t))
