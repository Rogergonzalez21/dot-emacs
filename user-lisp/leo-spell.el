;;
;; spelling
;;

(setq-default ispell-program-name "aspell")

(defun leo-spell-switch-mode ()
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (flyspell-mode)
    (flyspell-buffer)))

(defun leo-spell-switch-dictionary ()
  (interactive)
  (ispell-change-dictionary "deutsch"))

(eval-after-load "flyspell"
    '(progn
       (delq (assoc 'mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (delq (assoc 'down-mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)
       (define-key flyspell-mode-map [?\C-x (control ?\,)] 'flyspell-buffer)
       (define-key flyspell-mode-map [?\C-x (control ?\.)] 'leo-spell-switch-dictionary)))


