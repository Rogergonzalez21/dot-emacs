;;
;; general mode things
;;

(delete-selection-mode 1)
(when (eq system-type 'gnu/linux)
  (setq select-enable-clipboard t)
  (setq select-active-regions nil))

(fset 'yes-or-no-p 'y-or-n-p)
(modify-syntax-entry ?' ".   " text-mode-syntax-table)

;;; add these lines if you like color-based syntax highlighting
(global-font-lock-mode t)

;;
;; stuff for truncate-lines
;;
(setq-default truncate-lines nil)

;;
;; special modes
;;

;; magit
 (setq magit-last-seen-setup-instructions "1.4.0")
