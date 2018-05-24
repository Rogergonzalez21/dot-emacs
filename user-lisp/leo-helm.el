;;
;; helm configuration
;;
(require 'async)
(require 'helm)
(require 'helm-config)

;;
;; config stuff
;;
(helm-autoresize-mode t)
(setq helm-autoresize-min-height 15)
(when (eq system-type 'windows-nt)
  (setq helm-locate-command "locate --max-database-age -1 %s -e -A --regex %s"))


;;
;; keys in helm
;;
(setq helm-ff-lynx-style-map nil) ;; disable special meaning for left and right
(define-key  helm-map (kbd "<right>") 'right-char)
(define-key  helm-map (kbd "<left>") 'left-char)

;;
;; "normal" tab in helm-find-files...
;;
;; set the helm-tab action to C-z (this binding is helm-global!)
(define-key helm-map (kbd "C-z") 'helm-select-action) ;
;; unbind TAB helm-globally and set it in helm-find-files to persistent-action
(define-key helm-map (kbd "<tab>") 'nil)

(require 'helm-files)
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "^") 'helm-find-files-up-one-level)

;;
;; key-bindings
;;
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; bindings which shadow normal emacs commands
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)

(defun leo-helm-recentf-and-multi-files ()
  "Like `helm-multi-files' just the recent files before the buffers
and without locate support."
  (interactive)
  (require 'helm-x-files)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (setq helm-multi-files--toggle-locate nil)
  (let ((sources (remove 'helm-source-locate helm-for-files-preferred-list)))
    ;; remove and re-add helm-source-recentf
    (setq sources (remove 'helm-source-recentf sources))
    (add-to-list 'sources 'helm-source-recentf)
        
    (unwind-protect
         (helm :sources sources
               :ff-transformer-show-only-basename nil
               :buffer "*helm multi files*"
               :truncate-lines helm-buffers-truncate-lines))))

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'leo-helm-recentf-and-multi-files)
(define-key helm-command-map (kbd "C-x C-f") 'helm-for-files)

;;(global-set-key [?\C-x ?\M-l] 'helm-locate)

;;(helm-mode 1)
