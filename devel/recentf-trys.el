(defun leo-recentf-kill-ring-save-filename (filename 
                                            &optional wildcards)
  (kill-new filename))

;; this changes the behaviour of all fule-selects in a recentf buffer to 
;; just copying the path to the kill-ring
(setq recentf-menu-action 'leo-recentf-kill-ring-save-filename)
