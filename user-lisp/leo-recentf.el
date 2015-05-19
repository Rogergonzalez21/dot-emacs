;;
;; recentf customization
;;
(require 'recentf)
(setq recentf-save-file
      (concat leo-emacs-userdata-path ".recentf"))
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

(defadvice recentf-save-list (around recentf-save-list-around activate)
       "Save recentf list to `recentf-save-file' only if writable."
       (if (file-writable-p recentf-save-file)
           ad-do-it))

;; ---------------------------------------------------------------------------
(defun leo-recentf-open-files-other-frame ()
  (interactive)
  (select-frame-set-input-focus (make-frame-command))
  (recentf-open-files))
  

;; ---------------------------------------------------------------------------
(defcustom leo-recentf-split-column 25
  "*Split Column for displaying the recentf list with splitted filenames. 
Defines at what column the seperated path is displayed."
  :type 'integer
  :group 'recentf
  :group 'leos)

(defun recentf-make-default-menu-element (file-path) 
  "--- Changed by Leo: ---
MENU-ITEM now starts with the NAME of the file and adds the path more right.
--- End of `Changed'---

Make a new menu element (MENU-ITEM . MENU-VALUE).
Do so for the given recent file path FILE-PATH.  
 See also
`recentf-make-menu-element'."
  (let ((path (replace-regexp-in-string "/[^/]*$" "/" file-path))
        (title  (leo-fill-string-right (replace-regexp-in-string ".*/" "" file-path) leo-recentf-split-column)))    
    (recentf-make-menu-element (concat title " " path) file-path)))

(defun recentf-show-spaces-for-digits-filter (l)
  "Filter the list of menu-elements L to show spaces where earlier thet shortcut digits where."
  (dolist (e l)
    (recentf-set-menu-element-item
     e (format "    %s" (recentf-menu-element-item e))))
  l)

(setq recentf-menu-filter 'recentf-show-spaces-for-digits-filter)

;; ---------------------------------------------------------------------------
;;
;; find-file with recentf-list-completion
;; seen at emacs wiki
;;
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function 
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl))))) 

;;
;; dirctory should go in the recentf-list as well
;;
(add-hook 'dired-mode-hook
           (lambda ()
             (when recentf-mode
               (recentf-add-file default-directory))))

