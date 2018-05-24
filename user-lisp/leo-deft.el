;;
;; deft (should replace notes stuff)
;;
(require 'deft)

(defcustom leo-deft-done-directory (expand-file-name "~/.deft/_done")
  "Deft directory."
  :type 'directory
  :safe 'stringp
  :group 'leos
  :group 'deft)

(setq deft-directory
      (expand-file-name "~/notes/deft"))
(setq leo-deft-done-directory
      (expand-file-name "~/notes/deft/_done"))

(setq deft-default-extension "md")

(defun leo-deft-switch-and-filter-clear ()
  "switch to deft buffer and clear filter."
  (interactive)
  ;; just to make sure message is displayed if deft buffer has to be created
  (leo-deft-initialise-if-needed) 
  ;; go one in the normal way
  (deft)
  (deft-filter-clear))

(defun leo-deft-exists ()
  (let ((dft (get-buffer deft-buffer)))
    (if (bufferp dft)
        (with-current-buffer dft
          (eq major-mode 'deft-mode)))))

(defun leo-deft-initialise-if-needed ()
  "Make sure a deft buffer exists."
  (interactive)
  (if (not (leo-deft-exists))
      (with-current-buffer (get-buffer-create deft-buffer)        
        (if (not (eq major-mode 'deft-mode))
            (with-temp-message (format "Initialise %s buffer" deft-buffer)
              (deft-mode))))))

(defvar leo-deft-name-history nil
  "History list of names of deft notes entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defun leo-deft-new-file-named (file)
  "Create a new file named FILE (or interactively prompt for a filename).

Like `deft-new-file-named', just make sure deft buffer is created."
  (interactive 
   (list
    (read-string "New filename (without extension): " nil 'leo-deft-name-history "scratch")))
  (leo-deft-initialise-if-needed)
  (print file)
  (deft-new-file-named file))

(defun leo-deft-make-file-done ()
  "Make the file represented by the widget at the point done.
If the point is not on a file widget, do nothing. Does NOT prompt before
proceeding.

Note: \"Making\" a file done means moveing it to the directory 
`leo-deft-done-directory'."
  (interactive)
  (let ((filename (widget-get (widget-at) :tag)))
    (when filename
      ;;(when (y-or-n-p
      ;;     (format "Make file %s done?" (file-name-nondirectory filename)))
        (rename-file filename leo-deft-done-directory 1)
        (message (format "%s made done" (file-name-nondirectory filename)))
        (delq filename deft-current-files)
        (delq filename deft-all-files)
        (deft-refresh))))

(eval-after-load 'deft
    '(progn
       (define-key deft-mode-map (kbd "C-c C-d") 'leo-deft-make-file-done)))
    
