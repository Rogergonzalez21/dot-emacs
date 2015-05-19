;;
;; coding things
;;
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "iSync\\.vcf\\'" 'utf-16)
(modify-coding-system-alist 'file "leo-.*\\.el\\'" 'iso-latin-1-dos)

;;
;; settings for trash deletion
;;
(setq delete-by-moving-to-trash t)
(setq trash-directory
      (cond
       ((eq system-type 'windows-nt)
        nil)
       ((eq system-type 'darwin)
        "~/.Trash/")
       (t nil)))

;;
;; path handling
;;
(defun leo-path-convert-for-os (path)
  "Changes an emacs path on windows systems (windows-nt & cygwin) to be used
 outside emacs"
  (cond
   ;; cygwin
   ((eq system-type 'cygwin)
    (let ((winfile (with-output-to-string
                     (call-process "cygpath" nil standard-output
                                   nil "-w" "-a" path))))
      ;; cut the CR at the end
      (substring winfile 0 -1)))
   ((eq system-type 'windows-nt)
    (replace-regexp-in-string "/" "\\\\" path))
   (t path)))

(defun leo-path-convert-kill-for-os ()
  "convert the content of the kill ring for the target os.
It determines what to do by inspecting the varaible `system-type'."
  (interactive nil)
  (kill-new (leo-path-convert-for-os (current-kill 0)) t)
  (if (called-interactively-p 'interactive)
      (message "New kill: %s" (current-kill 0))))

;;
;; completion
;;
(setq completion-ignore-case t)
;;(partial-completion-mode t)

;; enhance expand completion list
;; attention: this list is used to fill `dired-omit-extensions' as well!

;; leave .log in the ignore list
;; (dolist (ext '(".pdf" ".log" ".dll" ".lib" ".ico" ".so"))
(dolist (ext '(".pdf" ".dll" ".lib" ".ico" ".so"))
  (delete ext completion-ignored-extensions))

(dolist (ext '(".out" ".snm" ".nav"))
   (add-to-list 'completion-ignored-extensions ext))

;; biblatex stuff
(dolist (ext '(".run.xml" ".bcf" ".fdb_latexmk" ".fls"))
   (add-to-list 'completion-ignored-extensions ext))


;;
;; drag and drop of files
;;

;; special handling on macosx
(when (eq window-system 'ns) 
  ;; don't use ns-find-file, because it can open new frames instead use my own function
  (define-key global-map [ns-drag-file] 'leo-ns-open-files)
  (defun leo-ns-open-files ()
    "Open files in the list `ns-input-file'."
    (interactive)
    (mapc 'find-file ns-input-file)
    (setq ns-input-file nil))
)

;;
;; special handling for special files
;;
(auto-compression-mode 1)
;; Allow editing of binary .plist files.
(when (eq system-type 'darwin) 
  (add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])
  (jka-compr-update))

