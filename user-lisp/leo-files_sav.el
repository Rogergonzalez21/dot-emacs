;;
;; coding things
;;
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "iSync\\.vcf\\'" 'utf-16)
(modify-coding-system-alist 'file "leo-.*\\.el\\'" 'iso-latin-1-dos)

;;
;; path handling
;;
(defun leo-path-convert-to-os (path)
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
  "convert the conetnt of the kill ring for the target os.
It determines what to do by inspecting the varaible `system-type'."
  (interactive nil)
  (kill-new (leo-path-convert-to-os (current-kill 0)) t)
  (if (called-interactively-p 'interactive)
      (message "New kill: %s" (current-kill 0)))))

;;
;; completion
;;
(setq completion-ignore-case t)
(partial-completion-mode t)

;; enhance expand completion list
;; attention: this list is used to fill `dired-omit-extensions' as well!

(dolist (ext '(".~pas" ".~dfm" ".~dpr" ".~bpg" ".~ddp" "~dsk" ".dcu" ".dpu"))
  (add-to-list 'completion-ignored-extensions ext))
(dolist (ext '(".pdf" ".log" ".dll" ".lib" ".ico" ".so"))
  (delete ext completion-ignored-extensions))

;;
;; drag and drop of files
;;

;; special handling on macosx
(when (eq window-system 'ns) 
  ;; don't use ns-find-file, beacuse it can open new frames instead use my own function
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
  (jka-compr-update)
)
)