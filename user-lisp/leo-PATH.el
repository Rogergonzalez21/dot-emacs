(defun leo-prepend-to-and-clean-PATH (add-me &optional no-clean)
  "Prepend paths in ADD-ME to environment variable \"PATH\"
and cleans (i.e. reduplicates) the paths.

* ADD-ME has to be a PATH-like string like PATH.
* If NO-CLEAN is non-nil, no deduplication happens.
"
  (let* ((PATHlist (pathstr-to-list (getenv "PATH")))
         (addlist (pathstr-to-list add-me))
         (res (append addlist PATHlist)))
    (if (not no-clean)
        (delete-dups res))
    (list-to-pathstr res)))

(defun pathstr-to-list (str &optional sys-type)
  (let* ((stype (or sys-type system-type))
         (pathsep (if (eq stype 'windows-nt) ";" ":")))
    (split-string str pathsep)))

(defun list-to-pathstr (lst &optional sys-type)
  (let* ((stype (or sys-type system-type))
         (pathsep (if (eq stype 'windows-nt) ";" ":")))
    (mapconcat 'identity lst pathsep)))

;;
;; update environment variable PATH from STORED_PATH
;; note: for command `shell-command'

(setenv "PATH" (if (getenv "STORED_PATH")
                   (leo-prepend-to-and-clean-PATH (getenv "STORED_PATH"))
                 (leo-prepend-to-and-clean-PATH "")))
  

;;;
;; Code snippets
;;
;;for the os-depended separator in paths
;; (sep
;;  (if (eq stype 'windows-nt)
;;      (string ?\\)
;;    "/"))
;;
;; (regexp-quote "\\")
;;
;; for exec-path (not needed when mangling PATH)
;; exec-path: add directories from $STORED_PATH or $PATH
;; note: for internal used executables like aspell et al
;; (let* ((paths (getenv "PATH"))
;;        (path-list (split-string paths path-separator)))
;;   (dolist (item (reverse path-list))
;;     (setq exec-path (add-to-list 'exec-path item))))
