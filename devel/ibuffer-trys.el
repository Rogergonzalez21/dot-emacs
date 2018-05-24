(define-ibuffer-filter filename
  "Toggle current view to buffers with filename matching QUALIFIER."
  (:description "file-or-dir"
   :reader (read-from-minibuffer "Filter by filename or directory (regexp): "))
  (ibuffer-awhen (ibuffer-make-column-filename buf nil)
    (string-match qualifier it)))
