
;; 1 try
(modify-syntax-entry ?_ "w" prog-mode-syntax-table)

;; maybe 2nd try
(defvar leo-syntax-table
  ;; has underscore as text char
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table))

(defun lx-chg-undetscore ()
  (interactive)
  (modify-syntax-entry ?_ "w")
  )
