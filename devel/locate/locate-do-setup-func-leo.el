(defun leo-locate-do-setup (search-string &optional run-locate-command)
  (goto-char (point-min))
  (save-excursion

    ;; Nothing returned from locate command?
    (let (db-err-text)

      (and (eobp)
           (progn
;;	   (kill-buffer locate-buffer-name)
             (setq db-err-text (if locate-fcodes-file
                               (format "database %s" locate-fcodes-file)
                             "default database"))
             (setq db-err-text (if locate-current-filter
                                   (format "no match in %s using filter %s"
                                           db-err-text locate-current-filter)
                                 (format "no match in %s" db-err-text)))))

      (locate-insert-header (concat search-string))
      (when (eobp)
        (insert-char ?\  locate-filename-indentation t)
        (insert db-err-text))

      (while (not (eobp))
        (insert-char ?\  locate-filename-indentation t)
        (locate-set-properties)
        (forward-line 1))
      (goto-char (point-min))
      (forward-char 2)
      (delete-backward-char 1)      
      (if (eq system-type 'windows-nt)
          (insert "c:"))
      (goto-char (point-min))
      (forward-line 1)

      (if run-locate-command
          (insert " ")
        (if leo-locate-basename-only
            (insert " Basename")
          (insert " Wholename")))
      ))    
  (goto-char (point-min)))
