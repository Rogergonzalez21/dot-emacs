(setq mylist '(a))

(print (car mylist))

(progn
  (message-box "result = %s" (map-y-or-n-p
   "delete helmet %s? "                        ;;prompter
   (lambda (arg)                               ;;actor
     (print "helmet %s deleted" arg))
   mylist                                      ;;list
   '("directory" "directories" "delete")
   ))
)

(defun x ()
  (interactive)
  (setq leo-delete-query nil)
  (dolist (fn '("file1" "file2" "fileDrei") )
    (let* ((help-form '(format "\
Type SPC or `y' to delete folder `%s',
DEL or `n' to skip this folder,
ESC or `q' to skip ALL follwoing folders (but specified files still get deleted),
`!' to delete ALL remaining files with no more questions.
C-g to abort everything." fn))
           ans)
      (setq ans (dired-query 'leo-delete-query 
                 "Recycle delete dir `%s'? "
                 fn))
      (if ans
          (message-box "DO file = %s (ans = %s / ldq = %s)" fn ans leo-delete-query)
        (message-box "DON'T DO file = %s (ans = %s / ldq = %s)" fn ans leo-delete-query)))
    ))
