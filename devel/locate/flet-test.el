(defun myprint (msg)
  (print msg)
  nil)

(progn
 (setq myvar "mytext")
 (defun x (msg)
   (myprint (format "--- msg=%s, myvar=%s---" msg myvar)))
)

(defun myprintoverride (str)
  (print "I don't take the argument"))


(defun y (msg)
  (flet ((myprint (str) (myprintoverride str)))
    (let ((myvar "dreizehn"))
      (x msg)
      nil)
    ))

(progn
  (myprint "yep")
  (x "hallo")
  (y "nochmal")
)
