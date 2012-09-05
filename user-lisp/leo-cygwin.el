;;
;; cygwin stuff
;;
(defcustom leo-cygwin-root
  (cond ((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
         "c:/cygwin/")
        (t
         ""))
  "*The (windows) path to the root of the cygwin file system (where the 
directories /bin /usr /etc et al live)."
  :type 'string
  :group 'leos
  :group 'cygwin)
