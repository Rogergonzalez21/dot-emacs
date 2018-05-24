;;
;; cygwin stuff
;;

;; many things here are taken from http://www.emacswiki.org/emacs/setup-cygwin.el

(defcustom leo-cygwin-root
  (cond ((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
         "c:/software/Git")
        (t
         ""))
  "*The (windows) path to the root of the cygwin file system (where the 
directories /bin /usr /etc et al live)."
  :type 'directory
  :group 'leos
  :group 'cygwin)

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  
  ;;; TRACK DIRECTORIES WITH CYGPATH
  ;;; commented out coz we use git-bash
  ;;(setq shell-dirstack-query "cygpath -m `dirs`")

  ;;; Use Unix-style line endings.
  (setq-default buffer-file-coding-system 'undecided-unix)

  ;;; Use /dev/null, not NUL.
  (setq null-device  "/dev/null")
)
