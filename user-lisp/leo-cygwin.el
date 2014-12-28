;;
;; cygwin stuff
;;

;; many things here are taken from http://www.emacswiki.org/emacs/setup-cygwin.el

(defcustom leo-cygwin-root
  (cond ((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
         "c:/cygwin/")
        (t
         ""))
  "*The (windows) path to the root of the cygwin file system (where the 
directories /bin /usr /etc et al live)."
  :type 'directory
  :group 'leos
  :group 'cygwin)

(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (require 'cygwin-mount)
  ;;; Make Cygwin paths accessible
  (cygwin-mount-activate)

  ;;; TRACK DIRECTORIES WITH CYGPATH
  (setq shell-dirstack-query "cygpath -m `dirs`")

;;; Follow Cygwin symlinks.
;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
  (defun follow-cygwin-symlink ()
    "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
    (save-excursion
      (goto-char 0)
      (if (looking-at
           "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
          (progn
            (re-search-forward
             "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
            (find-alternate-file (match-string 1)))
        (when (looking-at "!<symlink>")
          (re-search-forward "!<symlink>\\(.*\\)\0")
          (find-alternate-file (match-string 1))))))
  (add-hook 'find-file-hooks 'follow-cygwin-symlink)

;;; Use Unix-style line endings.
  (setq-default buffer-file-coding-system 'undecided-unix)

;;; Use /dev/null, not NUL.
  (setq null-device  "/dev/null")

;;; Add Cygwin Info pages
  (add-to-list 'Info-default-directory-list (expand-file-name "usr/info" leo-cygwin-root) 'APPEND)

;;; Use things for bash
  (setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

  (setq process-coding-system-alist
        (cons '("bash" . (raw-text-dos . raw-text-unix)) process-coding-system-alist))
)
