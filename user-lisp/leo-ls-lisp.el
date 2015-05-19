;;
;; ls-lisp
;;
(require 'ls-lisp)

(setq ls-lisp-use-insert-directory-program nil) ;; make sure we use ls-lisp!
(setq ls-lisp-dirs-first t)
(setq ls-lisp-ignore-case t)

(defcustom leo-ls-lisp-dotdirs-first t
  "*Non-nil causes leos adapted ls-lisp to sort the special directories '.' and '..' first in any ordering.
\(Or last if it is reversed.)"
  :type 'boolean
  :group 'leos
  :group 'ls-lisp)

;;
;; cloning
;;
(defun unused-ls-lisp-insert-directory
  (file switches time-index wildcard-regexp full-directory-p)
  "--- Changed by Leo: ---
Fix the bug with Dropbox error \"Format specifier doesn't match argument type\".
See http://stackoverflow.com/questions/7247327/osx-emacs-dired-error-on-dropbox
--- End of `Changed'---
Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  (if (or wildcard-regexp full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard-regexp t
					      (if (memq ?n switches)
						  'integer
						'string)))
	     (now (current-time))
	     (sum 0)
	     (max-uid-len 0)
	     (max-gid-len 0)
	     (max-file-size 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr
	     fuid fgid uid-len gid-len)
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
          ;; Find the appropriate format for displaying uid, gid, and
          ;; file size, by finding the longest strings among all the
          ;; files we are about to display.
          (dolist (elt file-alist)
            (setq attr (cdr elt)
                  fuid (or (nth 2 attr) "")
                  uid-len (if (stringp fuid) (string-width fuid)
                            (length (format "%d" fuid)))
                  fgid (or (nth 3 attr) "") ;; changed by leo
                  gid-len (if (stringp fgid) (string-width fgid)
                            (length (format "%d" fgid)))
                  file-size (or (nth 7 attr) 0)) ;; changed by leo
	    (if (> uid-len max-uid-len)
		(setq max-uid-len uid-len))
	    (if (> gid-len max-gid-len)
		(setq max-gid-len gid-len))
	    (if (> file-size max-file-size)
		(setq max-file-size file-size)))
	  (setq ls-lisp-uid-d-fmt (format " %%-%dd" max-uid-len))
	  (setq ls-lisp-uid-s-fmt (format " %%-%ds" max-uid-len))
	  (setq ls-lisp-gid-d-fmt (format " %%-%dd" max-gid-len))
	  (setq ls-lisp-gid-s-fmt (format " %%-%ds" max-gid-len))
	  (setq ls-lisp-filesize-d-fmt
		(format " %%%dd"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq ls-lisp-filesize-f-fmt
		(format " %%%d.0f"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 ;; Under -F, we have already decorated all
			 ;; directories, including "." and "..", with
			 ;; a /, so allow for that as well.
			 (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard-regexp full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (file-name-absolute-p file) (setq file (expand-file-name file)))
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file 'string)))
      (if fattr
	  (insert (ls-lisp-format
		   (if (memq ?F switches)
		       (ls-lisp-classify-file file fattr)
		     file)
		   fattr (nth 7 fattr)
				  switches time-index (current-time)))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!



(defun leo-ls-lisp-trala-char (c)
  "used in leo-ls-lisp-string-lessp to sort '_' and '~' at the beginning of the alphabet"
  (if (eq c ?_) (setq c 30))
  (if (eq c ?~) (setq c 31))
  (char-to-string c))

(defsubst leo-ls-lisp-string-lessp (s1 s2)
  "Return t if string S1 is less than string S2 in lexicographic order.
Case is significant if `ls-lisp-ignore-case' is nil.
Is tweaked for '~' and '_' to sort them at the beginning of the alphabet."
  (let* ((ss1 (mapconcat 'leo-ls-lisp-trala-char s1 ""))
         (ss2 (mapconcat 'leo-ls-lisp-trala-char s2 ""))
         (u (compare-strings ss1 0 nil ss2 0 nil ls-lisp-ignore-case)))
    (and (numberp u) (< u 0))))

(defun leo-move-dotdirs-to-front (list toback)
  "move the entries of LIST containig the dirs '.' and .\'..' to then front of LIST.
if argument TOBACK is t, append the entry at the back rather then adding at the front."
  (let (newlist found-d found-dd)
    (dolist (elt list newlist)
      (cond ((equal (car elt) ".")
             (setq found-d elt))
            ((equal (car elt) "..")
             (setq found-dd elt))
            (t
             (push elt newlist))))
    (setq newlist (nreverse newlist))
    (if found-dd
        (if toback
            (setq newlist (append newlist (list found-dd)))
          (setq newlist (cons found-dd newlist))))
    (if found-d
        (if toback
            (setq newlist (append newlist (list found-d)))
          (setq newlist (cons found-d newlist))))    
    newlist))

(defun ls-lisp-handle-switches (file-alist switches)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  Default sorting is alphabetic."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (or (memq ?U switches)		; unsorted
      ;; Catch and ignore unexpected sorting errors
      (condition-case err
	  (setq file-alist
		(let (index)
		  ;; Copy file-alist in case of error
		  (sort (copy-sequence file-alist) ; modifies its argument!
			(cond ((memq ?S switches)
			       (lambda (x y) ; sorted on size
				 ;; 7th file attribute is file size
				 ;; Make largest file come first
				 (< (nth 7 (cdr y))
				    (nth 7 (cdr x)))))
			      ((setq index (ls-lisp-time-index switches))
			       (lambda (x y) ; sorted on time
				 (time-less-p (nth index (cdr y))
					      (nth index (cdr x)))))
			      ((memq ?X switches)
			       (lambda (x y) ; sorted on extension
				 (leo-ls-lisp-string-lessp
				  (ls-lisp-extension (car x))
				  (ls-lisp-extension (car y)))))
			      (t
			       (lambda (x y) ; sorted alphabetically
				 (leo-ls-lisp-string-lessp (car x) (car y))))))))
	(error (message "Unsorted (ls-lisp sorting error) - %s"
			(error-message-string err))
	       (ding) (sit-for 2))))	; to show user the message!
  (if (memq ?F switches)		; classify switch
      (setq file-alist (mapcar 'ls-lisp-classify file-alist)))

  (if leo-ls-lisp-dotdirs-first
  ;; Re-sort directories for the special dirs '.' and '..'
      (setq file-alist (leo-move-dotdirs-to-front file-alist (memq ?U switches))))

  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (or (eq (cadr (setq el (car file-alist))) t) ; directory
                  (and (stringp (cadr el))
                       (file-directory-p (cadr el)))) ; symlink to a directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)
		))))
  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(not (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (not (memq ?U switches))	; unsorted order is reversed
	      (not (memq ?r switches)))	; reversed sort order requested
	  ls-lisp-dirs-first)		; already reversed
      (nreverse file-alist)
    file-alist))

(defun leo-toggle-ls-lisp-verbosity ()
  (interactive)
  (if (memq 'uid ls-lisp-verbosity)
      (progn
        (setq ls-lisp-verbosity (delq 'uid ls-lisp-verbosity))
        (setq ls-lisp-verbosity (delq 'gid ls-lisp-verbosity))
        (revert-buffer)
        (message "uid & gid hidden"))
    (progn
      (add-to-list 'ls-lisp-verbosity 'uid)
      (add-to-list 'ls-lisp-verbosity 'gid)
      (revert-buffer)
      (message "uid & gid visible"))))
