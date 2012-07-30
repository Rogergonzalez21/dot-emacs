;;
;; all web (html/php/javascript) things
;;

;;
;; html helper mode
;;
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(add-hook 'html-helper-load-hook
          (function (lambda () (load "leo-html-in-hook.el"))))

;; no messages
(setq html-helper-verbose nil)

;; template for new html files
(setq html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html>\n"
    "<head>\n"
    "  <title>" p "</title>\n"
    "</head>\n"
    "<body>\n"
    "  <h1><script language=\"javascript\" type=\"text/javascript\">"
    "document.write(document.title);</script></h1>\n"
    "<hr>\n"
    p
    "\n<hr>\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body> </html>\n"))


;; changed timestamp updater 
;; (no message when there is no timestamp start delimiter)
(defun html-helper-update-timestamp ()
  "--- Changed by Leo: ---
Doesn't write a message when start delimiter wasn't found.
--- End of `Changed'---

Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`html-helper-timestamp-start', deletes all text up to
`html-helper-timestamp-end', and runs `html-helper-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward html-helper-timestamp-start nil t))
	() ;; orig: (message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length html-helper-timestamp-start)))
	    (ts-end (if (search-forward html-helper-timestamp-end nil t)
			(- (point) (length html-helper-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'html-helper-timestamp-hook)))))
  nil)

;;
;; php mode
;;
(require 'php-mode)

(defun leo-php-mode-defaults ()  
   "Set some buffer-local variables."
   (interactive)
   (setq case-fold-search t)
   (setq indent-tabs-mode nil)
   (setq fill-column 78)
   (setq c-basic-offset 4)
   (c-set-offset 'arglist-cont 0)
   (c-set-offset 'arglist-intro '+)
   (c-set-offset 'case-label 2)
   (c-set-offset 'arglist-close 0))
 (add-hook 'php-mode-hook 'leo-php-mode-defaults)
