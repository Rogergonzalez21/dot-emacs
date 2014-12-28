;;
;; spelling and natural language helpers
;;

;;
;; ispell
;;
(setq-default ispell-program-name "aspell")

(defun leo-spell-toggle-mode ()
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (flyspell-mode)
    (flyspell-buffer)))

(defun leo-spell-switch-local-dictionary ()
  "Switch dictionary for Ispell.
If the dictionary was \"deutsch\", switch to \"britsh\";
if it was not \"deutsch\", switch to \"deutsch\".

Afterwards it spell checks the whole buffer."
  (interactive)
  (let ((olddict (or ispell-local-dictionary
		      ispell-dictionary "default")))
    (if (equal olddict "deutsch")
        (ispell-change-dictionary "british")
      (ispell-change-dictionary "deutsch"))
    (flyspell-buffer)))
    

(eval-after-load 'flyspell
    '(progn
       (delq (assoc 'mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (delq (assoc 'down-mouse-2 flyspell-mouse-map) flyspell-mouse-map)
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)
       (define-key flyspell-mode-map [?\C-x (control ?\,)] 'flyspell-buffer)
       (define-key flyspell-mode-map [?\C-x (control ?\.)] 'leo-spell-switch-local-dictionary)))

;;
;; synonyms
;;
(setq synonyms-file
      (cond ((eq system-type 'windows-nt)
             "c:/cygwin/usr/local/share/lang/mthesaur.txt")
            (t
             "/usr/local/share/lang/mthesaur.txt")))
(setq synonyms-cache-file
      (cond ((eq system-type 'windows-nt)
             "c:/cygwin/usr/local/var/synonyms")
            (t
             "/usr/local/var/cache/synonyms")))

;; because i have a synoyms autoload file in site-start.d I shouldn't need this: (but god knows why i do!)
(autoload 'synonyms "synonyms.el"
    "Show synonyms that match a regular expression (e.g. a word or phrase)." t)
