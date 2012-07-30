;;
;; apperance functions
;;

(defun leo-appearance-gnus ()
  (interactive)
  (setcdr (assq 'background-color default-frame-alist) "DarkSeaGreen1")
  (setcdr (assq 'foreground-color default-frame-alist) "black")
  (setq frame-title-format '("%b (%f)")))
(defun leo-appearance-root ()
  (interactive)
  (setcdr (assq 'background-color default-frame-alist) "moccasin")
  (setcdr (assq 'foreground-color default-frame-alist) "black")
  (setq frame-title-format '("root - %b (%f)")))

;;
;; titel, etc
;;


(defun leo-small-font-this-frame ()
  "Make the font in this frame small"
  (interactive)
  (modify-frame-parameters (selected-frame)
    '((font . "-apple-monaco-medium-r-*--12-*-*-*-*-*-mac-roman"))))

(defun leo-big-font-this-frame ()
  "Make the font in this frame big (i.e. normal)"
  (interactive)
  (modify-frame-parameters (selected-frame)
    '((font . "-apple-monaco-medium-r-*--14-*-*-*-*-*-mac-roman"))))

;;
;; appereance of the frames
;;
(setq large-mac 
      (and (eq window-system 'ns)
           (> (display-pixel-height) 1000)))
(setq leo-max-frame-height
      (cond ((eq large-mac t)
             60)
            ((eq window-system 'ns)
             50)
            ((eq window-system 'w32)
             64)
            ((eq window-system 'x)
             64)
            (t 50)))

(setq leo-min-frame-top 
      (cond ((eq large-mac t)
             270)
            ((eq window-system 'ns)
             25)
            ((eq window-system 'w32)
             0)
            ((eq window-system 'x)
             100)
            (t 0)))

(setq leo-min-frame-left
      (cond ((eq large-mac t)
             10)
            ((eq window-system 'ns)
             10)
            ((eq window-system 'w32)
             0)
            ((eq window-system 'x)
             10)
            (t 0)))

(setq leo-min-special-frame-left
      (cond ((eq large-mac t)
             700)
            ((eq window-system 'ns)
             665)
            ((eq window-system 'w32)
             605)
            ((eq window-system 'x)
             605)
            (t 0)))

;;
;; set default- and initial-frame-alist
;;
(setq default-frame-alist 
      ;; standard staff
      '((tool-bar-lines . 0)
        ;; my staff:
        (background-color . "white")
        (foreground-color . "DarkBlue")
        (width . 90) (height . 1))) ;;height is reset later
;; add a font
(setq default-frame-alist
      (cond ((eq window-system 'ns)
             default-frame-alist )
            ((eq window-system 'w32)
             (append default-frame-alist 
                     '((font . "-outline-Courier New-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1"))))
            (t default-frame-alist)))
;; change default-frame-alist according to leo's height var 
(setcdr (assq 'height default-frame-alist) 
        (- leo-max-frame-height 2))
; in case of root do 
(if (eq (user-uid) 0)
    (leo-appearance-root))


; initial frame set up
(setq initial-frame-alist 
      '((left . 0)     ;;is reset below
        (top . 0)      ;;is reset below
        (height . 1))) ;;is reset below

;; change initial-frame-alist according to leo's vars 
(setcdr (assq 'height initial-frame-alist) 
        leo-max-frame-height)
(setcdr (assq 'top initial-frame-alist) 
        leo-min-frame-top)
(setcdr (assq 'left initial-frame-alist) 
        leo-min-frame-left)

;;
;; cascade frames as new ones are opened.
;; visual idea from: andy.ling@quantel.com
;;
;; the default-positions emacs chooses for frames
;; (the lists are generated with frames-make-positions.)
;;
(defun frames-make-positions (pos-start pos-step pos-times)  
  (let ((growl '())
        (left-start (car pos-start))
        (left-step (car pos-step))
        (left-times (car pos-times)))
    (dotimes (i left-times)
      (let ((top-start (cdr pos-start))
            (top-step (cdr  pos-step))
            (top-times (cdr  pos-times)))
        (dotimes (j top-times)
          (push (cons left-start top-start) growl)
          (setq top-start (+ top-start top-step))
          (setq left-start (+ left-start left-step))
          )))
    (reverse growl)))

;; the parameter-format is: 
;; '(left-start . top-start) '(left-step . top-step) '(left-times . top-times) 

(setq leo-frames-default-positions 
      (frames-make-positions (cons leo-min-frame-left leo-min-frame-top) 
			     '(20 . 15) 
			     '(10 . 2)))

(setq leo-special-frames-default-positions
      (frames-make-positions (cons leo-min-special-frame-left leo-min-frame-top) 
			     '(-20 . 15) 
			     '(10 . 2)))

(defun leo-frames-frame-on-position (pos)
  "returns the first frame in list FRAMES with the position POS, otherwise nil.

internal func for leo's frames managment"
  (let ((framelist (frame-list)) 
        frame)
    (while (and (setq frame (pop framelist))                
                (not (and
                      (equal 
                       (cdr (assq 'left (frame-parameters frame))) 
                       (car pos))
                      (equal 
                       (cdr (assq 'top (frame-parameters frame))) 
                       (cdr pos))))))
    frame))

(defun leo-frames-first-free-position (positions)
  "returns the first free position in POSITIONS, i.e. it returns the first position not taken by any visible frame in the alist of positions.

internal func for leo's frames managment"
  (if positions
      (let ((pos (car positions)))
        (if (leo-frames-frame-on-position pos)
            (progn
              (leo-frames-first-free-position (cdr positions))
              )
          pos))))

(defun leo-set-frame-position-from-list (the-frame the-positions)
  "does the set-frame-pos work for frame THE-FRAME and position alist THE-POSITIONS"
  (let ((free-pos (leo-frames-first-free-position
                   the-positions)))
    (if (not free-pos)
        (setq free-pos (car the-positions)))
    (set-frame-position the-frame (car free-pos) (cdr free-pos))))

                
;;
;; special frames
;;
;; frame parameter 
;; --> to avoid (unsplittable . t)
;;(setq special-display-frame-alist default-frame-alist)

;; set the list
(setq leo-info-frame-alist 
      '((left-fringe . 10)(right-fringe . 0)
        (width . 81)(height . 1))) ;; height is set later
(setcdr (assq 'height leo-info-frame-alist) 
        (- leo-max-frame-height 2))

(setq Man-frame-parameters leo-info-frame-alist)

(setq leo-help-frame-alist 
      '((top . 130)(left . 0)(user-position . t)
        (width . 81)(height . 30))) 

(setq special-display-regexps
      (list (cons "\\*Man .*" Man-frame-parameters)
            (cons "\\*info\\*.*" leo-info-frame-alist)
            (cons "\\*Help\\*.*" leo-help-frame-alist)
            ))

;; kill info in same-window-buffer-names
(delete "\\*info\\*\\(\\|<[0-9]+>\\)" same-window-regexps)

(defun leo-special-display-popup-frame (buffer &optional args)
  "Displays BUFFER like special-display-popup-frame, but without dedicating."
  (let ((window (special-display-popup-frame buffer args)))    
    (set-window-dedicated-p window nil)
    (ding)
    window))

;;(setq special-display-function 'leo-special-display-popup-frame)
;;(setq special-display-function 'special-display-popup-frame)

(defun leo-special-display-kill-buffer ()
  (let ((found nil)
        (curr-buf (current-buffer)))
    (dolist (elt special-display-regexps)
      (if (not found)      
          (setq found (string-match (car elt) (buffer-name curr-buf)))))
    (when found
      (let ((win (get-buffer-window curr-buf)))
        (if win
            (delete-frame (window-frame win))
          (message 
           (format "leo-special-display-kill-buffer: could not find window to %s" (buffer-name curr-buf)))))
    )))
  ;;(message ))
  ;;special-display-regexps
(add-hook 'kill-buffer-hook 'leo-special-display-kill-buffer)

;;
;; kill buffer and frame
;;
(defun leo-kill-this-buffer-and-frame ()
  "acts like kill-this-buffer but closes the frame as well."
  (interactive)
  (let ((f (selected-frame)))
    (kill-this-buffer)
    (delete-frame f)))

;;
;; just a helper function for debugging
;;
(defun leo-print-frapas ()
  "prints the parameters of the current frame."
  (interactive)
  (print (frame-parameters)))
