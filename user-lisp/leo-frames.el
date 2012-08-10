;;
;; popwin
;;
(require 'popwin)
(global-set-key (kbd "C-x C-p") popwin:keymap)

;;
;; to restore specaildisplay-function with pop-win.el
;;
(defun leo-popwin:display-buffer (buffer-or-name &optional not-this-window)
  "Display BUFFER-OR-NAME, if possible, in a popup window, or as
usual. This function can be used as a value of
`display-buffer-function'."
  (interactive "BDisplay buffer:\n")
  ;; `special-display-p' returns either t or a list of frame
  ;; parameters to pass to `special-display-function'.
  (let ((pars (special-display-p name-of-buffer)))
    (if (and pars special-display-function)
        (funcall special-display-function
                 buffer (if (listp pars) pars))
      (popwin:display-buffer buffer-or-name not-this-window))))

;;(setq display-buffer-function 'leo-popwin:display-buffer)
;; TODO: delete leo=popwin:...

;;
;; apperance functions
;;
(defun leo-appearance-root ()
  (interactive)
  (setcdr (assq 'background-color default-frame-alist) "moccasin")
  (setcdr (assq 'foreground-color default-frame-alist) "black")
  (setq frame-title-format '("root - %b (%f)")))

;;
;; appereance of the frames
;;
(setq large-mac 
      (and (eq window-system 'ns)
           (> (display-pixel-height) 1000)))

(setq leo-max-frame-height
      (cond ((eq large-mac t)
             63)
            ((eq window-system 'ns)
             50)
            ((eq window-system 'w32)
             60)
            ((eq window-system 'x)
             65)
            (t 50)))

(setq leo-max-frame-width
      (cond ((eq large-mac t)
             85)
            ((eq window-system 'ns)
             83)
            ((eq window-system 'w32)
             83)
            ((eq window-system 'x)
             83)
            (t 83)))

(setq leo-min-frame-top 
      (cond ((eq large-mac t)
             223)
            ((eq window-system 'ns)
             25)
            ((eq window-system 'w32) 
             207)
            ((eq window-system 'x) 
             230)
            (t 0)))

(setq leo-min-frame-left
      (cond ((eq large-mac t)
             4)
            ((eq window-system 'ns)
             10)
            ((eq window-system 'w32)
             0)
            ((eq window-system 'x) 
             8)
            (t 0)))

(setq leo-inc-frame-top 0)

(setq leo-inc-frame-left
      (cond ((eq large-mac t)
             639)
            ((eq window-system 'ns)
             400)
            ((eq window-system 'w32)
             640)
            ((eq window-system 'x) 
             640)
            (t 0)))

;;
;; set default- and initial-frame-alist
;;
(setq default-frame-alist 
      ;; standard staff
      '((tool-bar-lines . 0)
        (horizontal-scroll-bars . t)
        ;;(window-system . x)
        ;; my staff:
        (background-color . "white")
        (foreground-color . "DarkBlue")
        (width . 1) 
        (height . 1))) ;;height is reset later
;; add a font
(setq default-frame-alist
      (cond ((eq window-system 'ns)
             (append default-frame-alist '((font . "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))))
            ((eq window-system 'w32)
             (append default-frame-alist '((font . "-outline-Courier New-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1"))))
            ((eq window-system 'x)
             (append default-frame-alist '((font . "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))))
            (t default-frame-alist)))

;; change default-frame-alist according to leo's dimension vars 
(setcdr (assq 'height default-frame-alist) 
        leo-max-frame-height)
(setcdr (assq 'width default-frame-alist) 
        leo-max-frame-width)
; in case of root do 
(if (eq (user-uid) 0)
    (leo-appearance-root))

(setq frame-title-format "%b - emacs")
; initial frame set up
(setq initial-frame-alist 
      (append '((name . "main - emacs") (top . 1) (left . 1)) default-frame-alist))


;; change initial-frame-alist according to leo's position vars 
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
			     (cons leo-inc-frame-left leo-inc-frame-top)
			     '(3 . 1)))


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

(add-hook 'after-make-frame-functions
          (lambda (arg)
            (leo-set-frame-position-from-list arg leo-frames-default-positions)
            ))

(defun leo-set-frame-position-from-index (index)
  "set frame postion from index in default position list."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     ;; Look for a default, a number in the buffer at point.
     (let* ((default 0))
       ;; Read the argument, offering that number (if any) as default.
       (list (read-number (format "Move frame to position: "
                                  default)
                          default)))))
  (let ((pos (nth index leo-frames-default-positions)))
    (set-frame-position (selected-frame) (car pos) (cdr pos))))

;;
;; set the special frame alist
;;

(setq special-display-frame-alist default-frame-alist)

(setq leo-help-frame-alist 
      '((top . 130)(left . 0)(user-position . t)
        (width . 83)(height . 30))) 

(setq special-display-regexps
      (list "\\*Man .*" "\\*info\\*.*"))

;; kill these from same-window-buffer-names/same-window-regexps
(setq same-window-regexps
      (delete "\\*info\\*\\(\\|<[0-9]+>\\)" same-window-regexps))
;;(setq same-window-buffer-names 
;;      (delete "*shell*" same-window-buffer-names))

(defun leo-special-display-popup-frame (buffer &optional args)
  "Displays BUFFER like special-display-popup-frame, but without dedicating."
  (let ((window (special-display-popup-frame buffer args)))    
    (set-window-dedicated-p window nil)
    (ding)
    window))

;;(setq special-display-function 'leo-special-display-popup-frame)
;;(setq special-display-function 'special-display-popup-frame)

;;
;; use this to put server documents in the initial frame
;;
(defun leo-switch-to-buffer-initial-frame (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in the initial frame frame.

The initial frame is defined by its title/name as \"main - emacs\"."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in initial frame: ")))
  (select-frame-by-name "main - emacs")
)

;;
;; function which kills the buffers frame if it is an special one
;; (needed when special buffer are NOT dedicated!)
;;
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
;;(add-hook 'kill-buffer-hook 'leo-special-display-kill-buffer)

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
(defun lx-print-frapas ()
  "prints the parameters of the current frame."
  (interactive)
  (dolist (elt (frame-parameters))
    (prin1 elt)
    (terpri)
    ))

(defun lx-print-particular-frapas ()
  "prints particular parameters of the current frame."
  (interactive)
  (let ((params '(top left width height font)))
    (dolist (elt params)
      (prin1 (assq elt (frame-parameters)))
      ;;(terpri)
      )))
