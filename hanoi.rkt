#lang racket

(require graphics/graphics racket/gui)
(provide hanoi)

(define-syntax-rule
  (define-values-block (value ...) expr ...)
  (define-values (value ...)
    (let () expr ...
      (values value ...))))

(define-syntax-rule (in-reversed-range n) (in-range (sub1 n) -1 -1))

(define (add-posn pos width height)
  (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
  
;=====================================================================================================
; Main procedure.

(define (main)
  (case mode
    ((manual) (manual))
    ((short)
     (short)
     (set! mode 'manual)
     ((draw-button-content vp) mode-pos "manual")
     (main))
    ((long)
     (reset #f)
     (long)
     (set! mode 'manual)
     ((draw-button-content vp) mode-pos "manual")
     (main))
    ((hamilton)
     (reset #f)
     (hamilton)
     (set! mode 'manual)
     ((draw-button-content vp) mode-pos "manual")
     (main))))

;=====================================================================================================
; A region is used to dispatch a mouse-click. Used for buttons and piles.

(struct region (pos width height)
  #:omit-define-syntaxes
  #:constructor-name make-region)

(define (in-region? pos region)
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define x-min (posn-x (region-pos region)))
  (define y-min (posn-y (region-pos region)))
  (define x-max (+ x-min (region-width region)))
  (define y-max (+ y-min (region-height region)))
  (and (<= x-min x x-max) (<= y-min y y-max)))

;=====================================================================================================
; State variables.

(define max-height 9)
(define height max-height) ; always (<= 1 height max-height)
(define mode 'manual)      ; manual, short, long or hamilton
(define delay 'click)      ; click, positive real
(define config (vector (range max-height) '() '())) ; each element an ascending sorted list of disks

;=====================================================================================================
; Elementary dimensions..

(define block 20)
(define border (* 3 block))

;=====================================================================================================
; Layout of buttons and related procedures.

(define-values-block (draw-button draw-button-content button-width button-height)
  (open-graphics)
  (define vp (open-pixmap "string-sizes" 500 500))
  (define button-strings (list "Height" "Mode" "Reset" "Setup" "Quit"))
  (define button-content-strings (list "Short" "Long" "Hamilton"))
  (define string-offset 4)
  (define-values (button-width button-height)
    (for/fold ((w 0) (h 0) #:result (values (+ w (* 2 string-offset)) (+ h (* 2 string-offset))))
      ((w/h (in-list (map (get-string-size vp) (append button-strings button-content-strings)))))
      (values
        (max w (inexact->exact (ceiling (car w/h))))
        (max h (inexact->exact (ceiling (cadr w/h)))))))
  (close-viewport vp)
  (close-graphics)
  (define ((draw-button vp) pos str)
    (define x (posn-x pos))
    (define y (posn-y pos))
    ((draw-solid-rectangle vp)
     pos button-width button-height "blue")
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- string-offset))) str "white"))
  (define ((draw-button-content vp) pos str)
    (define x (posn-x pos))
    (define y (+ (posn-y pos) button-height))
    ((clear-solid-rectangle vp) (make-posn x y) button-width button-height)
    ((draw-rectangle vp)  (make-posn x y) button-width button-height "blue")
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- (* 2 string-offset)))) str "blue")))

(define height-pos (make-posn border border))
(define mode-pos  (add-posn height-pos (+ button-width border) 0))
(define speed-pos (add-posn mode-pos   (+ button-width border) 0))
(define reset-pos (add-posn speed-pos  (+ button-width border) 0))
(define setup-pos (add-posn reset-pos  (+ button-width border) 0))
(define quit-pos  (add-posn setup-pos  (+ button-width border) 0))
(define height-region (make-region height-pos button-width button-height))
(define mode-region   (make-region mode-pos button-width button-height))
(define speed-region  (make-region speed-pos  button-width button-height))
(define reset-region  (make-region reset-pos  button-width button-height))
(define setup-region  (make-region setup-pos  button-width button-height))
(define quit-region   (make-region quit-pos   button-width button-height))

;=====================================================================================================
; Dispatch mouse-clicks.

(define (get-click (get? #t))
  (define click ((if get? get-mouse-click ready-mouse-click) vp))
  (cond
    (click
      (define pos (mouse-click-posn click))
      (cond
        ((in-region? pos height-region) 'height)
        ((in-region? pos mode-region) 'mode)
        ((in-region? pos speed-region) 'speed)
        ((in-region? pos reset-region) 'reset)
        ((in-region? pos setup-region) 'setup)
        ((in-region? pos quit-region) 'quit)
        ((in-region? pos (pile-region 0)) 0)
        ((in-region? pos (pile-region 1)) 1)
        ((in-region? pos (pile-region 2)) 2)
        (get? (get-click))))
    (get? (get-click))))

;=====================================================================================================
; Layout of window, disks and piles.

(define disk-height block)
(define min-disk-width (* 3 block))
(define disk-width-incr block)
(define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width (disk-width (sub1 max-height )))
(define pile-top (* 2 block))
(define pile-width 4)
(define pile-y (* 2 (+ border button-height)))
(define pile-height (+ pile-top (* max-height disk-height)))

(define (pile-region p)
  (define x (+ block border (* p (+ max-disk-width border))))
  (define y (- vp-height border block pile-top (* max-height disk-height)))
  (make-region (make-posn x y) max-disk-width (+ pile-top (* max-height disk-height))))

(define (pile-x p)
  (+ border
    block
    (* p (+ border max-disk-width))
    (/ (- max-disk-width pile-width) 2)))

;=====================================================================================================
; Actions.

(define (draw-piles)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle vp)
     (make-posn (pile-x p) pile-y)
     pile-width pile-height "green")))

(define (draw-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height "black")
  ((draw-rectangle vp) pos width disk-height "white"))

(define (mark-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height "red")
  ((draw-rectangle vp) pos width disk-height "white"))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle vp) pos width disk-height)
  ((draw-solid-rectangle vp)
   (make-posn (- center (/ pile-width 2)) y) pile-width disk-height "green"))

(define (remove-all-disks)
  ((clear-solid-rectangle vp)
   (make-posn (+ block border) (- vp-height border block (* max-height disk-height)))
   (+ (* 3 max-disk-width) (* 2 border))
   (* max-height disk-height))
  (draw-piles))

(define (setup)
  (let/ec exit
    (remove-all-disks)
    (set! config (make-vector 3 '()))
    (define msg "Setting up")
    (define pos (add-posn quit-pos (+ button-width border) button-height))
    (define (remove-msg) ((clear-string vp) pos msg))
    ((draw-string vp) pos msg "red")
    (for ((d (in-reversed-range height)))
      (define click (get-click))
      (case click
        ((0 1 2)
         (define pile (vector-ref config click))
         (vector-set! config click (cons d pile))
         (draw-disk d (length pile) click))
        ((mode) (remove-msg) (reset) (set-mode!) (exit))
        ((height) (remove-msg) (set-height!) (reset) (exit))
        ((setup) (remove-msg) (setup) (exit))
        ((speed) (remove-msg) (reset) (set-speed!) (exit))
        ((reset) (remove-msg) (reset) (exit))
        ((quit) (remove-msg) (exit))))
    (remove-msg)))

(define (set-mode!)
  (define modes (list "Manual" "Short" "Long" "Hamilton"))
  (define choice
    (get-choices-from-user
      "Mode"
      "Select a mode"
      modes))
  (when choice
    (define ch (car choice))
    ((draw-button-content vp) mode-pos (list-ref modes ch))
    (set! mode (vector-ref #(manual short long hamilton) ch))))

(define (set-height!)
  (define heights (range 1 10))
  (define h
    (get-choices-from-user
      "Height"
      "Select nr of disks"
      (map (curry format "~s") heights)))
  (when h
    (define hh (add1 (car h)))
    (set! height hh)
    ((draw-button-content vp) height-pos (format "~s" hh))))

(define (set-speed!)
  (define str
    (get-text-from-user
      "Speed"
      (string-append
        "Enter a finite positive real number for the approximate\n"
        "number of moves to be made per second\n"
        "or leave the default 'click' as it is")
      #f	 
      "click"	 
      '(disallow-invalid)	 
      #:validate validate-speed))
  (cond
    ((equal? str "click")
     (set! delay 'click)
     ((draw-button-content vp) speed-pos str))
    (else
      (define v (min 9999999 (inexact->exact (read (open-input-string str)))))
      (set! delay (/ (max 0.000001 v)))
      (cond
        ((integer? v) ((draw-button-content vp) speed-pos (format "~s" v)))
        ((< v 1) ((draw-button-content vp) speed-pos (~r v #:precision 5)))
        (else
          (define oom (order-of-magnitude (floor v)))
          ((draw-button-content vp) speed-pos (~r v #:precision (- 5 oom))))))))

(define (validate-speed str)
  (with-handlers ((exn:fail? (λ (e) #f)))
    (cond
      ((equal? str "click"))
      (else
        (define speed (inexact->exact (read (open-input-string str))))
        (cond
          ((infinite? speed) #f)
          ((and (real? speed) (positive? speed)) speed)
          (else #f))))))

(define (positive-real? x) (and (real? x) (positive? x)))

(define (manual)
  (define click (get-click))
  (case click
    ((0 1 2) (manual1 click))
    ((height) (set-height!) (reset) (main))
    ((mode) (set-mode!) (main))
    ((speed) (set-speed!) (manual))
    ((reset) (reset) (main))
    ((setup) (setup) (main))
    ((quit) (void))
    (else (manual))))

(define (manual1 p)
  (define pile (vector-ref config p))
  (cond
    ((null? pile) (manual))
    (else
      (define d (car pile))
      (define h (sub1 (length pile)))
      (mark-disk d h p)
      (manual2 d h p))))

(define (manual2 d h p)
  (define click (get-click))
  (case click
    ((0 1 2) (manual3 d h p click))
    ((height) (set-height!) (reset) (main))
    ((mode) (reset) (set-mode!) (main))
    ((speed) (set-speed!) (manual2 d h p))
    ((reset) (reset) (main))
    ((setup) (setup) (main))
    ((quit) (void))
    (else (manual2 d h p))))

(define (manual3 d h p dest-p)
  (cond
    ((= dest-p p) (draw-disk d h p) (manual))
    (else
      (define pile (vector-ref config dest-p))
      (cond
        ((null? pile)
         (remove-disk d h p)
         (vector-set! config p (cdr (vector-ref config p)))
         (draw-disk d 0 dest-p)
         (vector-set! config dest-p (cons d (vector-ref config dest-p)))
         (manual))
        (else
          (define dest-d (car pile))
          (define dest-h (length pile))
          (cond
            ((< d dest-d)
             (remove-disk d h p)
             (vector-set! config p (cdr (vector-ref config p)))
             (draw-disk d dest-h dest-p)
             (vector-set! config dest-p (cons d (vector-ref config dest-p)))
             (manual))
            (else (draw-disk d h p) (manual))))))))

(define (short)
  (define move-count 0)
  (define count-str (format "Move count: ~s" move-count))
  (define pos (add-posn quit-pos (+ button-width border) button-height))
  (define (draw-count)
    ((clear-string vp) pos count-str)
    (set! count-str (format "Move count: ~s" move-count))
    ((draw-string vp) pos count-str))
  ((draw-string vp) pos count-str)
  (let/cc return
    (define (exit)
      ((clear-string vp) pos count-str)
      (return))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref config p)))
        p))
    (define (short conf dest)
      (cond
        ((null? conf))
        ((= (car conf) dest) (short (cdr conf) dest))
        (else
          (short (cdr conf) (- 3 (car conf) dest))
          (move (car conf) dest)
          (set! move-count (add1 move-count))
          (draw-count)
          (short (make-list (length (cdr conf))  (- 3 (car conf) dest)) dest))))
    (define (move f t)
      (define ff (vector-ref config f))
      (define tt (vector-ref config t))
      (define d (car ff))
      (case delay
        ((click) (check-click #t))
        (else (sleep delay) (check-click #f)))
      (remove-disk d (sub1 (length ff)) f)
      (draw-disk d (length tt) t)
      (vector-set! config f (cdr ff))
      (vector-set! config t (cons d tt)))
    (define (check-click get?)
      (define click (get-click get?))
      (case click
        ((reset) (reset) (exit))
        ((quit) (exit))))
    (short p-list 2)
    (message-box "Short" "Finished")
    ((clear-string vp) pos count-str)
    (reset)))

(define (long)
  (define move-count 0)
  (define count-str (format "Move count: ~s" move-count))
  (define pos (add-posn quit-pos (+ button-width border) button-height))
  (define (draw-count)
    ((clear-string vp) pos count-str)
    (set! count-str (format "Move count: ~s" move-count))
    ((draw-string vp) pos count-str))
  ((draw-string vp) pos count-str)
  (let/cc return
    (define (exit)
      ((clear-string vp) pos count-str)
      (return))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref config p)))
        p))
    (define (long conf dest)
      (define third (and (not (null? conf)) (- 3 (car conf) dest)))
      (cond
        ((null? conf))
        (else
          (long (cdr conf) dest)
          (move (car conf) third)
          (set! move-count (add1 move-count))
          (draw-count)
          (long (make-list (length (cdr conf)) dest) (car conf))
          (move third dest)
          (set! move-count (add1 move-count))
          (draw-count)
          (long (make-list (length (cdr conf)) (car conf)) dest))))
    (define (move f t)
      (define ff (vector-ref config f))
      (define tt (vector-ref config t))
      (define d (car ff))
      (case delay
        ((click) (check-click #t))
        (else (sleep delay) (check-click #f)))
      (remove-disk d (sub1 (length ff)) f)
      (draw-disk d (length tt) t)
      (vector-set! config f (cdr ff))
      (vector-set! config t (cons d tt)))
    (define (check-click get?)
      (define click (get-click get?))
      (case click
        ((reset) (reset) (exit))
        ((quit) (exit))))
    (long p-list 2)
    (message-box "Long" "Finished")
    ((clear-string vp) pos count-str)
    (reset)))

(define (hamilton)
  (define move-count 0)
  (define count-str (format "Move count: ~s" move-count))
  (define pos (add-posn quit-pos (+ button-width border) button-height))
  (define (draw-count)
    ((clear-string vp) pos count-str)
    (set! count-str (format "Move count: ~s" move-count))
    ((draw-string vp) pos count-str))
  ((draw-string vp) pos count-str)
  (let/cc return
    (define (exit)
      ((clear-string vp) pos count-str)
      (return))
    (define (longest-circular-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path                h-1 f r)
        (move-disk                 h-1 f t)
        (longest-non-circular-path h-1 r f)
        (move-disk                 h-1 t r)
        (longest-non-circular-path h-1 f t)
        (move-disk                 h-1 r f)
        (finish-path               h-1 t f)))
    (define (longest-non-circular-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path h-1 f t)
        (move-disk                 h-1 f r)
        (longest-non-circular-path h-1 t f)
        (move-disk                 h-1 r t)
        (longest-non-circular-path h-1 f t)))
    (define (start-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path                h-1 f r)
        (move-disk                 h-1 f t)
        (longest-non-circular-path h-1 r t)))
    (define (finish-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path h-1 f r)
        (move-disk                 h-1 f t)
        (finish-path               h-1 r t)))
    (define (move-disk h f t)
      (define ff (vector-ref config f))
      (define tt (vector-ref config t))
      (define d (car ff))
      (case delay
        ((click) (check-click #t))
        (else (sleep delay) (check-click #f)))
      (remove-disk d (sub1 (length ff)) f)
      (draw-disk d (length tt) t)
      (vector-set! config f (cdr ff))
      (vector-set! config t (cons d tt))
      (set! move-count (add1 move-count))
      (draw-count))
    (define (check-click get?)
      (define click (get-click get?))
      (case click
        ((reset) (reset) (exit))
        ((quit) (exit))))
    (longest-circular-path height 0 2)
    (message-box "Hamilton" "Finished")
    ((clear-string vp) pos count-str)
    (reset)))

(define (reset (include-manual #t))
  (when include-manual
    (set! mode 'manual)
    ((draw-button-content vp) mode-pos "manual"))
  (set! config (vector (range height) '() '()))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Initialization.

(define vp-width (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
(define vp-height (+ (* 2 button-height) (* 3 border) pile-height block))
(define vp "yet to be assigned")

(define (initialize)
  (open-graphics)
  (set! vp (open-viewport "Tower of Hanoi" vp-width vp-height))
  ((draw-button vp) height-pos "Height")
  ((draw-button vp) mode-pos   "Mode")
  ((draw-button vp) speed-pos  "Speed")
  ((draw-button vp) reset-pos  "Reset")
  ((draw-button vp) setup-pos  "Setup")
  ((draw-button vp) quit-pos   "Quit")
  (set! delay 'click)
  (set! height max-height)
  (set! mode 'manual)
  ((draw-button-content vp) height-pos (format "~s" height))
  ((draw-button-content vp) speed-pos (format "~s" delay))
  ((draw-button-content vp) mode-pos "manual")
  ((draw-solid-rectangle vp)
   (make-posn border (- vp-height border block))
   (- vp-width (* 2 border))
   block
   "gray")
  (reset))

;=====================================================================================================
; Run the game protected.

(define (hanoi)
  (initialize)
  (dynamic-wind
    void
    main
    (λ () (close-viewport vp) (close-graphics))))
