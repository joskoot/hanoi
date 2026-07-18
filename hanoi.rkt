;=====================================================================================================
; A GUI playing the game of the tower of Hanoi.

#lang racket

(require graphics/graphics racket/gui)
(provide hanoi)
(define-syntax-rule (in-reversed-range n) (in-range (sub1 n) -1 -1))
(define (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))

;=====================================================================================================
; Run the game protected.

(define (hanoi)
  (initialize)
  (dynamic-wind
    void
    main
    close))

(define (close) (close-viewport vp) (close-graphics))

;=====================================================================================================
; Main procedure.

(define (main)
  (case mode
    ((manual) (manual))
    ((short) (short) (set-mode-manual))
    ((long) (reset) (long) (set-mode-manual))
    ((circular) (reset) (circular) (set-mode-manual))))

(define (set-mode-manual)
  (set! mode 'manual)
  ((draw-button-content vp) mode-pos "Manual")
  (main))

;=====================================================================================================
; Constants

(define max-height 9)
(define max-speed 999999)
(define min-speed 1/10)
(define max-speed-str (~a max-speed))
(define min-speed-str (~a min-speed))

;=====================================================================================================
; Initial state of the game, mutated while playing.

(define (make-disk-distribution) (vector (range height) '() '()))
(define height max-height)
(define mode 'manual)
(define speed 'click)
(define clock 0)
(define move-count 0)
(define count-str "")
(define config (make-disk-distribution))

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

(define (get-and-dispatch-click (get? #t))
  ; Ignore click when not in a region and asked for by ready-mouse-click.
  ; Repeat asking for a click when not in a region and asked for by get-mouse-click.
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
        (get? (get-and-dispatch-click))))
    (get? (get-and-dispatch-click))
    (else #f)))

(define (check-click get? exit)
  (define click (get-and-dispatch-click get?))
  (case click
    ((reset) (reset) (exit))
    ((quit) (exit))
    (else
      (or (and get? (member click '(0 1 2)))
        (not get?)))))

;=====================================================================================================
; Elementary dimensions.

(define block 20)
(define border (* 3 block))

;=====================================================================================================
; Colors.

(define green (make-rgb 0    8/10 0   ))
(define red   (make-rgb 1    0    0   ))
(define white (make-rgb 1    1    1   ))
(define black (make-rgb 0    0    0   ))
(define gray  (make-rgb 6/10 6/10 6/10))
(define blue  (make-rgb 0    0    1   ))

;=====================================================================================================
; Layout of the viewport and related procedures.

(define-syntax-rule
  (define-values-block (value ...) expr ...)
  (define-values (value ...) (let () expr ... (values value ...))))

; Determine dimensions of buttons and button contents.

(define-values-block (draw-button draw-button-content button-width button-height)
  (open-graphics)
  (define vp (open-pixmap "string-sizes" 500 500))
  (define button-strings (list "Height" "Mode" "Reset" "Setup" "Quit"))
  (define button-content-strings (list "Short" "Long" "Circular"))
  (define string-offset 4)
  (define *2string-offset (* 2 string-offset))
  (define-values (button-width button-height)
    (for/fold ((w 0) (h 0) #:result (values (+ w *2string-offset) (+ h *2string-offset)))
      ((w/h
         (in-list
           (map (get-string-size vp)
             (append button-strings button-content-strings (list "999999"))))))
      (values
        (max w (inexact->exact (ceiling (car w/h))))
        (max h (inexact->exact (ceiling (cadr w/h)))))))
  (close-viewport vp)
  (close-graphics)
  (define ((draw-button vp) pos str)
    (define x (posn-x pos))
    (define y (posn-y pos))
    ((draw-solid-rectangle vp)
     pos button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- string-offset))) str white))
  (define ((draw-button-content vp) pos str)
    (define x (posn-x pos))
    (define y (+ (posn-y pos) button-height))
    ((clear-solid-rectangle vp) (make-posn x y) button-width button-height)
    ((draw-rectangle vp)  (make-posn x y) button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- (* 2 string-offset)))) str blue)))

; Determine all other dimensions and regions.

(define height-pos (make-posn border border))
(define mode-pos  (add-posn height-pos (+ button-width border) 0))
(define speed-pos (add-posn mode-pos   (+ button-width border) 0))
(define reset-pos (add-posn speed-pos  (+ button-width border) 0))
(define setup-pos (add-posn reset-pos  (+ button-width border) 0))
(define quit-pos  (add-posn setup-pos  (+ button-width border) 0))
(define count-pos (add-posn quit-pos   (+ button-width border) button-height))
(define height-region (make-region height-pos button-width button-height))
(define mode-region   (make-region mode-pos   button-width button-height))
(define speed-region  (make-region speed-pos  button-width button-height))
(define reset-region  (make-region reset-pos  button-width button-height))
(define setup-region  (make-region setup-pos  button-width button-height))
(define quit-region   (make-region quit-pos   button-width button-height))
(define disk-height block)
(define min-disk-width (* 3 block))
(define disk-width-incr block)
(define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width (disk-width (sub1 max-height )))
(define pile-top (* 2 block))
(define pile-width 4)
(define pile-y (* 2 (+ border button-height)))
(define pile-height (+ pile-top (* max-height disk-height)))
(define vp-width (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
(define vp-height (+ (* 2 button-height) (* 3 border) pile-height block))

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
; Draw procedures.

(define (draw-piles)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle vp)
     (make-posn (pile-x p) pile-y)
     pile-width pile-height green)))

(define (draw-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height black)
  ((draw-rectangle vp) pos width disk-height white))

(define (mark-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height red)
  ((draw-rectangle vp) pos width disk-height white))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle vp) pos width disk-height)
  ((draw-solid-rectangle vp)
   (make-posn (- center (/ pile-width 2)) y) pile-width disk-height green))

(define (remove-all-disks)
  ((clear-solid-rectangle vp)
   (make-posn (+ block border) (- vp-height border block (* max-height disk-height)))
   (+ (* 3 max-disk-width) (* 2 border))
   (* max-height disk-height))
  (draw-piles))

(define (move-disk f t exit)
  (define ff (vector-ref config f))
  (define tt (vector-ref config t))
  (unless (null? ff)
    (define d (car ff))
    (define do?
      (case speed
        ((click) (check-click #t exit))
        (else (sleep speed) (check-click #f exit))))
    (cond
      (do?
        (remove-disk d (sub1 (length ff)) f)
        (draw-disk d (length tt) t)
        (draw-count)
        (vector-set! config f (cdr ff))
        (vector-set! config t (cons d tt)))
      (else (move-disk f t exit)))))

;=====================================================================================================
; Actions.

(define (setup)
  (let/ec exit
    (remove-all-disks)
    (set! config (make-vector 3 '()))
    (define msg "Setting up")
    (define (remove-msg) ((clear-string vp) count-pos msg))
    ((draw-string vp) count-pos msg red)
    (for ((d (in-reversed-range height)))
      (define click (get-and-dispatch-click))
      (case click
        ((0 1 2)
         (define pile (vector-ref config click))
         (vector-set! config click (cons d pile))
         (draw-disk d (length pile) click))
        ((mode) (remove-msg) (reset) (set-mode) (exit))
        ((height) (remove-msg) (set-height) (reset) (exit))
        ((setup) (remove-msg) (setup) (exit))
        ((speed) (remove-msg) (reset) (set-speed) (exit))
        ((reset quit) (remove-msg) (reset) (exit))))
    (remove-msg)))

(define (set-mode)
  (define modes (list "Manual" "Short" "Long" "Circular"))
  (define choice
    (get-choices-from-user
      "Mode"
      "Select a mode"
      modes))
  (when choice
    (define ch (car choice))
    ((draw-button-content vp) mode-pos (list-ref modes ch))
    (set! mode (vector-ref #(manual short long circular) ch))))

(define (set-height)
  (define heights (range 1 (add1 max-height)))
  (define h
    (get-choices-from-user
      "Height"
      "Select nr of disks"
      (map (curry format "~s") heights)))
  (when h
    (define hh (add1 (car h)))
    (set! height hh)
    ((draw-button-content vp) height-pos (format "~s" hh))))

(define (set-speed)
  (define (validate-speed str) 
    (and (<= 1 (string-length str) 7)
      (or
        (equal? str "click")
        (with-handlers ((exn:fail? (λ (e) #f)))
          (define speed (inexact->exact (read (open-input-string str))))
          (cond
            ((infinite? speed) #f)
            ((and (real? speed) (positive? speed)))
            (else #f))))))
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
     (set! speed 'click)
     ((draw-button-content vp) speed-pos "click"))
    ((not str))
    (else
      (define sp (read (open-input-string str)))
      (define v (max min-speed (min max-speed sp)))
      (set! speed (/ v))
      ((draw-button-content vp)
       speed-pos
       (cond
         ((> sp max-speed) max-speed-str)
         ((< sp min-speed) min-speed-str)
         (else str))))))

(define (manual)
  (define click (get-and-dispatch-click))
  (case click
    ((0 1 2) (manual1 click))
    ((height) (set-height) (reset) (main))
    ((mode) (set-mode) (main))
    ((speed) (set-speed) (main))
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
  (define click (get-and-dispatch-click))
  (case click
    ((0 1 2) (manual3 d h p click))
    ((height) (set-height) (reset) (main))
    ((mode) (reset) (set-mode) (main))
    ((speed) (set-speed) (manual2 d h p))
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
  (reset-time-and-counter)
  (let/cc return
    (define (exit)
      ((clear-string vp) count-pos count-str)
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
          (move-disk (car conf) dest exit)
          (short (make-list (length (cdr conf))  (- 3 (car conf) dest)) dest))))
    (short p-list 2)
    (finish "Short")))

(define (long)
  (reset-time-and-counter)
  (let/cc return
    (define (exit)
      ((clear-string vp) count-pos count-str)
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
          (move-disk (car conf) third exit)
          (long (make-list (length (cdr conf)) dest) (car conf))
          (move-disk third dest exit)
          (long (make-list (length (cdr conf)) (car conf)) dest))))
    (long p-list 2)
    (finish "Long")))

(define (circular)
  (reset-time-and-counter)
  (define pos (add-posn quit-pos (+ button-width border) button-height))
  (let/cc return
    (define (exit)
      ((clear-string vp) pos count-str)
      (return))
    (define (longest-circular-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path  h-1 f r)
        (move-disk f t exit)
        (longest-non-circular-path h-1 r f)
        (move-disk t r exit)
        (longest-non-circular-path h-1 f t)
        (move-disk r f exit)
        (finish-path  h-1 t f)))
    (define (longest-non-circular-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path h-1 f t)
        (move-disk f r exit)
        (longest-non-circular-path h-1 t f)
        (move-disk r t exit)
        (longest-non-circular-path h-1 f t)))
    (define (start-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path  h-1 f r)
        (move-disk f t exit)
        (longest-non-circular-path h-1 r t)))
    (define (finish-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path h-1 f r)
        (move-disk f t exit)
        (finish-path  h-1 r t)))
    (longest-circular-path height 0 2)
    (finish "Circular")))

(define (reset)
  (set! config (make-disk-distribution))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Count and time info for modes short, long and circular.

(define (reset-time-and-counter)
  (set! clock (current-inexact-milliseconds))
  (set! move-count -1)
  (set! count-str "")
  (draw-count))

(define (draw-count)
  ((clear-string vp) count-pos count-str)
  (set! move-count(add1 move-count))
  (set! count-str
    (format "Move count: ~s, time: ~a seconds"
      move-count (watch-clock)))
  ((draw-string vp) count-pos count-str))

(define (watch-clock)
  (~r #:precision 3 (/ (- (current-inexact-milliseconds) clock) 1000)))

(define (finish mode)
  (message-box mode (string-append mode " mode finished"))
  (viewport-flush-input vp)
  ((clear-string vp) count-pos count-str)
  (reset))

;=====================================================================================================
; Initialization.

(define vp 'yet-to-be-initialized)

(define (initialize)
  (open-graphics)
  (set! vp (open-viewport "Tower of Hanoi" vp-width vp-height))
  ((draw-button vp) height-pos "Height")
  ((draw-button vp) mode-pos   "Mode")
  ((draw-button vp) speed-pos  "Speed")
  ((draw-button vp) reset-pos  "Reset")
  ((draw-button vp) setup-pos  "Setup")
  ((draw-button vp) quit-pos   "Quit")
  ((draw-button-content vp) height-pos (format "~s" height))
  ((draw-button-content vp) speed-pos (format "~s" speed))
  ((draw-button-content vp) mode-pos "Manual")
  ((draw-solid-rectangle vp)
   (make-posn border (- vp-height border block))
   (- vp-width (* 2 border))
   block
   gray)
  (reset))

;=====================================================================================================
; The end