;=====================================================================================================
; A GUI playing the game of The Toer of Hanoi.
; Moves can be made manually but also automated by the GUI.
; The GUI has clickable buttons height, mode, delay, reset, setup, quit, pile1, pile2 and pile3.
; During an action some buttons may be disabled and disappear temporarily from the screen.
;=====================================================================================================

#lang racket/base

(provide play)

(require
  (for-syntax
    racket/base
    (only-in syntax/transformer make-variable-like-transformer))
  (only-in racket make-list infinite? range ~r)
  (only-in racket/base (define def))
  (only-in graphics/graphics
    open-graphics
    close-graphics
    open-viewport
    open-pixmap
    close-viewport
    draw-string
    clear-string
    get-string-size
    draw-rectangle
    draw-solid-rectangle
    clear-solid-rectangle
    get-mouse-click
    ready-mouse-click
    mouse-click-posn
    viewport-flush-input
    make-posn
    posn-x
    posn-y
    make-rgb)
  (only-in racket/gui
    message-box
    get-text-from-user
    get-choices-from-user))

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

(define-syntax-rule
  (define-values-block (value ...) expr ...)
  (define-values (value ...) (let () expr ... (values value ...))))

(define-syntax-rule
  (constant id value)
  (begin
    (def ident value)
    (define-syntax id (make-variable-like-transformer (syntax ident) #f))))

(define-syntax (deffun stx)
  (syntax-case stx ()
    ((_ (id arg ...) body ...)
     (syntax (def (id arg ...) body ...)))
    ((_ (id arg ... . rest-arg) body ...)
     (syntax (def (id arg ... . rest-arg) body ...)))
    ((_ (id rest-arg body ...))
     (identifier? #'rest-arg)
     (syntax (def (id rest-arg body ...))))))

(define-syntax (defmutable stx)
  (syntax-case stx ()
    ((_ id value)
     (and
       (identifier? #'id)
       (printf "Mutable variable: ~s ~s ~s~n"
         (syntax-line #'id)
         (syntax-column #'id)
         (syntax-e #'id)))
     (syntax (def id value)))))

;=====================================================================================================
; Main procedure. 

(deffun (play)
  (initialize)
  (dynamic-wind
    void
    main
    close))

(deffun (close)
  (close-viewport vp)
  (close-graphics))

(deffun (main)
  (constant pos (mouse-click-posn (get-mouse-click vp)))
  (dispatch pos
    (height-button (do-height) (main))
    (mode-button   (do-mode  ) (main))
    (delay-button  (do-delay ) (main))
    (reset-button  (reset    ) (main))
    (setup-button  (do-setup ) (main))
    (quit-button   (void     ))
    (pile1-button  (when (eq? mode 'manual) (do-manual 0)) (main))
    (pile2-button  (when (eq? mode 'manual) (do-manual 1)) (main))
    (pile3-button  (when (eq? mode 'manual) (do-manual 2)) (main))
    (else (main))))

;=====================================================================================================
; A button is displayed on the screen. It has procedure property can have a content.
; It is called as follows:
;
;    (button command arg ...) --> any/c
;    command : (or/c 'button-name 'in-button? 'disable 'enable 'get-content 'put-content)
;    arg : any/c
;
; 'get-content and 'put-content are for buttons with content only.

(define-syntax make-button
  (let ((no-kontent (string->uninterned-symbol "no-kontent")))
    (λ (stx)
      (syntax-case stx ()
        ((_ name pos) (quasisyntax (make-button name pos (unsyntax no-kontent))))
        ((_ name pos kontent)
         (let
           ((no-kontent-condition
              (and
                (identifier? #'kontent)
                (eq? (syntax-e #'kontent) no-kontent))))
           (quasisyntax
             (let ()
               (constant region (make-region pos button-width button-height))
               (constant name-str (string-titlecase (format "~a" 'name)))
               (defmutable enabled? #t)
               (deffun (proc button action . args)
                 (case action
                   ((button-name) 'name)
                   ((in-button?) (in-region? (car args) region))
                   (unsyntax-splicing
                     (if no-kontent-condition
                       (list)
                       (list
                         (syntax
                           ((put-content)
                            (set-button-content! button (car args))
                            ((draw-button-content vp) pos (car args))))
                         (syntax
                           ((get-content) (button-content button))))))
                   ((disable)
                    (set! enabled? #f)
                    ((clear-solid-rectangle vp) pos button-width button-height))
                   ((enable)
                    (set! enabled? #t)
                    ((draw-button vp) pos name-str))
                   (else
                     (apply
                       raise-argument-error
                       'name
                       "unknown button action"
                       0 action args))))
               (struct button
                 (unsyntax
                   (if no-kontent-condition
                     (syntax (pos))
                     (syntax (pos (content #:mutable)))))
                 #:property prop:procedure proc
                 #:omit-define-syntaxes
                 #:constructor-name make-button)
               (unsyntax
                 (if no-kontent-condition
                   (syntax (constant button (make-button pos)))
                   (syntax (constant button (make-button pos kontent)))))
               ; Draw the button and if it has content, the latter too.
               ((draw-button vp) pos name-str)
               (unsyntax-splicing
                 (if no-kontent-condition
                   (list)
                   (list (syntax ((draw-button-content vp) pos kontent)))))
               (λ (action . args)
                 (when
                   (or enabled? (member action '(enable disable get-content put-content in-button?)))
                   (apply button action args)))))))))))

;=====================================================================================================
; Dispatching mouse clicks.

(define-syntax (dispatch stx)
  (syntax-case stx (else)
    ((_ pos (button do-button ...) ... (else else-clause ...))
     (syntax
       (cond
         ((button 'in-button? pos) do-button ...) ...
         (else else-clause ...))))
    ((_ pos (button do-button ...) ...)
     (syntax
       (cond
         ((button 'in-button? pos) do-button ...) ...)))))

(struct region (pos width height)
  #:omit-define-syntaxes
  #:constructor-name make-region)

(deffun (in-region? pos region)
  (constant x (posn-x pos))
  (constant y (posn-y pos))
  (constant x-min (posn-x (region-pos region)))
  (constant y-min (posn-y (region-pos region)))
  (constant x-max (+ x-min (region-width  region)))
  (constant y-max (+ y-min (region-height region)))
  (and (<= x-min x x-max) (<= y-min y y-max)))

;=====================================================================================================
; Some constants required in early stage. Never mutated.

(constant height-str   "Height"  )
(constant mode-str     "Mode"    )
(constant delay-str    "Delay"   )
(constant reset-str    "Reset"   )
(constant setup-str    "Setup"   )
(constant quit-str     "Quit"    )
(constant undo-str     "Undo"    )
(constant manual-str   "manual"  )
(constant short-str    "short"   )
(constant long-str     "long"    )
(constant circular-str "circular")
(constant click-str    "click"   )
(constant click        'click    )
(constant red   (make-rgb 1   0   0  ))
(constant white (make-rgb 1   1   1  ))
(constant black (make-rgb 0   0   0  ))
(constant gray  (make-rgb 0.6 0.6 0.6))
(constant blue  (make-rgb 0   0   1  ))
(constant green (make-rgb 0   0.8 0  ))

;=====================================================================================================
; Procedures to draw buttons and their contents. Computation of their sizes.
; A temporary viewport is used to measure string sizes.

(define-values-block (draw-button draw-button-content button-width button-height)
  
  ; Measure the maximum size of strings used in buttons.
  
  (constant strings
    (list
      height-str
      mode-str
      reset-str
      setup-str
      quit-str
      undo-str
      manual-str
      long-str
      circular-str
      "999999"))
  
  (constant string-offset 4)
  (constant *2string-offset (* 2 string-offset))
  
  (open-graphics)
  (constant vp (open-pixmap "string-sizes" 1000 500))
  (define-values (button-width button-height)
    (for/fold ((w 0) (h 0) #:result (values (+ w *2string-offset) (+ h *2string-offset)))
      ((w/h
         (in-list
           (map (get-string-size vp)
             strings))))
      (values
        (max w (inexact->exact (ceiling (car w/h))))
        (max h (inexact->exact (ceiling (cadr w/h)))))))
  (close-viewport vp)
  (close-graphics)
  
  ; Two procedures to draw buttons and their contents.
  
  (deffun ((draw-button vp) pos str)
    (constant x (posn-x pos))
    (constant y (posn-y pos))
    ((draw-solid-rectangle vp) pos button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- string-offset))) str white))
  
  (deffun ((draw-button-content vp) pos value)
    (constant str (if (string? value) value (format "~a" value)))
    (constant x (posn-x pos))
    (constant y (+ (posn-y pos) button-height))
    ((clear-solid-rectangle vp) (make-posn x y) button-width button-height)
    ((draw-rectangle vp) (make-posn x y) button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- (* 2 string-offset)))) str blue)))

;=====================================================================================================
; Layout of the GUI.

(deffun (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
(deffun (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(constant max-height 9)
(constant block 20)
(constant border (* 3 block))
(constant height-pos (make-posn border border))
(constant mode-pos  (add-posn height-pos (+ button-width border) 0))
(constant delay-pos (add-posn mode-pos   (+ button-width border) 0))
(constant reset-pos (add-posn delay-pos  (+ button-width border) 0))
(constant setup-pos (add-posn reset-pos  (+ button-width border) 0))
(constant quit-pos  (add-posn setup-pos  (+ button-width border) 0))
(constant pile1-pos (add-posn quit-pos   (+ button-width border) 0))
(constant pile2-pos (add-posn pile1-pos  (+ button-width border) 0))
(constant pile3-pos (add-posn pile2-pos  (+ button-width border) 0))
(constant msg-pos   (add-posn pile3-pos  (+ button-width (/ border 2)) button-height))
(constant disk-height block)
(constant min-disk-width (* 3 block))
(constant disk-width-incr block)
(constant max-disk-width (disk-width (sub1 max-height)))
(constant pile-top (* 2 block))
(constant pile-width 4)
(constant pile-y (* 2 (+ border button-height)))
(constant pile-height (+ pile-top (* max-height disk-height)))
(constant vp-width  (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
(constant vp-height (+ (* 2 button-height ) (* 3 border) pile-height block))

(deffun (pile-region p)
  (constant x (+ block border (* p (+ max-disk-width border))))
  (constant y (- vp-height border block pile-top (* max-height disk-height)))
  (make-region (make-posn x y) max-disk-width (+ pile-top (* max-height disk-height))))

(deffun (pile-x p)
  (+ border
    block
    (* p (+ border max-disk-width))
    (/ (- max-disk-width pile-width) 2)))

;=====================================================================================================
; Action : respons to click on button height.

(deffun (do-height)
  (deffun (validate-height str)
    (and (= (string-length str) 1) (char<=? #\1 (string-ref str 0) #\9)))
  (constant str
    (get-text-from-user
      height-str
      (string-append
        "How many disks do you want.\n"
        "At least one, at most nine.\n"
        "Enter a decimal digit but not 0")
      #f
      "9"
      '(disallow-invalid)
      #:validate validate-height))
  (when str
    (constant h (- (char->integer (string-ref str 0)) (char->integer #\0)))
    (set! height h)
    (height-button 'put-content h)
    (reset)))

;=====================================================================================================
; Action : respons to click on button mode.

(deffun (do-mode)
  (constant modes (list short-str long-str circular-str))
  (constant choice
    (get-choices-from-user
      mode-str
      "Select a mode"
      modes))
  (when choice
    (constant ch (car choice))
    (set! mode (vector-ref #(short long circular) ch))
    (mode-button 'put-content mode)
    (case ch
      ((0)
       (prepare/finish-auto-move 'disable)
       (short)
       (finish "short")
       (prepare/finish-auto-move 'enable))
      ((1)
       (reset)
       (prepare/finish-auto-move 'disable)
       (long)
       (finish "long")
       (prepare/finish-auto-move 'enable))
      ((2)
       (reset)
       (prepare/finish-auto-move 'disable)
       (circular)
       (finish "circular")
       (prepare/finish-auto-move 'enable)))))

(deffun (prepare/finish-auto-move enable/disable)
  (for
    ((button
       (in-list
         (list
           height-button
           mode-button
           delay-button
           setup-button
           pile1-button
           pile2-button
           pile3-button))))
    (button enable/disable)))

(deffun (short)
  (reset-time-and-move-counter)
  (let/ec ec
    (deffun (exit) (clear-msg) (ec))
    (constant p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (deffun (short conf dest)
      (cond
        ((null? conf))
        ((= (car conf) dest) (short (cdr conf) dest))
        (else
          (short (cdr conf) (- 3 (car conf) dest))
          (move-disk (car conf) dest exit)
          (short (make-list (length (cdr conf)) (- 3 (car conf) dest)) dest))))
    (short p-list 2)))

(deffun (long)
  (reset)
  (reset-time-and-move-counter)
  (let/ec ec
    (deffun (exit) (clear-msg) (ec))
    (constant p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (deffun (long conf dest)
      (cond
        ((null? conf))
        (else
          (constant third (- 3 (car conf) dest))
          (long (cdr conf) dest)
          (move-disk (car conf) third exit)
          (long (make-list (length (cdr conf)) dest) (car conf))
          (move-disk third dest exit)
          (long (make-list (length (cdr conf)) (car conf)) dest))))
    (long p-list 2)))

(deffun (circular)
  (reset)
  (reset-time-and-move-counter)
  (let/ec ec
    (deffun (exit) (clear-msg) (ec))
    (deffun (longest-circular-path h f t)
      (unless (zero? h)
        (constant h-1 (sub1 h))
        (constant r (- 3 f t))
        (start-path h-1 f r)
        (move-disk  t exit)
        (longest-non-circular-path h-1 r f)
        (move-disk t r exit)
        (longest-non-circular-path h-1 f t)
        (move-disk r f exit)
        (finish-path h-1 t f)))
    (deffun (longest-non-circular-path h f t)
      (unless (zero? h)
        (constant h-1 (sub1 h))
        (constant r (- 3 f t))
        (longest-non-circular-path h-1 f t)
        (move-disk f r exit)
        (longest-non-circular-path h-1 t f)
        (move-disk r t exit)
        (longest-non-circular-path h-1 f t)))
    (deffun (start-path h f t)
      (unless (zero? h)
        (constant h-1 (sub1 h))
        (constant r (- 3 f t))
        (start-path h-1 f r)
        (move-disk f t exit)
        (longest-non-circular-path h-1 r t)))
    (deffun (finish-path h f t)
      (unless (zero? h)
        (constant h-1 (sub1 h))
        (constant r (- 3 f t))
        (longest-non-circular-path h-1 f r)
        (move-disk f t exit)
        (finish-path h-1 r t)))
    (longest-circular-path height 0 2)))

(deffun (finish who)
  (message-box who "finished" #f '(ok))
  (viewport-flush-input vp)
  (mode-button 'put-content 'manual)
  (set! mode 'manual)
  (clear-msg))

;=====================================================================================================
; Action : respons to click on button delay.

(deffun (do-delay)
  (deffun (validate-delay str)
    (and (<= 1 (string-length str) 6)
      (or
        (equal? str click-str)
        (with-handlers ((exn:fail? (λ (e) #f)))
          (constant input (open-input-string str))
          (constant delay (read input))
          (cond
            ((not (eof-object? (read input))) #f)
            ((infinite? delay) #f)
            ((and (real? delay) (>= delay 0)))
            (else #f))))))
  (constant str
    (get-text-from-user
      delay-str
      (string-append
        "Enter a non-negative real number for the\n"
        "approximate delay in seconds between moves\n"
        "or leave the default 'click' as it is.\n"
        "Do not enter more than 6 characters")
      #f	
      click-str	
      '(disallow-invalid)	
      #:validate validate-delay))
  (cond
    ((equal? str click-str)
     (set! delay click)
     (delay-button 'put-content click))
    ((not str))
    (else
      (constant d (read (open-input-string str)))
      (set! delay d)
      (delay-button 'put-content d))))

;=====================================================================================================
; Action : respons to click on button reset.

(deffun (reset)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Action : respons to click on button setup.

(deffun (do-setup)
  (constant disabled-buttons (list height-button mode-button delay-button reset-button setup-button))
  (for ((button (in-list disabled-buttons))) (button 'disable))
  (set! msg-str "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string vp) msg-pos msg-str red)
  (do-setup1 (reverse (range height)))
  (for ((button (in-list disabled-buttons))) (button 'enable))
  (clear-msg))

(deffun (do-setup1 disks)
  (unless (null? disks)
    (constant d (car disks))
    (constant pos (mouse-click-posn (get-mouse-click vp)))
    (dispatch pos
      (pile1-button  (do-setup2 d 0) (do-setup1 (cdr disks)))
      (pile2-button  (do-setup2 d 1) (do-setup1 (cdr disks)))
      (pile3-button  (do-setup2 d 2) (do-setup1 (cdr disks)))
      (quit-button   (clear-msg) (reset))
      (else (do-setup1 disks)))))

(deffun (do-setup2 d p)
  (constant pile (vector-ref disk-distr p))
  (vector-set! disk-distr p (cons d pile))
  (draw-disk d (length pile) p))

;=====================================================================================================
; Action : respons to click on a pile button in manual mode.

(deffun (do-manual p)
  (constant pile (vector-ref disk-distr p))
  (unless (null? pile)
    (constant d (car pile))
    (constant h (sub1 (length pile)))
    (mark-disk d h p)
    (do-manual1 d h p)))

(deffun (do-manual1 d h p)
  (constant pos (mouse-click-posn (get-mouse-click vp)))
  (dispatch pos
    (pile1-button (do-manual2 d h p 0))
    (pile2-button (do-manual2 d h p 1))
    (pile3-button (do-manual2 d h p 2))
    (else (draw-disk d h p))))

(deffun (do-manual2 d h p dest-p)
  (cond
    ((= dest-p p) (draw-disk d h p))
    (else
      (constant pile (vector-ref disk-distr dest-p))
      (cond
        ((null? pile)
         (remove-disk d h p)
         (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
         (draw-disk d 0 dest-p)
         (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
        (else
          (constant dest-d (car pile))
          (constant dest-h (length pile))
          (cond
            ((< d dest-d)
             (remove-disk d h p)
             (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
             (draw-disk d dest-h dest-p)
             (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
            (else (draw-disk d h p))))))))

;=====================================================================================================
; Move-count and real time clock.

(deffun (reset-time-and-move-counter)
  (clear-msg)
  (set! clock (current-inexact-milliseconds))
  (set! move-count -1)
  (set! msg-str "")
  (draw-msg))

(deffun (clear-msg) ((clear-string vp) msg-pos msg-str))

(deffun (draw-msg)
  (clear-msg)
  (set! move-count (add1 move-count))
  (set! msg-str
    (if (eq? delay click)
      (format "Move count: ~s" move-count)
      (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
  ((draw-string vp) msg-pos msg-str))

(deffun (watch-clock)
  (~r #:precision 3 (/ (- (current-inexact-milliseconds) clock) 1000)))

;=====================================================================================================
; Drawing procedure.

(deffun (draw-all-disks)
  (remove-all-disks)
  (for ((p (in-range 3)))
    (constant disks (vector-ref disk-distr p))
    (for ((d (in-range height)) (h (in-reversed-range (length disks))))
      (draw-disk d h p))))

(deffun (draw-piles)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle vp)
     (make-posn (pile-x p) pile-y)
     pile-width pile-height green)))

(deffun (remove-all-disks)
  ((clear-solid-rectangle vp)
   (make-posn (+ block border) (- vp-height border block (* max-height disk-height)))
   (+ (* 3 max-disk-width) (* 2 border))
   (* max-height disk-height))
  (draw-piles))

(deffun (draw-disk d h p (color black))
  (constant width (disk-width d))
  (constant center (+ (pile-x p) (/ pile-width 2)))
  (constant x (- center (/ width 2)))
  (constant y (- vp-height border block (* (add1 h) disk-height)))
  (constant pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height color)
  ((draw-rectangle vp) pos width disk-height white)
  ((draw-string vp) (make-posn (- (pile-x p) 2) (+ y block -3)) (format "~s" (add1 d)) white))

(deffun (mark-disk d h p) (draw-disk d h p red))

(deffun (remove-disk d h p)
  (constant width (disk-width d))
  (constant center (+ (pile-x p) (/ pile-width 2)))
  (constant x (- center (/ width 2)))
  (constant y (- vp-height border block (* (add1 h) disk-height)))
  (constant pos (make-posn x y))
  ((clear-solid-rectangle vp) pos width disk-height)
  ((draw-solid-rectangle vp)
   (make-posn (- center (/ pile-width 2)) y) pile-width disk-height green))

(deffun (move-disk f t exit)
  (constant ff (vector-ref disk-distr f))
  (constant tt (vector-ref disk-distr t))
  (unless (null? ff)
    (constant d (car ff))
    (constant move-to-be-made?
      (case delay
        ((click) (check-click #t exit))
        (else (sleep delay) (check-click #f exit))))
    (cond
      (move-to-be-made?
        (remove-disk d (sub1 (length ff)) f)
        (draw-disk d (length tt) t)
        (vector-set! disk-distr f (cdr ff))
        (vector-set! disk-distr t (cons d tt))
        (draw-msg))
      (else (move-disk f t exit)))))

; Procedure check-click enables abortion from short, long and circular mode.

(deffun (check-click click-required? exit)
  (constant pos ((if click-required? get-mouse-click ready-mouse-click) vp))
  (constant p (and pos (mouse-click-posn pos)))
  (cond
    (p
      (dispatch p
        (reset-button (reset) (exit))
        (quit-button (exit))))
    (else #t)))

;=====================================================================================================
; Initialization. Actions to be taken before the game can start and variables that must be initialized
; again when procedure play is called more than once or cannot be defined with their proper values
; before variable vp is initialized with a viewport. Variable vp always needs initialization because
; the viewport cannot be made before open-graphics is called, which is done during initialization.
; Notice that syntax make-button needs the viewport too. Therefore the buttons are included in the
; initialization.

(defmutable vp            'yet-to-be-initialized)
(defmutable height        'yet-to-be-initialized)
(defmutable delay         'yet-to-be-initialized)
(defmutable mode          'yet-to-be-initialized)
(defmutable disk-distr    'yet-to-be-initialized)
(defmutable height-button 'yet-to-be-initialized)
(defmutable mode-button   'yet-to-be-initialized)
(defmutable delay-button  'yet-to-be-initialized)
(defmutable reset-button  'yet-to-be-initialized)
(defmutable setup-button  'yet-to-be-initialized)
(defmutable quit-button   'yet-to-be-initialized)
(defmutable pile1-button  'yet-to-be-initialized)
(defmutable pile2-button  'yet-to-be-initialized)
(defmutable pile3-button  'yet-to-be-initialized)
(defmutable msg-str       'yet-to-be-initialized)
(defmutable clock         'yet-to-be-initialized)
(defmutable move-count    'yet-to-be-initialized)

(deffun (initialize)
  ; Store or restore some variables.
  (set! height max-height)
  (set! delay click)
  (set! mode 'manual)
  (set! disk-distr (vector (range height) '() '()))
  (set! msg-str "")
  (set! msg-str    "")
  (set! clock      0 )
  (set! move-count 0 )
  ; Open graphics and make the viewport and all buttons.
  (open-graphics)
  (set! vp (open-viewport "Tower of Hanoi" vp-width vp-height))
  (set! height-button (make-button height   height-pos max-height))
  (set! mode-button   (make-button mode     mode-pos  'manual))
  (set! delay-button  (make-button delay    delay-pos  click))
  (set! reset-button  (make-button reset    reset-pos))
  (set! setup-button  (make-button setup    setup-pos))
  (set! quit-button   (make-button quit     quit-pos ))
  (set! pile1-button  (make-button |pile 1| pile1-pos))
  (set! pile2-button  (make-button |pile 2| pile2-pos))
  (set! pile3-button  (make-button |pile 3| pile3-pos))
  ; Additional initialization.
  ((draw-solid-rectangle vp)
   (make-posn border (- vp-height border block)) (- vp-width (* 2 border)) block gray)
  ; Procedure reset draws the piles and disks.
  (reset))

;=====================================================================================================
; The end
