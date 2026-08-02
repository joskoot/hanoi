;=====================================================================================================
; A GUI playing the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has clickable buttons height, mode, delay, reset, setup, quit, pile1, pile2 and pile3.
; A click on such button initiates an action. During an action some buttons may be disabled and
; disappear temporarily from the screen.
;=====================================================================================================

#lang racket

(provide play)

(require graphics/graphics racket/gui/base)

(define-syntax-rule
  (define-values-block (value ...) expr ...)
  (define-values (value ...) (let () expr ... (values value ...))))

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

;=====================================================================================================
; Main procedure.

(define (play)
  (initialize)
  (dynamic-wind
    void
    main
    close))

(define (close)
  (close-viewport vp)
  (close-graphics))

(define (main)
  ; At this point the GUI lways is in manual mode,
  ; but to be sure let's check it anyway.
  (unless (eq? (mode-button 'get-content) 'manual)
    (error 'play "mode manual expected, but found: ~s" (mode-button 'get-content)))
  (dispatch (mouse-click-posn (get-mouse-click vp))
    (height-button (do-height  ) (main))
    (mode-button   (do-mode    ) (main))
    (delay-button  (do-delay   ) (main))
    (reset-button  (do-reset   ) (main))
    (setup-button  (do-setup   ) (main))
    (pile1-button  (do-manual 0) (main))
    (pile2-button  (do-manual 1) (main))
    (pile3-button  (do-manual 2) (main))
    (quit-button (void))
    (else (main))))

;=====================================================================================================
; A button is displayed on the screen. It has procedure property can have a content.
; It is called as follows:
;
; (button command arg ...) --> any/c
; command : (or/c 'button-name 'in-button? 'disable 'enable 'get-content 'put-content)
; arg : any/c
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
               (define region (make-region pos button-width button-height))
               (define name-str (string-titlecase (format "~a" 'name)))
               (define enabled? #t)
               (define (proc button action . args)
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
                   (syntax (define button (make-button pos)))
                   (syntax (define button (make-button pos kontent)))))
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
       (let ((p pos))
         (cond
           ((button 'in-button? p) do-button ...) ...
           (else else-clause ...)))))
    ((_ pos (button do-button ...) ...)
     (syntax
       (let ((p pos))
         (cond
           ((button 'in-button? p) do-button ...) ...))))))

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
; Some constants required in early stage. Never mutated.

(define height-str   "Height"  )
(define mode-str     "Mode"    )
(define delay-str    "Delay"   )
(define reset-str    "Reset"   )
(define setup-str    "Setup"   )
(define quit-str     "Quit"    )
(define undo-str     "Undo"    )
(define manual-str   "manual"  )
(define short-str    "short"   )
(define long-str     "long"    )
(define circular-str "circular")
(define click 'click)
(define click-str (symbol->string 'click))
(define red   (make-rgb 1   0   0))
(define white (make-rgb 1   1   1))
(define black (make-rgb 0   0   0))
(define gray  (make-rgb 0.6 0.6 0.6))
(define blue  (make-rgb 0   0   1))
(define green (make-rgb 0 0.8   0))

;=====================================================================================================
; Procedures to draw buttons and their contents. Computation of their sizes.
; A temporary pixmap is used to measure string sizes.

(define-values-block (draw-button draw-button-content button-width button-height)

  ; Measure the maximum size of strings used in buttons.

  (define strings
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

  (define string-offset 4)
  (define *2string-offset (* 2 string-offset))

  (open-graphics)
  (define vp (open-pixmap "string-sizes" 1000 500))

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

  (define ((draw-button vp) pos str)
    (define x (posn-x pos))
    (define y (posn-y pos))
    ((draw-solid-rectangle vp) pos button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- string-offset))) str white))

  (define ((draw-button-content vp) pos value)
    (define str (if (string? value) value (format "~a" value)))
    (define x (posn-x pos))
    (define y (+ (posn-y pos) button-height))
    ((clear-solid-rectangle vp) (make-posn x y) button-width button-height)
    ((draw-rectangle vp) (make-posn x y) button-width button-height blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-height (- (* 2 string-offset)))) str blue)))

;=====================================================================================================
; Layout of the GUI. Computation of coordinates and sizes of all elements in the viewport of the GUI.

(define (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
(define max-height 9)
(define block 20)
(define border (* 3 block))
(define height-pos (make-posn border border))
(define mode-pos   (add-posn height-pos (+ button-width border) 0))
(define delay-pos  (add-posn mode-pos   (+ button-width border) 0))
(define reset-pos  (add-posn delay-pos  (+ button-width border) 0))
(define setup-pos  (add-posn reset-pos  (+ button-width border) 0))
(define quit-pos   (add-posn setup-pos  (+ button-width border) 0))
(define pile1-pos  (add-posn quit-pos   (+ button-width border) 0))
(define pile2-pos  (add-posn pile1-pos  (+ button-width border) 0))
(define pile3-pos  (add-posn pile2-pos  (+ button-width border) 0))
(define msg-pos (add-posn pile3-pos    (+ button-width (/ border 2)) button-height))
(define disk-height block)
(define max-tower-height (* max-height disk-height))
(define min-disk-width (* 3 block))
(define disk-width-incr block)
(define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width (disk-width (sub1 max-height)))
(define pile-top (* 2 block))
(define pile-width 4)
(define pile-y (* 2 (+ border button-height)))
(define pile-height (+ pile-top max-tower-height))
(define vp-width (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
(define vp-height (+ (* 2 button-height) (* 3 border) pile-height block))
(define girder-pos (make-posn border (- vp-height border block)))

(define (pile-x p)
  (+ border
    block
    (* p (+ border max-disk-width))
    (/ (- max-disk-width pile-width) 2)))

;=====================================================================================================
; Action : respons to click on button height.

(define (do-height)
  (define (validate-height str)
    (and (= (string-length str) 1) (char<=? #\1 (string-ref str 0) #\9)))
  (define str
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
    (define h (- (char->integer (string-ref str 0)) (char->integer #\0)))
    (set! height h)
    (height-button 'put-content h)
    (do-reset)))

;=====================================================================================================
; Action : respons to click on a pile button in manual mode.

(define (do-manual p)
  (define pile (vector-ref disk-distr p))
  (unless (null? pile)
    (define d (car pile))
    (define h (sub1 (length pile)))
    (mark-disk d h p)
    (do-manual1 d h p)))

(define (do-manual1 d h p)
  (dispatch (mouse-click-posn (get-mouse-click vp))
    (pile1-button (do-manual2 d h p 0))
    (pile2-button (do-manual2 d h p 1))
    (pile3-button (do-manual2 d h p 2))
    (else (draw-disk d h p))))

(define (do-manual2 d h p dest-p)
  (cond
    ((= dest-p p) (draw-disk d h p))
    (else
      (define pile (vector-ref disk-distr dest-p))
      (cond
        ((null? pile)
         (remove-disk d h p)
         (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
         (draw-disk d 0 dest-p)
         (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
        (else
          (define dest-d (car pile))
          (define dest-h (length pile))
          (cond
            ((< d dest-d)
             (remove-disk d h p)
             (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
             (draw-disk d dest-h dest-p)
             (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
            (else (draw-disk d h p))))))))

;=====================================================================================================
; Actions short, long and circular: Respons to click on button mode.

(define (do-mode)
  (define modes (list short-str long-str circular-str))
  (define choice
    (get-choices-from-user
      mode-str
      "Select a mode\nCancel in order to remain in manual mode."
      modes))
  (when choice
    (define ch (car choice))
    (define do-mode (vector-ref  (vector short long circular) ch))
    (define mode    (vector-ref #(       short long circular) ch))
    (mode-button 'put-content mode)
    (unless (eq? mode 'short) (do-reset))
    (prepare/finish-do-mode 'disable)
    (do-mode)
    (finish (symbol->string mode))))

(define (prepare/finish-do-mode enable/disable)
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

(define (finish who)
  (message-box who "finished" #f '(ok))
  ; Ignore clicks on reset and quit button while the message box is waiting.
  ; Clicks on other buttons already were ignored because they still are disabled.
  (viewport-flush-input vp)
  (clear-msg)
  (prepare/finish-do-mode 'enable)
  (mode-button 'put-content 'manual))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Actions short, long and circular disable some buttons.

(define (short)
  (reset-time-and-move-counter)
  (let/ec ec
    (define (exit) (clear-msg) (ec))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (define (short conf dest)
      (cond
        ((null? conf))
        ((= (car conf) dest) (short (cdr conf) dest))
        (else
          (define new-conf (- 3 (car conf) dest))
          (short (cdr conf) new-conf)
          (move-disk (car conf) dest exit)
          (short (make-list (length (cdr conf)) new-conf) dest))))
    (short p-list 2)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (long)
  (do-reset)
  (reset-time-and-move-counter)
  (let/ec ec
    (define (exit) (clear-msg) (ec))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (define (long conf dest)
      (cond
        ((null? conf))
        (else
          (define h (sub1 (length conf)))
          (define third (- 3 (car conf) dest))
          (long (cdr conf) dest)
          (move-disk (car conf) third exit)
          (long (make-list h dest) (car conf))
          (move-disk third dest exit)
          (long (make-list h (car conf)) dest))))
    (long p-list 2)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (circular)
  (do-reset)
  (reset-time-and-move-counter)
  (let/ec ec
    (define (exit) (clear-msg) (ec))
    (define (longest-circular-path     h   f t     )
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path                    h-1 f r     )
        (move-disk                         f t exit)
        (longest-non-circular-path     h-1 r f     )
        (move-disk                         t r exit)
        (longest-non-circular-path     h-1 f t     )
        (move-disk                         r f exit)
        (finish-path                   h-1 t f     )))
    (define (longest-non-circular-path h   f t     )
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path     h-1 f t     )
        (move-disk                         f r exit)
        (longest-non-circular-path     h-1 t f     )
        (move-disk                         r t exit)
        (longest-non-circular-path     h-1 f t     )))
    (define (start-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path h-1 f r)
        (move-disk                         f t exit)
        (longest-non-circular-path     h-1 r t     )))
    (define (finish-path               h   f t     )
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-non-circular-path     h-1 f r     )
        (move-disk                         f t exit)
        (finish-path                   h-1 r t     )))
    (longest-circular-path height          0 2     )))

;=====================================================================================================
; Action : respons to click on button delay.

(define (do-delay)
  (define (validate-delay str)
    (and (<= 1 (string-length str) 6)
      (or
        (equal? str click-str)
        (with-handlers ((exn:fail? (λ (e) #f)))
          (define input (open-input-string str))
          (define delay (read input))
          (cond
            ((not (eof-object? (read input))) #f)
            ((infinite? delay) #f)
            ((and (real? delay) (>= delay 0)))
            (else #f))))))
  (define str
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
      (define d (read (open-input-string str)))
      (set! delay d)
      (delay-button 'put-content d))))

;=====================================================================================================
; Action : respons to click on button reset.

(define (do-reset)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Action : respons to click on button setup.

(define (do-setup)
  (define disabled-buttons (list height-button mode-button delay-button reset-button setup-button))
  (for ((button (in-list disabled-buttons))) (button 'disable))
  (set! msg-str "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string vp) msg-pos msg-str red)
  (do-setup1 (reverse (range height)))
  (for ((button (in-list disabled-buttons))) (button 'enable))
  (clear-msg))

(define (do-setup1 disks)
  (unless (null? disks)
    (define d (car disks))
    (dispatch (mouse-click-posn (get-mouse-click vp))
      (pile1-button (do-setup2 d 0) (do-setup1 (cdr disks)))
      (pile2-button (do-setup2 d 1) (do-setup1 (cdr disks)))
      (pile3-button (do-setup2 d 2) (do-setup1 (cdr disks)))
      (quit-button (clear-msg) (do-reset))
      (else (do-setup1 disks)))))

(define (do-setup2 d p)
  (define pile (vector-ref disk-distr p))
  (vector-set! disk-distr p (cons d pile))
  (draw-disk d (length pile) p))

;=====================================================================================================
; Move-count and real time clock.

(define (reset-time-and-move-counter)
  (clear-msg)
  (set! clock (current-inexact-milliseconds))
  (set! move-count -1)
  (set! msg-str "")
  (draw-msg))

(define (clear-msg) ((clear-string vp) msg-pos msg-str))

(define (draw-msg)
  (clear-msg)
  (set! move-count (add1 move-count))
  (set! msg-str
    (if (eq? delay click)
      (format "Move count: ~s" move-count)
      (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
  ((draw-string vp) msg-pos msg-str))

(define (watch-clock)
  (~r #:precision 3 (/ (- (current-inexact-milliseconds) clock) 1000)))

;=====================================================================================================
; Drawing procedure.

(define (draw-piles)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle vp)
     (make-posn (pile-x p) pile-y)
     pile-width pile-height green)))

(define (remove-all-disks)
  ((clear-solid-rectangle vp)
   (make-posn (+ block border) (- vp-height border block max-tower-height))
   (+ (* 3 max-disk-width) (* 2 border))
   max-tower-height)
  (draw-piles))

(define (draw-disk d h p (color black))
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height color)
  ((draw-rectangle vp) pos width disk-height white)
  ((draw-string vp) (make-posn (- (pile-x p) 2) (+ y block -3)) (format "~s" (add1 d)) white))

(define (mark-disk d h p) (draw-disk d h p red))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (pile-x p) (/ pile-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle vp) pos width disk-height)
  ((draw-solid-rectangle vp)
   (make-posn (- center (/ pile-width 2)) y) pile-width disk-height green))

(define (move-disk f t exit)
  (define ff (vector-ref disk-distr f))
  (define tt (vector-ref disk-distr t))
  (unless (null? ff)
    (define d (car ff))
    (define move-to-be-made?
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

(define (check-click click-required? exit)
  (define pos ((if click-required? get-mouse-click ready-mouse-click) vp))
  (define p (and pos (mouse-click-posn pos)))
  (cond
    (p
      (dispatch p
        (reset-button (do-reset) (exit))
        (quit-button (exit))))
    (else #t)))

;=====================================================================================================
; Initialization. Actions to be taken before the game can start and introduction of variables that
; cannot be defined with their proper values before variable vp is initialized with a viewport.
; Variable vp always needs initialization because the viewport cannot be made before open-graphics is
; called, which is done during initialization. Some variables may need reinitialization when procedure
; play is called more than once because the procedure may mutate them. Notice that syntax make-button
; needs the viewport too. Therefore the buttons are included in the initialization.

(define vp 'yet-to-be-initialized)
(define height        'yet-to-be-initialized)
(define delay         'yet-to-be-initialized)
(define disk-distr    'yet-to-be-initialized)
(define height-button 'yet-to-be-initialized)
(define mode-button   'yet-to-be-initialized)
(define delay-button  'yet-to-be-initialized)
(define reset-button  'yet-to-be-initialized)
(define setup-button  'yet-to-be-initialized)
(define quit-button   'yet-to-be-initialized)
(define pile1-button  'yet-to-be-initialized)
(define pile2-button  'yet-to-be-initialized)
(define pile3-button  'yet-to-be-initialized)
(define msg-str       'yet-to-be-initialized)
(define clock         'yet-to-be-initialized)
(define move-count    'yet-to-be-initialized)

(define (initialize)
  ; Store variables not yet initialized.
  ; Also needed for variables they may have been mutated in a previous call to procedure play.
  (set! height     max-height)
  (set! delay      click     )
  (set! msg-str    ""        )
  (set! msg-str    ""        )
  (set! clock      0         )
  (set! move-count 0         )
  (set! disk-distr (vector (range height) '() '()))
  ; Open graphics and the viewport.
  (open-graphics)
  (set! vp (open-viewport "Tower of Hanoi" vp-width vp-height))
  ; Initalize and draw the buttons.
  (set! height-button (make-button height   height-pos max-height))
  (set! mode-button   (make-button mode     mode-pos   'manual   ))
  (set! delay-button  (make-button delay    delay-pos  click     ))
  (set! reset-button  (make-button reset    reset-pos            ))
  (set! setup-button  (make-button setup    setup-pos            ))
  (set! quit-button   (make-button quit     quit-pos             ))
  (set! pile1-button  (make-button |pile 1| pile1-pos            ))
  (set! pile2-button  (make-button |pile 2| pile2-pos            ))
  (set! pile3-button  (make-button |pile 3| pile3-pos            ))
  ; Draw a girder.
  ((draw-solid-rectangle vp) girder-pos (- vp-width (* 2 border)) block gray)
  ; Procedure do-reset draws the piles and the initial distribution of disks at the pile at the left.
  (do-reset))

;=====================================================================================================
; The end
