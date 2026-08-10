
;=====================================================================================================
;
; A GUI playing the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has clickable buttons height, mode, delay, reset, setup, quit, peg1, peg2 and peg3.
; A click on such button initiates an action. During an action some buttons may be disabled and
; disappear temporarily from the screen.
;
;=====================================================================================================

#lang racket

(provide play idle-limit)

;=====================================================================================================
; Imports and two simple macros.

(require
  graphics/graphics
  racket/gui/base)

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

(define-syntax-rule
  (define-values-block (value ...)         def/expr ...)
  (define-values       (value ...) (let () def/expr ... (values value ...))))

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
  ; At this point the GUI always is in manual mode,
  (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
  (dispatch-button pos
    (height-button (do-height  ) (main))
    (mode-button   (do-mode    ) (main))
    (delay-button  (do-delay   ) (main))
    (reset-button  (do-reset   ) (main))
    (setup-button  (do-setup   ) (main))
    (peg1-button   (do-manual 0) (main))
    (peg2-button   (do-manual 1) (main))
    (peg3-button   (do-manual 2) (main))
    (quit-button   (void       )       ) ; Exit from the game.
    (else
      (define p (dispatch-peg pos))
      (when p (do-manual p))
      (main))))

;=====================================================================================================
; Tool for aborting when waiting too long for for a mouseclick or answer to a dialog.

(define default-idle-minutes 10) ; minutes.
(define min-idle-minutes 5)

(define idle-limit ; minutes
  (make-parameter
    default-idle-minutes
    (λ (t)
      (cond
        ((and (real? t) (>= t min-idle-minutes)) t)
        (else
          (raise-user-error 'time-limit
            "~n  positive real number expected, at least 5. Given ~s" t))))
    'time-limit))

(define (abort)
  (error '|Tower of Hanoi|
    (string-append
      "~n  No activity during ~s minutes. GUI aborted."
      "~n  Use parameter time-limit to increase the allowed idle time"
      "~n  or use the Idle time button.")
    (idle-limit)))

(define (call-with-time-out thunk)
  (define time-out-custodian (make-custodian))
  (define user-choice (box #f))
  (define result
    (parameterize ((current-custodian time-out-custodian))
      (define dialog-eventspace (make-eventspace))
      (define task-thread
        (parameterize ((current-eventspace dialog-eventspace))
          (thread
            (λ ()
              (define choice (thunk))
              (set-box! user-choice choice)))))
      (sync/timeout (* (idle-limit) 60) task-thread)))
  (cond
    ((not result)
     (custodian-shutdown-all time-out-custodian) 
     (abort))
    (else
      (define final-response (unbox user-choice))
      (custodian-shutdown-all time-out-custodian)
      final-response)))

;=====================================================================================================
; A button is displayed on the screen. It has procedure property and can have a content.
; It is called as follows:
;
; (button command arg ...) --> any/c
; command : (or/c 'in-button? 'disable 'enable 'get-content 'put-content)
; arg : any/c
;
; 'get-content and 'put-content are for buttons with content only.

(define-syntax make-button
  (let ((no-kontent (string->uninterned-symbol "no-kontent")))
    (λ (stx)
      (syntax-case stx ()
        ((_ name position) (quasisyntax (make-button name position (unsyntax no-kontent))))
        ((_ name position kontent)
         (let
           ((no-kontent-condition
              (and
                (identifier? (syntax kontent))
                (eq? (syntax-e (syntax kontent)) no-kontent))))
           (quasisyntax
             (let ((pos position))
               (define region (make-region pos button-width button-height))
               (define name-str (str-title-case (format "~a" 'name)))    
               (define enabled? #t)
               (define (proc button action . args)
                 (case action
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
                       "(or/c 'in-button? 'disable 'enable 'get-content 'put-content)"
                       0 action args))))
               (struct button
                 (unsyntax
                   (if no-kontent-condition
                     (syntax (pos))
                     (syntax (pos (content #:mutable)))))
                 #:property prop:procedure proc
                 #:omit-define-syntaxes
                 #:constructor-name button-maker)
               (unsyntax
                 (if no-kontent-condition
                   (syntax (define button (button-maker pos)))
                   (syntax (define button (button-maker pos kontent)))))
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

(define (str-title-case str)
  (define lst (string->list str))
  (list->string (cons (char-upcase (car lst)) (cdr lst))))

;=====================================================================================================
; Dispatching mouse clicks.

(define-syntax (dispatch-button stx)
  (syntax-case stx (else)
    ((_ pos (button do-button ...) ... (else else-clause ...))
     (syntax
       (let ((p pos))
         (cond
           ((idle-button 'in-button? p) (do-idle) (main))
           ((button 'in-button? p) do-button ...) ...
           (else else-clause ...)))))
    ((_ pos (button do-button ...) ...)
     (syntax
       (let ((p pos))
         (cond
           ((idle-button 'in-button? p) (do-idle) (main))
           ((button 'in-button? p) do-button ...) ...))))))

(define (dispatch-peg pos)
  (cond
    ((in-region? pos peg0-region) 0)
    ((in-region? pos peg1-region) 1)
    ((in-region? pos peg2-region) 2)
    (else #f)))

(struct region (pos width height)
  #:omit-define-syntaxes
  #:constructor-name make-region)

(define (in-region? pos region)
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define x-min (posn-x ( region-pos    region)))
  (define y-min (posn-y  (region-pos    region)))    
  (define x-max (+ x-min (region-width  region)))
  (define y-max (+ y-min (region-height region)))
  (and (<= x-min x x-max) (<= y-min y y-max)))

;=====================================================================================================
; Some constants required in early stage. Never mutated.

(define height-str " Height "   )
(define mode-str   " Mode "     )
(define delay-str  " Delay "    )
(define reset-str  " Reset "    )
(define setup-str  " Setup "    )
(define quit-str   " Quit "     )
(define undo-str   " Undo "     )
(define manual-str " manual "   )
(define shrt-str   " short "    )
(define long-str   " long "     )
(define circ-str   " circular " )
(define idle-str   " Idle time ")
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

(define-values-block (draw-button draw-button-content button-width button-height string-offset)

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
      circ-str
      idle-str
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
(define blok 20)
(define border (* 3 blok))
(define height-pos (make-posn border border))
(define mode-pos   (add-posn height-pos (+ button-width blok) 0))
(define delay-pos  (add-posn mode-pos   (+ button-width blok) 0))
(define reset-pos  (add-posn delay-pos  (+ button-width blok) 0))
(define setup-pos  (add-posn reset-pos  (+ button-width blok) 0))
(define quit-pos   (add-posn setup-pos  (+ button-width blok) 0))
(define peg1-pos   (add-posn quit-pos   (+ button-width blok) 0))
(define peg2-pos   (add-posn peg1-pos   (+ button-width blok) 0))
(define peg3-pos   (add-posn peg2-pos   (+ button-width blok) 0))
(define idle-pos   (add-posn peg3-pos   (+ button-width blok) 0))  
(define msg-pos    (add-posn idle-pos   (+ button-width border) (- button-height string-offset)))
(define disk-height blok)
(define max-tower-height (* max-height disk-height))
(define min-disk-width (* 3 blok))
(define disk-width-incr blok)
(define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width (disk-width (sub1 max-height)))
(define peg-top (* 2 blok))
(define peg-width 4)
(define peg-y (* 2 (+ border button-height)))
(define peg-height (+ peg-top max-tower-height))
(define vp-width (+ (* 3 max-disk-width) (* 2 blok) (* 4 border)))
(define vp-height (+ (* 2 button-height) (* 3 border) peg-height blok))
(define girder-pos (make-posn border (- vp-height border blok)))

(define peg0-region
  (make-region
    (make-posn
      (+ border blok) (- vp-height border blok max-tower-height))
    max-disk-width
    peg-height))

(define peg1-region
  (make-region
    (add-posn (region-pos peg0-region) (+ max-disk-width border) 0)
    max-disk-width
    peg-height))

(define peg2-region
  (make-region
    (add-posn (region-pos peg1-region) (+ max-disk-width border) 0)
    max-disk-width
    peg-height))

(define (peg-x p)
  (+ border
    blok
    (* p (+ border max-disk-width))
    (/ (- max-disk-width peg-width) 2)))

;=====================================================================================================
; Action Height.

(define (do-height)
  (define (validate-height str)
    (and (= (string-length str) 1) (char<=? #\1 (string-ref str 0) #\9)))
  (define str
    (call-with-time-out
      (λ ()
        (get-text-from-user
          height-str
          (string-append
            "How many disks do you want.\n"
            "At least one, at most nine.\n"
            "Enter a decimal digit but not 0")
          #f
          "9"
          '(disallow-invalid)
          #:validate validate-height))))
  (viewport-flush-input vp)
  (when str
    (define h (- (char->integer (string-ref str 0)) (char->integer #\0)))
    (set! height h)
    (height-button 'put-content h)
    (do-reset)))

;=====================================================================================================
; Action Mode.

(define (do-manual p)
  (define peg (vector-ref disk-distr p))
  (unless (null? peg)
    (define d (car peg))
    (define h (sub1 (length peg)))
    (mark-disk d h p)
    (do-manual1 d h p)))

(define (do-manual1 d h p)
  (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
  (dispatch-button pos
    (peg1-button (do-manual2 d h p 0))
    (peg2-button (do-manual2 d h p 1))
    (peg3-button (do-manual2 d h p 2))
    (else
      (define dest (dispatch-peg pos))
      (if dest
        (do-manual2 d h p dest)
        (draw-disk d h p)))))

(define (do-manual2 d h p dest-p)
  (cond
    ((= dest-p p) (draw-disk d h p))
    (else
      (define peg (vector-ref disk-distr dest-p))
      (cond
        ((null? peg)
         (remove-disk d h p)
         (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
         (draw-disk d 0 dest-p)
         (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
        (else
          (define dest-d (car peg))
          (define dest-h (length peg))
          (cond
            ((< d dest-d)
             (remove-disk d h p)
             (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
             (draw-disk d dest-h dest-p)
             (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p))))
            (else (draw-disk d h p))))))))

;=====================================================================================================
; Action Mode: short, long and circular.

(define (do-mode)
  (define modes (list shrt-str long-str circ-str))
  (define choice
    (call-with-time-out
      (λ ()
        (get-choices-from-user
          mode-str
          "Select a mode\nCancel in order to remain in manual mode."
          modes))))
  (viewport-flush-input vp)
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
           peg1-button
           peg2-button
           peg3-button
           idle-button))))
    (button enable/disable)))

(define (finish who)
  (call-with-time-out (λ () (message-box who "\n\n\nfinished\n\n\n" #f '(ok))))
  (viewport-flush-input vp)
  ; Ignore clicks on reset and quit button while the message box is waiting.
  ; Clicks on other buttons already were ignored because they still are disabled.
  (viewport-flush-input vp)
  (clear-msg)
  (prepare/finish-do-mode 'enable)
  (mode-button 'put-content 'manual))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Actions short.

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
; Action long.

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
; Action circular.

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
; Action Delay.

(define (do-delay)
  (define (catcher e) #f)
  (define (validate-delay str)
    (and (<= 1 (string-length str) 6)
      (or
        (equal? str click-str)
        (with-handlers ((exn:fail? catcher))
          (define input (open-input-string str))
          (define delay (read input))
          (cond
            ((not (eof-object? (read input))) #f)
            ((infinite? delay) #f)
            ((and (real? delay) (>= delay 0)))
            (else #f))))))
  (define str
    (call-with-time-out
      (λ ()
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
          #:validate validate-delay))))  
  (viewport-flush-input vp)
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
; Action Reset.

(define (do-reset)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Action Setup.

(define (do-setup)
  (define disabled-buttons (list height-button mode-button delay-button quit-button setup-button))
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
    (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
    (dispatch-button pos
      (peg1-button  (do-setup2 d 0) (do-setup1 (cdr disks)))
      (peg2-button  (do-setup2 d 1) (do-setup1 (cdr disks)))
      (peg3-button  (do-setup2 d 2) (do-setup1 (cdr disks)))
      (reset-button (clear-msg    ) (do-reset))
      (else
        (define p (dispatch-peg pos))
        (cond
          (p (do-setup2 d p) (do-setup1 (cdr disks)))
          (else (do-setup1 disks)))))))

(define (do-setup2 d p)
  (define peg (vector-ref disk-distr p))
  (vector-set! disk-distr p (cons d peg))
  (draw-disk d (length peg) p))

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

(define (do-idle)
  (define (catcher e) #f)
  (define (validate-delay str)
    (and (<= 1 (string-length str) 6)
      (or
        (equal? str click-str)
        (with-handlers ((exn:fail? catcher))
          (define input (open-input-string str))
          (define idle-time (read input))
          (cond
            ((not (eof-object? (read input))) #f)
            ((infinite? idle-time) #f)
            ((and (real? idle-time) (>= idle-time 0) (exact? idle-time)))
            (else #f))))))
  (define str
    (call-with-time-out
      (λ ()
        (get-text-from-user
          idle-str
          (string-append
            "Enter a non-negative exact real number for the\n"
            "for the allowed idle time in minutes.\n"
            "Do not enter more than 6 characters.\n"
            "Idle times less than 5 minutes are set to 5 minutes.")
          #f	
          "10"	
          '(disallow-invalid)	
          #:validate validate-delay))))
  (when str
    (define minutes (max min-idle-minutes (read (open-input-string str))))
    (idle-limit minutes)
    ((draw-button-content vp) idle-pos minutes)))

;=====================================================================================================
; Drawing procedure.

(define (draw-pegs)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle vp)
     (make-posn (peg-x p) peg-y)
     peg-width peg-height green)))

(define (remove-all-disks)
  ((clear-solid-rectangle vp)
   (make-posn (+ blok border) (- vp-height border blok max-tower-height))
   (+ (* 3 max-disk-width) (* 2 border))
   max-tower-height)
  (draw-pegs))

(define (draw-disk d h p (color black))
  (define width (disk-width d))
  (define center (+ (peg-x p) (/ peg-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border blok (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle vp) pos width disk-height color)
  ((draw-rectangle vp) pos width disk-height white)
  ((draw-string vp) (make-posn (- (peg-x p) 2) (+ y blok -3)) (format "~s" (add1 d)) white))

(define (mark-disk d h p) (draw-disk d h p red))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (peg-x p) (/ peg-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border blok (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle vp) pos width disk-height)
  ; Draw the part of the pile that was hidden by the disk.pile 
  ((draw-solid-rectangle vp)
   (make-posn (- center (/ peg-width 2)) y) peg-width disk-height green))

(define (move-disk f t exit)
  (define ff (vector-ref disk-distr f))
  (define tt (vector-ref disk-distr t))
  (define first-doze-time (min delay 1))
  (unless (null? ff)
    (define d (car ff))
    (define move-to-be-made?
      (case delay
        ((click) (check-click #t exit))
        (else
          (cond
            ((= move-count 0) (doze first-doze-time exit) (check-click #f exit))
            (else (doze delay exit) (check-click #f exit))))))
    (cond
      (move-to-be-made?
        (remove-disk d (sub1 (length ff)) f)
        (draw-disk d (length tt) t)
        (vector-set! disk-distr f (cdr ff))
        (vector-set! disk-distr t (cons d tt))
        (draw-msg))
      (else (move-disk f t exit)))))

(define (doze t exit)
  (define starting-time (current-inexact-milliseconds))
  (define finish-time (+ starting-time (* 1000 t)))
  (let loop ()
    (cond
      ((>= (current-inexact-milliseconds) finish-time))
      (else
        (define click (ready-mouse-click vp))
        (cond
          (click
            (dispatch-button (mouse-click-posn click)
              (reset-button (do-reset) (exit))
              (quit-button (exit))
              (else (sleep 1/4) (loop))))
          (else (sleep 1/4) (loop)))))))

; Procedure check-click enables abortion from short, long and circular mode.

(define (check-click click-required? exit)
  (define pos
    (if click-required? (call-with-time-out (λ () (get-mouse-click vp))) (ready-mouse-click vp)))
  (define p (and pos (mouse-click-posn pos)))
  (cond
    (p
      (dispatch-button p
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

(define vp            'yet-to-be-initialized)
(define height        'yet-to-be-initialized)
(define delay         'yet-to-be-initialized)
(define disk-distr    'yet-to-be-initialized)
(define msg-str       'yet-to-be-initialized)
(define clock         'yet-to-be-initialized)
(define move-count    'yet-to-be-initialized)
(define height-button 'yet-to-be-initialized)
(define mode-button   'yet-to-be-initialized)
(define delay-button  'yet-to-be-initialized)
(define reset-button  'yet-to-be-initialized)
(define setup-button  'yet-to-be-initialized)
(define quit-button   'yet-to-be-initialized)
(define peg1-button   'yet-to-be-initialized)
(define peg2-button   'yet-to-be-initialized)
(define peg3-button   'yet-to-be-initialized)
(define idle-button   'yet-to-be-initialized)

(define (initialize)
  ; Store variables not yet initialized.
  ; Also needed for variables they may have been mutated in a previous call to procedure play.
  (idle-limit default-idle-minutes)
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
  (set! height-button (make-button height      height-pos max-height))
  (set! mode-button   (make-button mode        mode-pos  'manual    ))
  (set! delay-button  (make-button delay       delay-pos  click     ))
  (set! reset-button  (make-button reset       reset-pos            ))
  (set! setup-button  (make-button setup       setup-pos            ))
  (set! quit-button   (make-button quit        quit-pos             ))
  (set! peg1-button   (make-button |peg 1|     peg1-pos             ))
  (set! peg2-button   (make-button |peg 2|     peg2-pos             ))
  (set! peg3-button   (make-button |peg 3|     peg3-pos             ))
  (set! idle-button   (make-button |idle time| idle-pos  (idle-limit)))
  ; Draw a girder.
  ((draw-solid-rectangle vp) girder-pos (- vp-width (* 2 border)) blok gray)
  (for ((p (in-range 0 3)))
    (define str (format "Peg ~s" (add1 p)))
    (define size (car ((get-string-size vp) str)))
    ((draw-string vp)
     (add-posn (make-posn (peg-x p) (- vp-height border))
       (- (/ size 2))
       (- (/ string-offset 2)))
     str white))
  ; Procedure do-reset draws the pegs and the initial distribution of disks at the peg at the left.
  (do-reset))

;=====================================================================================================
; The end
