
;=====================================================================================================
;
; A GUI playing the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has clickable buttons Height, Mode, Delay, Reset, Setup, Quit, Peg1, Peg2, Peg3, Idle-
; time and Comp. A click on such button initiates an action. During an action some buttons may be
; disabled and disappear temporarily from the screen.
;
;=====================================================================================================
; Exports, imports and two simple macros.

#lang racket

(provide play idle-limit)

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
  ; Each action, Quit excepted, return to manual mode.
  ; The Quit button can be used to exit from the GUI,
  ; but sometimes it just terminates the current action.
  (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
  (dispatch-button pos
    (height-button (do-height  ) (main))
    (  mode-button (do-mode    ) (main))
    ( delay-button (do-delay   ) (main))
    ( reset-button (do-reset   ) (main))
    ( setup-button (do-setup   ) (main))
    (  peg1-button (do-manual 0) (main))
    (  peg2-button (do-manual 1) (main))
    (  peg3-button (do-manual 2) (main))
    (  idle-button (do-idle    ) (main))
    (  comp-button (do-comp    ) (main))
    (  quit-button (void       )       ) ; Exit from the game.
    (else
      (define p (dispatch-peg pos))
      (when p (do-manual p))
      (main))))

;=====================================================================================================
; Dispatching mouse clicks.

(define-syntax (dispatch-button stx)
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
  (define x-min (posn-x  (region-pos    region)))
  (define y-min (posn-y  (region-pos    region)))
  (define x-max (+ x-min (region-width  region)))
  (define y-max (+ y-min (region-height region)))
  (and (<= x-min x x-max) (<= y-min y y-max)))

(define (enable/disable-buttons buttons enable/disable)
  (for ((button (in-list buttons))) (button enable/disable)))

(define (enable/disable-all-buttons enable/disable)
  (define buttons
    (list
      height-button
      mode-button
      delay-button
      reset-button
      setup-button
      quit-button
      peg1-button
      peg2-button
      peg3-button
      idle-button
      comp-button))
  (enable/disable-buttons buttons enable/disable))

;=====================================================================================================
; Tool for aborting when waiting too long for for a mouseclick or answer to a dialog.

(define default-idle-minutes 10) ; minutes.
(define min-idle-minutes 1)

(define idle-limit ; minutes
  (make-parameter
    default-idle-minutes
    (λ (t)
      (cond
        ((and (real? t) (exact? t) (>= t min-idle-minutes)) t)
        (else
          (raise-user-error 'time-limit
            "~n  positive exact real number expected, at least ~s. Given ~s" min-idle-minutes t))))
    'time-limit))

(define (abort)
  (error '|Tower of Hanoi|
    (string-append
      "~n  No activity during ~s minutes. Game aborted."
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
; A button is displayed on the screen.
; It has procedure property and can have a content.
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
(define comp-str   " Comp "     )
(define click 'click)
(define click-str (symbol->string 'click))
(define red   (make-rgb 1.0 0.0 0.0))
(define white (make-rgb 1.0 1.0 1.0))
(define black (make-rgb 0.0 0.0 0.0))
(define gray  (make-rgb 0.6 0.6 0.6))
(define blue  (make-rgb 0.0 0.0 1.0))
(define green (make-rgb 0.0 0.8 0.0))

;=====================================================================================================
; Procedures to draw buttons and their contents. Computation of their sizes.
; A temporary pixmap is used to measure string sizes.

(define-values-block (draw-button draw-button-content button-width button-height string-offset)

  ; Measure the maximum size of strings used in buttons.

  (define strings
    (list
      height-str
      mode-str
      delay-str
      reset-str
      setup-str
      quit-str
      undo-str
      manual-str
      idle-str
      comp-str
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
(define   mode-pos (add-posn height-pos (+ button-width blok) 0))
(define  delay-pos (add-posn   mode-pos (+ button-width blok) 0))
(define  reset-pos (add-posn  delay-pos (+ button-width blok) 0))
(define  setup-pos (add-posn  reset-pos (+ button-width blok) 0))
(define   quit-pos (add-posn  setup-pos (+ button-width blok) 0))
(define   peg1-pos (add-posn   quit-pos (+ button-width blok) 0))
(define   peg2-pos (add-posn   peg1-pos (+ button-width blok) 0))
(define   peg3-pos (add-posn   peg2-pos (+ button-width blok) 0))
(define   idle-pos (add-posn   peg3-pos (+ button-width blok) 0))
(define   comp-pos (add-posn   idle-pos (+ button-width blok) 0))
(define    msg-pos (add-posn   comp-pos (+ button-width border) (- button-height string-offset)))
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
; Action manual.

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
; Action height.

(define (do-height)
  (enable/disable-all-buttons 'disable)
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
    (do-reset-help))
  (enable/disable-all-buttons 'enable))
;=====================================================================================================
; Action mode

(define (do-mode)
  (prepare/finish-do-mode 'disable)
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
    (unless (eq? mode 'short) (do-reset-help))
    (do-mode)
    (finish (symbol->string mode))))

(define (prepare/finish-do-mode enable/disable)
  (define buttons
    (list
      height-button
      mode-button
      delay-button
      setup-button
      peg1-button
      peg2-button
      peg3-button
      idle-button
      comp-button))
  (enable/disable-buttons buttons enable/disable))

(define (finish who)
  (call-with-time-out (λ () (message-box who "\n\n\nfinished\n\n\n" #f '(ok))))
  ; Ignore clicks on reset and quit button while the message box is waiting.
  ; Clicks on other buttons already were ignored because they still are disabled.
  (viewport-flush-input vp)
  (clear-msg)
  (prepare/finish-do-mode 'enable)
  (mode-button 'put-content 'manual))

;=====================================================================================================
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

;=====================================================================================================
; Action long.

(define (long)
  (do-reset-help)
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

;=====================================================================================================
; Action circular.

(define (circular)
  (do-reset-help)
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
; Action delay.

(define (do-delay)
  (enable/disable-all-buttons 'disable)
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
  (cond
    ((equal? str click-str)
     (set! delay click)
     (delay-button 'put-content click))
    ((not str))
    (else
      (define d (read (open-input-string str)))
      (set! delay d)
      (delay-button 'put-content d)))
  (enable/disable-all-buttons 'enable)
  (viewport-flush-input vp))

;=====================================================================================================
; Action reset.

(define (do-reset-help)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

(define (do-reset)
  (enable/disable-all-buttons 'disable)
  (do-reset-help)
  (enable/disable-all-buttons 'enable))

;=====================================================================================================
; Action setup.

(define (do-setup)
  (define buttons
    (list
      height-button
      mode-button
      delay-button
      quit-button
      setup-button
      idle-button
      comp-button))
  (enable/disable-buttons buttons 'disable)
  (set! msg-str "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string vp) msg-pos msg-str red)
  (do-setup1 (reverse (range height)))
  (enable/disable-buttons buttons 'enable)
  (clear-msg))

(define (do-setup1 disks)
  (unless (null? disks)
    (define d (car disks))
    (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
    (dispatch-button pos
      (peg1-button  (do-setup2 d 0) (do-setup1 (cdr disks)))
      (peg2-button  (do-setup2 d 1) (do-setup1 (cdr disks)))
      (peg3-button  (do-setup2 d 2) (do-setup1 (cdr disks)))
      (reset-button (clear-msg    ) (do-reset-help))
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
; Action idle time

(define (do-idle)
  (enable/disable-all-buttons 'disable)
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
            ((and (real? idle-time) (exact? idle-time) (>= idle-time 0)))
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
            "Idle times less than 1 minutes are set to 1 minutes.")
          #f	
          "10"	
          '(disallow-invalid)	
          #:validate validate-delay))))
  (when str
    (define minutes (max min-idle-minutes (read (open-input-string str))))
    (idle-limit minutes)
    ((draw-button-content vp) idle-pos minutes))
  (viewport-flush-input vp)
  (enable/disable-all-buttons 'enable))

;=====================================================================================================
; Action comp
; calc-short, calc-long and calc-circ number disks from 0 and use pegs 0, 1 and 2.
; When calling them the pegs must be decreased by 1 and upon return
; the disk and returned pegs must be increased by 1, in the returned distribution too.
; The three procedures start counting moves from 1, hence no modification of the move-count.

(define (do-comp)
  (enable/disable-all-buttons 'disable)
  (define (catcher e) (values (quote not-ok) #f #f #f #f))
  (define (validate-comp str)
    (with-handlers ((exn:fail? catcher))
      (define input (open-input-string str))
      (define L (read input))
      (define h (read input))
      (define m (read input))
      (define f (read input))
      (define t (read input))
      (cond
        ((and
           (member L (quote (S L C)))
           (exact-integer? h)
           (exact-integer? m)
           (exact-integer? f)
           (exact-integer? t)
           (<= 1 f 3)
           (<= 1 t 3)
           (not (= f t))
           (let ((expt3h (expt 3 h)))
             (< m (case L ((S) (expt 2 h)) ((L) expt3h) ((C) (add1 expt3h)))))
           (case L
             ((0) (<= 1 m (sub1 (expt 2 h))))
             ((1) (<= 1 m (sub1 (expt 3 h))))
             ((2) (<= 1 m       (expt 3 h)))))
         (values L h m f t))
        (else (catcher #f)))))
  (define answer
    (call-with-time-out
      (λ ()
        (message-box comp-str
          (string-append
            "Computation of move m:\n"
            "  which disk is moved,\n"
            "  from which peg it is taken,\n"
            "  onto which peg it put\n"
            "  and the resulting distribution of disks\n"
            "You will be asked for the following details:\n"
            "  mode  : capital letter: S for short, L for long and C for circular\n"
            "  height: number of disks (can be greater than 9)\n"
            "  from  : starting peg 1, 2 or 3\n"
            "  onto  : destination-peg 1, 2 or 3, but t≠f")
          #f
          (quote (ok-cancel))))))
  (when (eq? answer 'ok)
    (let loop ((first? #t))
      (define str
        (call-with-time-out
          (λ ()
            (get-text-from-user comp-str
              (string-append
                (if first? "" "Wrong data, try again\n")
                "Give mode, height, move -r from-disk and onto-disk")
              #f
              ""
              (quote ())))))
      (viewport-flush-input vp)
      (when str
        (define-values (L h m f t) (validate-comp str))
        (cond
          ((eq? L (quote not-ok)) (loop #f))
          (else
            (define input (open-input-string str))
            (define mode (read input))
            (define h    (read input))
            (define m    (read input))
            (define f    (read input))
            (define t    (read input))
            (define-values (d ff tt distr)
              ((case mode
                 ((S) calc-short)
                 ((L) calc-long)
                 ((C) calc-circ))
               h m (sub1 f) (sub1 t)))
            (when d ; catch not yet implemented calc-circ, make without when after implementing.
              (call-with-time-out
                (λ ()
                  (message-box comp-str
                    (string-append
                      (format
                        (string-append
                          "Results for move ~s for path ~a from peg ~s to peg ~s with ~s disks.~n~n"
                          "Disk ~s from peg ~s onto peg ~s.~n"
                          "Resulting distribution: "
                          "positions of disks in order of increasing size:~n~a~n")
                        m L f t h (add1 d) (add1 ff) (add1 tt)
                        (apply string-append
                          (for/fold
                            ((result '()) #:result (reverse result))
                            ((p (in-list distr)) (n (in-cycle (in-range 1 31))))
                            (if (= n 30)
                              (cons "\n" (cons (format "~s" (add1 p)) result))
                              (cons (format "~s" (add1 p)) result)))))))))))))))
  (enable/disable-all-buttons 'enable)
  (viewport-flush-input vp))

(define (calc-short h m f t)
  (define (exp2 n        ) (expt   2 n))
  (define (mod2 n        ) (modulo n 2))
  (define (mod3 n        ) (modulo n 3))
  (define (pari n        ) (add1 (mod2 (add1 n))))
  (define (rotd   h d f t) (mod3 (* (- t f) (pari (- h d)))))
  (define (rotr   h   f t) (rotd h 0 t f))
  (define (mcnt m   d    ) (quotient (+ m (exp2 d)) (exp2 (add1 d))))
  (define (thrd m h   f t) (mod3 (+ f (* m (rotr h f t)))))
  (define (onto m h   f t) (mod3 (- (thrd m h f t) (rotd h (disk m) f t))))
  (define (from m h   f t) (mod3 (+ (thrd m h f t) (rotd h (disk m) f t))))
  (define (posi m h d f t) (mod3 (+ f (* (rotd h d f t) (mcnt m d)))))
  (define (disk m        ) (sub1 (integer-length (bitwise-xor m (sub1 m)))))
  (values
    (disk m)
    (from m h f t)
    (add1 (onto m h f t))
    (for/list ((p (in-range h))) (posi m h p f t))))

(define (calc-long h m f t)
  (define (exp3 n      ) (expt   3 n))
  (define (mod3 n      ) (modulo n 3))
  (define (mod4 n      ) (modulo n 4))
  (define (thrd m   f t) (if (odd? m) t f))
  (define (onto m h f t) (posi m (disk m) f t))
  (define (from m h f t) (- 3 (onto m h f t) (thrd m f t)))
  #;(define (disk m)
      (let*
        ((log3 (inexact->exact (log 3)))
         (first-guess (ceiling (/ (inexact->exact (log m)) log3)))
         (upper-bound (expt 3 first-guess))
         (lowest-power-3 (gcd m upper-bound)))
        (inexact->exact (round (/ (log lowest-power-3) log3)))))
  (define (disk m) (if (zero? (mod3 m)) (add1 (disk (quotient m 3))) 0))
  (define (posi m d f t)
    (case (mod4 (mcnt m d))
      ((0) f)
      ((1 3) (- 3 f t))
      ((2) t)))
  (define (mcnt m d)
    (+
      (* 2  (quotient m (exp3 (add1 d))))
      (mod3 (quotient m (exp3       d)))))
  (values
    (disk m)
    (from m h f t)
    (onto m h f t)
    (for/list ((p (in-range h))) (posi m p f t))))

(define (calc-circ h m f t)
  (call-with-time-out
    (λ ()
      (message-box comp-str "Circular not yet implemented")))
  (viewport-flush-input vp)
  (values #f #f #f #f))
  
;=====================================================================================================
; Limit time waiting for a mouseclick or response to a dialog.

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
              (else (sleep (/ delay 10)) (loop))))
          (else (sleep (/ delay 10)) (loop)))))))

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
  (unless (null? ff)
    (define d (car ff))
    (define move-to-be-made?
      (case delay
        ((click) (check-click #t exit))
        (else
          (define first-doze-time (min delay 1))
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

; Procedure check-click enables abortion from short, long and circular mode when delay is not click.

(define (check-click click-required? exit)
  (define pos
    (if click-required?
      (call-with-time-out (λ () (get-mouse-click vp)))
      (ready-mouse-click vp)))
  (define p (and pos (mouse-click-posn pos)))
  (when p
    (dispatch-button p
      (reset-button (do-reset) (exit))
      ( quit-button (exit)))))

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
(define   mode-button 'yet-to-be-initialized)
(define  delay-button 'yet-to-be-initialized)
(define  reset-button 'yet-to-be-initialized)
(define  setup-button 'yet-to-be-initialized)
(define   quit-button 'yet-to-be-initialized)
(define   peg1-button 'yet-to-be-initialized)
(define   peg2-button 'yet-to-be-initialized)
(define   peg3-button 'yet-to-be-initialized)
(define   idle-button 'yet-to-be-initialized)
(define   comp-button 'yet-to-be-initialized)

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
  (set! height-button (make-button height    height-pos max-height  ))
  (set!   mode-button (make-button mode        mode-pos 'manual     ))
  (set!  delay-button (make-button delay      delay-pos click       ))
  (set!  reset-button (make-button reset      reset-pos             ))
  (set!  setup-button (make-button setup      setup-pos             ))
  (set!   quit-button (make-button quit        quit-pos             ))
  (set!   peg1-button (make-button |peg 1|     peg1-pos             ))
  (set!   peg2-button (make-button |peg 2|     peg2-pos             ))
  (set!   peg3-button (make-button |peg 3|     peg3-pos             ))
  (set!   idle-button (make-button |idle time| idle-pos (idle-limit)))
  (set!   comp-button (make-button comp        comp-pos             ))
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
