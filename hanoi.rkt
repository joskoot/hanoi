
;=====================================================================================================
;
; A GUI playing the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has clickable buttons: Height, Mode, Delay, Idle limit, Reset, Setup, Quit, Peg1, Peg2,
; Peg3, and Compute. A click on such button initiates an action. During an action some buttons may be
; disabled. Modal dialogs are used to exchange information between the GUI and the user.
;
;=====================================================================================================

#lang racket/base

(provide play idle-limit)

;=====================================================================================================

(module hanoi racket

  (require
    graphics/graphics
    racket/gui/base)
  
  (provide ; Hide all except procedure play and parameter idle-limit.
    play
    idle-limit)

  ;===================================================================================================
  ; General purpose tool.

  (define-syntax-rule
    (in-reversed-range n)
    (in-range (sub1 n) -1 -1))

  ;===================================================================================================
  ; Main procedure.

  (define (play)
    (let/ec ec
      ; Procedure initialize stores the continuation in variable escape.
      ; This variable is referred to by procedure abort and button quit.
      (initialize ec)
      (dynamic-wind
        void
        main
        close)))

  (define (close)
    (close-viewport viewport)
    (close-graphics))

  (define (main)
    ; At this point the GUI always is in manual mode.
    (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
    (dispatch pos))

  ;===================================================================================================
  ; Some constants that must be defined before being referred to. Not mutated.

  (define str-height   " Height "    )
  (define str-mode     " Mode "      )
  (define str-delay    " Delay "     )
  (define str-idle     " Idle limit ")
  (define str-reset    " Reset "     )
  (define str-setup    " Setup "     )
  (define str-quit     " Quit "      )
  (define str-undo     " Undo "      )
  (define str-manual   " manual "    )
  (define str-short    " short "     )
  (define str-long     " long "      )
  (define str-circular " circular "  )
  (define str-compute  " Compute "   )
  (define str-warn1    " A dialog is waiting."              )
  (define str-warn2    " Look for it when you don't see it.")
  (define click 'click)
  (define click-str (symbol->string click))
  (define white (make-rgb 1.0 1.0 1.0))
  (define black (make-rgb 0.0 0.0 0.0))
  (define gray  (make-rgb 0.6 0.6 0.6))
  (define red   (make-rgb 1.0 0.0 0.0))
  (define green (make-rgb 0.0 0.8 0.0))
  (define blue  (make-rgb 0.0 0.0 1.0))

  ;===================================================================================================
  ; Buttons:
  ; Are displayed on the screen.
  ; They have procedure property and some can have a content. Buttons are called as follows:
  ;
  ; (button command arg ...) --> any/c
  ; command : (or/c 'in-button? 'disable 'enable 'get-content 'put-content)
  ; arg : any/c ; as prescribed by the command.
  ;
  ; 'get-content and 'put-content are for buttons with content only.

  (struct button1 (proc pos)                    ; For buttons without content.
    #:property prop:procedure 0
    #:constructor-name button1-maker)

  (struct button2 button1 ((content #:mutable)) ; For buttons with    content.
    #:omit-define-syntaxes
    #:constructor-name button2-maker)

  (define-syntax make-button
    (let ((no-content (string->uninterned-symbol "no-content")))
      (λ (stx)
        (syntax-case stx ()
          ((_ name position) #`(make-button name position #,no-content))
          ((_ name position content)
           (let
             ((no-content?
                (and
                  (identifier? #'content)
                  (eq? (syntax-e #'content) no-content))))
             (define (if-no-content no-content . with-content)
               (if no-content? no-content with-content))
             #`(let ((pos position))
                 #,@(if no-content? null
                      (list #'(define the-content content)))
                 (define region (make-region pos width-of-button height-of-button))
                 (define name-str (str-title-case (format "~a" 'name)))
                 (define enabled? #t)
                 (define (proc action . args)
                   (case action
                     ((in-button?) (in-region? (car args) region))
                     #,@(if no-content? null
                          (list
                            #'((put-content)
                               (set-button2-content! button (car args))
                               ((draw-button-content viewport) pos (car args)))
                            #'((get-content) (button2-content button))))
                     ((disable)
                      (set! enabled? #f)
                      ((draw-disabled-button viewport) pos name-str))
                     ((enable)
                      (set! enabled? #t)
                      ((draw-button viewport) pos name-str))
                     (else
                       (apply
                         raise-argument-error
                         'name
                         #,(if no-content?
                             #'"(or/c 'in-button? 'disable 'enable)"
                             #'"(or/c 'in-button? 'disable 'enable 'get-content 'put-content)")
                         0 action args))))
                 #,(if no-content?
                     #'(define button (button1-maker proc pos))
                     #'(define button (button2-maker proc pos the-content)))
                 ; Draw the button and if it has content, the latter too.
                 ((draw-button viewport) pos name-str)
                 #,@(if no-content? null
                      (list #'((draw-button-content viewport) pos the-content)))
                 ; The procedure proper.
                 (λ (action . args)
                   (when
                     (or enabled?
                       (member action '(enable disable get-content put-content in-button?)))
                     (apply button action args))))))))))

  (define (enable/disable-all-buttons enable/disable)
    (define buttons
      (list
        button-height
        button-mode
        button-delay
        button-idle
        button-reset
        button-setup
        button-quit
        button-compute
        button-peg1
        button-peg2
        button-peg3))
    (enable/disable-buttons buttons enable/disable))

  (define (enable/disable-buttons buttons enable/disable)
    (for ((button (in-list buttons))) (button enable/disable)))

  (define (str-title-case str)
    (define lst (string->list str))
    (list->string (cons (char-upcase (car lst)) (cdr lst))))

  ; Procedures to draw buttons and their contents. Computation of their sizes.
  ; A temporary pixmap is used to measure string sizes.

  (define-syntax-rule
    (define-values-block (id ...)         def/expr ...                  )
    (define-values       (id ...) (let () def/expr ... (values id ...))))

  (define-values-block
    (draw-button
      draw-disabled-button
      draw-button-content
      width-of-button
      height-of-button
      str-offset)

    ; Measure the maximum size of strings used in buttons.

    (define strings
      (list
        str-height
        str-mode
        str-delay
        str-idle
        str-reset
        str-setup
        str-quit
        str-undo
        str-manual
        str-compute
        str-long
        str-circular
        "999999"))

    (define str-offset 4)
    (define *2str-offset (* 2 str-offset))

    ; Compute the required width and height of buttons.
    ; Use a temporary pixmap or this purpose.

    (open-graphics)
    (define pixmap (open-pixmap "string-sizes" 1000 500))

    (define-values (width-of-button height-of-button)
      (for/fold ((w 0) (h 0) #:result (values (+ w *2str-offset) (+ h *2str-offset)))
        ((w/h
           (in-list
             (map (get-string-size pixmap)
               strings))))
        (values
          (max w (inexact->exact (ceiling (car w/h))))
          (max h (inexact->exact (ceiling (cadr w/h)))))))

    (close-viewport pixmap)
    (close-graphics)

    ; Procedures drawing buttons.

    (define ((draw-button vp) pos str)
      (define x (posn-x pos))
      (define y (posn-y pos))
      ((draw-solid-rectangle vp) pos width-of-button height-of-button blue)
      ((draw-string vp)
       (make-posn (+ x str-offset) (+ y height-of-button (- str-offset))) str white))

    (define ((draw-disabled-button vp) pos str)
      (define x (posn-x pos))
      (define y (posn-y pos))
      ((clear-solid-rectangle vp) pos width-of-button height-of-button)
      ((draw-rectangle vp) pos width-of-button height-of-button blue)
      ((draw-string vp)
       (make-posn (+ x str-offset) (+ y height-of-button (- (* 2 str-offset)))) str blue))

    (define ((draw-button-content vp) pos value)
      (define str (if (string? value) value (format "~a" value)))
      (define x (posn-x pos))
      (define y (+ (posn-y pos) height-of-button))
      ((clear-solid-rectangle vp) (make-posn x y) width-of-button height-of-button)
      ((draw-rectangle        vp) (make-posn x y) width-of-button height-of-button blue)
      ((draw-string vp)
       (make-posn (+ x str-offset) (+ y height-of-button (- (* 2 str-offset)))) str blue)))

  ;===================================================================================================
  ; Dispatching mouse clicks.

  (define (dispatch pos)
    (dispatch-button pos
      (button-height  (action-height  ) (main))
      (button-mode    (action-mode    ) (main))
      (button-delay   (action-delay   ) (main))
      (button-idle    (action-idle    ) (main))
      (button-reset   (action-reset   ) (main))
      (button-setup   (action-setup   ) (main))
      (button-peg1    (action-manual 0) (main))
      (button-peg2    (action-manual 1) (main))
      (button-peg3    (action-manual 2) (main))
      (button-compute (action-compute ) (main))
      (button-quit    (escape         )       )
      (else
        (define p (dispatch-peg pos))
        (when p (action-manual p))
        (main))))

  (define-syntax (dispatch-button stx)
    (syntax-case stx (else)
      ((_ pos (button button-action ...) ... (else else-clause ...))
       #'(let ((p pos))
           (cond
             ((button 'in-button? p) button-action ...) ...
             (else else-clause ...))))
      ((_ pos (button button-action ...) ...)
       #'(let ((p pos))
           (cond
             ((button 'in-button? p) button-action ...) ...)))))

  (define (dispatch-peg pos)
    (cond
      ((in-region? pos region-peg0) 0)
      ((in-region? pos region-peg1) 1)
      ((in-region? pos region-peg2) 2)
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

  ;===================================================================================================
  ; Tool for aborting when waiting too long for for a mouseclick or answer to a dialog.

  (define default-idle-minutes 10)
  (define min-idle-minutes 1)

  (define idle-limit ; minutes
    (make-parameter
      default-idle-minutes
      (λ (t)
        (cond
          ((and (real? t) (exact? t) (>= t min-idle-minutes)) t)
          (else
            (raise-user-error 'idle-limit
              "~n  positive exact real number expected, at least ~s. Given ~s"
              min-idle-minutes t))))
      'idle-limit))

  (define (abort)
    (define abort-msg
      (format
        (string-append
          "~n  No activity during ~s minutes. Game aborted."
          "~n  Use parameter idle-limit to increase the allowed idle time"
          "~n  or use the Idle limit button.~n~n") (idle-limit)))
    (fprintf (current-error-port) (string-append  "~nTower of Hanoi~n" abort-msg))
    ; (message-box "Tower of Hanoi" abort-msg #f '(ok caution no-icon))
    (escape))

  (define-syntax (time-out stx)
    (syntax-case stx ()
      ((_ #f expr) #'(time-out-proc (λ () expr) #f))
      ((_ #t expr) #'(time-out-proc (λ () expr) #t))
      ((_    expr) #'(time-out-proc (λ () expr) #f))))

  (define (time-out-proc thunk warn?)
    (when warn? ((draw-string viewport) pos-warn1 str-warn1 red))
    (when warn? ((draw-string viewport) pos-warn2 str-warn2 red))
    (define time-out-custodian (make-custodian))
    (define answer-box (box #f))
    (define result
      (parameterize ((current-custodian time-out-custodian))
        (define dialog-eventspace (make-eventspace))
        (define task-thread
          (parameterize ((current-eventspace dialog-eventspace))
            (thread (λ () (set-box! answer-box (call-with-values thunk list))))))
        (sync/timeout (* (idle-limit) 60) task-thread)))
    (cond
      ((or (not result) (not (unbox answer-box)))
       (custodian-shutdown-all time-out-custodian)
       (abort))
      (else
        (custodian-shutdown-all time-out-custodian)
        (when warn?
          ((clear-string viewport) pos-warn1 str-warn1)
          ((clear-string viewport) pos-warn2 str-warn2))
        (apply values (unbox answer-box)))))

  ;===================================================================================================
  ; Layout of the GUI. Computation of coordinates and sizes
  ; of all elements in the viewport of the GUI. Not mutated.

  (define (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
  (define max-height 9)
  (define block 20)
  (define border (* 3 block))
  (define pos-height (make-posn border border))
  (define button-width+blok (+ width-of-button block))
  (define pos-mode    (add-posn pos-height  button-width+blok 0                                    ))
  (define pos-delay   (add-posn pos-mode    button-width+blok 0                                    ))
  (define pos-idle    (add-posn pos-delay   button-width+blok 0                                    ))
  (define pos-reset   (add-posn pos-idle    button-width+blok 0                                    ))
  (define pos-setup   (add-posn pos-reset   button-width+blok 0                                    ))
  (define pos-quit    (add-posn pos-setup   button-width+blok 0                                    ))
  (define pos-peg1    (add-posn pos-quit    button-width+blok 0                                    ))
  (define pos-peg2    (add-posn pos-peg1    button-width+blok 0                                    ))
  (define pos-peg3    (add-posn pos-peg2    button-width+blok 0                                    ))
  (define pos-compute (add-posn pos-peg3    button-width+blok 0                                    ))
  (define pos-msg     (add-posn pos-idle    button-width+blok (- (* 2 height-of-button) str-offset)))
  (define pos-warn1   (add-posn pos-compute button-width+blok (-      height-of-button  str-offset)))
  (define pos-warn2   (add-posn pos-warn1   0                 block                                ))
  (define disk-height block)
  (define max-tower-height (* max-height disk-height))
  (define min-disk-width (* 3 block))
  (define disk-width-incr block)
  (define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
  (define max-disk-width (disk-width (sub1 max-height)))
  (define peg-top (* 2 block))
  (define peg-width 4)
  (define peg-y (* 2 (+ border height-of-button)))
  (define peg-height (+ peg-top max-tower-height))
  (define vp-width (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
  (define vp-height (+ (* 2 height-of-button) (* 3 border) peg-height block))
  (define girder-pos (make-posn border (- vp-height border block)))

  (define region-peg0
    (make-region
      (make-posn
        (+ border block) (- vp-height border block max-tower-height))
      max-disk-width
      peg-height))

  (define region-peg1
    (make-region
      (add-posn (region-pos region-peg0) (+ max-disk-width border) 0)
      max-disk-width
      peg-height))

  (define region-peg2
    (make-region
      (add-posn (region-pos region-peg1) (+ max-disk-width border) 0)
      max-disk-width
      peg-height))

  (define (peg-x p)
    (+ border
      block
      (* p (+ border max-disk-width))
      (/ (- max-disk-width peg-width) 2)))

  ;===================================================================================================
  ; Action manual.

  (define (action-manual p)
    (define peg (vector-ref disk-distr p))
    (unless (null? peg)
      (define d (car peg))
      (define h (sub1 (length peg)))
      (mark-disk d h p)
      (action-manual1 d h p)))

  (define (action-manual1 d h p)
    (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
    (dispatch-button pos
      (button-peg1 (action-manual2 d h p 0))
      (button-peg2 (action-manual2 d h p 1))
      (button-peg3 (action-manual2 d h p 2))
      (else
        (define dest (dispatch-peg pos))
        (cond
          (dest (action-manual2 d h p dest))
          (else
            (draw-disk d h p)
            (reset-manual-count)
            (unless (button-quit 'in-button? pos) (dispatch pos)))))))

  (define (action-manual2 d h p dest-p)
    (cond
      ((= dest-p p) (draw-disk d h p))
      (else
        (define peg (vector-ref disk-distr dest-p))
        (cond
          ((null? peg)
           (remove-disk d h p)
           (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
           (draw-disk d 0 dest-p)
           (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p)))
           (inc-man-count))
          (else
            (define dest-d (car peg))
            (define dest-h (length peg))
            (cond
              ((< d dest-d)
               (remove-disk d h p)
               (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
               (draw-disk d dest-h dest-p)
               (vector-set! disk-distr dest-p (cons d (vector-ref disk-distr dest-p)))
               (inc-man-count))
              (else (draw-disk d h p))))))))

  (define (inc-man-count)
    (set! manual-count (add1 manual-count))
    (clear-man-count)
    (draw-man-count))

  (define (clear-man-count) ((clear-string viewport) pos-msg msg-str))

  (define (draw-man-count)
    (clear-man-count)
    (set! msg-str (format "Manual moves ~s" manual-count))
    (when (> manual-count 0) ((draw-string viewport) pos-msg msg-str)))

  (define (reset-manual-count) (clear-man-count) (set! manual-count 0))

  ;===================================================================================================
  ; Action height.

  (define (action-height)
    (enable/disable-all-buttons 'disable)
    (define (validate-height str)
      (and (= (string-length str) 1) (char<=? #\1 (string-ref str 0) #\9)))
    (define str
      (time-out #t
        (get-text-from-user
          str-height
          (string-append
            "How many disks do you want.\n"
            "At least one, at most nine.\n"
            "Enter a decimal digit but not 0")
          #f
          "9"
          '(disallow-invalid)
          #:validate validate-height)))
    (viewport-flush-input viewport)
    (when str
      (define h (- (char->integer (string-ref str 0)) (char->integer #\0)))
      (set! height h)
      (button-height 'put-content h)
      (reset-manual-count)
      (action-reset-help))
    (enable/disable-all-buttons 'enable))

  ;===================================================================================================
  ; Action mode

  (define (action-mode)
    (prepare/finish-action-mode 'disable)
    (define modes (list str-short str-long str-circular))
    (define choice
      (time-out #t
        (get-choices-from-user
          str-mode
          "Select a mode\nCancel in order to remain in manual mode."
          modes
          #f
          null
          '(single))))
    (viewport-flush-input viewport)
    (cond
      (choice
        (reset-manual-count)
        (define ch (car choice))
        (define action (vector-ref  (vector short long circular) ch))
        (define mode   (vector-ref #(       short long circular) ch))
        (button-mode 'put-content mode)
        (unless (eq? mode 'short) (action-reset-help))
        (action)
        (finish (symbol->string mode)))
      ((prepare/finish-action-mode 'enable))))

  (define (prepare/finish-action-mode enable/disable)
    (define buttons
      (append
        (list
          button-height
          button-mode
          button-delay
          button-setup
          button-peg1
          button-peg2
          button-peg3
          button-idle
          button-compute)
        (if (eq? (button-delay 'get-content) click)
          (list button-peg1 button-peg2 button-peg3)
          null)))
    (enable/disable-buttons buttons enable/disable))

  (define (finish who)
    (time-out #t (message-box who "\n\n\nfinished\n\n\n" #f '(ok no-icon)))
    ; Ignore clicks on reset and quit button while the message box is waiting.
    ; Clicks on other buttons already are ignored because they still are disabled.
    (viewport-flush-input viewport)
    (prepare/finish-action-mode 'enable)
    ((clear-string viewport) pos-msg msg-str)
    (button-mode 'put-content 'manual))

  ;===================================================================================================
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

  ;===================================================================================================
  ; Action long.

  (define (long)
    (action-reset-help)
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

  ;===================================================================================================
  ; Action circular.

  (define (circular)
    (action-reset-help)
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
      (longest-circular-path height          0 1     )))

  ;===================================================================================================
  ; Move disk for actions short, long or circular.

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

  ; Enable termination of actions short, long and circular by means of reset and quit.
  ; When mode is click, also use abort after the idle time has expired for a required mouseclick.

  (define (check-click click-required? exit)
    (define pos
      (if click-required?
        (time-out (get-mouse-click viewport))
        (ready-mouse-click viewport)))
    (define p (and pos (mouse-click-posn pos)))
    (when p
      (dispatch-button p
        (button-reset (action-reset) (exit))
        (button-quit                 (exit)))))

  ;===================================================================================================
  ; Doze is like sleep , but catching reset and quit during sleep.
  ; Used during actions short, long and circular.
  ; In this case quit and reset terminate these actions and restore mode manual.

  (define (doze t exit) ; t in seconds
    (cond
      ((zero? delay) (doze-help exit))
      (else
        (define starting-time (current-inexact-milliseconds))
        (define finish-time (+ starting-time (* 1000 t)))
        (define sleeping-time (min 0.25 (/ delay 1.5)))
        (let loop ()
          (cond
            ((>= (current-inexact-milliseconds) finish-time))
            (else (sleep sleeping-time) (doze-help exit) (loop)))))))

  (define (doze-help exit)
    (define click (ready-mouse-click viewport))
    (when click
      (dispatch-button (mouse-click-posn click)
        (button-reset (action-reset) (exit))
        (button-quit (exit)))))

  ;===================================================================================================
  ; Action delay.

  (define (action-delay)
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
      (time-out #t
        (get-text-from-user
          str-delay
          (string-append
            "Enter a non-negative real number for the\n"
            "approximate delay in seconds between moves\n"
            "or leave the default 'click' as it is.\n"
            "Do not enter more than 6 characters")
          #f	
          click-str	
          '(disallow-invalid)	
          #:validate validate-delay)))
    (cond
      ((equal? str click-str)
       (set! delay click)
       (button-delay 'put-content click))
      ((not str))
      (else
        (define d (read (open-input-string str)))
        (set! delay d)
        (button-delay 'put-content d)))
    (enable/disable-all-buttons 'enable)
    (viewport-flush-input viewport))

  ;===================================================================================================
  ; Action idle limit

  (define (action-idle)
    (enable/disable-all-buttons 'disable)
    (define (catcher e) #f)
    (define (validate-delay str)
      (and (<= 1 (string-length str) 6)
        (or
          (equal? str click-str)
          (with-handlers ((exn:fail? catcher))
            (define input (open-input-string str))
            (define idle-limit (read input))
            (cond
              ((not (eof-object? (read input))) #f)
              ((infinite? idle-limit) #f)
              ((and (real? idle-limit) (exact? idle-limit) (>= idle-limit 0)))
              (else #f))))))
    (define str
      (time-out #t
        (get-text-from-user
          str-idle
          (string-append
            "Enter a non-negative exact real number for the\n"
            "for the allowed idle time in minutes.\n"
            "Do not enter more than 6 characters.\n"
            "Idle limits less than 1 minutes are set to 1 minutes.")
          #f	
          "10"	
          '(disallow-invalid)	
          #:validate validate-delay)))
    (when str
      (define minutes (max min-idle-minutes (read (open-input-string str))))
      (idle-limit minutes)
      ((draw-button-content viewport) pos-idle minutes))
    (viewport-flush-input viewport)
    (enable/disable-all-buttons 'enable))

  ;===================================================================================================
  ; Action reset.

  (define (action-reset)
    (enable/disable-all-buttons 'disable)
    (action-reset-help)
    (enable/disable-all-buttons 'enable))

  (define (action-reset-help)
    (set! disk-distr (vector (range height) null null))
    (remove-all-disks)
    (reset-manual-count)
    (for ((d (in-range height)) (h (in-reversed-range height)))
      (draw-disk d h 0)))

  ;===================================================================================================
  ; Action setup.

  (define (action-setup)
    (define buttons
      (list
        button-height
        button-mode
        button-delay
        button-quit
        button-setup
        button-idle
        button-compute))
    (reset-manual-count)
    (enable/disable-buttons buttons 'disable)
    (set! msg-str "Setting up")
    (remove-all-disks)
    (set! disk-distr (make-vector 3 null))
    ((draw-string viewport) pos-msg msg-str red)
    (action-setup1 (reverse (range height)))
    (enable/disable-buttons buttons 'enable)
    (clear-msg))

  (define (action-setup1 disks)
    (unless (null? disks)
      (define d (car disks))
      (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
      (dispatch-button pos
        (button-peg1  (action-setup2 d 0) (action-setup1 (cdr disks)))
        (button-peg2  (action-setup2 d 1) (action-setup1 (cdr disks)))
        (button-peg3  (action-setup2 d 2) (action-setup1 (cdr disks)))
        (button-reset (clear-msg    ) (action-reset-help))
        (else
          (define p (dispatch-peg pos))
          (cond
            (p (action-setup2 d p) (action-setup1 (cdr disks)))
            (else (action-setup1 disks)))))))

  (define (action-setup2 d p)
    (define peg (vector-ref disk-distr p))
    (vector-set! disk-distr p (cons d peg))
    (draw-disk d (length peg) p))

  ;===================================================================================================
  ; Action compute
  ; calc-short, calc-long and calc-circ number disks from 0 and use pegs ordinals 0, 1 and 2. When
  ; calling them the peg ordinals must be decreased by 1 and upon return the ordinals of the disks and
  ; the pegs must be increased by 1, in the returned distribution too. The three procedures start
  ; counting moves from 1, hence no modification of the move-count.

  (define (action-compute)
    (enable/disable-all-buttons 'disable)
    (define (catcher e) (values 'not-ok #f #f #f #f))
    (define (validate-compute str)
      (with-handlers ((exn:fail? catcher))
        (define input (open-input-string str))
        (define L (read input))
        (define h (read input))
        (define m (read input))
        (define f (read input))
        (define t (read input))
        (cond
          ((and
             (member L '(S L C))
             (exact-integer? h)
             (exact-integer? m)
             (exact-integer? f)
             (exact-integer? t)
             (<= 1 f 3)
             (<= 1 t 3)
             (not (= f t))
             (let ((expt3h (expt 3 h)))
               (< 0 m (case L ((S) (expt 2 h)) ((L) expt3h) ((C) (add1 expt3h))))))
           (values L h m f t))
          (else (catcher #f)))))
    (define-values (ok answer)
      (cond
        (allow-intro
          (time-out #t
            (message+check-box str-compute
              (string-append
                "Computation of move m:\n"
                "  which disk is moved,\n"
                "  from which peg it is taken,\n"
                "  onto which peg it put\n"
                "  and the resulting distribution of disks\n\n"
                "You will be asked for the following details:\n\n"
                "  mode  : capital letter: S for short, L for long and C for circular\n"
                "  height: number of disks (can be greater than 9)\n"
                "  from  : starting peg 1, 2 or 3\n"
                "  onto  : destination-peg 1, 2 or 3, but t≠f")
              "Do not show this message next time again"
              #f
              '(ok-cancel no-icon))))
        (else (values 'ok #f))))
    (when answer (set! allow-intro #f))
    (when (eq? ok 'ok)
      (let loop ((first? #t))
        (define str
          (time-out #t
            (get-text-from-user str-compute
              (string-append
                (if first? "" "Wrong data, try again\n")
                "Give mode, height, move -r from-disk and onto-disk\n"
                "separated by spaces")
              #f
              "")))
        (viewport-flush-input viewport)
        (when str
          (define-values (L h m f t) (validate-compute str))
          (cond
            ((eq? L 'not-ok) (loop #f))
            (else
              (define input (open-input-string str))
              (define mode (read input))
              (define h    (read input))
              (define m    (read input))
              (define f    (read input))
              (define t    (read input))
              (define-values (d ff tt distr)
                ((case mode
                   ((S) calculate-short)
                   ((L) calculate-long )
                   ((C) calculate-circular ))
                 h m (sub1 f) (sub1 t)))
              (time-out #t
                (message-box str-compute
                  (string-append
                    (format
                      (string-append
                        "Results for move ~s for path ~a from peg ~s to peg ~s with ~s disks.~n~n"
                        "Disk ~s from peg ~s onto peg ~s.~n"
                        "Resulting distribution:~n"
                        "Positions of disks in order of increasing size:~n~a~n")
                      m L f t h (add1 d) (add1 ff) (add1 tt)
                      (apply string-append
                        (for/fold
                          ((result null) #:result (reverse result))
                          ((p (in-list distr)) (n (in-cycle (in-range 0 50))))
                          (if (= n 49)
                            (cons "\n" (cons (format "~s" (add1 p)) result))
                            (cons (format "~s" (add1 p)) result))))))
                  #f
                  (quote (ok no-icon)))))))))
    (enable/disable-all-buttons 'enable)
    (viewport-flush-input viewport))

  ;===================================================================================================
  ; Short option of action-compute.

  (define (calculate-short h m f t)
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
      (onto m h f t)
      (for/list ((p (in-range h))) (posi m h p f t))))

  ;===================================================================================================
  ; Long option of action-compute.

  (define (calculate-long h m f t)
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

  ;===================================================================================================
  ; Circular option of action-compute.

  (define (calculate-circular h M f t)
    (define (long m h f t r)
      (define-values (d F T distr) (calculate-long h m f t))
      (values d F T (append distr (list r))))
    (define (mover h f t r) (values h f t (append (make-list h r) (list t))))
    (cond
      ((= h 1)
       (define r (- 3 f t))
       (case M
         ((1) (values 0 f t (list t)))
         ((2) (values 0 t r (list r)))
         ((3) (values 0 r f (list f)))))
      (else
        (define r (- 3 f t))
        (define h-1 (sub1 h))
        (define 3^<h-1> (expt 3 h-1))
        (define 3^h (* 3 3^<h-1>))
        (define 3^<h-1>-1 (sub1 3^<h-1>))
        (define <3^<h-1>-1>/2 (/ 3^<h-1>-1 2))
        (define m (modulo (+ M <3^<h-1>-1>/2) 3^h))
        (cond
          ((zero? m)                 (mover  h-1 r f t))
          ((< m 3^<h-1>)             (long m h-1 t r f))
          ((= m 3^<h-1>)             (mover  h-1 f t r))
          ((< m (+ 3^<h-1> 3^<h-1>)) (long m h-1 r f t))
          ((= m (+ (* 2 3^<h-1>)))   (mover  h-1 t r f))
          ((< m (+ (* 3 3^<h-1>) 2)) (long m h-1 f t r))))))

  ;===================================================================================================
  ; Move-count and real time clock.

  (define (reset-time-and-move-counter)
    (clear-msg)
    (set! clock (current-inexact-milliseconds))
    (set! move-count -1)
    (set! msg-str "")
    (draw-msg))

  (define (clear-msg) ((clear-string viewport) pos-msg msg-str))

  (define (draw-msg)
    (clear-msg)
    (set! move-count (add1 move-count))
    (set! msg-str
      (if (eq? delay click)
        (format "Move count: ~s" move-count)
        (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
    ((draw-string viewport) pos-msg msg-str))

  (define (watch-clock)
    (~r #:precision 3 (/ (- (current-inexact-milliseconds) clock) 1000)))

  ;===================================================================================================
  ; Drawing procedures.

  (define (draw-pegs)
    (for ((p (in-range 3)))
      ((draw-solid-rectangle viewport)
       (make-posn (peg-x p) peg-y)
       peg-width peg-height green)))

  (define (remove-all-disks)
    ((clear-solid-rectangle viewport)
     (make-posn (+ block border) (- vp-height border block max-tower-height))
     (+ (* 3 max-disk-width) (* 2 border))
     max-tower-height)
    (draw-pegs))

  (define (draw-disk d h p (color black))
    (define width (disk-width d))
    (define center (+ (peg-x p) (/ peg-width 2)))
    (define x (- center (/ width 2)))
    (define y (- vp-height border block (* (add1 h) disk-height)))
    (define pos (make-posn x y))
    ((draw-solid-rectangle viewport) pos width disk-height color)
    ((draw-rectangle viewport) pos width disk-height white)
    ((draw-string viewport) (make-posn (- (peg-x p) 2) (+ y block -3)) (format "~s" (add1 d)) white))

  (define (mark-disk d h p) (draw-disk d h p red))

  (define (remove-disk d h p)
    (define width (disk-width d))
    (define center (+ (peg-x p) (/ peg-width 2)))
    (define x (- center (/ width 2)))
    (define y (- vp-height border block (* (add1 h) disk-height)))
    (define pos (make-posn x y))
    ((clear-solid-rectangle viewport) pos width disk-height)
    ; Draw the part of the pile that was hidden by the disk.
    ((draw-solid-rectangle viewport)
     (make-posn (- center (/ peg-width 2)) y) peg-width disk-height green))

  ;===================================================================================================
  ; Initialization. Actions to be taken before the game can start:
  ;
  ;   Setting mutable variables to their original values.
  ;   Opening graphics and the viewport.
  ;   Drawing the GUI.
  ;   Initialization of variable escape.
  ;
  ; Some variables cannot be defined with their proper values before variable viewport is initialized.
  ; This is especially true for buttons. Mutable variables may have been mutated in earlier calls to
  ; procedure play and therefore may require reinitialization. Continuation escape is a special case.
  ; Graphics is opened by procedure initialize. Making a button needs the viewport. Therefore all
  ; variables for buttons are included in the list of variables to be initialized.

  (define escape         'to-be-set-by-initialize)
  (define height         'mutable                )
  (define delay          'mutable                )
  (define msg-str        'mutable                )
  (define clock          'mutable                )
  (define move-count     'mutable                )
  (define manual-count   'mutable                )
  (define allow-intro    'mutable                )
  (define disk-distr     'mutable                )
  (define viewport       'needs-open-graphics    )
  (define button-height  'needs-the-viewport     )
  (define button-mode    'needs-the-viewport     )
  (define button-delay   'needs-the-viewport     )
  (define button-idle    'needs-the-viewport     )
  (define button-reset   'needs-the-viewport     )
  (define button-setup   'needs-the-viewport     )
  (define button-quit    'needs-the-viewport     )
  (define button-peg1    'needs-the-viewport     )
  (define button-peg2    'needs-the-viewport     )
  (define button-peg3    'needs-the-viewport     )
  (define button-compute 'needs-the-viewport     )

  (define (initialize  ec)
    ; Restore variables that may have been mutated in earlier calls to procedure play.
    ; Initialize variable escape too.
    (set! escape       ec        )
    (set! height       max-height)
    (set! delay        click     )
    (set! msg-str      ""        )
    (set! clock        0         )
    (set! move-count   0         )
    (set! manual-count 0         )
    (set! allow-intro  #t        )
    ; disk-distr: will be initialized later by means of procedure action-reset-help.
    ; Open graphics and the viewport.
    (open-graphics)
    (set! viewport (open-viewport "Tower of Hanoi" vp-width vp-height))
    ; Initialize and draw the buttons.
    (set! button-height  (make-button height       pos-height max-height  ))
    (set! button-mode    (make-button mode         pos-mode   'manual     ))
    (set! button-delay   (make-button delay        pos-delay  click       ))
    (set! button-idle    (make-button |idle limit| pos-idle   (idle-limit)))
    (set! button-reset   (make-button reset        pos-reset              ))
    (set! button-setup   (make-button setup        pos-setup              ))
    (set! button-quit    (make-button quit         pos-quit               ))
    (set! button-peg1    (make-button |peg 1|      pos-peg1               ))
    (set! button-peg2    (make-button |peg 2|      pos-peg2               ))
    (set! button-peg3    (make-button |peg 3|      pos-peg3               ))
    (set! button-compute (make-button compute      pos-compute            ))
    ; Draw a girder on which the pegs are attached.
    ((draw-solid-rectangle viewport) girder-pos (- vp-width (* 2 border)) block gray)
    (for ((p (in-range 0 3)))
      (define str (format "Peg ~s" (add1 p)))
      (define size (car ((get-string-size viewport) str)))
      ((draw-string viewport)
       (add-posn (make-posn (peg-x p) (- vp-height border))
         (- (/ size 2))
         (- (/ str-offset 2)))
       str white))
    ; Procedure action-reset-help initializes variable disk-distr and draws the pegs and the initial
    ; distribution of disks at the peg on the left.
    (action-reset-help)))

;=====================================================================================================

(require 'hanoi)

;=====================================================================================================
; The end
