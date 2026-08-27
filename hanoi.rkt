#lang racket

;=====================================================================================================
; The Tower of Hanoi, a GUI.
; By Jacob J. A. Koot
;=====================================================================================================
;
; A GUI playing the game of The Tower of Hanoi. Implemented with racket, graphics/graphics and
; racket/gui/base. Moves can be made manually but also automatically by the GUI. It has clickable
; buttons. A click on a button initiates an action. Modal dialogs are used to exchange information
; between the GUI and the user. Documentation for the user can be made from module "hanoi.scrbl".
;
;=====================================================================================================

(require graphics/graphics racket/gui/base)
(provide play idle-limit)

;=====================================================================================================

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

; The following macro is for the dispatch of mouse clicks. Could be defined within procedure play but
; defining it here makes DrRacket show binding arrows within the macro, which is not the case within
; a local context.

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

;=====================================================================================================
; When the GUI is waiting for a mouse-click or a response to a modal dialog but the user does not act
; or answer within a certain time, the GUI aborts. The limit is hold in parameter idle-limit.

(define default-idle-minutes 10)
(define max-idle-minutes 100000) ; Almost 70 days.
(define min-idle-minutes      1)

(define idle-limit ; minutes
  (make-parameter
    default-idle-minutes
    (λ (t)
      (cond
        ((and (exact-positive-integer? t) (<= t max-idle-minutes)) t)
        (else
          (raise-user-error 'idle-limit
            "~n exact positive integer <= ~s wanted. Given ~s" max-idle-minutes t))))
    'parameter-idle-limit))

;=====================================================================================================

(define (play)
  
  (let/ec escape

    ; The escape is for abort with a message on the error-port but without raising an exeption.

    (define (time-out-abort)
      (define limit (idle-limit))
      (fprintf (current-error-port)
        "~nTower of Hanoi~n~
         ~n  No activity during ~a. Game aborted.~
         ~n  Use parameter idle-limit to increase the allowed~
         ~n  idle time or use the Idle limit button.~n~n"
        (if (= limit 1) "1 minute" (format "~s minutes" limit)))
      (escape))
    
    ;=================================================================================================
    ; The following variables can be mutated while playing. Procedure initialize stores the initial
    ; values or restores them when GUI is started again after a session that mutated the variables.

    (define height       'mutable)
    (define delay        'mutable)
    (define msg-str      'mutable)
    (define clock        'mutable)
    (define move-count   'mutable)
    (define manual-count 'mutable)
    (define allow-intro  'mutable)
    (define disk-distr   'mutable)

    ;=================================================================================================
    ; Variables that can and must be defined in early stage.
    ; They are referred to in early stage. Not mutated.
    
    (define block            20                                        )
    (define border           (* 3 block)                               )
    (define max-height       9                                         )
    (define disk-height      block                                     )
    (define max-tower-height (* max-height disk-height)                )
    (define str-offset       4                                         )
    (define *2str-offset     (* 2 str-offset)                          )
    (define min-disk-width   (* 3 block)                               )
    (define disk-width-incr  block                                     )
    (define (disk-width d)   (+ min-disk-width (* 2 d disk-width-incr)))
    (define max-disk-width   (disk-width (sub1 max-height))            )
    (define peg-top          (* 2 block)                               )
    (define peg-width        4                                         )
    (define click            'click                                    )
    (define str-click        (symbol->string click)                    )
    (define str-height       " Height "                                )
    (define str-mode         " Mode "                                  )
    (define str-delay        " Delay "                                 )
    (define str-idle         " Idle limit "                            )
    (define str-reset        " Reset "                                 )
    (define str-setup        " Setup "                                 )
    (define str-quit         " Quit "                                  )
    (define str-undo         " Undo "                                  )
    (define str-manual       " manual "                                )
    (define str-short        " short "                                 )
    (define str-long         " long "                                  )
    (define str-circular     " circular "                              )
    (define str-compute      " Compute "                               )
    (define str-warn1        " A dialog is waiting."                   )
    (define str-warn2        " Look for it when you don't see it."     )
    (define white            (make-rgb 1.0 1.0 1.0)                    )
    (define black            (make-rgb 0.0 0.0 0.0)                    )
    (define gray             (make-rgb 0.6 0.6 0.6)                    )
    (define red              (make-rgb 1.0 0.0 0.0)                    )
    (define green            (make-rgb 0.0 0.8 0.0)                    )
    (define blue             (make-rgb 0.0 0.0 1.0)                    )

    ; List of all strings that must fit within a button.
    ; In the next section used to determine the dimensions of buttons.

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

    ;=================================================================================================
    ; Computation of the dimensions of buttons. These depend on the sizes of the texts they must
    ; hold, which are given by procedure get-string-size. A temporary pixmap is used. This requires
    ; graphics, wich is temporarily opened too. Both will be closed after the computation is finished.

    (define-values (with-of-button height-of-button)
      (let ()
        (open-graphics)
        (define pixmap (open-pixmap "string-sizes" 1000 500))
        (dynamic-wind
          void
          (λ ()
            (for/fold ((w 0) (h 0) #:result (values (+ w *2str-offset) (+ h *2str-offset)))
              ((w/h (in-list (map (get-string-size pixmap)  strings))))
              (values
                (max w (inexact->exact (ceiling (car w/h))))
                (max h (inexact->exact (ceiling (cadr w/h)))))))
          (λ () (close-viewport pixmap) (close-graphics)))))

    ;=================================================================================================
    ; Lay out of the GUI. Locations and dimensions of all objects to be drtawn in the GUI.
    
    (define (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
    (define peg-y (* 2  (+ border height-of-button)))
    (define peg-height  (+ peg-top max-tower-height))
    (define vp-width    (+ (* 3 max-disk-width) (* 2 block) (* 4 border)))
    (define vp-height   (+ (* 2 height-of-button) (* 3 border) peg-height block))
    (define pos-height  (make-posn border border))
    (define button-w+b  (+ with-of-button block))
    (define pos-mode    (add-posn pos-height  button-w+b 0))
    (define pos-delay   (add-posn pos-mode    button-w+b 0))
    (define pos-idle    (add-posn pos-delay   button-w+b 0))
    (define pos-reset   (add-posn pos-idle    button-w+b 0))
    (define pos-setup   (add-posn pos-reset   button-w+b 0))
    (define pos-quit    (add-posn pos-setup   button-w+b 0))
    (define pos-peg1    (add-posn pos-quit    button-w+b 0))
    (define pos-peg2    (add-posn pos-peg1    button-w+b 0))
    (define pos-peg3    (add-posn pos-peg2    button-w+b 0))
    (define pos-compute (add-posn pos-peg3    button-w+b 0))
    (define pos-msg     (add-posn pos-idle    button-w+b (- (* 2 height-of-button) str-offset)))
    (define pos-warn1   (add-posn pos-compute button-w+b (-      height-of-button  str-offset)))
    (define pos-warn2   (add-posn pos-warn1   0 block))
    (define girder-pos  (make-posn border     (- vp-height border block)))

    ;=================================================================================================
    ; Define drawing procedures with implied viewport argument.
    ; For this purpose the viewport must be opened now.

    (open-graphics)
    (define viewport (open-viewport "Tower of Hanoi" vp-width vp-height))
    (define paint-rectangle        (draw-rectangle        viewport))
    (define paint-solid-rectangle  (draw-solid-rectangle  viewport))
    (define remove-solid-rectangle (clear-solid-rectangle viewport))
    (define paint-string           (draw-string           viewport))
    (define remove-string          (clear-string          viewport))

    ;=================================================================================================
    ; A region records the position and dimensions of objects whose clicks must be dispatched.
    ; Regions for buttons are included in the buttons themselves.

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
  
    (define region-peg1
      (make-region
        (make-posn
          (+ border block) (- vp-height border block peg-height))
        max-disk-width
        peg-height))

    (define region-peg2
      (make-region
        (add-posn (region-pos region-peg1) (+ max-disk-width border) 0)
        max-disk-width
        peg-height))

    (define region-peg3
      (make-region
        (add-posn (region-pos region-peg2) (+ max-disk-width border) 0)
        max-disk-width
        peg-height))

    (define (peg-x p)
      (+ border
        block
        (* p (+ border max-disk-width))
        (/ (- max-disk-width peg-width) 2)))
    
    ;=================================================================================================
    ; Buttons. Some have a content. The constructors not only make the buttons but draw them too. They
    ; contain their positions and regions and have procedure property. Some buttons have a content. 
    
    (define (proc-button1 button action (pos #f))
      (case action
        ((in-button?)
         (in-region? pos (button1-region button)))
        ((enabled?)
         (button1-enabled? button))
        ((disable)
         (set-button1-enabled?! button #f)
         (draw-disabled-button (button1-pos button) (button1-name button)))
        ((enable)
         (set-button1-enabled?! button #t)
         (draw-button (button1-pos button) (button1-name button)))))

    (define (proc-button2 button action (arg #f))
      (case action
        ((get-content)
         (button2-content button))
        ((put-content)
         (set-button2-content! button arg)
         (draw-button-content (button1-pos button) arg))
        (else (proc-button1 button action arg))))

    (struct button1 ((enabled? #:mutable) region pos name)
      #:property prop:procedure proc-button1
      #:constructor-name make-button1)

    (struct button2 button1 ((content #:mutable))
      #:property prop:procedure proc-button2
      #:omit-define-syntaxes
      #:constructor-name make-button2)

    ; Constructor make-button is called either without content or with a true content, never false.

    (define (make-button name position (content #f))
      (let*
        ((pos position)
         (region (make-region pos with-of-button height-of-button))
         (str-name (str-title-case (symbol->string name))))
        (draw-button pos str-name)
        (cond
          (content ; Make button with    content.
            (draw-button-content pos content)
            (make-button2 #t region pos str-name content))
          (else    ; Make button without content.
            (make-button1 #t region pos str-name)))))

    (define (str-title-case str)
      (define lst (string->list str))
      (list->string (cons (char-upcase (car lst)) (cdr lst))))

    ; Buttons can be disabled and enabled.

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

    ;=================================================================================================
    ; Procedures drawing buttons and when applicable their contents.

    (define (draw-button pos str)
      (define x (posn-x pos))
      (define y (posn-y pos))
      (paint-solid-rectangle pos with-of-button height-of-button blue)
      (paint-string (make-posn (+ x str-offset) (+ y height-of-button (- str-offset))) str white))

    (define (draw-disabled-button pos str)
      (define x (posn-x pos))
      (define y (posn-y pos))
      (remove-solid-rectangle pos with-of-button height-of-button)
      (paint-rectangle pos with-of-button height-of-button blue)
      (paint-string (make-posn (+ x str-offset) (+ y height-of-button (- *2str-offset)))  str blue))

    (define (draw-button-content pos value)
      (define str (if (string? value) value (format "~a" value)))
      (define x (posn-x pos))
      (define y (+ (posn-y pos) height-of-button))
      (remove-solid-rectangle (make-posn x y) with-of-button height-of-button)
      (paint-rectangle  (make-posn x y) with-of-button height-of-button blue)
      (paint-string (make-posn (+ x str-offset) (+ y height-of-button (- *2str-offset)))  str blue))

    ;=================================================================================================
    ; Dispatch of mouse-clicks on buttons    
    ; Procedure main always is called in tail position of procedure dispatch and itself.
    ; In DrRacket check this by tacking arrows on the first left parenthesis to follow from here.
    
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

    (define (dispatch-peg pos)
      (cond
        ((in-region? pos region-peg1) 0)
        ((in-region? pos region-peg2) 1)
        ((in-region? pos region-peg3) 2)
        (else #f)))

    ;=================================================================================================
    ; Timing out after exceeding the idle-limit.

    (define-syntax (time-out stx)
      (syntax-case stx ()
        ((_ #f expr) #'(time-out-proc #f (λ () expr)))
        ((_ #t expr) #'(time-out-proc #t (λ () expr)))
        ((_    expr) #'(time-out-proc #f (λ () expr)))))

    (define (time-out-proc warn? thunk)
      (when warn?
        (paint-string pos-warn1 str-warn1 red)
        (paint-string pos-warn2 str-warn2 red))
      (define custodian (make-custodian))
      (define result-box (box #f))
      (define in-time?
        (parameterize ((current-custodian custodian))
          (define dialog-eventspace (make-eventspace))
          (define task
            (parameterize ((current-eventspace dialog-eventspace))
              (thread (λ () (set-box! result-box (call-with-values thunk list))))))
          (sync/timeout (* (idle-limit) 60) task)))   ; Minutes to seconds.
      (cond
        ((or (not in-time?) (not (unbox result-box))) ; Maximum idle time exceeded. Abort.
         (custodian-shutdown-all custodian)
         (time-out-abort))
        (else                                         ; Answer within maximum idle time.
          (custodian-shutdown-all custodian)          ; Return the result, possibly a multiple value.
          (when warn?
            (remove-string pos-warn1 str-warn1)
            (remove-string pos-warn2 str-warn2))
          (apply values (unbox result-box)))))

    ;=================================================================================================
    ; Action manual. peg is the one that has been selected to be moved.

    (define (action-manual peg)
      (define peg-disks (vector-ref disk-distr peg))
      (unless (null? peg-disks)
        (define d (car peg-disks))
        (when ; Act only if the selected disk can be moved, else do nothing.
          (or
            (< d (size-of-top-disk (modulo (+ 1 peg) 3)))
            (< d (size-of-top-disk (modulo (+ 2 peg) 3))))
          (define h (sub1 (length peg-disks)))
          (mark-disk d h peg)
          (action-manual1 d h peg))))

    (define (action-manual1 d h p) ; Ask on which peg to put the disk. 
      (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
      (dispatch-button pos
        (button-peg1 (action-manual2 d h p 0)) ; Yes, peg 1 selected.
        (button-peg2 (action-manual2 d h p 1)) ; Yes, peg 2 selected.
        (button-peg3 (action-manual2 d h p 2)) ; Yes, peg 3 selected.
        (else ; Disk not selected by means of a peg button. May be by a click near a peg.
          (define dest (dispatch-peg pos))
          (cond
            (dest (action-manual2 d h p dest))      ; Yes, a destination peg has been selected.
            (else                                   ; Something else than a peg selected.
              (draw-disk d h p)                     ; Unmark the selected disk.
              (unless (button-quit 'in-button? pos) ; In this case button quit finishes the sequence
                (reset-manual-count)                ; of manual moves without aborting. Other clicks
                (dispatch pos)))))))                ; are processed normally as far as enabled.

    (define (action-manual2 d h p dest-peg)
      (cond
        ((= dest-peg p) (draw-disk d h p)) 
        (else
          (define dest-peg-list-of-disks (vector-ref disk-distr dest-peg))
          (cond
            ((< d (size-of-top-disk dest-peg))
             ; The move is allowed. Move it.
             (remove-disk d h p)
             (vector-set! disk-distr p (cdr (vector-ref disk-distr p)))
             (vector-set! disk-distr dest-peg (cons d dest-peg-list-of-disks))
             (draw-disk d (length dest-peg-list-of-disks) dest-peg)
             (increment-manual-count))
            ; The move is not allowed. Unmark the selected disk and ignore the mouse-click.
            (else (draw-disk d h p))))))

    (define (size-of-top-disk p)
      (define peg (vector-ref disk-distr p))
      (if (null? peg) max-height (car peg)))

    ;=================================================================================================
    ; Show the number of manual moves made sofar.

    (define (increment-manual-count)
      (set! manual-count (add1 manual-count))
      (clear-manual-count)
      (draw-manual-count))

    (define (clear-manual-count) (remove-string pos-msg msg-str))

    (define (draw-manual-count)
      (clear-manual-count)
      (set! msg-str (format "Manual moves ~s" manual-count))
      (when (> manual-count 0) (paint-string pos-msg msg-str)))

    (define (reset-manual-count) (clear-manual-count) (set! manual-count 0))

    ;=================================================================================================
    ; Action height.
    
    (define (action-height)
      (enable/disable-all-buttons 'disable)
      (define (validate-height str)
        (and (= (string-length str) 1) (char<=? #\1 (string-ref str 0) #\9)))
      (define str
        (time-out #t
          (get-text-from-user
            str-height
            (format
              "How many disks do you want.\n~
               At least one, at most nine.\n~
               Enter a decimal digit but not 0")
            #f
            "9"
            '(disallow-invalid)
            #:validate validate-height)))
      (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
      (when str
        (define h (- (char->integer (string-ref str 0)) (char->integer #\0)))
        (set! height h)
        (button-height 'put-content h)
        (reset-manual-count)
        (action-reset-help))
      (enable/disable-all-buttons 'enable))

    ;=================================================================================================
    ; Action mode.
    ; When finishing mode short, long or circular, notify the user.
    ; Disable/enable all buttons, reset and quit excepted.
    ; The latter two allow aborting the short, long and circular action.

    (define (finish who)
      (time-out #t (message-box who "\n\n\nfinished\n\n\n" #f '(ok no-icon)))
      (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
      (prepare/finish-action-mode 'enable)
      (remove-string pos-msg msg-str)
      (button-mode 'put-content 'manual))

    (define (prepare/finish-action-mode enable/disable)
      (define buttons
        (list
          button-height
          button-mode
          button-delay
          button-setup
          button-peg1
          button-peg2
          button-peg3
          button-idle
          button-compute))
      (enable/disable-buttons buttons enable/disable))

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
            '()
            '(single))))
      (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
      (cond
        (choice
          (reset-manual-count)
          (define ch (car choice))
          (define action (vector-ref (vector short long circular) ch))
          (define mode (vector-ref #( short long circular) ch))
          (button-mode 'put-content mode)
          (unless (eq? mode 'short) (action-reset-help))
          (action)
          (finish (symbol->string mode)))
        ((prepare/finish-action-mode 'enable))))

    ;=================================================================================================
    ; Action short mode.

    (define (short)
      (reset-time-and-move-counter)
      (let/ec ec
        ; The escape allows procedure move-disk to stop the action.
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
        ; Peg 2 always is the destination (peg 3 for the user)
        (short p-list 2)))

    ;=================================================================================================
    ; Action long mode.

    (define (long)
      (action-reset-help)
      (reset-time-and-move-counter)
      (let/ec ec
        ; The escape allows procedure move-disk to stop the action.
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

    ;=================================================================================================
    ; Action circular mode.

    (define (circular)
      (action-reset-help)
      (reset-time-and-move-counter)
      (let/ec ec
        ; The escape allows procedure move-disk to stop the action.
        (define (exit) (clear-msg) (ec))
        (define (longest-circular-path h f t)
          (unless (zero? h)
            (define h-1 (sub1 h))
            (define r (- 3 f t))
            (start-path h-1 f r)
            (move-disk f t exit)
            (longest-path h-1 r f)
            (move-disk t r exit)
            (longest-path h-1 f t)
            (move-disk r f exit)
            (finish-path h-1 t f)))
        (define (longest-path h f t)
          (unless (zero? h)
            (define h-1 (sub1 h))
            (define r (- 3 f t))
            (longest-path h-1 f t)
            (move-disk f r exit)
            (longest-path h-1 t f)
            (move-disk r t exit)
            (longest-path h-1 f t)))
        (define (start-path h f t)
          (unless (zero? h)
            (define h-1 (sub1 h))
            (define r (- 3 f t))
            (start-path h-1 f r)
            (move-disk f t exit)
            (longest-path h-1 r t)))
        (define (finish-path h f t)
          (unless (zero? h)
            (define h-1 (sub1 h))
            (define r (- 3 f t))
            (longest-path h-1 f r)
            (move-disk f t exit)
            (finish-path h-1 r t)))
        (longest-circular-path height 0 1)))

    ;=================================================================================================
    ; Actions short, long and circular use procedure move-disk.
    ; It allows abort from the action by means of buttons reset and quit.
    ; For this purpose procedure move-disk receives a continuation of the calling action. 
    ; If the delay is not click It uses procedure doze in order to make the moves at the desired rate.

    (define (move-disk f t exit)
      (define ff (vector-ref disk-distr f))
      (define tt (vector-ref disk-distr t))
      (unless (null? ff)
        (define d (car ff))
        (define move-to-be-made?
          (case delay
            ((click) (check-click #t exit))
            (else
              (define first-doze-time (min delay 1.5))
              (cond
                ((= move-count 0) (doze first-doze-time exit) (check-click #f exit))
                (else (doze delay exit) (check-click #f exit))))))
        (cond
          (move-to-be-made?
            (remove-disk d (sub1 (length ff)) f)
            (draw-disk d (length tt) t)
            (vector-set! disk-distr f (cdr ff))
            (vector-set! disk-distr t (cons d tt))
            (draw-count-msg))
          (else (move-disk f t exit)))))

    (define (check-click click-required? exit)
      (define pos
        (if click-required?
          (time-out (get-mouse-click viewport))
          (ready-mouse-click viewport)))
      (define p (and pos (mouse-click-posn pos)))
      (when p
        (dispatch-button p
          (button-reset (action-reset) (exit))
          (button-quit (exit)))))

    (define (doze t exit) ; t in seconds. Like sleep, but with time out.
      (cond
        ((zero? delay) (doze-help exit))
        (else
          (define starting-time (current-inexact-milliseconds))
          (define finish-time (+ starting-time (* 1000 t)))
          (define sleeping-time (min 0.25 (/ delay 1.5)))
          (define (loop) ; Periodically sleep and check for time out.
            (cond
              ((>= (current-inexact-milliseconds) finish-time))
              (else (sleep sleeping-time) (doze-help exit) (loop))))
          (loop))))

    ; Cancel the action when button-reset or button-quit is clicked.

    (define (doze-help exit)
      (define click (ready-mouse-click viewport))
      (when click
        (dispatch-button (mouse-click-posn click)
          (button-reset (action-reset) (exit))
          (button-quit (exit)))))

    ;=================================================================================================
    ; Action delay.

    (define (action-delay)
      (enable/disable-all-buttons 'disable)
      (define (catcher e) #f)
      (define (validate-delay str)
        (and (<= 1 (string-length str) 6)
          (or
            (equal? str str-click)
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
            (format
              "Enter a non-negative real number for the\n~
               approximate delay in seconds between moves\n~
               or leave the default 'click' as it is.\n~
               Do not enter more than 6 characters")
            #f	
            str-click	
            '(disallow-invalid) 	
            #:validate validate-delay)))
      (cond
        ((equal? str str-click)
         (set! delay click)
         (button-delay 'put-content click))
        ((not str))
        (else
          (define d (read (open-input-string str)))
          (set! delay d)
          (button-delay 'put-content d)))
      (enable/disable-all-buttons 'enable)
      (viewport-flush-input viewport)) ; Ignore mouse-clicks made before a response on the dialog.

    ;=================================================================================================
    ; Action idle limit.
    
    (define (action-idle)
      (enable/disable-all-buttons 'disable)
      (define (catcher e) #f)
      (define (validate-delay str)
        (and (<= 1 (string-length str) 6)
          (with-handlers ((exn:fail? catcher))
            (define input (open-input-string str))
            (define idle-limit (read input))
            (and (exact-positive-integer? idle-limit)
              (<= min-idle-minutes idle-limit max-idle-minutes)))))
      (define str
        (time-out #t
          (get-text-from-user
            str-idle
            (format
              "Enter an exact positive integer number not exceeding\n~
               ~s for the maximally allowed idle time in\n~
               minutes. Do not enter more than 6 characters."
              max-idle-minutes)
            #f	
            "10"	
            '(disallow-invalid) 	
            #:validate validate-delay)))
      (when str
        (define minutes (read (open-input-string str)))
        (idle-limit minutes)
        (draw-button-content  pos-idle minutes))
      (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
      (enable/disable-all-buttons 'enable))

    ;=================================================================================================
    ; Action reset.
    ; Remove all disks, redraw the pegs and draw the full tower at peg 0 (peg 1 for the user)
   
    (define (action-reset)
      (enable/disable-all-buttons 'disable)
      (action-reset-help)
      (enable/disable-all-buttons 'enable))


    (define (action-reset-help)
      (set! disk-distr (vector (range height) '() '()))
      (remove-all-disks)
      (reset-manual-count)
      (for ((d (in-range height)) (h (in-reversed-range height)))
        (draw-disk d h 0)))

    ;=================================================================================================
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
      (set! disk-distr (make-vector 3 '()))
      (paint-string pos-msg msg-str red)
      (action-setup1 (reverse (range height)))
      (enable/disable-buttons buttons 'enable)
      (clear-msg))

    (define (action-setup1 disks)
      (unless (null? disks)
        (define d (car disks))
        (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
        (dispatch-button pos
          (button-peg1 (action-setup2 d 0) (action-setup1 (cdr disks)))
          (button-peg2 (action-setup2 d 1) (action-setup1 (cdr disks)))
          (button-peg3 (action-setup2 d 2) (action-setup1 (cdr disks)))
          (button-reset (clear-msg) (action-reset-help))
          (else
            (define p (dispatch-peg pos))
            (cond
              (p (action-setup2 d p) (action-setup1 (cdr disks)))
              (else (action-setup1 disks)))))))

    (define (action-setup2 d p)
      (define peg (vector-ref disk-distr p))
      (vector-set! disk-distr p (cons d peg))
      (draw-disk d (length peg) p))

    ;=================================================================================================
    ; Action compute.
    
    (define (action-compute)
      (define namespace (make-base-namespace))
      (define-values (SLC h m f t) (values #f #f #f #f #f))
      (enable/disable-all-buttons 'disable)
      (define (catcher e) (set! SLC 'not-ok))
      (define (validate-compute str)
        (with-handlers ((exn:fail? catcher))
          (define input (open-input-string str))
          (set! SLC (read input))
          (set! h (read input))
          (set! m (let ((m (read input))) (if (real? m) m (eval m namespace))))
          (set! f (read input))
          (set! t (read input)))
        (or
          (and
            (member SLC '(S L C))
            (exact-integer? h)
            (exact-integer? m)
            (exact-integer? f)
            (exact-integer? t)
            (<= 1 f 3)
            (<= 1 t 3)
            (not (= f t))
            (let ((expt3h (expt 3 h)))
              (< 0 m (case SLC ((S) (expt 2 h)) ((L) expt3h) ((C) (add1 expt3h))))))
          (set! SLC 'not-ok)))
      (define-values (ok answer)
        (cond
          (allow-intro
            (time-out #t
              (message+check-box str-compute
                (format
                  "Computation of move m:\n ~
                   which disk is moved,\n ~
                   from which peg it is taken,\n ~
                   onto which peg it put\n ~
                   and the resulting distribution of disks\n\n~
                   You will be asked for the following details:\n\n ~
                    mode: capital letter: S for short, L for long and C for circular.\n ~
                    height: number of disks (can be greater than 9) .\n ~
                    move: move number, starting from 1.\n ~
                    from: starting peg 1, 2 or 3.\n ~
                    onto: destination-peg 1, 2 or 3, but t≠f.\n\n~
                   The move can be any expression for a positive exact integer\n~
                   number not greater than allowed for the mode and height.\n\n ~
                    For mode S: (<= 1 move (sub1 (expt 2 height))) \n ~
                    For mode L: (<= 1 move (sub1 (expt 3 height))) \n ~
                    For mode C: (<= 1 move (expt 3 height))")
                " Do not show this message next time."
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
                  (if first? "" "Wrong data, try again or cancel.\n")
                  "Give mode, height, move, from-disk and onto-disk\n")
                #f
                "")))
          (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
          (when str
            (validate-compute str)
            (cond
              ((eq? SLC 'not-ok) (loop #f))
              (else
                (define-values (d ff tt distr)
                  ((case SLC
                     ((S) calculate-short)
                     ((L) calculate-long)
                     ((C) calculate-circular))
                   h m (sub1 f) (sub1 t)))
                (define distr-str
                  (apply string-append
                    (for/fold
                      ((result '()) #:result (reverse result))
                      ((p (in-list distr)) (n (in-cycle (in-range 0 50))))
                      (if (= n 49)
                        (cons "\n" (cons (format "~s" (add1 p)) result))
                        (cons (format "~s" (add1 p)) result)))))
                (time-out #t
                  (message-box str-compute
                    (if (eq? SLC 'C)
                      (format
                        "Results for move ~s of path C with ~s disks from peg ~s~
                         via peg ~s and peg ~s back to peg ~s~n~n~
                         Disk ~s from peg ~s to peg ~s.~n~
                         Resulting distribution:~n~
                         Positions of disks in order of increasing size:~n~a~n"
                        m h f t (- 6 f t) f (add1 d) (add1 ff) (add1 tt) distr-str)
                      (format
                        "Results for move ~s of path ~a from peg ~s to peg ~s with ~s disks.~n~n~
                         Disk ~s from peg ~s onto peg ~s.~n~
                         Resulting distribution:~n~
                         Positions of disks in order of increasing size:~n~a~n"
                        m SLC f t h (add1 d) (add1 ff) (add1 tt) distr-str))
                    #f
                    '(ok no-icon))))))))
      (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
      (enable/disable-all-buttons 'enable))

    ;=================================================================================================
    ; Action compute short.
    
    (define (calculate-short h m f t)
      (define (exp2 n) (expt 2 n))
      (define (mod2 n) (modulo n 2))
      (define (mod3 n) (modulo n 3))
      (define (pari n) (add1 (mod2 (add1 n))))
      (define (rotd h d f t) (mod3 (* (- t f) (pari (- h d)))))
      (define (rotr h f t) (rotd h 0 t f))
      (define (mcnt m d) (quotient (+ m (exp2 d)) (exp2 (add1 d))))
      (define (thrd m h f t) (mod3 (+ f (* m (rotr h f t)))))
      (define (onto m h f t) (mod3 (- (thrd m h f t) (rotd h (disk m) f t))))
      (define (from m h f t) (mod3 (+ (thrd m h f t) (rotd h (disk m) f t))))
      (define (posi m h d f t) (mod3 (+ f (* (rotd h d f t) (mcnt m d)))))
      (define (disk m) (sub1 (integer-length (bitwise-xor m (sub1 m)))))
      (values
        (disk m)
        (from m h f t)
        (onto m h f t)
        (for/list ((p (in-range h))) (posi m h p f t))))

    ;=================================================================================================
    ; Action compute long.
    
    (define (calculate-long h m f t)
      (define (exp3 n) (expt 3 n))
      (define (mod3 n) (modulo n 3))
      (define (mod4 n) (modulo n 4))
      (define (thrd m f t) (if (odd? m) t f))
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

    ;=================================================================================================
    ; Action compute circular.
    
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

    ;=================================================================================================
    ; Keeping record of the number of moves and the real CPU time used so far for them.
    
    (define (reset-time-and-move-counter)
      (clear-msg)
      (set! clock (current-inexact-milliseconds))
      (set! move-count -1)
      (set! msg-str "")
      (draw-count-msg))

    (define (clear-msg) (remove-string pos-msg msg-str))

    (define (draw-count-msg)
      (clear-msg)
      (set! move-count (add1 move-count))
      (set! msg-str
        (if (eq? delay click)
          (format "Move count: ~s" move-count)
          (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
      (paint-string pos-msg msg-str))

    (define (watch-clock)
      (~r #:precision (list '= 3) (/ (- (current-inexact-milliseconds) clock) 1000)))

    ;=================================================================================================
    ; Some additional drawing procedures.
    
    (define (draw-pegs)
      (for ((p (in-range 3)))
        (paint-solid-rectangle
          (make-posn (peg-x p) peg-y)
          peg-width peg-height green)))

    (define (remove-all-disks)
      (remove-solid-rectangle
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
      (paint-solid-rectangle pos width disk-height color)
      (paint-rectangle pos width disk-height white)
      (paint-string (make-posn (- (peg-x p) 2) (+ y block -3)) (format "~s" (add1 d)) white))

    (define (mark-disk d h p) (draw-disk d h p red))

    (define (remove-disk d h p)
      (define width (disk-width d))
      (define center (+ (peg-x p) (/ peg-width 2)))
      (define x (- center (/ width 2)))
      (define y (- vp-height border block (* (add1 h) disk-height)))
      (define pos (make-posn x y))
      (remove-solid-rectangle pos width disk-height)
      (paint-solid-rectangle
        (make-posn (- center (/ peg-width 2)) y) peg-width disk-height green))

    ;=================================================================================================
    ; Now define the buttons. Constructor make-button draws them too. Contents are mutable, but the
    ; initial contents always are the same, with exception of that of button-idle, whose initial value
    ; is taken from parameter idle-limit.

    (define button-height  (make-button 'height       pos-height max-height  ))
    (define button-mode    (make-button 'mode         pos-mode   'manual     ))
    (define button-delay   (make-button 'delay        pos-delay  click       ))
    (define button-idle    (make-button '|idle limit| pos-idle   (idle-limit)))
    (define button-reset   (make-button 'reset        pos-reset              ))
    (define button-setup   (make-button 'setup        pos-setup              ))
    (define button-quit    (make-button 'quit         pos-quit               ))
    (define button-peg1    (make-button '|peg 1|      pos-peg1               ))
    (define button-peg2    (make-button '|peg 2|      pos-peg2               ))
    (define button-peg3    (make-button '|peg 3|      pos-peg3               ))
    (define button-compute (make-button 'compute      pos-compute            ))

    ;=================================================================================================
    ; Some variables may be have been mutated in earlier calls to procedure play.
    ; Procedure initialize restores the original values.

    (define (initialize)
      (set! height max-height)
      (set! delay click)
      (set! msg-str "")
      (set! clock 0)
      (set! move-count 0)
      (set! manual-count 0)
      (set! allow-intro #t)
      (paint-solid-rectangle girder-pos (- vp-width (* 2 border)) block gray)
      (for ((p (in-range 0 3)))
        (define str (format "Peg ~s" (add1 p)))
        (define size (car ((get-string-size viewport) str)))
        (paint-string
          (add-posn (make-posn (peg-x p) (- vp-height border))
            (- (/ size 2))
            (- (/ str-offset 2))) str white))
      (action-reset-help))

    ;=================================================================================================
    ; The main procedure.

    (define (close)
      (close-viewport viewport)
      (close-graphics))

    (define (main) ; When calling itself recursively, always in tail position of itself.
      (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
      (dispatch pos))

    (dynamic-wind
      void
      (λ () (initialize) (main))
      close)))

;=====================================================================================================
; The end
