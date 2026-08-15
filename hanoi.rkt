
;=====================================================================================================
;
; A GUI playing the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has clickable buttons Height, Mode, Delay, Reset, Setup, Quit, Peg1, Peg2, Peg3, Idle-
; limit and Compute. A click on such button initiates an action. During an action some buttons may be
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
  (dynamic-wind
    void
    (λ () (let/ec ec (initialize ec) (main)))
    close))

(define (close)
  (close-viewport vp)
  (close-graphics))

(define (main)
  ; At this point the GUI always is in manual mode,
  ; Each action, Quit excepted, returns to manual mode.
  ; The Quit button can be used to exit from the GUI,
  ; but sometimes it just terminates the current action.
  (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
  (dispatch-all pos))

(define (dispatch-all pos)
  (dispatch-button pos
    (button-height (do-height  ) (main))
    (button-mode   (do-mode    ) (main))
    (button-delay  (do-delay   ) (main))
    (button-idle   (do-idle    ) (main))
    (button-reset  (do-reset   ) (main))
    (button-setup  (do-setup   ) (main))
    (button-peg1   (do-manual 0) (main))
    (button-peg2   (do-manual 1) (main))
    (button-peg3   (do-manual 2) (main))
    (button-comp   (do-comp    ) (main))
    (button-quit   (quit-exit       )       )
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

(define (enable/disable-buttons buttons enable/disable)
  (for ((button (in-list buttons))) (button enable/disable)))

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
      button-peg1
      button-peg2
      button-peg3
      button-comp))
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
          (raise-user-error 'idle-limit
            "~n  positive exact real number expected, at least ~s. Given ~s" min-idle-minutes t))))
    'idle-limit))

(define (abort)
  (define abort-msg
    (format
      (string-append
        "~n  No activity during ~s minutes. Game aborted."
        "~n  Use parameter idle-limit to increase the allowed idle limit"
        "~n  or use the Idle limit button.~n~n") (idle-limit)))
  (fprintf (current-error-port) (string-append  "~nTower of Hanoi~n" abort-msg))
  (message-box "Tower of Hanoi" abort-msg #f (quote (ok caution no-icon)))
  (quit-exit))

(define (call-with-time-out thunk (warn? #f))
  (when warn? ((draw-string vp) pos-warn str-warn red))
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
      (when warn? ((clear-string vp) pos-warn str-warn))
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
               (define region (make-region pos button-width button-hight))
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
                    ((draw-disabled-button vp) pos name-str))
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

(define str-height " Height "    )
(define str-mode   " Mode "      )
(define str-delay  " Delay "     )
(define str-idle   " Idle limit ")
(define str-reset  " Reset "     )
(define str-setup  " Setup "     )
(define str-quit   " Quit "      )
(define str-undo   " Undo "      )
(define str-manual " manual "    )
(define str-short  " short "     )
(define str-long   " long "      )
(define str-circ   " circular "  )
(define str-comp   " Compute "   )
(define str-warn   " A dialog is waiting. Look for it when you don't see it.")
(define click (quote click))
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

(define-values-block
  (draw-button draw-disabled-button draw-button-content button-width button-hight string-offset)

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
      str-comp
      str-long
      str-circ
      "999999"))

  (define string-offset 4)
  (define *2string-offset (* 2 string-offset))

  (open-graphics)
  (define vp (open-pixmap "string-sizes" 1000 500))

  (define-values (button-width button-hight)
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
    ((draw-solid-rectangle vp) pos button-width button-hight blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-hight (- string-offset))) str white))

  (define ((draw-disabled-button vp) pos str)
    (define x (posn-x pos))
    (define y (posn-y pos))
    ((clear-solid-rectangle vp) pos button-width button-hight)
    ((draw-rectangle vp) pos button-width button-hight blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-hight (- (* 2 string-offset)))) str blue))

  (define ((draw-button-content vp) pos value)
    (define str (if (string? value) value (format "~a" value)))
    (define x (posn-x pos))
    (define y (+ (posn-y pos) button-hight))
    ((clear-solid-rectangle vp) (make-posn x y) button-width button-hight)
    ((draw-rectangle vp) (make-posn x y) button-width button-hight blue)
    ((draw-string vp)
     (make-posn (+ x string-offset) (+ y button-hight (- (* 2 string-offset)))) str blue)))

;=====================================================================================================
; Layout of the GUI. Computation of coordinates and sizes of all elements in the viewport of the GUI.

(define (add-posn pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
(define max-height 9)
(define blok 20)
(define border (* 3 blok))
(define pos-height (make-posn border border))
(define pos-mode   (add-posn pos-height (+ button-width blok) 0))
(define pos-delay  (add-posn pos-mode   (+ button-width blok) 0))
(define pos-idle   (add-posn pos-delay  (+ button-width blok) 0))
(define pos-reset  (add-posn pos-idle   (+ button-width blok) 0))
(define pos-setup  (add-posn pos-reset  (+ button-width blok) 0))
(define pos-quit   (add-posn pos-setup  (+ button-width blok) 0))
(define pos-peg1   (add-posn pos-quit   (+ button-width blok) 0))
(define pos-peg2   (add-posn pos-peg1   (+ button-width blok) 0))
(define pos-peg3   (add-posn pos-peg2   (+ button-width blok) 0))
(define pos-comp   (add-posn pos-peg3   (+ button-width blok) 0))
(define pos-msg    (add-posn pos-comp   (+ button-width blok) (- button-hight string-offset)))
(define pos-warn   (add-posn pos-msg    0                     (+ button-hight string-offset)))
(define disk-height blok)
(define max-tower-height (* max-height disk-height))
(define min-disk-width (* 3 blok))
(define disk-width-incr blok)
(define (disk-width d) (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width (disk-width (sub1 max-height)))
(define peg-top (* 2 blok))
(define peg-width 4)
(define peg-y (* 2 (+ border button-hight)))
(define peg-height (+ peg-top max-tower-height))
(define vp-width (+ (* 3 max-disk-width) (* 2 blok) (* 4 border)))
(define vp-height (+ (* 2 button-hight) (* 3 border) peg-height blok))
(define girder-pos (make-posn border (- vp-height border blok)))

(define region-peg0
  (make-region
    (make-posn
      (+ border blok) (- vp-height border blok max-tower-height))
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
    (button-peg1 (do-manual2 d h p 0))
    (button-peg2 (do-manual2 d h p 1))
    (button-peg3 (do-manual2 d h p 2))
    (else
      (define dest (dispatch-peg pos))
      (cond
        (dest (do-manual2 d h p dest))
        (else
          (draw-disk d h p)
          (reset-manual-count)
          (dispatch-all pos))))))

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

(define (clear-man-count) ((clear-string vp) pos-msg msg-str))

(define (draw-man-count)
  (clear-man-count)
  (set! msg-str (format "Manual moves ~s" manual-count))
  (when (> manual-count 0) ((draw-string vp) pos-msg msg-str)))

(define (reset-manual-count) (clear-man-count) (set! manual-count 0))

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
          str-height
          (string-append
            "How many disks do you want.\n"
            "At least one, at most nine.\n"
            "Enter a decimal digit but not 0")
          #f
          "9"
          '(disallow-invalid)
          #:validate validate-height))
      #t))
  (viewport-flush-input vp)
  (when str
    (define h (- (char->integer (string-ref str 0)) (char->integer #\0)))
    (set! height h)
    (button-height 'put-content h)
    (reset-manual-count)
    (do-reset-help))
  (enable/disable-all-buttons 'enable))

;=====================================================================================================
; Action mode

(define (do-mode)
  (prepare/finish-do-mode 'disable)
  (define modes (list str-short str-long str-circ))
  (define choice
    (call-with-time-out
      (λ ()
        (get-choices-from-user
          str-mode
          "Select a mode\nCancel in order to remain in manual mode."
          modes))
      #t))
  (viewport-flush-input vp)
  (cond
    (choice
      (reset-manual-count)
      (define ch (car choice))
      (define do-mode (vector-ref  (vector short long circular) ch))
      (define mode    (vector-ref #(       short long circular) ch))
      (button-mode 'put-content mode)
      (unless (eq? mode 'short) (do-reset-help))
      (do-mode)
      (finish (symbol->string mode)))
    ((prepare/finish-do-mode 'enable))))

(define (prepare/finish-do-mode enable/disable)
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
        button-comp)
      (if (eq? (button-delay 'get-content) click)
        (list button-peg1 button-peg2 button-peg3)
        (quote ()))))
  (enable/disable-buttons buttons enable/disable))

(define (finish who)
  (call-with-time-out (λ () (message-box who "\n\n\nfinished\n\n\n" #f '(ok))) #t)
  ; Ignore clicks on reset and quit button while the message box is waiting.
  ; Clicks on other buttons already were ignored because they still are disabled.
  (viewport-flush-input vp)
  (prepare/finish-do-mode 'enable)
  (button-mode 'put-content 'manual))

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
          str-delay
          (string-append
            "Enter a non-negative real number for the\n"
            "approximate delay in seconds between moves\n"
            "or leave the default 'click' as it is.\n"
            "Do not enter more than 6 characters")
          #f	
          click-str	
          '(disallow-invalid)	
          #:validate validate-delay))
      #t))
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
  (viewport-flush-input vp))

;=====================================================================================================
; Action reset.

(define (do-reset-help)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks)
  (reset-manual-count)
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
      button-height
      button-mode
      button-delay
      button-quit
      button-setup
      button-idle
      button-comp))
  (reset-manual-count)
  (enable/disable-buttons buttons 'disable)
  (set! msg-str "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string vp) pos-msg msg-str red)
  (do-setup1 (reverse (range height)))
  (enable/disable-buttons buttons 'enable)
  (clear-msg))

(define (do-setup1 disks)
  (unless (null? disks)
    (define d (car disks))
    (define pos (mouse-click-posn (call-with-time-out (λ () (get-mouse-click vp)))))
    (dispatch-button pos
      (button-peg1  (do-setup2 d 0) (do-setup1 (cdr disks)))
      (button-peg2  (do-setup2 d 1) (do-setup1 (cdr disks)))
      (button-peg3  (do-setup2 d 2) (do-setup1 (cdr disks)))
      (button-reset (clear-msg    ) (do-reset-help))
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
; Action idle limit

(define (do-idle)
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
    (call-with-time-out
      (λ ()
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
          #:validate validate-delay))
      #t))
  (when str
    (define minutes (max min-idle-minutes (read (open-input-string str))))
    (idle-limit minutes)
    ((draw-button-content vp) pos-idle minutes))
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
             (< 0 m (case L ((S) (expt 2 h)) ((L) expt3h) ((C) (add1 expt3h))))))
         (values L h m f t))
        (else (catcher #f)))))
  (define answer
    (call-with-time-out
      (λ ()
        (message-box str-comp
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
          (quote (ok-cancel))))
      #t))
  (when (eq? answer 'ok)
    (let loop ((first? #t))
      (define str
        (call-with-time-out
          (λ ()
            (get-text-from-user str-comp
              (string-append
                (if first? "" "Wrong data, try again\n")
                "Give mode, height, move -r from-disk and onto-disk")
              #f
              ""
              (quote ())))
          #t))
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
                 ((L) calc-long )
                 ((C) calc-circ ))
               h m (sub1 f) (sub1 t)))
            (call-with-time-out
              (λ ()
                (message-box str-comp
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
                          ((p (in-list distr)) (n (in-cycle (in-range 0 50))))
                          (if (= n 49)
                            (cons "\n" (cons (format "~s" (add1 p)) result))
                            (cons (format "~s" (add1 p)) result))))))))
              #t))))))
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
      (message-box str-comp "Circular not yet implemented"))
    #t)
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
              (button-reset (do-reset) (exit))
              (button-quit (exit))
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

(define (clear-msg) ((clear-string vp) pos-msg msg-str))

(define (draw-msg)
  (clear-msg)
  (set! move-count (add1 move-count))
  (set! msg-str
    (if (eq? delay click)
      (format "Move count: ~s" move-count)
      (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
  ((draw-string vp) pos-msg msg-str))

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
      (button-reset (do-reset) (exit))
      ( button-quit (exit)))))

;=====================================================================================================
; Initialization. Actions to be taken before the game can start and introduction of variables that
; cannot be defined with their proper values before variable vp is initialized with a viewport.
; Variable vp always needs initialization because the viewport cannot be made before open-graphics is
; called, which is done during initialization. Some variables may need reinitialization when procedure
; play is called more than once because the procedure may mutate them. Notice that syntax make-button
; needs the viewport too. Therefore the buttons are included in the initialization.

(define quit-exit     'yet-to-be-initialized)
(define vp            'yet-to-be-initialized)
(define height        'yet-to-be-initialized)
(define delay         'yet-to-be-initialized)
(define disk-distr    'yet-to-be-initialized)
(define msg-str       'yet-to-be-initialized)
(define clock         'yet-to-be-initialized)
(define move-count    'yet-to-be-initialized)
(define manual-count  'yet-to-be-initialized)
(define button-height 'yet-to-be-initialized)
(define button-mode   'yet-to-be-initialized)
(define button-delay  'yet-to-be-initialized)
(define button-idle   'yet-to-be-initialized)
(define button-reset  'yet-to-be-initialized)
(define button-setup  'yet-to-be-initialized)
(define button-quit   'yet-to-be-initialized)
(define button-peg1   'yet-to-be-initialized)
(define button-peg2   'yet-to-be-initialized)
(define button-peg3   'yet-to-be-initialized)
(define button-comp   'yet-to-be-initialized)

(define (initialize ec)
  ; Store variables not yet initialized.
  ; Also needed for variables they may have been mutated in a previous call to procedure play.
  (set! quit-exit    ec        )
  (set! height       max-height)
  (set! delay        click     )
  (set! msg-str      ""        )
  (set! msg-str      ""        )
  (set! clock        0         )
  (set! move-count   0         )
  (set! manual-count 0         )
  (set! disk-distr (vector (range height) '() '()))
  ; Open graphics and the viewport.
  (open-graphics)
  (set! vp (open-viewport "Tower of Hanoi" vp-width vp-height))
  ; Initalize and draw the buttons.
  (set! button-height (make-button height       pos-height max-height  ))
  (set! button-mode   (make-button mode         pos-mode   'manual     ))
  (set! button-delay  (make-button delay        pos-delay  click       ))
  (set! button-idle   (make-button |idle limit| pos-idle   (idle-limit)))
  (set! button-reset  (make-button reset        pos-reset              ))
  (set! button-setup  (make-button setup        pos-setup              ))
  (set! button-quit   (make-button quit         pos-quit               ))
  (set! button-peg1   (make-button |peg 1|      pos-peg1               ))
  (set! button-peg2   (make-button |peg 2|      pos-peg2               ))
  (set! button-peg3   (make-button |peg 3|      pos-peg3               ))
  (set! button-comp   (make-button compute      pos-comp               ))
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
  ; Procedure do-reset draws the pegs and the initial distribution of disks on the peg at the left.
  (do-reset))

;=====================================================================================================
; The end
