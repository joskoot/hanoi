#lang scribble/manual
@;----------------------------------------------------------------------------------------------------
@(require
   scribble/core
   scribble/eval
   racket/sandbox
   racket
   ; "hanoi.rkt"
   (for-label
     "hanoi.rkt"
     racket
     (only-in typed/racket Setof Natural Sequenceof Index))
   (for-syntax racket))

@(define-for-syntax local #f)

@(define-syntax-rule
   (Interaction x ...)
   (interaction/no-prompt
     #:eval
     (make-base-eval
       #:pretty-print? #f
       #:lang
       '(begin
          (require racket/base)
          (print-reader-abbreviations #f)
          (define (hex str) (read (open-input-string str)))))
     x ...))

@(define-syntax-rule
   (Interaction* x ...)
   (interaction/no-prompt #:eval evaller x ...))

@(define (make-evaller)
   (make-base-eval
     #:pretty-print? #f
     #:lang
     '(begin
        (require racket)
        (print-reader-abbreviations #f)
        (define (hex str) (read (open-input-string str))))))
@(define-syntax (Defmodule stx)
   (if local
     #'(defmodule "hanoi.rkt" #:packages ())
     #'(defmodule hanoi/hanoi #:packages ())))

@(define (reset-Interaction*) (set! evaller (make-evaller)))

@(define evaller (make-evaller))

@(define nb nonbreaking)

@(define lb linebreak)
@(define(minus) (tt "-"))

@title[#:version ""]{Tower of Hanoi}
@author{Jacob J. A. Koot}

@(Defmodule)

@section[#:style '(unnumbered)]{Introduction}

The Tower of Hanoi is a game.
It has 3 pegs, one at the left, one in the middle and one at the right.
It has a number of disks.
The disks have different sizes and a hole in the center.
They are put on the pegs.
A disk never rests upon a smaller disk.
Initially all disks are on the peg at the left,
forming a conical tower with the disks in order of decreasing size from bottom to top.
The goal of the game is to move all disks to the peg at the right by making successive moves.
A move is made by taking the top disk of a non-empty peg and putting it on top onto another peg
or just putting there if the peg of destination currently has no disks.
However, it is not allowed to put a disk upon a smaller one.
Hence a disk never rests upon a smaller one.

@section[#:style '(unnumbered)]{How to play}

@defproc[(play) void?]{
 Opens a GUI for playing the game of the
 @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi}.
 The user can instruct the GUI which action to take by means of the buttons described below
 and by clicking nearby a peg.
 A button can temporarily be absent when not applicable during the current action.
 For some actions the GUI asks a question in a separate modal dialog.
 Instructions given before the question is answered are ignored.}

@defparam[time-limit time (and/c real? positive?) #:value 60]{
 The GUI waits at most @racket[time] seconds when needing a mouseclick or
 answer to a modal dialog.}

@subsection[#:tag "Height"]{Height}

Opens a modal dialog for selection of the desired number of disks,
at least one, at most nine.
Initially the height is 9.

@subsection[#:tag "Mode" ""]{Mode}

Opens a modal dialog for selection of the mode, which is manual, short, long or circular.
Initially the mode is manual.

In manual mode the user is supposed to click the @seclink["Peg n"]{peg} button
the disk is to be taken from
followed by a click on the @seclink["Peg n"]{peg} button of destination.
In stead of clicking a @seclink["Peg n"]{peg} button one can click nearby the corresponding peg.
The disk selected to be moved is colored red.
The selection can be canceled by clicking nearby the peg were it is
or clicking the corresponding @seclink["Peg n"]{peg} button
or by clicking the @seclink["Quit"]{quit} button.
An attempt to make an illegal move is ignored.

In short mode the disks are moved by the GUI to the peg at the right
with the least possible number of moves,
at most @tt{2@superscript{h}-1} moves, where @tt{h} is the @seclink["Height"]{height}.
Exactly @tt{2@superscript{h}-1} moves when starting with all disks on @seclink["Peg n"]{peg} 1 or 2.
The shortest way always is uniquely defined.

When the long mode is selected, first all disks are placed on the peg at the left and
subsequently moved to the peg at the right with the largest number of moves possible
without passing any distribution of disks more than once.
@tt{3@superscript{h}-1} moves, where @tt{h} is the @seclink["Height"]{height}.
In fact every legal distribution of disks is visited. The sequence of moves is uniquely defined.

When the circular mode is selected, first all disks are placed on the peg at the left and
subsequently moved such as to pass exactly once along
every legal distribution
of disks and finishing with all disks at the peg started from.
@tt{3@superscript{h}} moves, where @tt{h} is the @seclink["Height"]{height}.
The sequence of moves is uniquely defined.

The short, long and circular mode can be aborted by clicking the
@seclink["Reset"]{reset} or @seclink["Quit"]{quit} button,
which make the GUI return to manual mode.

@subsection[#:tag "Delay"]{Delay}

The delay is specified by means of a modal dialog.
It is either @tt{click} or a non-negative real number
written with not more than 6 characters.
It applies to @seclink["Mode"]{modes} short, long and circular.

When the delay is @tt{click} a move is made after each mouseclick
at arbitrary position in the window of the GUI
but not on the @seclink["Reset"]{reset} or @seclink["Quit"]{quit} button,
which terminate the @seclink["Mode"]{modes} short, long and circular
and return to manual @seclink["Mode"]{mode}.

If the delay is a non-negative real number, say d,
the GUI waits d seconds between successive moves.
In fact the delay is d+ε seconds,
ε being the minimum time for a single move,
which is the non-zero real time needed for calculations and graphical rendering and
the real time lost while no processor was evailable for the GUI.
ε depends on your CPU and GPU and the load by other programs. It may be about 1 ms.
 
@subsection[#:tag "Reset"]{Reset}

Puts all disks on the peg at the left.
May cancel a current action in order to return to @seclink["Mode"]{mode} manual.

@subsection[#:tag "Setup"]{Setup}

Removes all disks and subsequently places disks on the pegs in a
distribution chosen by the user.
Disks are placed in order of decreasing size.
The user is supposed to click a @seclink["Peg n"]{peg} button or nearby the corresponding peg
indicating where each next disk is to be placed.
Requires @seclink["Height"]{height} such clicks.
Click the @seclink["Reset"]{reset} button to cancel setup.

@subsection[#:tag "Quit"]{Quit}

Closes and terminates the GUI, but can also be used to cancel an ongoing action.
In that case, the GUI returns to manual mode and
the GUI is neither closed nor terminated.

@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))

@note{The window of the GUI can be closed by means of the close button in the title bar
 (at the top-right corner),
 but procedure @racket[play] probably is not terminated because it may keep waiting for a mouseclick.
 However, after closing the GUI window, no such mouseclick can be made.
 @nb{In @other-doc['(lib "graphics/scribblings/graphics.scrbl")]}
 I have not found a mean to check the state of a viewport.@(lb)
 (open, hidden or closed)}

@subsection[#:tag "Peg n"]{Peg n}

n is 1, 2 or 3.
Used to make moves manually and for @seclink["Setup"]{setup} of a distribution of disks.
The same can be done by clicking nearby the corresponding peg.

@section[#:style '(unnumbered)]{Appendix}

@subsection{Graphical representation}

Let @tt{h} be the number of disks.
@nb{The disks} can be distributed among the pegs in 3@superscript{@tt{h}} ways.
@nb{The distributions} can be taken as the vertices of a connected graph
with the moves as bidirectional edges of length one.
Connectivity means that there is at least one path between every two vertices.
The graph can be drawn such as to resemble a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}.
For example, for 5 disks:

@(hspace 5) @image["hanoi-whole-5.gif" #:scale 0.25]

In contrast to a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}
the vertices of the triangles of a Hanoi graph do not coincide with the vertices of other triangles.
The vertices of two triangles are separated by at least one edge.
The three vertices of the largest triangle represent distributions of all disks on the same peg.
These have two edges only,
the two moving the smallest disk to one of the two empty pegs.
All other vertices of the graph represent distributions with at most one empty peg.
Such a vertex has three edges. Let Pa, Pb and Pc
be the three pegs in increasing order of the size of the disk on top,
regarding an empty peg as having the largest disk on top.
Then there are three edges: Pa→Pb, Pa→Pc and Pb→Pc.
The graph has
@nb{@tt{(3(3@superscript{h}@(minus)3)+3×2)/2)}} =
@nb{@tt{(3@superscript{h+1}@(minus)@smaller{3})/2}}
edges because it has @tt{h}@superscript{3} vertices, of which
@tt{h@superscript{3}@(minus)3} have @nb{3 edges} and 3 vertices have 2 edges only.
The division by 2 is needed because every edge connects 2 vertices.

Consider paths from a distribution of all disks on the same peg
to a distribution with all disks on another peg.
These distributions are represented by the vertices of the largest triangle.
The least number of moves required is @tt{h}@superscript{2}@tt{-}1 with uniquely defined
sequence of moves. This is the short mode, in the graph shown by the sides of the largest triangle.

The largest number of moves without passing a distribution of disks among the pegs more than once
is @tt{h}@superscript{3}@(minus)1, implying that every legal distribution is visited exactly once.
This is the long mode and is uniquely defined too.
For three disks and labeling the vertices such as to show on which pegs the disks are: 

@(hspace 5) @image["long-3.gif" #:scale 0.3]

Another interesting way is the circular mode, moving the disks such as to visit all legal
distributions of disks among the pegs exactly once
and finishing with all disks on the starting peg.
This takes @tt{h}@superscript{3} moves. The circular mode is uniquely defined too when disregarding
the fact that the path can be followed in opposit direction too. For three disks:

@(hspace 5) @image["circular3.gif" #:scale 0.3]

@subsection{Number of paths}

The number of distinct non self-crossing paths from a distribution with all disks on the same peg
to one with all disks on another peg, those not passing all vertices included,
is a(n) with @nb{a(0)=1} and
@nb{a(n+1)=a(n)@superscript{2}+a(n)@superscript{3}}.
This is a very fast increasing sequence.
See @hyperlink["https://oeis.org/A125295"]{A125295} of @hyperlink["https://oeis.org/"]{OEIS}.

@subsection{Non recursive formulas}

Regarding elementary arithmetic operations as non-recursive,
given height @tt{h}, starting peg @tt{f} and destination peg @tt{t},
all information about the @tt{m}@superscript{th} move
along the shortest path from @tt{f} to @tt{t}
and the resulting distribution of disks
can be computed without recursion.
Written in @hyperlink["https://www.scheme.org/"]{Scheme} or
@hyperlink["https://racket-lang.org/"]{Racket}:

@elemtag["example" ""]

@tabular[
 (list
   (list "Legend" @tt{m}
     (element #f
       (list
         "move number, starting from "
         @tt{1}
         ".")))
   (list "" @tt{h}
     (element #f
       (list
         "height of the tower, id est the number of disks. "
         @tt{h}
         ">"
         @tt{0}
         ".")))
   (list "" @tt{f}
     (element #f
       (list
         "starting peg, "
         @tt{0}
         ", "
         @tt{1}
         " or "
         @tt{2}
         ". More convenient than ordinals " @tt{1} ", " @tt{2} " and "@tt{3} ".")))
   (list "" @tt{t}
     (element #f
       (list
         "destination peg, "
         @tt{0}
         ", "
         @tt{1}
         " or "
         @tt{2}
         ", but "
         @tt{t}
         "≠"
         @tt{f} ".")))
   (list "" @tt{d}
     (element #f
       (list
         "disk, "
         @tt{0} "≤"
         @tt{d}"<"
         @tt{h}
         ", in order of size, "
         @tt{0}
         " being the smallest disk."))))
 #:sep (hspace 2)]

Notice that in the formulas the pegs have ordinals @tt{0}, @tt{1} and @tt{2}.
For the formulas this is more convenient than the ordinals @tt{1}, @tt{2} and @tt{3}
as used in the GUI.
The disks are numbered starting from 0 for the smallest one, whereas the GIU starts from 1.

@Interaction*[
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
 (define (disk m        ) (sub1 (integer-length (bitwise-xor m (sub1 m)))))]

@tt{(disk m)} identifies the disk being moved during move @tt{m}
and is the number of times @tt{m} can be divided by @tt{2}.
@tt{from}, @tt{onto} and @tt{thrd} are the
peg the disk is taken from, the peg it is moved to and the remaining third peg.
@tt{posi} computes the position of disk @tt{d} after move @tt{m}.
                                        
@Interaction*[
 (time
   (begin
     (define h 80)
     (define N-Avogadro #e6.02214076e23)
     (define f 0)
     (define t 2)
     (printf "Length of the whole path: ~s~n" (sub1 (expt 2 h)))
     (printf "Move     : ~s~n"       N-Avogadro)
     (printf "Disk     : ~s~n" (disk N-Avogadro))
     (printf "From peg : ~s~n" (from N-Avogadro h f t))
     (printf "Onto peg : ~s~n" (onto N-Avogadro h f t))
     (printf "Thrd peg : ~s~n" (thrd N-Avogadro h f t))
     (printf "Positions of the disks in the resulting distribution~n")
     (printf "of disks in increasing order their sizes~n")
     (for ((d (in-range 80)))
       (when (= d 40) (newline))
       (printf "~s" (posi N-Avogadro h d f t)))
     (newline)
     (printf "~nTimes in ms: ")))]

@(reset-Interaction*)

Similar formulas exist for the longest non-selfcrossing path from @tt{f} to @tt{t}:

@Interaction*[
 (define (exp3 n) (expt   3 n))
 (define (mod3 n) (modulo n 3))
 (define (mod4 n) (modulo n 4))
 (define (thrd m   f t) (if (odd? m) t f))
 (define (onto m h f t) (posi m (disk m) f t))
 (define (from m h f t) (- 3 (onto m h f t) (thrd m f t)))
 (define (disk m) (if (zero? (mod3 m)) (add1 (disk (quotient m 3))) 0))
 (code:comment " ")
 (code:comment "(disk m) is recursive. There is no simple way to make it non-recursive.")
 (code:comment " ")
 (define (posi m d f t)
   (case (mod4 (mcnt m d))
     ((0) f)
     ((1 3) (- 3 f t))
     ((2) t)))
 (code:comment " ")
 (define (mcnt m d)
   (+
     (* 2  (quotient m (exp3 (add1 d))))
     (mod3 (quotient m (exp3 d)))))]

The following example resembles the one for the shortest path,
but the differences are such that making a procedure that can handle both examples
would make the code less easy to read.

@Interaction*[
 (time
   (begin
     (define h 80)
     (define N-Avogadro #e6.02214076e23)
     (define f 0)
     (define t 2)
     (printf "Length of the whole path: ~s~n" (sub1 (expt 3 h)))
     (printf "Move     : ~s~n"       N-Avogadro)
     (printf "Disk     : ~s~n" (disk N-Avogadro))
     (printf "From peg : ~s~n" (from N-Avogadro h f t))
     (printf "Onto peg : ~s~n" (onto N-Avogadro h f t))
     (printf "Thrd peg : ~s~n" (thrd N-Avogadro   f t))
     (printf "Positions of the disks in the resulting distribution~n")
     (printf "of disks in increasing order their sizes~n")
     (for ((d (in-range 80)))
       (when (= d 40) (newline))
       (printf "~s" (posi N-Avogadro d f t)))
     (printf "~nTimes in ms: ")))]

The formulas are not used by procedure @racket[play].
When walking a whole path, recursion is faster because it can use information about
what happened during previous moves,
but when wanting the information for one move only, for example
move 6.02214076×10@superscript{23} (Avogadro's number) for a tower of 80 disks,
the formulas produce results instantaneously without the need to pass along previous moves.
See the @elemref["example"]{examples above}.

@subsection{Counting distributions}

Let h be the number of disks, h≥3.
A distribution of these h disks can have one or two empty pegs.
Obviously there are three distributions with two empty pegs.
There are three ways to select two pegs out of three
and 2@superscript{h} ways to distribute the disks among
these two pegs. However, two of these distributions leave the other peg empty.
Hence given a selection of the two pegs we find @nb{2@superscript{h}@(minus)2} distributions and
because the two pegs can be chosen in three ways, we find a total of @nb{3(2@superscript{h}@(minus)2)}
distributions with one peg empty.
@nb{The total} number of distributions is 3@superscript{h}. Hence there are
@nb{3@superscript{h}@(minus)3(2@superscript{h}@(minus)2)@(minus)3} distributions without empty peg.
Contemplating separately all distributions for h=1 and h=2,
the formulas appear to be correct for these two cases too,
although this does not follow from the above deduction.

@bold{@larger{@larger{The end}}}
@(collect-garbage)