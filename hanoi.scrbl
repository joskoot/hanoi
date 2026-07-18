#lang scribble/manual
@;----------------------------------------------------------------------------------------------------
@(require
   scribble/core
   scribble/eval
   racket
   ; "hanoi.rkt"
   (for-label
     "hanoi.rkt"
     racket
     (only-in typed/racket Setof Natural Sequenceof Index))
   (for-syntax racket))

@(define-for-syntax local #f)

@(define-syntax (Defmodule stx)
   (if local
     #'(defmodule "hanoi.rkt" #:packages ())
     #'(defmodule hanoi/hanoi #:packages ())))

@(define lb linebreak)

@title[#:version ""]{Tower of Hanoi}
@author{Jacob J. A. Koot}

@(Defmodule)

@section{Introduction}

The Tower of Hanoi is a game.
It has 3 piles, one at the left, one in the middle and one at the right.
It has a number of disks. Let @tt{h} be the number of disks.
All disks have different sizes and a hole in the center.
They are slipped on the piles.
A disk never rests upon a smaller disk.
Initially all disks are on the pile at the left,
forming a conical tower with the disks in order of decreasing size from bottom to top.
The goal of the game is to move all disks to the pile at the right by making successive moves.
During a move the top disk of a (non-empty) pile is taken and slipped on top onto another pile,
but it is not allowed to move a disk upon a smaller one.
Hence a disk never rests upon a smaller one.
The distributions of disks among the pieces can be taken as the vertices of a connected graph
with the moves as bidirectional edges of length one.
Connectivity means that there is a path between every two vertices.
The graph resembles a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}.
The least number of moves required is @racket[(sub1 (expt h 2))] with uniquely defined
sequence of moves. This is the short mode.
The largest number of moves without passing a distribution of disks among the piles more than once
is @racket[(sub1 (expr 3 h))], implying that every feasible distribution is visited exactly once.
This is the long mode and is uniquely defined too.
Another interesting way is the circular mode, moving the disks such as to visit all feasible
distributions of disks among the piles exactly once
and finishing with all disks on the starting pile at the left.
This takes @racket[(expt h 3)] moves. The circular mode is uniquely defined too when disregarding
the fact that the path of moves can be followed in opposit direction too.

@section{How to play}

@defproc[(play) void?]{
 Opens a GUI for playing the game of the
 @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi}.
 The following buttons are available:}

@elemtag["height" ""]
@bold{@tt{Height}}@(lb)
Opens a modal dialog for selection of the desired number of disks,
at least one, at most nine.
Initially the height is 9.

@elemtag["Mode" ""]
@bold{@tt{Mode}}@(lb)
Opens a modal dialog for selection of the mode, which is manual, short, long or circular.
Initially the mode is manual.

In manual mode the user is supposed to click near the pile the disk is to be taken from
followed by a click near the pile of destination.
An attempt to make an illegal moves is ignored.

In short mode the disks are moved by the GUI to the pile at the right
with the least possible number of moves,
at most @nonbreaking{@tt{(@racket[sub1] (@racket[expt 2] @elemref["height"]{height}))}} moves.

When the long mode is selected, first all disks are placed on the pile at the left and
subsequently moved by the GUI to the pile at the right with the largest number of moves possible
without passing any distribution of disks more than once.
@nonbreaking{@tt{(@racket[sub1] (@racket[expt 3] @elemref["height"]{height}))}} moves.
In fact every feasible distribution of disks is visited.

When the circular mode is selected, first all disks are placed on the pile at the left.
Subsequently the GUI makes moves such as to pass exactly once along
every feasible distribution of disks and finishing with all disks at the pile started from.
@nonbreaking{@tt{(@racket[expt 3] @elemref["height"]{height})}} moves.

The short, long and circular mode can be halted by clicking the
@elemref["Reset"]{reset} or @elemref["Quit"]{quit} button.

@elemtag["Speed" ""]
@bold{@tt{Speed}}@(lb)
The speed is either @tt{click} or a positive real number written with not more than 6 characters.
It applies to @elemref["Mode"]{modes} short, long and circular.
If it is @tt{click} the GUI makes a move after a click near a pile.
If it is a positive real number, the GUI makes about speed moves per second.
In fact slightly less, because the speed
only applies to the sleeping time between moves.
and does not take into account the time spent on calculations and graphical rendering.
Enter a fraction for less than one move per second, for example @racket[1/3]
for one move per three seconds.
@nonbreaking{A speed} greater than @racket[999999] is truncated to @racket[999999].
@nonbreaking{A speed} less than 1/10 is increased to 1/10.
 
@elemtag["Reset" ""]
@bold{@tt{Reset}}@(lb)
Puts all disks on the pile at the left.

@elemtag["Setup" ""]
@bold{@tt{Setup}}@(lb)
Removes all disks and subsequently places disks on the piles in a distribution chosen by the user.
Disks are placed in order of decreasing size.
The user is supposed to click near the pile where each next disk is to be placed.
Requires @elemref["height"]{height} such clicks. Click a button to cancel setup.

@elemtag["Quit" ""]
@bold{@tt{Quit}}@(lb)
Closes and terminates the GUI.
The GUI can be closed by means of the close button in the title bar (at the top-right corner),
but procedure @racket[hanoi] may remain running when waiting for a mouseclick
because it may have called procedure
@seclink["Mouse_Operations"
         #:doc '(lib "graphics/scribblings/graphics.scrbl")]{get-mouse-click}.
However, after closing the GUI window, no such mouse-click can be made.
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@note{In @other-doc['(lib "graphics/scribblings/graphics.scrbl")]
I have not found a mean to check the state of a viewport.@(lb)
(open, hidden or closed)}

@bold{@larger{@larger{The end}}}
@(collect-garbage)
