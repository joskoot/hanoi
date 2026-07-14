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

@defproc[(hanoi) void?]{
 Opens a GUI for playing the game of the
 @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi}.
 The following buttons are available:}

@bold{@tt{Height}}@(lb)
The number of disks, at least one, at most nine.
Clicking the button opens a modal dialog allowing to select the desired number of disks.
Initially the height is 9.

@bold{@tt{Mode}}@(lb)
Opens a modal dialog for selection of the mode, which is manual, short, long or hamilton.
Initially the mode is manual.

In manual mode the user is supposed to click near the pile the disk is to be taken from
followed by a click near the pile of destination.

In short mode the disks are moved by the GUI
with the least possible number of moves to the pile on the right,
at most @racket[(sub1 (expt 2 height))] moves.

When long mode is selected, first all disks are placed on the pile at the left and
subsequently moved to the pile at the right with the largest number of moves possible
without passing any distribution of disks more than once. @racket[(sub1 (expt height 3))] moves.
In fact every feasible distribution of disks is visited.

When hamilton mode is selected, first all disks are placed on the pile at the left
and @nonbreaking{@racket[(expt 3 height)]} moves are made such as to pass exactly once along every
feasible distribution of disks and finishing with all disks at the pile started from.

The short, long and hamilton mode can be halted by clicking the reset or quit button.

@bold{@tt{Speed}}@(lb)
The speed is either "click" or a positive real number written with not more than 7 characters.
It applies to modes short, long and hamilton.
If it is click, the GUI makes a move after a click near a pile.
If it is a positive real number, the GUI makes about @tt{speed} moves per second
(in fact somewhat less).
Enter a fraction for less than one move per second, for example @racket[1/3]
for one move per three seconds.
A speed greater than @racket[9999999] is truncated to @racket[9999999].
A speed less than 1/10 is increased to 1/10.
 
@bold{@tt{Reset}}@(lb)
Puts all disks on the pile at the left.

@bold{@tt{Setup}}@(lb)
Removes all disks and subsequently places disks on the piles in a distribution chosen by the user.
Disks are placed in order of decreasing size.
The user is supposed to click near the pile where each next disk is to be placed.
Requires ‘height’ such clicks. Click a button to cancel setup.

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
