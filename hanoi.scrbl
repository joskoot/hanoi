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
 Opens a GUI for playing the game of the Tower of Hanoi.
 The following buttons are available:}

@bold{@tt{Height}}@(lb)
The number of disks, at least one, at most nine.
Clicking the button opens a modal dialog allowing to select the desired number of disks.
Initially the height is 9.

@bold{@tt{Mode}}@(lb)
Opens a modal dialog for selection of the mode, which is manual, short or long.
Initially the mode is manual.
In manual mode the user is supposed to click near the pile the disk is to be taken from
followed by a click near the pile of destination.
In short mode the disks are moved by the GUI
with the least possible number of moves to the pile on the right,
at most @racket[(sub1 (expt 2 height))] moves.
When long mode is selected, first all disks are placed on the pile at the left and
subsequently moved to the pile at the right with the largest number of moves possible
without passing any distribution of disks more than once. @racket[(sub1 (expt height 3))] moves.
The process can be halted by clicking the reset or quit button.

@bold{@tt{Speed}}@(lb)
The speed is either slow, fast or click and applies to modes short and long.
If it is slow, about one move per second is made.
If it is fast, disks are moved at fast speed.
If it is click, the GUI makes a move after a click near a pile.
 
@bold{@tt{Reset}}@(lb)
Puts all disks one the pile at the left.

@bold{@tt{Setup}}@(lb)
Vacates the piles and places disks in a distribution chosen by the user.
Disks are placed in order of decreasing size.
The user is supposed to click near the pile where each next disk is to be placed.
Requires height such clicks. Click a button to cancel setup.

@bold{@tt{Quit}}@(lb)
Closes and terminates the GUI.

@bold{@larger{@larger{The end}}}
@(collect-garbage)
