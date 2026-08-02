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
@(define(minus) (tt "-"))

@title[#:version ""]{Tower of Hanoi}
@author{Jacob J. A. Koot}

@(Defmodule)

@section{Introduction}

The Tower of Hanoi is a game.
It has 3 piles, one at the left, one in the middle and one at the right.
It has a number of disks. Let @tt{h} be this number.
The disks have different sizes and a hole in the center.
They are put on the piles.
A disk never rests upon a smaller disk.
Initially all disks are on the pile at the left,
forming a conical tower with the disks in order of decreasing size from bottom to top.
The goal of the game is to move all disks to the pile at the right by making successive moves.
A move is made by taking the top disk of a (non-empty) pile and putting it on top onto another pile
or just putting there if the pile of destination currently has no disks.
However, it is not allowed to put a disk upon a smaller one.
Hence a disk never rests upon a smaller one.

The distributions of disks among the piles can be taken as the vertices of a connected graph
with the moves as bidirectional edges of length one.
Connectivity means that there is at least one path between every two vertices.
The graph has
@nonbreaking{@tt{(3(3@superscript{h}@(minus)3)+3×2)/2)}} =
@nonbreaking{@tt{(3@superscript{h+1}@(minus)@smaller{3})/2}}
edges because it has @tt{h}@superscript{3} vertices, of which
@tt{h@superscript{3}@(minus)3} have 3 edges and 3 vertices have 2 edges only.
The division by 2 is needed because every edge connects 2 vertices.
The graph resembles a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}.
For example, for 5 disks the graph is:

@(hspace 5) @image["hanoi-whole-5.gif" #:scale 0.25]

In contrast to a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}
the vertices of the triangles of a Hanoi graph do not coincide with the vertices of other triangles.
The vertices of two triangles are separated by at least one edge. The sides of the largest triangle
correspond to the shortest path of moving all disks from a pile to another pile.
The least number of moves required is @tt{h}@superscript{2}@tt{-}1 with uniquely defined
sequence of moves. This is the short mode.
The largest number of moves without passing a distribution of disks among the piles more than once
is @tt{h}@superscript{3}@(minus)1, implying that every feasible distribution is visited exactly once.
This is the long mode and is uniquely defined too.
Another interesting way is the circular mode, moving the disks such as to visit all feasible
distributions of disks among the piles exactly once
and finishing with all disks on the starting pile.
This takes @tt{h}@superscript{3} moves. The circular mode is uniquely defined too when disregarding
the fact that the path of moves can be followed in opposit direction too. The number of
distinct non self-crossing paths from one pile to another one is a(n) with a(0) = 1 and
@nonbreaking{a(n+1)=a(n)@superscript{2}+a(n)@superscript{3}}.
This is a very fast increasing sequence.
See sequence @hyperlink["https://oeis.org/A125295"]{A125295} of @hyperlink["https://oeis.org/"]{OEIS}.

@section{How to play}

@defproc[(play) void?]{
 Opens a GUI for playing the game of the
 @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi}.
 The GUI shows buttons which are described below.
 A button can temporarily be absent when not allowed by the current action instructed by the user.}

@elemtag["Height" ""]
@bold{@tt{Height}}@(lb)
Opens a modal dialog for selection of the desired number of disks,
at least one, at most nine.
Initially the height is 9.

@elemtag["Mode" ""]
@bold{@tt{Mode}}@(lb)
Opens a modal dialog for selection of the mode, which is manual, short, long or circular.
Initially the mode is manual.

In manual mode the user is supposed to click the @elemref["Pile n"]{pile button}
the disk is to be taken from
followed by a click on the @elemref["Pile n"]{pile button} of destination.
An attempt to make an illegal move is ignored.

In short mode the disks are moved by the GUI to the pile at the right
with the least possible number of moves,
at most @nonbreaking{@tt{(@racket[sub1] (@racket[expt 2] @elemref["Height"]{height}))}} moves.

When the long mode is selected, first all disks are placed on the pile at the left and
subsequently moved to the pile at the right with the largest number of moves possible
without passing any distribution of disks more than once.
@nonbreaking{@tt{(@racket[sub1] (@racket[expt 3] @elemref["Height"]{height}))}} moves.
In fact every feasible distribution of disks is visited.

When the circular mode is selected, first all disks are placed on the pile at the left and
subsequently moved such as to pass exactly once along
every feasible distribution of disks and finishing with all disks at the pile started from.
@nonbreaking{@tt{(@racket[expt 3] @elemref["Height"]{height})}} moves.

The short, long and circular mode can be aborted by clicking the
@elemref["Reset"]{reset} or @elemref["Quit"]{quit} button,
which make the GUI return to manual mode.

@elemtag["Delay" ""]
@bold{@tt{Delay}}@(lb)
The delay is specified by means of a modal dialog.
It is either @tt{click} or a non-negative real number
written with not more than 6 characters.
It applies to @elemref["Mode"]{modes} short, long and circular.

When the delay is @tt{click} a move is made after each mouse click
at arbitrary position in the window of the GUI
but not on the @elemref["Reset"]{reset} or @elemref["Quit"]{quit} button,
which terminate the @elemref["Mode"]{modes} short, long and circular
and return to manual @elemref["Mode"]{mode}.

If the delay is a non-negative real number, say d,
the GUI waits d seconds between successive moves.
In fact the GUI makes 1/(d+ε) moves per second,
ε being the minimum time for a single move,
which is not zero because the delay is not corrected for
the real time lost on calculations and graphical rendering or
the real time no processor was evailable for the GUI.
This time depends on your CPU and GPU and may be in the order of magnitude of 1 ms per move.
 
@elemtag["Reset" ""]
@bold{@tt{Reset}}@(lb)
Puts all disks on the pile at the left.

@elemtag["Setup" ""]
@bold{@tt{Setup}}@(lb)
Removes all disks and subsequently places disks on the piles in a distribution chosen by the user.
Disks are placed in order of decreasing size.
The user is supposed to click a @elemref["Pile n"]{pile button}
indicating on which pile the next disk is to be placed.
Requires @elemref["Height"]{height} such clicks.
Click the @elemref["Reset"]{reset} or @elemref["Quit"]{quit} button to cancel setup.

@elemtag["Quit" ""]
@bold{@tt{Quit}}@(lb)
Closes and terminates the GUI, but during @elemref["Setup"]{setup}
cancels the setup without closing the GUI.
The GUI window can be closed by means of the close button in the title bar (at the top-right corner),
but procedure @racket[play] may remain running when waiting for a mouseclick
because it may have called procedure
@seclink["Mouse_Operations"
         #:doc '(lib "graphics/scribblings/graphics.scrbl")]{get-mouse-click}.
However, after closing the GUI window, no such mouse-click can be made.
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@note{In @other-doc['(lib "graphics/scribblings/graphics.scrbl")]
I have not found a mean to check the state of a viewport.@(lb)
(open, hidden or closed)}

@elemtag["Pile n" ""]
@bold{@tt{Pile n}}, @tt{n} being 1, 2 or 3.@(lb)
Used to make moves manually and for @elemref["Setup"]{setup} of a distribution of disks.

@bold{@larger{@larger{The end}}}
@(collect-garbage)
