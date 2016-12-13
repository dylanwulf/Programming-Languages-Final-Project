Dylan Wulf
David Shull
Fernando Faria

Konane Game implementation

================ Overview ================

In this project, we implemented a program to play the game of Konane
(Hawaiian Checkers) using the assigned language, Scheme. The game is typically
played on an 8 × 8 board of light and dark pieces as shown (using X for dark
and O for light).

To have a meaningful implementation, our program used the Minimax search
algorithm with Alpha-Beta pruning. For each game, our program also provides the
following information:

  1. The number of times a static evaluation was done.
  2. The average branching factor.
  3. The number of cut offs that took place.

================ How to Run ================

To run this project, open the file "PL Final.rkt" using DrRacket. Next, press
the keys Ctrl+R on a Windows/Linux machine and Cmd+R on an OS X machine. Type in
"Yes" if the computer is going first and "No" if the computer is going second.
Next, enter moves into the prompt and view the computer's response. Continue to
do this until either player has won.

================ Input Format ================

To enter pieces to be removed at game start, enter the x and y coordinates of
the piece to be removed. To enter moves, type in first the x coordinate, then
the y coordinate of the piece that is moving, then the x and y coordinate of
where it should be going.

================ Bugs ================

There are no bugs in our program. All errors that we could think of were
handled, and the game itself plays perfectly with correct input.

================ Data Structures ================

For this project, we used Scheme lists and Scheme vectors (similar to arrays in
C-based languages). The Scheme lists are what everything in Scheme is composed
of, and we used them wherever car and cdr functionality would be helpful. The
vectors made it easy to select various coordinates on the game board, because
they are indexed by values.

================ Algorithms ================

We used a minimax algorithm, which tries to find the best possible move by
comparing all of the possible outcomes 3 moves out. It uses a static evaluation
function which returns a ratio of the number of computer player moveable pieces
by the number of opponent's moveable pieces. Our research indicated that this
evaluation function yielded the highest chance of success. Finally, the
Alpha-Beta pruning algorithm determined whether it was possible for a branch's
minimum node to be higher than an already-explored branch's minimum. If not,
then the branch can be 'pruned' - no longer searched.
