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
  2. The number of cut offs that took place.
  3. The average branching factor. 

================ How to Run ================

To run this project, open the file "PL_Final.rkt" using DrRacket version 6.7. Select the R5RS language
in the list of languages. Next, press the keys Ctrl+R on a Windows/Linux machine 
or Cmd+R on an OS X machine. Type in "Yes" if the computer is going first or
"No" if the computer is going second. Next, enter moves into the prompt and view the 
computer's response. Continue to do this until either player has won.

================ Input Format ================

To enter pieces to be removed at game start, enter the x and y coordinates of
the piece to be removed. To enter moves, type in first the x coordinate, then
the y coordinate of the piece that is moving, then the x and y coordinate of
where it should be going.

================ Bugs ================

All errors that we could think of were handled, and the game itself plays
perfectly with correct input. One issue is that the computer is not able to decide
the very first move by itself. If the computer is going first, the user must input its first
move for it. We were going to have the computer choose a random move for its first move,
but R5RS does not have any sort of random functionality.

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
evaluation function yielded the highest chance of success (see page 13 of http://cs.brynmawr.edu/Theses/Thompson.pdf)
Finally, the Alpha-Beta pruning algorithm determined whether it was possible for a branch's
minimum node to be higher than an already-explored branch's minimum. If not,
then the branch can be 'pruned' - no longer searched.

================ Depth and Alpha-Beta Pruning Results ================
Please see file Plots.pdf for visual graphs of this data

Our results were:

Minimax without Alpha-Beta Pruning

  Depth 1: evaluations: 630, cuts: 0 Avg branch factor: 5.4
  Depth 2: evaluations: 1439, cuts: 0, Avg. branch factor: 5.2
  Depth 3: evaluations: 17108, cuts: 0, Avg. branch factor: 13.3
  Depth 4: evaluations: 193060, cuts: 0, Avg. branch factor: 9.1
  Depth 5: evaluations: 1597450, cuts: 0, Avg. branch factor: 13.4
  Depth 6: evaluations: 21549265, cuts: 0, Avg. branch factor: 9.2

Minimax with Alpha-Beta Pruning

Depth 1:
  Number of evaluations: 176
  Number of cuts: 2
  Average branch factor: 13/5 ~= 2.6
Depth 2:
  Number of evaluations: 698
  Number of cuts: 2
  Average branch factor: 703/194 ~= 3.62
Depth 3:
  Number of evaluations: 5809
  Number of cuts: 695
  Average branch factor: 1730/317 ~= 5.46
Depth 4:
  Number of evaluations: 45155
  Number of cuts: 8779
  Average branch factor: 58934/13977 ~= 4.22
Depth 5:
  Number of evaluations: 864093
  Number of cuts: 92718
  Average branch factor: 1022379/158462 ~= 6.46
Depth 6:
  Number of evaluations: 2972575
  Number of cuts: 917364
  Average branch factor: 4178495/1206184 ~= 3.46

  ================ Analysis of Depth and Alpha-Beta Pruning  ================

By looking at these results, it is very clear that adding Alpha-Beta pruning
has a great impact on the game statistics. First of all, it drastically reduces
the number of static evaluations performed. Next, it decreases the average
branching factor. Finally, it increases the number of cuts, since the Alpha-Beta
pruning is what does the actual cuts. One statistic not measured, but witnessed
by our eyes, was that the game took considerably less time when using pruning.
