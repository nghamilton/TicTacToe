TicTacToe
=========

"The goal is to write an API for the tic-tac-toe game. An API user, should be able to play a game of tic-tac-toe using this API, but importantly, it should be impossible for the API user to break the rules of the game. Specifically, if an attempt is made to break a rule, the API should reject the program. This is often done by way of a *compile-time type error*."

* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is empty or in-play. Calling `move` on a game board that is finished is a *compile-time type error*. e.g. move (p1 X) (new O) ... p1 for position 1, new creates a new board.

* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling `whoWon` on a game board that is empty or in-play is a *compile-time type error*. As an optional consideration, `whoWon` should never be a draw if fewer than nine moves have been played. In the case that the game is completed, but fewer than nine moves have been played, return a value that can only be one of two possibilities (the winner) and never a draw.

More info at https://github.com/NICTA/course/blob/master/projects/TicTacToe/
