# MineSweeper_Game
MineSweeper : Haskell Project 
In this project you are going to implement the AI module for a minesweeper robot in
Haskell. The environment in which the robot operates is an 4 × 4 grid of cells. Initially,
a cell on the grid is either empty, contains the robot, or contains a mine. The robot can
move in all four directions and is able to collect a mine only if it is in the same cell as the
mine. Your program will take as input the initial position of the robot and the positions
of all of the mines. The objective of the AI module is to compute a sequence of actions
that the robot can follow in order to go to all the mines and collect them. Below is an
example grid.
The robot R starts at (3,0) while the mines are at (2,2) and (1,2). One possible generated
sequence of actions by your program is:
["up","right","right","collect","up","collect"]
