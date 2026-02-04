# Game of Life
This is my version of the simulation, coined Conway's Game of Life. This was my final project for the Introduction to Computer Science course at the University of Dallas.
## The Game
The simulation displays a grid of squares, with each square containing its own state: alive or dead.
Each square's state is determined by the adjacent squares or neighbors and four rules. Each square is checked every iteration for all rules.
### The Rules
The following rules determine whether a square is alive or dead.
1. <ins>Underpopulation</ins>- If the square is alive and has fewer than 2 neighbors, then the square dies.
2. <ins>Overpopulation</ins>- If the square is alive and has more than 3 neighbors, then the square dies.
3. <ins>Reproduction</ins>- If the square is dead and has exactly 3 neighbors, then the square comes alive.
4. <ins>Maintain</ins>- If a square is alive and has 2 or 3 neighbors, then the square remains alive.
### Controls
These are the controls to run the simulation.\
**Space**- This toggles the simulation's run state\
**R**- This completely resets the grid to the starting state\
**Left-Click**- This toggles the state of the square\
**Num-1**- Toggles glider mode. Use to place a glider instead of a single square\
**Up-Arrow**- Changes the state of a random square\

More information about the original simulation can be found [here](https://conwaylife.com/).
