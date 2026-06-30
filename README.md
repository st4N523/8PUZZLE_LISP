# 8-Puzzle Solver

## Overview

This project implements an automatic solver for the classic 8-puzzle problem using two Artificial Intelligence search algorithms in Common Lisp. The solver finds a sequence of moves from an initial puzzle configuration to the goal state while demonstrating the differences between uninformed and informed search strategies.

## Algorithms Implemented

### Breadth-First Search (BFS)

- Explores the search tree level by level.
- Guarantees the shortest solution path when all moves have equal cost.
- Suitable for finding optimal solutions but may require significant memory for large search spaces.

### A* Search

- Uses heuristic evaluation to guide the search toward the goal.
- Combines path cost `g(n)` and heuristic estimate `h(n)` to calculate the evaluation function:

  ```
  f(n) = g(n) + h(n)
  ```

- Significantly reduces the number of explored states compared to BFS while still producing an optimal solution when using an admissible heuristic.

## Features

- Automatic solving of the 8-puzzle problem
- Breadth-First Search (BFS) implementation
- A* Search implementation
- State-space exploration
- Optimal solution generation
- Comparison of uninformed and informed search strategies

## Technologies

- Common Lisp
- Artificial Intelligence
- Search Algorithms
- Graph Search

## Learning Outcomes

- Implemented both uninformed and informed search algorithms.
- Applied graph search techniques to solve state-space problems.
- Understood the performance differences between BFS and A* Search.
- Gained experience with algorithm implementation using Common Lisp.

## Future Improvements

- Support additional heuristic functions (e.g., Manhattan Distance).
- Add Greedy Best-First Search.
- Visualize the solution path.
- Compare execution time and memory usage between algorithms.

## Author

**Kong Lee Jie**
