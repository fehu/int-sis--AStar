### Todo:

- [ ] reimplement `SlidingPuzzleInstance` as sparse (with a map). Compare the performance.
      Reason: indices are used many times, and every time it's with `zipWithIndex`

- [ ] Example_v1 fails at initial = List(List(7, 6, 5), List(8, None, 4), List(1, 2, 3))
                                    List(List(7, 6, 5), List(1, 2, 3), List(8, None, 4))
      Works for: List(List(7, 5, 6), List(1, 8, 3), List(4, None, 2)) (1900+ calls) 
                 [[2,  , 5], [4, 3, 7], [6, 8, 1]] (300+ calls)

- [ ] Should have an option to return full solution search history on both success and error (for analysis)

- [ ] Should provide a description for generated `transformations`

- [ ] Visualize solutions tree:
    - [ ] for a result
    - [ ] during execution (?)

- [ ] Write Tests for:
    - [ ] instance operations
    - [ ] heuristics
    - [ ] solver (with hardcoded initial)