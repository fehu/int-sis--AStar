### Todo:

- [ ] reimplement `SlidingPuzzleInstance` as sparse (with a map). Compare the performance.
      Reason: indices are used many times, and every time it's with `zipWithIndex`

- [ ] Example_v1 fails at initial = `[[7, 6, 5], [8,  , 4], [1, 2, 3]]`,
                                    `[[7, 6, 5], [1, 2, 3], [8,  , 4]]`,
                                    `[[ , 2, 8], [1, 5, 4], [3, 6, 7]]`

      Works for: `[[7, 5, 6], [1, 8, 3], [4,  , 2]]` (1900+ calls) 
                 `[[2,  , 5], [4, 3, 7], [6, 8, 1]]` (300+ calls)

- [x] Should have an option to return full solution search history on both success and error (for analysis)
- [ ] maximum history size configuration
- [x] history for LimitedHorizon

- [x] Should provide a description for generated `transformations`

- [x] Visualize solutions tree:
    - [x] for a result
    - [ ] during execution (?)
    - [x] optimize drawing
    - [x] distinguish different A* runs in case of LimitedHorizon
    - [x] reset _order_ numeration each _run_
    - [x] mark solution
    - [x] show pruned

- [ ] Write Tests for:
    - [x] instance operations
    - [ ] heuristics
    - [ ] solver (with hardcoded initial)

- [x] examples generation for heuristics testing

- [x] create generic puzzle solver with LimitedHorizon and BeamSearch

#### GUI

- [x] update config when a solver is chosen
- [ ] block buttons until a solver is chosen
- [x] show status
- [ ] block interface duting execution
- [ ] fix layout
- [x] show solution (out of the tree)
- [ ] heuristics constructor