### Todo:

- [ ] reimplement SlidingPuzzleInstance as sparse (with a map). Compare the performance.
      Reason: indices are used many times, and every time it's with zipWithIndex

- [ ] Example_v1 fails at initial = List(List(7, 6, 5), List(8, None, 4), List(1, 2, 3))

- [ ] Should return solutions tree on both success and error (for analysis)

- [ ] Write Tests for:
    - [ ] instance operations
    - [ ] heuristics
    - [ ] solver (with hardcoded initial)