# Fortran 2018 – Grammar Audit (status: in progress)

This document summarizes **Fortran 2018** grammar support using:

- `grammars/Fortran2018Lexer.g4`, `grammars/Fortran2018Parser.g4`
- Tests in `tests/Fortran2018/`
- References to closed issues for teams, events, collectives and
  SELECT RANK

## 1. Implemented and tested areas

From `tests/Fortran2018`:

- Basic F2018 parser existence and inheritance from F2008:
  - `test_f2018_lexer_parser_exists`
  - `test_basic_module_parsing_works`
  - `test_f2018_grammar_inheritance` (F2008 coarray features in F2018).
- Comparison with F2008 parsing on the same code.
- Complex program structure:
  - `complex_program.f90` ensures that non-trivial F2018 code parses
    without syntax errors.
- Teams, events, collectives and related features:
  - `test_issue61_teams_and_collectives.py` and fixtures like
    `coarray_test_module.f90`.
- Assumed-rank dummy arguments:
  - `assumed_rank_dummy.f90`.
- DO CONCURRENT with locality and mask:
  - `do_concurrent_locality.f90` (mirrored in the top-level
    `do_concurrent_locality.mod` file).

The tests are deliberately “honest status” checks that assert both
basic parsing success and error counts for representative F2018
constructs.

## 2. Experimental F2018-specific tokens

Additional tests attempt to use F2018-looking syntax, including:

- `co_sum`, `select rank`, `event post`, `form team`, etc., inside
  subroutines.
- The test suite prints which of these currently parse without errors
  and which do not, documenting the line between implemented and
  unimplemented F2018 constructs.

## 3. Error recovery and robustness

The tests include:

- An invalid module (`invalid_bad_syntax_module.f90`) to check error
  detection and recovery.
- A simple coverage assessment that counts how many basic program forms
  (module, program, subroutine) parse cleanly.

## 4. Summary

The Fortran 2018 grammar:

- Inherits F2008 features and adds core 2018 constructs such as
  teams, events, SELECT RANK and assumed-rank dummies.
- Has targeted tests for DO CONCURRENT locality and coarray extensions.
- Uses an “honest status” test philosophy to document which
  F2018-specific features are currently accepted.

Future work should:

- Expand the test matrix for teams/events/collectives and SELECT RANK.
- Use this audit and the tests to keep track of any remaining F2018
  syntax not yet implemented.

