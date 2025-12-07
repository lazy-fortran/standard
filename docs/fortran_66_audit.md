# FORTRAN 66 (ANSI X3.9‑1966) – Grammar Audit (status: in progress)

This document summarizes the current **FORTRAN 66** grammar based on:

- `grammars/FORTRAN66Lexer.g4`, `grammars/FORTRAN66Parser.g4`
- `docs/fixed_form_support.md`
- Tests under `tests/FORTRAN66/`
- XPASS fixtures documented in `tests/test_fixture_parsing.py`

It describes implemented behavior and known gaps relative to the
standard.

## 1. Core language and extensions over FORTRAN II

Implemented (per grammar and tests):

- Basic FORTRAN core inherited from earlier grammars (labels, DO,
  GOTO, arithmetic IF, etc.).
- Extended data types (LOGICAL, DOUBLE PRECISION, COMPLEX).
- Declarative statements such as BLOCK DATA, EXTERNAL/INTRINSIC
  (insofar as the grammar tokens exist).

The tests under `tests/FORTRAN66` exercise standard program fragments,
functions, and subroutines.

## 2. Fixed-form model

As with other historical dialects, `docs/fixed_form_support.md` states
that FORTRAN 66:

- Uses a **layout‑lenient** fixed-form model:
  - Labels and statements are parsed by token order, not strict
    column positions.
  - Sequence numbers and column‑6 continuation are not modeled.
- Classic column‑1 `C`/`*` comments and sequence fields are treated as
  historical context, not enforced semantics.

This means the grammar is suitable for syntactic analysis of FORTRAN 66
code written in typical styles, but it does not try to replicate all
card-layout behavior.

## 3. Known limitations and XPASS fixtures

`tests/test_fixture_parsing.py` marks several FORTRAN 66 fixtures as
XPASS, with reasons indicating that they are “more ambitious” than the
current grammar or that full historical coverage is out of scope:

- `FORTRAN66/test_fortran66_parser/first_standard_demo.f`
- `FORTRAN66/test_fortran66_parser/function_program.f`
- `FORTRAN66/test_fortran66_parser/main_program.f`
- `FORTRAN66/test_fortran66_parser/standard_program.f`
- `FORTRAN66/test_fortran66_parser/subroutine_program.f`

These examples represent:

- Richer combinations of program units, functions, and subroutines.
- More complete exercise of the FORTRAN 66 statement set.

Their XPASS status confirms that:

- The grammar parses simpler FORTRAN 66 fragments, but not all
  realistic programs in the fixture suite.
- Certain statement combinations or declarations used in these
  fixtures are not yet fully modeled.

## 4. Summary

The FORTRAN 66 grammar:

- Builds on earlier dialects and introduces key F66 features.
- Uses a simplified fixed-form model without 80-column enforcement.
- Successfully parses selected test programs, but still rejects several
  richer, spec-inspired fixtures.

Further work should:

- Use the XPASS fixtures as a concrete checklist for extending FORTRAN
  66 coverage.
- Capture more detailed, clause-by-clause feature mapping in this audit
  as the grammar is refined.

