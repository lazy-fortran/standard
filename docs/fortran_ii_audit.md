# FORTRAN II (1958) – Grammar Audit (status: in progress)

This document describes what the **FORTRAN II** grammar in this
repository currently supports, based on:

- `grammars/FORTRANIILexer.g4`, `grammars/FORTRANIIParser.g4`
- `docs/fixed_form_support.md`
- `tests/FORTRANII/test_fortran_ii_parser.py`
- `tests/test_fixture_parsing.py` and its XPASS entries

It reflects the implementation status, not full conformance to every
FORTRAN II dialect.

## 1. Program structure and subprograms

Implemented and tested (see `tests/FORTRANII/test_fortran_ii_parser.py`):

- Subroutine subprograms (`SUBROUTINE ... END`) with parameters.
- Function subprograms (`FUNCTION ... END`) with parameters and
  multiple `RETURN` statements.
- CALL statements with optional argument lists.

Fixtures demonstrate:

- A simple subroutine program (`subroutine_program.f`).
- A function example (`function_text.f`).
- A separate subroutine definition (`subroutine_text.f`).

Current limitations:

- The generic fixture parser marks several FORTRAN II fixtures as XPASS
  with messages that they exceed the simplified grammar.
- Only subprograms (functions/subroutines) are explicitly tested; full
  mixtures of main program plus multiple subprogram units are not.

## 2. COMMON and shared storage

Implemented:

- `COMMON` statements, including:
  - Unnamed COMMON lists.
  - Named COMMON blocks (`COMMON /BLOCK/ X, Y`).
  - Arrays in COMMON (`ARRAY(100)`).

These are exercised in `test_common_statement` and in the FORTRAN II
fixtures.

## 3. Fixed-form and labels

From `docs/fixed_form_support.md`:

- The lexer models labels as explicit tokens (1–5 digits, no leading
  zero).
- The parser uses a **logical** fixed-form model:
  - Optional label followed by a statement and optional newline.
  - No strict enforcement of label columns, continuation, or sequence
    numbers.
- Column‑1 `C`/`*` comments and strict label column ranges are not
  enforced.

Implication:

- Many historically “card-accurate” FORTRAN II sources will be
  accepted as long as the token sequence is valid, regardless of
  physical columns.

## 4. Known gaps and XPASS fixtures

In `tests/test_fixture_parsing.py` the following fixtures are marked
XPASS for FORTRAN II:

- `FORTRANII/test_fortran_ii_parser/function_text.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_program.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_text.f`

The XPASS reasons state that these represent richer historical usage
than the simplified grammar accepts and that the subroutine program
“exceeds the current stub grammar”.

These XPASS fixtures indicate:

- The grammar does not yet fully accept all realistic FORTRAN II
  examples in the tests.
- There are still missing or incomplete rules for subprogram structure
  and/or statement forms in FORTRAN II.

## 5. Summary

The FORTRAN II grammar today:

- Correctly recognizes and parses:
  - CALL statements.
  - FUNCTION and SUBROUTINE subprogram definitions.
  - COMMON statements (unnamed and named blocks).
- Uses a layout‑lenient fixed-form model without 80-column enforcement.
- Still rejects several test fixtures that represent plausible
  historical FORTRAN II programs.

Future work should:

- Use the XPASS fixtures as concrete targets for expanding FORTRAN II
  coverage.
- Decide whether to remain layout‑lenient or to add an optional strict
  fixed-form mode, as discussed in open issues.

