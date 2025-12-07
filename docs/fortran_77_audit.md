# FORTRAN 77 (1977) – Grammar Audit (status: in progress)

This document captures the current status of the **FORTRAN 77** grammar
in this repository, based on:

- `grammars/FORTRAN77Lexer.g4`, `grammars/FORTRAN77Parser.g4`
- `docs/fixed_form_support.md`
- Tests under `tests/FORTRAN77/`
- XPASS fixtures noted in `tests/test_fixture_parsing.py`

## 1. Language features implemented

The FORTRAN 77 grammar builds on FORTRAN 66 and adds:

- CHARACTER type and associated declarations.
- Structured IF–THEN–ELSE–ENDIF constructs.
- DO/ENDDO loop constructs in addition to labeled DO.
- PARAMETER, SAVE, PROGRAM and expanded I/O capabilities.

The dedicated F77 tests exercise basic structured control and
declarations.

## 2. Fixed-form handling

Per `docs/fixed_form_support.md`:

- FORTRAN 77 uses the same **layout‑lenient** fixed-form model as the
  other historical grammars:
  - Labels and statements are recognized by tokens, not absolute
    columns.
  - Sequence numbers and card-image details are not modeled.
  - Column‑1 comment conventions are not strictly enforced.

This is sufficient for grammar-based parsing but does not attempt full
card compatibility.

## 3. Nested IF and structured control gaps

`tests/test_fixture_parsing.py` marks the following F77 fixture as
XPASS:

- `FORTRAN77/test_fortran77_parser_extra/nested_if_block.f`

The XPASS reason states that it:

- “Exercises complex structured programming patterns that still cause
  {errors} syntax errors in the current FORTRAN 77 grammar.”

This indicates:

- The grammar handles some IF/THEN/ELSE/ENDIF usage but fails on more
  deeply nested or complex structured control examples.

## 4. Summary

The FORTRAN 77 grammar:

- Implements key F77 features and structured control constructs.
- Uses a simplified fixed-form model.
- Still fails on at least one realistic nested IF example from the
  fixture suite.

Future work should:

- Strengthen the structured IF/ENDIF and DO/ENDDO rules until
  nested_if_block.f and similar programs parse cleanly.
- Extend this audit with a more exhaustive mapping from the F77
  standard once those changes land.

