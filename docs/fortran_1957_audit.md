# FORTRAN (1957, IBM 704) – Grammar Audit (status: in progress)

This document summarizes what the **FORTRAN (1957)** grammar in this
repository actually supports today, based only on the contents of:

- `grammars/FORTRANLexer.g4`, `grammars/FORTRANParser.g4`
- `docs/fixed_form_support.md`
- `tests/FORTRAN/test_fortran_historical_stub.py`
- `tests/test_fixture_parsing.py` and the fixtures it references

It is intentionally descriptive of the current implementation, not a
claim of full conformance to the original IBM 704 FORTRAN compiler.

## 1. Program structure

- Implemented:
  - Single compilation unit consisting of a sequence of labeled or
    unlabeled statements.
  - No explicit `PROGRAM` statement (authentic 1957 convention).
  - Labels used as the primary control-flow mechanism.
- Not implemented / out of scope:
  - Multiple separately compiled program units in a single source.
  - Any post‑1960s program unit forms (`PROGRAM`, `MODULE`, etc.).

## 2. Statements and control flow

Implemented and tested in `tests/FORTRAN/test_fortran_historical_stub.py`
and the associated fixtures:

- Arithmetic IF: three-way branch `IF (expr) n1, n2, n3`.
- Computed GOTO: `GO TO (n1, n2, n3), i`.
- Unconditional GOTO: `GO TO n`.
- DO loops using labeled termination (no `END DO`).
- STOP and CONTINUE.
- PAUSE and FREQUENCY (unique early FORTRAN features).

Out-of-scope / not yet modeled:

- Any modern structured control (`IF ... THEN`, `SELECT CASE`, etc.).
- Explicit standard-conforming error handling beyond syntax errors.

## 3. Data types, expressions and arrays

Implemented:

- Implicit typing and simple numeric types (INTEGER and REAL style),
  exercised by arithmetic expression fixtures.
- Basic arithmetic operators including exponentiation (`**`), with
  operator precedence validated by the math fixtures.
- Array syntax with subscripts (e.g. `A(I,J)`), as used in array
  example fixtures.

Out-of-scope / not explicitly audited:

- Exact numeric range and kind semantics (pure syntax only).
- Any later-era types (LOGICAL, COMPLEX, CHARACTER, etc.).

## 4. I/O and FORMAT

Implemented (core subset):

- `READ`, `PRINT`, `PUNCH` statements, as exercised in
  `io_statements.f` and `io_operations_1957.f`.
- Basic `FORMAT` statements and edit descriptors sufficient for the
  focused stub tests.

Known limitations (from fixtures and comments):

- Several more ambitious FORMAT and I/O fixtures under
  `tests/fixtures/FORTRAN/test_fortran_historical_stub` are currently
  XPASS in `tests/test_fixture_parsing.py` and still produce syntax
  errors. These include richer FORMAT tests and full historical
  programs using more complex I/O.
- FORMAT grammar does not attempt to fully reconstruct all edit
  descriptors and edge cases from IBM manuals.

## 5. Hollerith, EQUIVALENCE and other 1957-specific features

Implemented in a limited way:

- `EQUIVALENCE` and `DIMENSION` are present in the grammar and used in
  fixtures.
- `FREQUENCY` and `PAUSE` have explicit tests and parse successfully.

Not fully implemented:

- Hollerith constants (`nHtext`) are **not** modeled as dedicated
  tokens with length checking. The tests explicitly state that
  “HOLLERITH tokens [are] not yet implemented in this stub” and only
  verify that the lexer can consume example text.

## 6. Fixed-form source and card layout

As documented in `docs/fixed_form_support.md`:

- The FORTRAN 1957 grammar uses a statement‑oriented, layout‑lenient
  model:
  - No enforcement of strict 80‑column layout.
  - Labels and statements are recognized based on tokens and rule
    order, not physical column numbers.
  - Sequence numbers (columns 73–80) are not modeled.
- Comments:
  - Modern `!` comments are supported.
  - Classic `C`/`*` column‑1 comments from historical examples are
    present in fixtures, but those fixtures are documented as
    intentionally exceeding the subset and are expected to fail.

Strict card-image semantics (labels in 1–5, continuation in 6, text in
7–72, sequence numbers in 73–80) remain outside the current subset and
would need new work (see issue `FORTRAN 1957: promote historical stub
toward full IBM 704 coverage`).

## 7. Fixtures and XPASS status

The generic fixture test (`tests/test_fixture_parsing.py`) treats many
1957 fixtures as XPASS with reason strings explaining that:

- They “go beyond the simplified FORTRAN grammar”.
- They “intentionally stretch the FORMAT grammar”.
- They “remain outside the strict subset accepted by the stub”.

These XPASS entries are the authoritative list of historical examples
that still fail to parse and should be used as a to‑do list when
extending the grammar toward fuller 1957 coverage.

## 8. Summary

Today the FORTRAN (1957) grammar is:

- A **historical, educational subset** that:
  - Compiles and parses a core of early FORTRAN constructs.
  - Provides realistic arithmetic, control flow, I/O and unique 1957
    features for demonstration and testing.
- Not yet:
  - A complete reconstruction of the IBM 704 FORTRAN compiler.
  - Column‑accurate for fixed-form card images.
  - Fully supportive of all FORMAT and Hollerith usage seen in
    historical programs.

Further work on this standard should reference this audit together with
the open issues for expanding 1957 coverage.

