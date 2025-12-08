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

## 2. Spec-based statement coverage (FORTRAN for the IBM 704)

The IBM reference manual **“FORTRAN Automatic Coding System for the
IBM 704 Data Processing System” (Form C28-6003, Oct 1958)** lists 32
statement types in *Appendix B. Table of FORTRAN Statements* in the
local OCR’d copy
`validation/pdfs/FORTRAN_1957_IBM704_C28-6003_Oct58.txt`. These
include DIMENSION/EQUIVALENCE, assignment, arithmetic IF,
exception-checking IFs, sense-switch/light control, GO TO/ASSIGN /
assigned GO TO, DO, formatted and unformatted I/O, END FILE /
REWIND / BACKSPACE, PAUSE, STOP / CONTINUE and FREQUENCY.

Mapping that Appendix‑B list to the current grammar:

- **DIMENSION, EQUIVALENCE**
  - Status: **implemented and tested**.
  - Evidence: `dimension_stmt` and `equivalence_stmt` rules exist in
    `FORTRANParser.g4` and are wired into `statement_body`. Dedicated
    tests in `tests/FORTRAN/test_fortran_historical_stub.py` validate
    parsing of single/multi-dimensional arrays and multi-variable
    equivalence sets with zero syntax errors.

- **Assignment**
  - Status: **implemented and tested.**
  - Evidence: `assignment_stmt` rule and multiple fixtures (e.g. math
    expression blocks) that parse without errors.

- **Arithmetic IF**
  - Status: **implemented and tested.**
  - Evidence: `if_stmt_arithmetic` rule and fixtures
    `arithmetic_if_stmt.f` / `arithmetic_if_control_flow.f`.

- **Exception-checking IFs and sense switch/light control**
  - Status: **not implemented.**
  - Evidence: no ACCUMULATOR, QUOTIENT, DIVIDE, SENSE, SWITCH or LIGHT
    tokens in `FORTRANLexer.g4`; no dedicated parser rules.

- **GO TO, computed GO TO, ASSIGN, assigned GO TO**
  - Status: **all implemented and tested.**
  - Evidence: `goto_stmt`, `computed_goto_stmt`, `assign_stmt` and
    `assigned_goto_stmt` rules exist in `FORTRANParser.g4` and are
    wired into `statement_body`. All forms are tested with explicit
    zero-error assertions in `tests/FORTRAN/test_fortran_historical_stub.py`
    using fixtures `assign_stmt.f`, `assigned_goto_stmt.f` and
    `assign_goto_combined.f`.

- **DO loops**
  - Status: **implemented (basic 1957 form) and tested.**
  - Evidence: `do_stmt_basic` implements `DO label var = expr, expr
    [, expr]`, and fixtures like `do_loop_with_label.f` parse.

- **Formatted I/O (FORMAT, READ/WRITE variants, PRINT, PUNCH)**
  - Status:
    - `READ`/`WRITE`: **implemented in a simplified form only.**
    - `FORMAT`, `PRINT`, `PUNCH` and tape-specific forms:
      **partial / not implemented.**
  - Evidence: `read_stmt_basic` / `write_stmt_basic` accept only
    `READ input_list` / `WRITE output_list` (no format labels or
    devices). `FORMAT`, `PRINT`, `PUNCH` exist as tokens but have no
    corresponding parser rules; richer FORMAT/I/O fixtures are XPASS.

- **Unformatted I/O (`READ TAPE`, `READ DRUM`, `WRITE TAPE`, `WRITE DRUM`)**
  - Status: **not implemented.**
  - Evidence: no dedicated TAPE/DRUM I/O forms in lexer or parser.

- **Other I/O (`END FILE`, `REWIND`, `BACKSPACE`)**
  - Status: **not implemented.**
  - Evidence: these keywords do not appear in `FORTRANLexer.g4` and
    have no parser rules.

- **PAUSE, STOP, CONTINUE**
  - Status: **all implemented and tested.**
  - Evidence: `CONTINUE`, `STOP` and `PAUSE` appear in `statement_body`.
    `pause_stmt` accepts `PAUSE` or `PAUSE n` per the IBM 704 manual,
    and tests assert `getNumberOfSyntaxErrors() == 0`.

- **FREQUENCY**
  - Status: **implemented and tested.**
  - Evidence: `frequency_stmt` rule plus fixtures
    (`frequency_stmt.f`, `frequency_test_1957.f`) that assert
    `getNumberOfSyntaxErrors() == 0`.

## 3. Statements and control flow (implemented subset)

Implemented and tested in `tests/FORTRAN/test_fortran_historical_stub.py`
and the associated fixtures:

- Arithmetic IF: three-way branch `IF (expr) n1, n2, n3`.
- Computed GOTO: `GO TO (n1, n2, n3), i`.
- Unconditional GOTO: `GO TO n`.
- ASSIGN: `ASSIGN i TO n` stores label `i` in variable `n`.
- Assigned GOTO: `GO TO n, (l1, l2, ..., lm)` branches to the label
  stored in variable `n`.
- DO loops using labeled termination (no `END DO`).
- STOP and CONTINUE.
- FREQUENCY as an optimization hint.
- PAUSE is fully modeled via `pause_stmt` (accepts `PAUSE` or
  `PAUSE n`) and tested with zero syntax errors.

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

- Simplified `READ` and `WRITE` forms (`read_stmt_basic`,
  `write_stmt_basic`) without devices or explicit FORMAT labels.
- Basic use of `FORMAT` in fixtures, but without a dedicated
  `format_stmt` rule in `FORTRANParser.g4`.

Known limitations (from fixtures and comments):

- Several more ambitious FORMAT and I/O fixtures under
  `tests/fixtures/FORTRAN/test_fortran_historical_stub` are currently
  XPASS in `tests/test_fixture_parsing.py` and still produce syntax
  errors. These include richer FORMAT tests and full historical
  programs using more complex I/O.
- FORMAT grammar does not attempt to fully reconstruct all edit
  descriptors and edge cases from IBM manuals.
- `PRINT`, `PUNCH`, tape/drum I/O forms and `END FILE`/`REWIND`/
  `BACKSPACE` are not modeled as full statements in the 1957 parser.

## 5. Hollerith, DIMENSION/EQUIVALENCE and other 1957-specific features

Implemented / partially implemented:

- `DIMENSION` and `EQUIVALENCE`:
  - **Fully implemented** via `dimension_stmt` and `equivalence_stmt`
    rules in `FORTRANParser.g4`. Both statements now parse with zero
    syntax errors and are validated by dedicated tests.
- `FREQUENCY`:
  - Fully modeled via `frequency_stmt` and tested with zero syntax
    errors.
- `PAUSE`:
  - Fully modeled via `pause_stmt` and tested with zero syntax errors.

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
