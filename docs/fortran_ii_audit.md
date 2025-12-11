# FORTRAN II (1958) – Grammar Audit (status: in progress)

This document describes what the **FORTRAN II** grammar in this
repository currently supports, based on:

- `grammars/src/FORTRANIILexer.g4`, `grammars/src/FORTRANIIParser.g4`
- `grammars/src/FORTRANLexer.g4`, `grammars/src/FORTRANParser.g4`
- `docs/fixed_form_support.md`
- `tests/FORTRANII/test_fortran_ii_parser.py`
- `tests/test_fixture_parsing.py` (XPASS fixtures)

This is a descriptive audit of the current implementation. Full
conformance to the IBM 704 FORTRAN II manual (C28-6000-2, 1958) is
substantially complete. Tape/drum I/O statements are implemented per
issue #153 (closed). File-control statements (END FILE, REWIND, BACKSPACE)
are implemented per issue #384.

## 1. Program structure and subprograms

Specification-wise, FORTRAN II is essentially FORTRAN I plus support
for separately compiled subroutines and functions, together with a
`COMMON` statement and a formal `END` statement for program units.

In this repository:

- Top-level program rule:
  - `fortran_program` accepts exactly one of:
    - `main_program` (a sequence of statements),
    - `subroutine_subprogram`,
    - `function_subprogram`.
  - Multiple program units per file are not modeled; instead, each
    unit would be parsed separately.
- Subprograms:
  - `subroutine_subprogram` parses:
    - `SUBROUTINE name (optional-parameter-list)` on the first line,
      followed by a `statement_list` and `END`.
  - `function_subprogram` parses:
    - Optional `type_spec` (`INTEGER` or `REAL`) followed by
      `FUNCTION name(parameter-list)`, a `statement_list` and `END`.
- Tests:
  - `tests/FORTRANII/test_fortran_ii_parser.py` exercises:
    - `subroutine_subprogram` (`subroutine_text.f`).
    - `function_subprogram` (`function_text.f`).
    - A subroutine-style program (`subroutine_program.f`).

The generic fixture harness (`tests/test_fixture_parsing.py`) now uses
the `fortran_program` entry rule for FORTRANII (per issue #157), and
all three FORTRAN II fixtures parse successfully with zero syntax
errors.

## 2. Spec-based statement coverage

The IBM reference manual **“FORTRAN II for the IBM 704 Data
Processing System” (Form C28-6000-2, 1958)** describes FORTRAN II
as:

- “compatible with the original FORTRAN, and [containing] six new
  types of statements and incorporat[ing] all the statements in the
  original FORTRAN language” (Part I, Chapter 1, “Types of
  Statements” and the discussion quoted on p. 2 of the local OCR at
  `validation/pdfs/FORTRANII_1958_IBM704_C28-6000-2.txt`).
- Listing all FORTRAN II statements in **Appendix A. Summary of
  FORTRAN II Statements** (page 59 in the same OCR file).

From the manual, the “six new” FORTRAN II statement/subprogram
forms are:

- `SUBROUTINE`
- `FUNCTION`
- `CALL`
- `RETURN`
- `COMMON`
- `END` (used as a standardized program‑unit terminator)

All original FORTRAN (1957) statements (assignment, arithmetic IF,
DO, GO TO / computed GO TO, FORMAT and I/O, DIMENSION,
EQUIVALENCE, FREQUENCY, PAUSE, STOP, CONTINUE, tape/drum/file
control, etc.) remain available.

The FORTRAN II parser in this repo implements these categories
explicitly at the syntax level, with a few notable differences from
the strict 1958 specification (see COMMON and I/O sections below).

### 2.1 FORTRAN I statement set (inherited)

The FORTRAN II parser inherits the entire 1957 statement set from
`grammars/src/FORTRANParser.g4` (assignment, arithmetic IF, DO, GO TO /
computed GO TO, FORMAT and I/O, DIMENSION, EQUIVALENCE, FREQUENCY,
PAUSE, STOP, CONTINUE, tape/drum/file control, etc.).

`grammars/src/FORTRANIIParser.g4` does not re‑define any 1957 statement
syntax; it extends the inherited `statement_body` only by adding the
FORTRAN II innovations. This reflects the manual’s description of
FORTRAN II as “FORTRAN I plus six new statement forms.”

### 2.2 FORTRAN II additions

New FORTRAN II features and their representation:

- `CALL`:
  - `call_stmt : CALL IDENTIFIER (LPAREN expr_list? RPAREN)?`
  - Tested in `test_call_statements` with different argument counts.

- `SUBROUTINE` and `FUNCTION`:
  - `subroutine_subprogram` and `function_subprogram` rules, with
    optional type spec for functions.
  - Tested in `test_subroutine_definition` and `test_function_definition`.

- `RETURN`:
  - `return_stmt : RETURN`
  - Tested indirectly via fixtures that include RETURN statements and
    by asserting that function examples contain two RETURNs.

- `COMMON`:
  - `common_stmt : COMMON (SLASH IDENTIFIER SLASH)? variable_list`
  - Supports both blank COMMON and named `COMMON /BLOCK/` forms.
  - Tested explicitly in `test_common_statement`.

- `END`:
  - `end_stmt : END` as a generic terminator for main program or
    subprogram; there is no explicit modeling of sense-switch
    overrides or integer arguments some compilers allowed.

## 3. Data types and expressions

Data types:

- The grammar allows optional `type_spec` for functions:
  - `type_spec : INTEGER | REAL`.
- There is no explicit support for DOUBLE PRECISION or COMPLEX types
  in the FORTRAN II parser; those appear later in the grammar chain
  (e.g. FORTRAN 66 and beyond).

Expressions:

- Expression syntax is inherited from `grammars/src/FORTRANParser.g4`.
  The FORTRAN II manual states that expression syntax is unchanged from
  FORTRAN I, so `FORTRANIIParser.g4` relies on the inherited rules.

  Inherited hierarchy:

  ```
  expr -> relational_expr -> additive_expr -> multiplicative_expr -> unary_expr -> power_expr -> primary
  ```

  Operator precedence (highest to lowest):
  1. `**` (exponentiation) - **right associative**
  2. unary `+`, `-`
  3. `*`, `/` (multiplication, division) - left associative
  4. binary `+`, `-` (addition, subtraction) - left associative
  5. relational operators (`.EQ.`, `.NE.`, `.LT.`, `.LE.`, `.GT.`, `.GE.`)

- The `**` operator is correctly right-associative:
  - `2**3**4` parses as `2**(3**4)`, not `(2**3)**4`.
  - This matches the FORTRAN language specification across all standards.

- Unary operators have higher precedence than binary `+`/`-` but lower
  than `**`:
  - `-2**3` parses as `-(2**3)` = -8, not `(-2)**3` = -8.

- Function calls and array subscripts share the same inherited syntax
  (`IDENTIFIER LPAREN expr_list RPAREN`); semantic analysis distinguishes
  them. The inherited `variable` rule handles both forms.

- Tests in `test_fortran_ii_parser.py` verify:
  - Chained exponentiation parses correctly (`test_chained_exponentiation_parses`)
  - Right-associativity of `**` (`test_power_expr_right_associativity`)
  - Mixed operator precedence (`test_mixed_operator_precedence`)
  - Unary operator precedence (`test_unary_operator_precedence`)
  - Parenthesized expressions (`test_parenthesized_expressions`)
  - Function calls in expressions (`test_function_call_in_expression`)

## 4. COMMON semantics

Specification:

- Historically, FORTRAN II only provided a single "blank" COMMON area;
  named COMMON blocks (`COMMON /BLOCK/ ...`) were introduced later
  (FORTRAN 66 era).
- The 1958 IBM 704 FORTRAN II manual (C28-6000-2, Section 3.5) defines
  COMMON as: `COMMON a1, a2, ..., an` where each `a` is a variable name
  or array name.

Implementation in this grammar:

The grammar provides **two modes** for COMMON handling (per issue #156):

1. **Relaxed mode** (default):
   - Entry points: `fortran_program`, `main_program`, `subroutine_subprogram`,
     `function_subprogram`
   - Uses `common_stmt` which accepts both blank and named COMMON:
     - `COMMON A, B, C` (blank COMMON)
     - `COMMON /BLOCK/ X, Y` (named COMMON)
   - Recommended for practical compatibility with later standards.

2. **Strict 1958 mode**:
   - Entry points: `fortran_program_strict`, `main_program_strict`,
     `subroutine_subprogram_strict`, `function_subprogram_strict`
   - Uses `common_stmt_strict` which accepts only blank COMMON:
     - `COMMON A, B, C` (allowed)
     - `COMMON /BLOCK/ X, Y` (syntax error)
   - Recommended for historical audits requiring strict 1958 conformance.

The strict mode entry points use `statement_body_strict` which routes to
`common_stmt_strict` instead of `common_stmt`. All other statement types
are shared between modes.

Test coverage:

- `tests/FORTRANII/test_fortran_ii_parser.py::TestStrictCommon` validates:
  - Blank COMMON parses in both relaxed and strict modes.
  - Named COMMON parses in relaxed mode but fails in strict mode.
  - Strict mode fixtures parse correctly via `*_strict` entry points.
- Fixtures in `tests/fixtures/FORTRANII/test_strict_common/`:
  - `blank_common.f` - 1958-compliant blank COMMON (strict mode compatible)
  - `named_common.f` - FORTRAN 66-style named COMMON (relaxed mode only)
  - `main_blank_common.f` - Main program with blank COMMON

## 5. Fixed-form and labels

From `docs/fixed_form_support.md`:

- Labels:
  - `FORTRANIILexer.g4` defines a `LABEL` token as 1–5 digits with no
    leading zero, matching the 1–99999 label range historically used
    in FORTRAN.
  - The parser `label` rule uses this token in `statement` forms.
- Layout:
  - The grammar uses a **logical** fixed-form model:
    - `statement : label? statement_body NEWLINE? | NEWLINE`.
    - It does not enforce physical column positions for labels,
      continuation marks or sequence numbers.
  - Sequence fields (columns 73–80) are not modeled.
  - Column‑1 `C`/`*` comments and strict column ranges are not
    enforced.

### 5.1 Strict Fixed-Form Mode (Issue #143 resolved)

The `tools/strict_fixed_form.py` module provides optional **strict fixed-form
validation** for IBM 704 card-image source files per C28-6000-2:

- **Card layout validation**:
  - Columns 1–5: Statement labels (numeric 1–99999 or blank)
  - Column 6: Continuation mark (non-blank = continuation)
  - Columns 7–72: Statement text
  - Columns 73–80: Sequence/identification field (ignored)
  - Column 1: `C` or `*` marks a comment card

- **Entry points**:
  - `validate_strict_fixed_form(source)`: Validates card layout, returns
    `ValidationResult` with errors and warnings
  - `convert_to_lenient(source, strip_comments=True)`: Validates and converts
    to layout-lenient form suitable for `FORTRANIIParser`

- **Test coverage** in `tests/FORTRANII/test_strict_fixed_form.py`:
  - Card parsing tests: blank, comment (C/*), statement, continuation
  - Validation tests: invalid labels, continuation errors
  - Conversion tests: comment handling, continuation joining
  - Integration tests: converted output parses with FORTRANIIParser

- **Fixtures** in `tests/fixtures/FORTRANII/test_strict_fixed_form/`:
  - `valid_subroutine.f`, `valid_function.f`, `valid_main.f`: Authentic
    80-column card layouts with sequence numbers
  - `valid_continuation.f`: Multi-card statement continuations
  - `invalid_label_alpha.f`, `invalid_continuation_labeled.f`: Invalid
    layouts that strict mode correctly rejects

This provides a two-mode approach:
1. **Lenient mode** (default): Grammar accepts any token order
2. **Strict mode**: Preprocessor validates card layout before parsing

## 6. Hollerith constants

Lexer:

- `HOLLERITH : [1-9] [0-9]* H ~[\r\n]*? ;`
  - A simple pattern matching `nH...` sequences up to end of line.

Parser usage:

- `format_item` allows `HOLLERITH` as a standalone format item.

Limitations:

- The grammar does not enforce that the digit count `n` matches the
  number of characters following `H`:
  - This is a lexical/semantic check left to downstream tools.
- Outside FORMAT, Hollerith usage is limited to the simple lexer rule;
  the tests treat Hollerith handling primarily as “token is
  recognized” rather than as strict, length-checked semantics.

## 7. Fixture status

All three FORTRAN II fixtures now parse successfully:

- `FORTRANII/test_fortran_ii_parser/function_text.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_program.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_text.f`

The generic fixture harness uses the `fortran_program` entry rule
(updated per issue #157), which correctly routes programs through
`main_program`, `subroutine_subprogram`, or `function_subprogram`
rules as appropriate.

The dedicated FORTRAN II rules (CALL, SUBROUTINE, FUNCTION, COMMON)
work on both the targeted `test_fortran_ii_parser.py` tests and the
generic fixture harness tests.

## 8. C28-6000-2 Appendix A Crosswalk

The IBM FORTRAN II manual (Form C28-6000-2, 1958) Appendix A lists all
FORTRAN II statements. This section provides an exhaustive crosswalk from
each Appendix A entry to the corresponding grammar rule(s) or notes gaps.

### 8.1 FORTRAN II New Statements (Chapter 3)

The six new FORTRAN II statement/subprogram forms added to FORTRAN I:

| Statement Form                  | Grammar Rule(s)              | Status          |
|---------------------------------|------------------------------|-----------------|
| SUBROUTINE name [(args)]        | `subroutine_subprogram`      | Implemented     |
| [type] FUNCTION name (args)     | `function_subprogram`        | Implemented     |
| CALL name [(args)]              | `call_stmt`                  | Implemented     |
| RETURN                          | `return_stmt`                | Implemented     |
| COMMON list                     | `common_stmt`, `common_stmt_strict` | Implemented |
| END                             | `end_stmt`                   | Implemented     |

Note on COMMON: The grammar provides two modes (see Section 4):
- `common_stmt` (relaxed): accepts both blank and named COMMON blocks.
- `common_stmt_strict` (strict 1958): accepts only blank COMMON per C28-6000-2.
Use `*_strict` entry points for historical audits. Issue #156 resolved.

### 8.2 Inherited FORTRAN I Statements (C28-6003 Appendix B)

Most core FORTRAN (1957) statements remain available in FORTRAN II.
These are either inherited from `FORTRANParser.g4` or redefined in
`FORTRANIIParser.g4`:

| Statement Form                       | Grammar Rule(s)                | Status          |
|--------------------------------------|--------------------------------|-----------------|
| v = e (assignment)                   | `assignment_stmt`              | Implemented     |
| GO TO n                              | `goto_stmt`                    | Implemented     |
| GO TO (n1, n2, ..., nm), i           | `computed_goto_stmt`           | Implemented     |
| IF (e) n1, n2, n3                    | `arithmetic_if_stmt`           | Implemented     |
| IF (e) n1, n2 (>= 0 branch)          | `if_stmt_two_way_positive`     | Implemented     |
| IF (e) n1, n2 (= 0 branch)           | `if_stmt_two_way_zero`         | Implemented     |
| DO n i = m1, m2 [, m3]               | `do_stmt`                      | Implemented     |
| CONTINUE                             | `continue_stmt`                | Implemented     |
| STOP [n]                             | `stop_stmt`                    | Implemented     |
| PAUSE [n]                            | `pause_stmt`                   | Implemented     |
| DIMENSION v, v, ...                  | `dimension_stmt`               | Implemented     |
| EQUIVALENCE (a,b,...), ...           | `equivalence_stmt`             | Implemented     |
| FREQUENCY n (i1, i2, ...)            | `frequency_stmt`               | Implemented     |
| FORMAT (specification)               | `format_stmt`                  | Implemented     |
| f(a, b, ...) = e (statement func)    | `statement_function_stmt`      | Implemented     |
| READ n, list                         | `read_stmt`                    | Implemented     |
| PRINT n, list                        | `print_stmt`                   | Implemented     |
| PUNCH n, list                        | `punch_stmt`                   | Implemented     |

### 8.3 FORTRAN I Features Not Included in FORTRAN II Statement Set

The following FORTRAN I (C28-6003) features are either implemented only
at the FORTRAN I level (not wired into the FORTRAN II `statement_body`)
or are out of scope for the current grammar:

| Statement Form                       | Grammar Rule(s)                | Status          |
|--------------------------------------|--------------------------------|-----------------|
| ASSIGN i TO n                        | (FORTRAN I: `assign_stmt`)     | FORTRAN I only  |
| GO TO n, (n1, n2, ...)               | (FORTRAN I: `assigned_goto_stmt`) | FORTRAN I only |
| IF (SENSE SWITCH i) n1, n2           | `if_stmt_sense_switch`         | Implemented     |
| IF (SENSE LIGHT i) n1, n2            | `if_stmt_sense_light`          | Implemented     |
| IF ACCUMULATOR OVERFLOW n1, n2       | `if_stmt_accumulator_overflow` | Implemented     |
| IF QUOTIENT OVERFLOW n1, n2          | `if_stmt_quotient_overflow`    | Implemented     |
| IF DIVIDE CHECK n1, n2               | `if_stmt_divide_check`         | Implemented     |
| SENSE LIGHT i                        | `sense_light_stmt`             | Implemented     |
| READ INPUT TAPE i, n, list           | `read_tape_drum_stmt`          | Implemented     |
| READ TAPE i, list                    | `read_tape_drum_stmt`          | Implemented     |
| READ DRUM i, j, list                 | `read_tape_drum_stmt`          | Implemented     |
| WRITE OUTPUT TAPE i, n, list         | `write_tape_drum_stmt`         | Implemented     |
| WRITE TAPE i, list                   | `write_tape_drum_stmt`         | Implemented     |
| WRITE DRUM i, j, list                | `write_tape_drum_stmt`         | Implemented     |
| END FILE i                           | `end_file_stmt`                | Implemented     |
| REWIND i                             | `rewind_stmt`                  | Implemented     |
| BACKSPACE i                          | `backspace_stmt`               | Implemented     |

### 8.4 Implementation Status

The following features from the original IBM 704 FORTRAN II context have
been addressed:

- **ASSIGN/assigned GO TO**: Implemented and tested in `FORTRANParser.g4`
  as part of the FORTRAN I grammar (issue #141, closed). These constructs
  are not yet wired into the FORTRAN II `statement_body` and therefore
  remain FORTRAN I–only in this repository.
- **FORMAT and Hollerith**: Implemented (issue #154, closed)
- **Sense switch/light and hardware IFs**: Implemented in FORTRAN I parser
  and wired into FORTRAN II `statement_body` and `statement_body_strict`
  (issue #395, closed). Includes IF (SENSE SWITCH i), IF (SENSE LIGHT i),
  IF ACCUMULATOR OVERFLOW, IF QUOTIENT OVERFLOW, IF DIVIDE CHECK, and
  SENSE LIGHT statements per C28-6003 Appendix B rows 4–8, 11.
- **File-control statements**: END FILE, REWIND, and BACKSPACE are
  implemented in FORTRAN I parser and wired into FORTRAN II `statement_body`
  and `statement_body_strict` (issue #384, closed). These statements
  control magnetic tape operations per C28-6003 Chapter III.F.
- **Strict fixed-form card layout**: Implemented via `tools/strict_fixed_form.py`
  preprocessor (issue #143, closed). Validates IBM 704 card layout per
  C28-6000-2 and converts to lenient form for parsing.

Tape/drum I/O forms (READ INPUT TAPE, WRITE OUTPUT TAPE, READ/WRITE
TAPE/DRUM) are now implemented (issue #153 closed). There are no
remaining significant gaps for FORTRAN II coverage.

## 9. Summary

The FORTRAN II grammar in this repository:

- Implements a largely complete statement set for FORTRAN I and FORTRAN
  II, including:
  - Assignment, GOTO, computed GOTO, arithmetic IF, DO, CONTINUE,
    STOP, PAUSE, DIMENSION, EQUIVALENCE, FREQUENCY, READ/PRINT/PUNCH,
    FORMAT.
  - SUBROUTINE and FUNCTION subprograms, CALL and RETURN, COMMON and
    END.
- Contains inline C28-6000-2 spec references in grammar comments for
  traceability to the original IBM FORTRAN II manual.
- Provides dual-mode COMMON handling (issue #156 resolved):
  - Relaxed mode (default): accepts both blank and named COMMON blocks
    for practical compatibility with later standards.
  - Strict 1958 mode: enforces blank COMMON only per C28-6000-2 via
    `*_strict` entry points for historical audits.
- Provides dual-mode fixed-form handling (issue #143 resolved):
  - Lenient mode (default): Grammar accepts any valid token order
  - Strict mode: Preprocessor validates IBM 704 card layout per C28-6000-2
    before converting to lenient form for parsing
- Provides Hollerith support for FORMAT items but does not enforce
  length checks on the `nH...` form.

Further work on this standard should reference this audit together with
the open issues for expanding FORTRAN II coverage.
