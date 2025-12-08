# FORTRAN II (1958) – Grammar Audit (status: in progress)

This document describes what the **FORTRAN II** grammar in this
repository currently supports, based on:

- `grammars/FORTRANIILexer.g4`, `grammars/FORTRANIIParser.g4`
- `grammars/FORTRANLexer.g4`, `grammars/FORTRANParser.g4`
- `docs/fixed_form_support.md`
- `tests/FORTRANII/test_fortran_ii_parser.py`
- `tests/test_fixture_parsing.py` (XPASS fixtures)

It is intentionally descriptive of the current implementation, not a
claim of full conformance to every historical FORTRAN II dialect.

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

### 2.1 FORTRAN I statement set (inherited and redefined)

The FORTRAN II parser redefines a full set of 1957-style statements:

- Assignment:
  - `assignment_stmt : variable EQUALS expr`
  - Implemented and widely used in fixtures (math expressions, array
    examples, etc.).

- Branching and control:
  - `goto_stmt` – unconditional `GO TO label`.
  - `computed_goto_stmt` – `GO TO (label-list), expr`.
  - `arithmetic_if_stmt` – `IF (expr) l1, l2, l3`.
  - `do_stmt` – `DO label var ASSIGN expr, expr [, expr]`.
  - `continue_stmt`, `stop_stmt`, `pause_stmt`.
  - These rules represent the core 1957 control constructs, with
    `pause_stmt` and `frequency_stmt` modeling the original PAUSE and
    FREQUENCY features.

- Data layout and optimization:
  - `dimension_stmt` – array declarations with constant bounds.
  - `equivalence_stmt` – memory overlay sets.
  - `frequency_stmt` – compiler optimization hints with integer
    counts.

- I/O and FORMAT:
  - `read_stmt` – forms with unit and format label variations.
  - `print_stmt` – `PRINT format, list`.
  - `punch_stmt` – `PUNCH format, list`.
  - `format_stmt` – FORMAT with a list of `format_item`s; items can
    be numeric descriptors or Hollerith text.

Compared to the FORTRAN I stub, the FORTRAN II parser is significantly
more complete: it includes explicit rules for DIMENSION,
EQUIVALENCE, FORMAT, PRINT and PUNCH, rather than treating them as
tokens only.

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

- `expr` in `FORTRANIIParser.g4` defines a proper multi-level precedence
  hierarchy aligned with both the 1957 FORTRAN spec and the FORTRAN II
  manual (Form C28-6000-2, 1958):

  ```
  expr -> additive_expr -> multiplicative_expr -> unary_expr -> power_expr -> primary
  ```

  Operator precedence (highest to lowest):
  1. `**` (exponentiation) - **right associative**
  2. unary `+`, `-`
  3. `*`, `/` (multiplication, division) - left associative
  4. binary `+`, `-` (addition, subtraction) - left associative

- The `**` operator is correctly right-associative:
  - `2**3**4` parses as `2**(3**4)`, not `(2**3)**4`.
  - This matches the FORTRAN language specification across all standards.

- Unary operators have higher precedence than binary `+`/`-` but lower
  than `**`:
  - `-2**3` parses as `-(2**3)` = -8, not `(-2)**3` = -8.

- `integer_expr` is a syntactic alias for `expr`; integer semantics
  are left to a later semantic phase or consumer.

- Function calls and array subscripts share the same syntax
  (`IDENTIFIER LPAREN expr_list RPAREN`); semantic analysis distinguishes
  them. The `variable` rule handles both forms.

- Tests in `test_fortran_ii_parser.py` verify:
  - Chained exponentiation parses correctly (`test_chained_exponentiation_parses`)
  - Right-associativity of `**` (`test_power_expr_right_associativity`)
  - Mixed operator precedence (`test_mixed_operator_precedence`)
  - Unary operator precedence (`test_unary_operator_precedence`)
  - Parenthesized expressions (`test_parenthesized_expressions`)
  - Function calls in expressions (`test_function_call_in_expression`)

## 4. COMMON semantics

Specification:

- Historically, FORTRAN II only provided a single “blank” COMMON area;
  named COMMON blocks (`COMMON /BLOCK/ ...`) were introduced later
  (FORTRAN 66 era).

Implementation in this grammar:

- `common_stmt` accepts both:
  - `COMMON A, B, C` (blank COMMON).
  - `COMMON /BLOCK/ X, Y` (named COMMON).

Implications:

- The grammar is intentionally more generous than pure historical
  FORTRAN II with respect to COMMON naming: it allows named COMMON
  blocks as a convenience, even though that feature is associated with
  later standards.

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

Implication:

- The grammar accepts a wide variety of FORTRAN II code layouts as
  long as the token sequence is valid, but does not attempt
  card-image fidelity.

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
| COMMON list                     | `common_stmt`                | Implemented*    |
| END                             | `end_stmt`                   | Implemented     |

*Note: `common_stmt` extends beyond strict FORTRAN II by accepting named
COMMON blocks (`COMMON /name/ list`), which historically were introduced
in FORTRAN 66. See issue #156 for design decision tracking.

### 8.2 Inherited FORTRAN I Statements (C28-6003 Appendix B)

All original FORTRAN (1957) statements remain available in FORTRAN II.
These are inherited from `FORTRANParser.g4` or redefined in
`FORTRANIIParser.g4`:

| Statement Form                       | Grammar Rule(s)                | Status          |
|--------------------------------------|--------------------------------|-----------------|
| v = e (assignment)                   | `assignment_stmt`              | Implemented     |
| GO TO n                              | `goto_stmt`                    | Implemented     |
| GO TO (n1, n2, ..., nm), i           | `computed_goto_stmt`           | Implemented     |
| IF (e) n1, n2, n3                    | `arithmetic_if_stmt`           | Implemented     |
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

### 8.3 FORTRAN I Features Not Yet Implemented in FORTRAN II Parser

The following FORTRAN I (C28-6003) features are not directly implemented
in the FORTRAN II parser. They are handled at the FORTRAN I level or
remain as gaps:

| Statement Form                       | Grammar Rule(s)                | Status          |
|--------------------------------------|--------------------------------|-----------------|
| ASSIGN i TO n                        | (FORTRAN I: `assign_stmt`)     | Gap: see #141   |
| GO TO n, (n1, n2, ...)               | (FORTRAN I: `assigned_goto_stmt`) | Gap: see #141 |
| IF (SENSE SWITCH i) n1, n2           | (FORTRAN I: `if_stmt_sense_switch`) | Inherited     |
| IF (SENSE LIGHT i) n1, n2            | (FORTRAN I: `if_stmt_sense_light`) | Inherited     |
| IF ACCUMULATOR OVERFLOW n1, n2       | (FORTRAN I: see FORTRANParser) | Inherited       |
| IF QUOTIENT OVERFLOW n1, n2          | (FORTRAN I: see FORTRANParser) | Inherited       |
| IF DIVIDE CHECK n1, n2               | (FORTRAN I: see FORTRANParser) | Inherited       |
| SENSE LIGHT i                        | (FORTRAN I: `sense_light_stmt`) | Inherited      |
| READ INPUT TAPE i, n, list           | Not implemented                | Gap: see #153   |
| READ TAPE i, list                    | Not implemented                | Gap: see #153   |
| READ DRUM i, j, list                 | Not implemented                | Gap: see #153   |
| WRITE OUTPUT TAPE i, n, list         | Not implemented                | Gap: see #153   |
| WRITE TAPE i, list                   | Not implemented                | Gap: see #153   |
| WRITE DRUM i, j, list                | Not implemented                | Gap: see #153   |
| END FILE i                           | Not implemented                | Gap: see #153   |
| REWIND i                             | Not implemented                | Gap: see #153   |
| BACKSPACE i                          | Not implemented                | Gap: see #153   |

### 8.4 Gaps Requiring Follow-up Issues

The following gaps have been identified during this crosswalk and are
tracked by existing issues:

- **#141**: FORTRAN 1957 historical stub promotion (general coverage,
  includes ASSIGN/assigned GO TO)
- **#143**: FORTRAN II strict fixed-form card layout semantics
- **#153**: Full 704 I/O statement family (READ/WRITE/TAPE/DRUM/END FILE/
  REWIND/BACKSPACE)
- **#154**: FORMAT grammar and Hollerith constants
- **#156**: Reconcile COMMON semantics with 1958 spec (no named blocks)

All identified gaps have corresponding GitHub issues; no new issues
required from this crosswalk.

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
- Extends COMMON to allow both blank and named forms, slightly beyond
  strictly historical FORTRAN II.
- Uses a layout‑lenient fixed-form model with explicit LABEL tokens,
  without strict 80-column enforcement.
- Provides Hollerith support for FORMAT items but does not enforce
  length checks on the `nH...` form.

Remaining design decisions (tracked separately):

- Issue #156 tracks whether to reconcile COMMON semantics with the
  strict 1958 spec (blank COMMON only) or continue accepting named
  COMMON blocks as a forward-compatibility extension.

Further work on this standard should reference this audit together with
the open issues for expanding FORTRAN II coverage.
