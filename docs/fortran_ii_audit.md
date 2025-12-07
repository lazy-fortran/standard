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

The generic fixture harness (`tests/test_fixture_parsing.py`) still
uses the imported `program_unit_core` entry rule from `FORTRANParser`
for FORTRANII, which is one reason some FORTRAN II fixtures remain
XPASS even though targeted subprogram tests succeed.

## 2. Spec-based statement coverage

Historically, FORTRAN II:

- Retained the FORTRAN I (1957) statement set: assignment,
  arithmetic IF, DO, GO TO and computed GO TO, FORMAT and I/O
  statements, DIMENSION, EQUIVALENCE, FREQUENCY, PAUSE, STOP,
  CONTINUE, etc.
- Added six key statements/subprogram forms:
  - `SUBROUTINE`
  - `FUNCTION`
  - `CALL`
  - `RETURN`
  - `COMMON`
  - `END` (as a standard program unit terminator)

The FORTRAN II parser in this repo implements all of these categories
explicitly and, in several cases, extends beyond a minimal subset.

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

- `expr` in `FORTRANIIParser.g4` defines a full precedence hierarchy:
  - Exponentiation (`**`), multiplication/division, addition/subtraction,
    unary plus/minus, and parenthesized expressions.
- `integer_expr` is a syntactic alias for `expr`; integer semantics
  are left to a later semantic phase or consumer.
- `function_reference` supports calls to intrinsic functions (e.g.
  `SIN(X)`), modeled as `IDENTIFIER LPAREN expr_list RPAREN`.

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

## 7. Known gaps and XPASS fixtures

`tests/test_fixture_parsing.py` marks several FORTRAN II fixtures as
XPASS with messages indicating that they exceed the current grammar:

- `FORTRANII/test_fortran_ii_parser/function_text.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_program.f`
- `FORTRANII/test_fortran_ii_parser/subroutine_text.f`

The XPASS reasons explain that these fixtures:

- Represent richer historical usage than the simplified grammar
  accepts.
- Exceed what is described as the “current stub” for FORTRAN II in the
  generic fixture harness.

Taken together with the targeted `test_fortran_ii_parser.py` tests,
this means:

- The dedicated FORTRAN II rules (CALL, SUBROUTINE, FUNCTION, COMMON)
  work on the focused examples.
- The generic “one size fits all” fixture parser, which still uses
  `program_unit_core` as its entry rule for FORTRANII, does not
  successfully parse some of the richer subprogram fixtures and treats
  them as expected failures.

## 8. Summary

The FORTRAN II grammar in this repository:

- Implements a largely complete statement set for FORTRAN I and FORTRAN
  II, including:
  - Assignment, GOTO, computed GOTO, arithmetic IF, DO, CONTINUE,
    STOP, PAUSE, DIMENSION, EQUIVALENCE, FREQUENCY, READ/PRINT/PUNCH,
    FORMAT.
  - SUBROUTINE and FUNCTION subprograms, CALL and RETURN, COMMON and
    END.
- Extends COMMON to allow both blank and named forms, slightly beyond
  strictly historical FORTRAN II.
- Uses a layout‑lenient fixed-form model with explicit LABEL tokens,
  without strict 80-column enforcement.
- Provides Hollerith support for FORMAT items but does not enforce
  length checks on the `nH...` form.
- Still rejects several richer FORTRAN II fixtures in the generic
  fixture harness, indicating remaining integration or coverage gaps.

Future work should:

- Align the fixture harness entry rule for FORTRAN II with the
  dedicated `fortran_program` or subprogram rules.
- Tighten the implementation and tests until the XPASS fixtures parse
  with zero syntax errors or any unsupported constructs are explicitly
  documented as out of scope.

