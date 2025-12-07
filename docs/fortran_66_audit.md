# FORTRAN 66 (ANSI X3.9‑1966) – Grammar Audit (status: in progress)

This document summarizes the **FORTRAN 66** grammar implemented in this
repository, based on:

- `grammars/FORTRAN66Lexer.g4`, `grammars/FORTRAN66Parser.g4`
- Inherited grammars (`FORTRANIILexer.g4`, `FORTRANIIParser.g4`)
- `docs/fixed_form_support.md`
- Tests in `tests/FORTRAN66/test_fortran66_parser.py`
- XPASS fixtures listed in `tests/test_fixture_parsing.py`

It describes what is implemented vs. what the 1966 standard family
expects (FORTRAN IV features plus standardization), without claiming
full conformance.

## 1. Program units and structure

Specification-wise, FORTRAN 66 standardizes three program unit types:

- Main program
- Subroutine subprogram
- Function subprogram
- BLOCK DATA subprogram (for COMMON block initialization)

In this grammar:

- `fortran66_program`
  - Top-level rule:
    - `fortran66_program : (main_program | subprogram | block_data_subprogram) EOF`.
  - Represents a single program unit per parse.

- `main_program`
  - Defined as `statement_list` (imported and slightly specialized from
    FORTRAN II).

- `subprogram`
  - Union of `subroutine_subprogram` and `function_subprogram`, both
    imported from the FORTRAN II parser (thus retaining that
    subprogram structure).

- `block_data_subprogram`
  - New in FORTRAN 66 grammar:
    - `BLOCKDATA block_data_name? NEWLINE data_initialization_part? END`.
  - Provides a standardized BLOCK DATA unit with optional name and a
    `data_initialization_part` composed of COMMON, DIMENSION,
    EQUIVALENCE and type declarations.

Tests in `tests/FORTRAN66/test_fortran66_parser.py` exercise:

- Main program (`main_program.f`).
- Function subprogram (`function_program.f`).
- Subroutine subprogram (`subroutine_program.f`).
- BLOCK DATA examples (`test_block_data_subprogram` and
  `test_block_data_structure`).

## 2. Type system (FORTRAN IV features merged into FORTRAN 66)

The FORTRAN 66 grammar extends the FORTRAN II type system by adding
FORTRAN IV’s data types:

- `type_spec` in `FORTRAN66Parser.g4`:
  - `INTEGER`
  - `REAL`
  - `LOGICAL`
  - `DOUBLE PRECISION`
  - `COMPLEX`

Corresponding tokens are defined in `FORTRAN66Lexer.g4`:

- `LOGICAL`, `DOUBLE` / `PRECISION`, `COMPLEX`, and a `D` exponent
  marker for double-precision literals.

Type usage:

- `type_declaration : type_spec variable_list`
  - Used in `data_initialization_body` for BLOCK DATA and allowed as a
    statement in `statement_body`.
- Tests:
  - `test_type_declarations` covers simple declarations such as:
    - `INTEGER I, J, K`
    - `REAL X, Y, Z`
    - `LOGICAL FLAG, READY`
    - `DOUBLE PRECISION PI`
    - `COMPLEX Z`

What is *not* present here:

- No `CHARACTER` type (this appears first in the Fortran 77 grammar).

## 3. Logical and relational expressions, IF statements

FORTRAN IV introduced logical data and operations; FORTRAN 66 adopts
them as part of the standard. The grammar models these as:

- Logical expressions:
  - `logical_expr` → `logical_term (DOT_OR logical_term)*`
  - `logical_term` → `logical_factor (DOT_AND logical_factor)*`
  - `logical_factor` → `DOT_NOT logical_primary` | `logical_primary`
  - `logical_primary` → `logical_literal` | `relational_expr` |
    `logical_variable` | `LPAREN logical_expr RPAREN`

- Logical literals:
  - `logical_literal` → `.TRUE.` or `.FALSE.` (`DOT_TRUE`, `DOT_FALSE`).

- Logical variables:
  - `logical_variable` → `IDENTIFIER` or array element.

- Relational expressions:
  - `relational_expr : expr relational_op expr`.
  - `relational_op` includes `.EQ.`, `.NE.`, `.LT.`, `.LE.`, `.GT.`,
    `.GE.` tokens.

- Logical IF:
  - `logical_if_stmt : IF LPAREN logical_expr RPAREN statement_body`.

Tests:

- `test_logical_literals` and `test_logical_operators` confirm
  literals and operators parse as `logical_literal` and `logical_expr`.
- `test_relational_operators` asserts parsing of `relational_expr`.
- `test_logical_if_statement` exercises various `IF (logical-expr)`
  forms, including ones mixing logical and relational operators.

Arithmetic IF is inherited from FORTRAN II (`arithmetic_if_stmt`) and
remains available alongside the logical IF.

## 4. Statement coverage versus specification

The FORTRAN 66 grammar builds on FORTRAN II and aims to cover the
standardized statement set. Mapping major statement families to rules:

- **Control flow and branching**
  - Implemented:
    - `goto_stmt` – unconditional GO TO.
    - `computed_goto_stmt` – `GO TO (label-list), expr`.
    - `arithmetic_if_stmt` – three-way arithmetic IF.
    - `logical_if_stmt` – logical IF.
    - `do_stmt` – DO with label and bounds (inherited from FORTRAN II).
    - `continue_stmt`, `stop_stmt`, `pause_stmt`.
  - Not implemented:
    - Assigned GO TO (`ASSIGN n TO k` and `GO TO k`) – the grammar
      has no explicit assigned-GOTO syntax; there is an `ASSIGN` token
      inherited from FORTRAN I, but no rule uses it for assigned GO TO.

- **Data declaration and layout**
  - Implemented:
    - `dimension_stmt`, `equivalence_stmt` – array bounds and memory
      overlays, inherited from FORTRAN II.
    - `common_stmt` – COMMON blocks (blank and named).
    - `type_declaration` – `type_spec variable_list` using INTEGER,
      REAL, LOGICAL, DOUBLE PRECISION and COMPLEX.
  - Extended behavior:
    - As with FORTRAN II, `common_stmt` accepts named COMMON blocks,
      which is historically characteristic of later standards but
      convenient for modern usage.

- **I/O and FORMAT**
  - Implemented:
    - `read_stmt`, `print_stmt`, `punch_stmt`, `format_stmt` inherited
      from FORTRAN II.
    - FORMAT items include full numeric descriptors (through
      `format_item` and `format_descriptor`) and Hollerith literals via
      the `HOLLERITH` token.
  - Not implemented:
    - Standard FORTRAN 66 sequential I/O statements `REWIND`,
      `BACKSPACE`, `ENDFILE` and explicit unit control are not present
      as tokens or rules in this grammar; corresponding data
      management must be handled at a higher level or is currently
      unsupported.

- **DATA statement**
  - Specification:
    - FORTRAN 66 includes the `DATA` statement for initializing
      variables.
  - Grammar:
    - There is no `data_stmt` rule or `DATA` token in the FORTRAN 66
      grammars; initialization inside BLOCK DATA is modeled via
      assignments and declarations, not via DATA statements.
  - Effect:
    - Any code using `DATA` will be rejected by this grammar.

- **EXTERNAL and INTRINSIC**
  - Lexer:
    - `FORTRAN66Lexer.g4` defines `EXTERNAL` and `INTRINSIC` tokens.
  - Parser:
    - No dedicated `external_stmt` or `intrinsic_stmt` rules exist in
      `FORTRAN66Parser.g4`; these keywords are not referenced in
      `statement_body`.
  - Effect:
    - Statements such as `EXTERNAL F` or `INTRINSIC SIN` are not
      recognized as distinct declarations and will not parse as
      intended under FORTRAN 66.

## 5. Fixed-form model

`docs/fixed_form_support.md` describes the Fortran 66 fixed-form
handling as:

- Layout‑lenient:
  - Statements are parsed according to token sequence, not strict
    column positions.
  - No enforcement of exact 80‑column semantics or sequence numbers.
- Comments:
  - Classic column‑1 `C`/`*` forms and sequence fields are treated as
    historical context but are not modeled with strict semantics in
    this grammar.

This makes the grammar suitable for analyzing typical FORTRAN 66 code
without reproducing exact card-image behavior.

## 6. Tests and XPASS fixtures

Direct tests (`tests/FORTRAN66/test_fortran66_parser.py`) cover:

- Program structure:
  - Main program, function, subroutine parsing via fixtures:
    - `main_program.f`
    - `function_program.f`
    - `subroutine_program.f`
  - BLOCK DATA subprograms and type declarations.
- FORTRAN IV data types:
  - LOGICAL, DOUBLE PRECISION, COMPLEX declarations and literals.
- Logical expressions and IF:
  - `logical_expr`, `logical_literal`, `relational_expr`,
    `logical_if_stmt`.

The fixture harness (`tests/test_fixture_parsing.py`) marks several
FORTRAN 66 fixtures as XPASS, indicating that they still produce
syntax errors under the generic entry rule:

- `FORTRAN66/test_fortran66_parser/first_standard_demo.f`
- `FORTRAN66/test_fortran66_parser/function_program.f`
- `FORTRAN66/test_fortran66_parser/main_program.f`
- `FORTRAN66/test_fortran66_parser/standard_program.f`
- `FORTRAN66/test_fortran66_parser/subroutine_program.f`

The XPASS reason strings describe these fixtures as:

- “more ambitious than the simplified grammar”.
- exercising constructs “only partially modeled”.
- representing full standard programs that still yield syntax errors.

This confirms that:

- The targeted tests validate key FORTRAN 66/IV features in isolation.
- The integrated, whole-program behavior for richer examples still has
  gaps relative to the standard.

## 7. Summary

The FORTRAN 66 grammar in this repository:

- Standardizes program units (main, subprograms, BLOCK DATA) on top of
  the FORTRAN II base.
- Extends the type system with LOGICAL, DOUBLE PRECISION and COMPLEX
  and provides explicit type declarations.
- Implements logical and relational expressions and logical IF in
  addition to arithmetic IF.
- Retains the full FORTRAN II–style statement family for DIMENSION,
  EQUIVALENCE, COMMON, FORMAT, and basic I/O.
- Uses a layout‑lenient fixed-form model, without enforcing strict
  80‑column semantics.
- Does **not** yet implement several standard FORTRAN 66 statements
  and declarations such as DATA, EXTERNAL, INTRINSIC, and sequential
  I/O control (`REWIND`, `BACKSPACE`, `ENDFILE`).
- Still rejects some richer, spec-inspired fixtures, which are
  tracked as XPASS in the generic fixture harness.

Future work should:

- Add explicit grammar rules and tests for the missing standard
  statements (DATA, EXTERNAL, INTRINSIC, REWIND/BACKSPACE/ENDFILE) if
  full FORTRAN 66 coverage is desired.
- Align the generic fixture parser entry rule and expectations for
  FORTRAN 66 with the dedicated `fortran66_program` rule.
- Use the XPASS fixtures as a concrete checklist for closing the
  remaining gaps.

