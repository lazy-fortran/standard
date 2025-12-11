# FORTRAN 66 (ANSI X3.9‑1966) – Grammar Audit

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

## ANSI X3.9-1966 Section Reference

The grammar files are annotated with section references to ANSI X3.9-1966:

| Standard Section | Description                        | Grammar Coverage |
|------------------|-----------------------------------|------------------|
| Section 3        | Program Form and Organization     | ✓ Implemented    |
| Section 4        | Data Types and Constants          | ✓ Implemented    |
| Section 5        | Variables, Arrays, and Subscripts | ✓ Implemented    |
| Section 6        | Expressions                       | ✓ Implemented    |
| Section 7.1      | Executable Statements             | ✓ Implemented    |
| Section 7.2      | Non-executable Statements         | ✓ Implemented    |
| Section 8        | Procedures                        | ✓ Implemented    |

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

**COMPLEX constants** (X3.9-1966 Section 4.4.2):

- `complex_literal : LPAREN complex_part COMMA complex_part RPAREN`
- `complex_part` restricts to signed integer and real literal parts.
- Examples: `(1.0, 2.0)`, `(3, -4)`, `(-1.5E2, +2.5E-1)`.

**Semantic Constraint Enforcement (X3.9-1966 Section 4.4.2)**:
- **Valid**: Both parts must be literal constants (e.g., `(1.0, 2.0)`, `(3, -4)`)
- **Invalid (rejected by grammar)**: Expressions, variables, or array references
  - Invalid examples: `(X, Y)` (variables), `(1+2, 3)` (expression), `(A(1), B(2))` (array)
- **Grammar enforcement**: `complex_part` restricts to `signed_real_literal` or `signed_int_literal`,
  rejecting non-literal tokens at the parse level.

**Tests** (`tests/FORTRAN66/test_fortran66_parser.py`):
- `test_complex_literals`: Valid complex constant forms as literals
- `test_complex_literals_in_assignment`: Complex constants in assignment statements
- `test_complex_literals_in_data_statement`: Complex constants in DATA statements
- `test_complex_literals_invalid_forms`: Documents rejection of invalid forms (variables, expressions, array refs)

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

## 4. Statement coverage versus ANSI X3.9‑1966

Section 7 of the FORTRAN 66 standard (“Statements”, pp. 12ff in
`validation/pdfs/FORTRAN66_ANSI_X3.9-1966.txt`) classifies:

- Executable statements (7.1):
  - Assignment statements:
    - Arithmetic assignment.
    - Logical assignment.
    - GO TO assignment (`ASSIGN k TO i`).
  - Control statements (7.1.2):
    - Unconditional GO TO.
    - Assigned GO TO.
    - Computed GO TO.
    - Arithmetic IF.
    - Logical IF.
    - CALL.
    - RETURN.
    - CONTINUE.
    - Program control statements:
      - STOP (with octal code).
      - PAUSE (with octal code).
    - DO.
  - Input/output statements (7.1.3):
    - READ / WRITE (formatted and unformatted).
    - Auxiliary I/O:
      - REWIND.
      - BACKSPACE.
      - ENDFILE.
- Non‑executable statements (7.2):
  - Type statements (`INTEGER`, `REAL`, `DOUBLE PRECISION`, `COMPLEX`,
    `LOGICAL`).
  - DIMENSION.
  - COMMON.
  - EQUIVALENCE.
  - DATA.
  - FORMAT.
  - EXTERNAL.
  - INTRINSIC.
  - Statement function statements.
  - Others related to program unit classification.

Mapping these families to the current grammar:

- **Assignment statements**
  - Arithmetic assignment:
    - Implemented via `assignment_stmt : variable EQUALS expr` inherited
      from FORTRAN II; used for INTEGER, REAL, DOUBLE PRECISION and
      COMPLEX variables.
  - Logical assignment:
    - Implemented: logical variables participate in `assignment_stmt`
      and `logical_expr`; tests cover logical variables and literals.
  - GO TO assignment (`ASSIGN k TO i`):
    - Implemented:
      - Lexer: `ASSIGN` and `TO` tokens exist in `FORTRANLexer.g4`
        (inherited through the lexer chain).
      - Parser: `assign_stmt : ASSIGN label TO variable` rule defined
        in `FORTRAN66Parser.g4` per X3.9-1966 Section 7.1.1.3.
      - `statement_body` includes `assign_stmt` as an alternative.
      - Tests: `test_assign_statement`, `test_assign_in_statement_body`,
        and `test_assign_goto_fixture` in
        `tests/FORTRAN66/test_fortran66_parser.py`.
    - **NON-COMPLIANT with ISO/IEC 1539-1:2018 (Fortran 2018):**
      - ASSIGN is a deleted feature per Annex B.2.
      - Retained for historical FORTRAN 66 accuracy only.

- **Control statements**
  - Implemented:
    - Unconditional GO TO: `goto_stmt : GOTO label`.
    - Computed GO TO: `computed_goto_stmt : GOTO LPAREN label_list RPAREN COMMA expr`.
    - Assigned GO TO (`GO TO i, (k1, k2, ...)`):
      - `assigned_goto_stmt : GOTO variable COMMA LPAREN label_list RPAREN`
        defined in `FORTRAN66Parser.g4` per X3.9-1966 Section 7.1.2.1.2.
      - `statement_body` includes `assigned_goto_stmt` as an alternative.
      - Tests: `test_assigned_goto_statement`, `test_assigned_goto_in_statement_body`,
        and `test_assign_goto_fixture` in
        `tests/FORTRAN66/test_fortran66_parser.py`.
      - **NON-COMPLIANT with ISO/IEC 1539-1:2018 (Fortran 2018):**
        - Assigned GO TO is a deleted feature per Annex B.2.
        - Retained for historical FORTRAN 66 accuracy only.
    - Arithmetic IF: `arithmetic_if_stmt : IF (expr) l1, l2, l3`.
    - Logical IF: `logical_if_stmt : IF (logical_expr) statement_body`.
    - CALL: `call_stmt : CALL IDENTIFIER (LPAREN expr_list? RPAREN)?`.
    - RETURN: `return_stmt : RETURN`.
    - CONTINUE: `continue_stmt : CONTINUE`.
    - DO: `do_stmt` inherited from FORTRAN II, with integer control
      variable and label.
    - STOP / PAUSE:
      - Implemented as `stop_stmt` and `pause_stmt` rules inherited
        from FORTRAN II, with `INTEGER_LITERAL?` argument (fixes #400).
      - X3.9-1966 Section 7.1.2.5 specifies that codes must be 1-5 octal
        digits (0-7 only).
      - **Known limitation**: Parser accepts any INTEGER_LITERAL for
        simplicity. Full octal validation (rejecting codes with digits 8-9
        or >5 digits) would require semantic validation beyond ANTLR
        parser capabilities. A semantic checker could enforce this
        constraint if needed.

- **Input/output statements**
  - Implemented:
    - READ / WRITE:
      - `read_stmt` inherited from FORTRAN II implements the
        "READ (u, f) list" form using `input_list`.
      - `write_stmt` implemented in FORTRAN66Parser.g4 per X3.9-1966
        Section 7.1.3.1 implements the "WRITE (u, f) list" form using
        `output_list`; tests cover various forms and a comprehensive
        fixture (`write_stmt.f`).
    - PRINT, PUNCH:
      - Implemented via `print_stmt` and `punch_stmt`.
    - FORMAT:
      - Implemented as `format_stmt` with `format_specification`,
        `format_item` and `format_descriptor`, plus the `HOLLERITH`
        token for Hollerith constants; the FORMAT grammar is shared
        with FORTRAN II.
      - **HOLLERITH CONSTANTS (X3.9-1966 Section 4.7)**:
        - Syntax: `nHxxx` where n is a positive integer and xxx are
          exactly n characters (Hollerith format specification).
        - Lexer: `HOLLERITH : [1-9] [0-9]* H ~[,)\r\n]*` rule in
          `FORTRANLexer.g4` recognizes Hollerith tokens but does NOT
          validate that declared count matches actual character count.
        - Semantic validation: `tools/hollerith_validator.py` provides
          `validate_hollerith()` and related functions to check count
          compliance per X3.9-1966 Section 4.7.
        - Known limitation: Lexer uses greedy matching and accepts
          mismatched Hollerith (e.g., `5HHELL` with 4 chars). A semantic
          validation pass must be applied post-parse to enforce strict
          X3.9-1966 compliance.
        - Tests: `tests/FORTRAN66/test_fortran66_parser.py` includes
          `test_hollerith_constants_valid` and
          `test_hollerith_constants_invalid_count`; detailed validator
          unit tests in `tests/test_hollerith_validator.py`.
        - Acceptance: All valid Hollerith per X3.9-1966 parse correctly.
          Invalid Hollerith (count mismatches) parse but can be detected
          via semantic validation utility.
  - Implemented:
    - REWIND, BACKSPACE, ENDFILE:
      - Per X3.9-1966 Section 7.1.3.3, these auxiliary I/O statements
        control sequential file positioning.
      - Lexer: `REWIND`, `BACKSPACE`, `ENDFILE` tokens defined in
        `FORTRAN66Lexer.g4`.
      - Parser: `rewind_stmt`, `backspace_stmt`, `endfile_stmt` rules
        defined in `FORTRAN66Parser.g4`, wired into `statement_body`.
      - Syntax: `REWIND u`, `BACKSPACE u`, `ENDFILE u` where u is an
        unsigned integer expression identifying the I/O unit.
      - Tests: covered by `test_rewind_statement`, `test_backspace_statement`,
        `test_endfile_statement`, `test_auxiliary_io_in_statement_body`,
        and `test_auxiliary_io_fixture` in
        `tests/FORTRAN66/test_fortran66_parser.py`.

- **Non‑executable (declarative) statements**
  - Implemented:
    - Type statements:
      - Via `type_declaration : type_spec variable_list` with
        `type_spec` = `INTEGER | REAL | LOGICAL | DOUBLE PRECISION | COMPLEX`.
    - DIMENSION:
      - Implemented by the inherited `dimension_stmt` / `array_declarator`
        and `dimension_list` rules.
      - Syntax: `DIMENSION array-declarator, array-declarator, ...` where
        `array-declarator` = `IDENTIFIER (dimension-list)`.
      - Dimensions may be:
        - Single bound: `d` (implicit 1 to d)
        - Explicit bounds: `d1:d2` (lower bound d1 to upper bound d2)
      - **SEMANTIC CONSTRAINTS (X3.9-1966 Section 5.3):**
        - `d` (single bound) must be a positive integer constant
        - `d2` (upper bound) must be positive (> 0)
        - `d1` (lower bound) may be any signed integer constant (negative values allowed)
        - `d2` must be >= `d1`
        - Parser accepts syntax; constraint validation requires semantic pass
      - Tests: covered by `test_dimension_statement_simple_bounds`,
        `test_dimension_statement_explicit_bounds`,
        `test_dimension_statement_multiple_declarators`,
        `test_dimension_bounds_semantic_constraint_zero_upper`,
        `test_dimension_bounds_in_typed_declaration`,
        `test_dimension_bounds_variable_list_subscripting`, and
        `test_dimension_statement_with_negative_bounds` in
        `tests/FORTRAN66/test_fortran66_parser.py`.
      - **NON-COMPLIANT with X3.9-1966 Section 5.3:**
        - Parser currently accepts violations like:
          - `DIMENSION A(0)` (zero upper bound)
          - `DIMENSION B(-5)` (negative single bound)
          - `DIMENSION C(10, 5)` (d2 < d1)
        - These would require semantic validation to reject properly
        - See issue #403 for tracking semantic validation work
    - EQUIVALENCE:
      - Implemented by `equivalence_stmt` / `equivalence_set`.
    - COMMON:
      - Implemented by `common_stmt : COMMON (SLASH IDENTIFIER SLASH)? variable_list`.
      - Extended: as with FORTRAN II, named COMMON blocks are accepted,
        which matches FORTRAN 66 but also allows more flexible usage
        than the strict wording of some implementations.
    - FORMAT:
      - Implemented as described above.
    - Statement functions:
      - Parser: `statement_function_stmt` rule implemented in
        `FORTRANIIParser.g4` (inherited by FORTRAN66Parser.g4) per
        X3.9-1966 Section 7.2.
      - Syntax: `name(dummy-arg-list) = expression` where dummy-arg-list
        is a non-empty comma-separated list of identifier dummy arguments.
      - Wired into `statement_body` as a distinct alternative from
        `assignment_stmt`, enabling syntactic differentiation.
      - Tests: covered by `test_statement_function_simple`,
        `test_statement_function_multiple_args`,
        `test_statement_function_complex_expr`,
        `test_statement_function_in_statement_body`, and
        `test_statement_function_fixture` in
        `tests/FORTRAN66/test_fortran66_parser.py`.
      - Semantic constraint: statement functions must appear in the
        specification part (after declarations, before executable
        statements); this constraint is not enforced syntactically.
    - DATA:
      - Lexer: `DATA` token defined in `FORTRAN66Lexer.g4`.
      - Parser: `data_stmt` rule implemented in `FORTRAN66Parser.g4`
        per X3.9-1966 Section 7.2, wired into both `statement_body`
        and `data_initialization_body` (for BLOCK DATA).
      - Syntax: `DATA nlist /clist/, nlist /clist/, ...` where:
        - `nlist` = list of variables, array elements, or implied DO loops
        - `clist` = list of constants with optional repeat count (e.g., `3*0.0`)
      - Supports: simple variables, array elements, implied DO loops,
        signed constants, and repeat counts.
      - Tests: covered by `test_data_statement_simple`,
        `test_data_statement_repeat_count`, `test_data_statement_multiple_sets`,
        `test_data_statement_signed_constants`, `test_data_statement_array_elements`,
        `test_data_statement_implied_do`, `test_data_statement_in_statement_body`,
        `test_data_statement_fixture`, and `test_data_in_block_data` in
        `tests/FORTRAN66/test_fortran66_parser.py`.
    - EXTERNAL / INTRINSIC:
      - Lexer: `EXTERNAL` and `INTRINSIC` tokens exist.
      - Parser: `external_stmt` and `intrinsic_stmt` rules are
        implemented in `FORTRAN66Parser.g4` per X3.9-1966 Section 7.2.
        These are wired into `statement_body` as non-executable
        declaration statements.
      - Syntax: `EXTERNAL name, name, ...` and `INTRINSIC name, name, ...`
      - Tests: covered by `test_external_statement`, `test_intrinsic_statement`,
        `test_external_intrinsic_in_program`, and `test_external_intrinsic_fixture`
        in `tests/FORTRAN66/test_fortran66_parser.py`.

## 5. Fixed-form model

`docs/fixed_form_support.md` describes the Fortran 66 fixed-form
handling as:

- Layout‑lenient grammar:
  - Statements are parsed according to token sequence, not strict
    column positions.
  - No enforcement of exact 80‑column semantics or sequence numbers.
  - This makes the grammar suitable for analyzing typical FORTRAN 66 code
    without reproducing exact card-image behavior.
- Strict fixed-form validation (optional):
  - The `tools/strict_fixed_form.py` preprocessor provides optional
    strict ANSI X3.9-1966 Section 3.3 fixed-form validation and conversion
    per issue #394.
  - Supports card-image validation: columns 1-5 (labels), column 6
    (continuation), columns 7-72 (statement), columns 73-80 (sequence).
  - Dialect "66" validates labels (1-99999), comment markers (C/*),
    and continuation rules.
  - Provides conversion from strict to layout-lenient form for parsing.
  - Test coverage: `tests/FORTRAN66/test_strict_fixed_form_66.py`
    includes 22 test cases for card parsing, validation, and conversion.
- Comments:
  - Classic column‑1 `C`/`*` forms are recognized as comment cards
    (per X3.9-1966 Section 3.3).
  - Sequence fields (columns 73-80) are recognized but not validated
    (historical context only).

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

Fixtures that fail are marked as xfail with explicit issue references
(see issue #311 for Fortran 90 grammar gaps that affect FORTRAN 66
compatibility).

The gaps identified represent grammar limitations tracked in the
crosswalk table above. Resolution requires implementing the missing
ANSI X3.9-1966 statement forms.

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
- Implements `EXTERNAL` and `INTRINSIC` declaration statements per
  X3.9-1966 Section 7.2.
- Implements the WRITE statement per X3.9-1966 Section 7.1.3.1 with
  syntax `WRITE (unit, format) [output-list]`, supporting expressions
  and optional output list (empty writes are also accepted).
- Implements auxiliary I/O statements (`REWIND`, `BACKSPACE`, `ENDFILE`)
  per X3.9-1966 Section 7.1.3.3.
- Implements GO TO assignment (`ASSIGN k TO i`) and assigned GO TO
  (`GO TO i, (k1, k2, ...)`) per X3.9-1966 Sections 7.1.1.3 and 7.1.2.1.2.
  (NON-COMPLIANT: both are deleted features per ISO/IEC 1539-1:2018 Annex B.2)
- Implements the DATA statement for compile-time initialization per
  X3.9-1966 Section 7.2, including:
  - Simple variable and array element initialization
  - Implied DO loops for array initialization
  - Repeat counts for constant values
  - Integration with BLOCK DATA subprograms
- Still rejects some richer, spec-inspired fixtures, which are
  tracked as XPASS in the generic fixture harness.

Future work should:

- Align the generic fixture parser entry rule and expectations for
  FORTRAN 66 with the dedicated `fortran66_program` rule.
- Use the XPASS fixtures as a concrete checklist for closing the
  remaining gaps.
