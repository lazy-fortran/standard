# FORTRAN 77 (ANSI X3.9‑1978) – Grammar Audit (status: in progress)

This document summarizes the **FORTRAN 77** grammar in this repository
and compares it against the ISO/ANSI FORTRAN 77 language as described
in the local copy of the standard:

- `validation/pdfs/FORTRAN77_ISO_1539-1980.html` (HTML text of the
  ANSI X3J3/90.4 “Full Language” document, including Section 7
  “Executable and Nonexecutable Statement Classification”).

The implementation view is based on:

- `grammars/FORTRAN77Lexer.g4`, `grammars/FORTRAN77Parser.g4`
- Inherited grammars (`FORTRAN66Lexer.g4`, `FORTRAN66Parser.g4`)
- `docs/fixed_form_support.md`
- Tests under `tests/FORTRAN77/`
- XPASS fixtures in `tests/test_fixture_parsing.py`

It is descriptive of the current implementation, not a formal claim of
full conformance to ISO 1539:1980 / ANSI X3.9‑1978.

## 1. Program units and structure

Specification-wise, FORTRAN 77 retains the program unit forms of
FORTRAN 66:

- Main program (with optional `PROGRAM` statement).
- External subroutine and function subprograms.
- BLOCK DATA subprograms.

In this grammar:

- `main_program`
  - Redefined in `FORTRAN77Parser.g4` to allow an optional `PROGRAM`
    statement followed by a sequence of statements and a terminating
    `END` statement.
  - Effect: programs can be written with or without an explicit
    `PROGRAM name` line; when present, it is modeled explicitly by
    `program_stmt`.

- `program_stmt`
  - New in `FORTRAN77Parser.g4` for FORTRAN 77:
    `PROGRAM IDENTIFIER` at the start of a main program unit.

- `subroutine_subprogram` / `function_subprogram`
  - Retained from FORTRAN II/66 via the imported parser.
  - Type specifications for functions are extended by the new
    `type_spec` rule, which now includes `CHARACTER`.

- `BLOCK DATA`
  - Still supported via the FORTRAN 66 `block_data_subprogram` rule.
  - There is no FORTRAN 77–specific extension here; the same
    `BLOCKDATA` token and `block_data_subprogram` structure applies.

There are no new program-unit forms beyond those inherited from
FORTRAN 66; the key structural innovation is the IF‑THEN‑ELSE block
within execution parts.

## 2. Type system and CHARACTER support

FORTRAN 77 extends the FORTRAN 66 type system mainly by adding the
`CHARACTER` type with length specifications and character expressions.

In this grammar:

- `type_spec` (redefined in `FORTRAN77Parser.g4`) accepts:
  - `INTEGER`, `REAL`, `LOGICAL`, `DOUBLE PRECISION`, `COMPLEX`
    (inherited from FORTRAN 66/IV).
  - `CHARACTER character_length?` (new in FORTRAN 77).

**COMPLEX constants** (inherited from FORTRAN 66/IV, ISO 1539:1980
Section 4.4.2):

- `complex_literal : LPAREN complex_part COMMA complex_part RPAREN`
- `complex_part` supports signed integer and real parts.
- Examples: `(1.0, 2.0)`, `(3, -4)`, `(-1.5E2, +2.5E-1)`.
- Tests (`tests/FORTRAN77/test_fortran77_parser.py`):
  - `test_complex_literals_fortran77` covers all forms of complex constants
    as literals (passing individual `(r,i)` forms to the `literal` rule).
  - `test_complex_literals_in_assignment_fortran77` exercises complex
    constants in assignment statements (assignment to variables with
    complex-constant RHS).
  - `test_complex_literals_in_data_statement_fortran77` exercises complex
    constants in DATA statements.

- `character_length` supports:
  - `CHARACTER*10`
  - `CHARACTER*(* )`
  - `CHARACTER*(LEN)` via integer expressions in the parentheses.

- Type declarations:
  - `type_declaration : type_spec variable_list`.
  - Retained from FORTRAN 66 and extended to support CHARACTER.

Tests (`tests/FORTRAN77/test_fortran77_parser.py`):

- `test_character_data_type` exercises `CHARACTER`, `CHARACTER*10`,
  and `CHARACTER*(*)` forms.
- `test_proper_inheritance` checks that `type_declaration` still
  accepts INTEGER, REAL, LOGICAL, COMPLEX, and CHARACTER declarations,
  and that the ANTLR limitation comment is present.

## 3. Character expressions and string processing

FORTRAN 77 adds CHARACTER expressions with concatenation and substring
operations.

Grammar coverage:

- `character_expr`:
  - `character_operand (CONCAT character_operand)*`, where `CONCAT` is
    the `//` operator.

- `character_primary`:
  - Character literal constants (`STRING_LITERAL` or `HOLLERITH`).
  - Character variables and substrings.
  - Character function references.
  - Parenthesized character expressions.

- `literal`:
  - Accepts `HOLLERITH` constants per ISO 1539:1980 Section 4.8.1, which lets these legacy text constants appear inside expressions, assignment RHSs, and actual arguments (issue #589).

- `substring_range`:
  - `IDENTIFIER(L:U)` with integer expressions for start/end.
  - ISO 1539:1980 Section 5.7 allows either bound to be omitted, enabling
    `(:5)`, `(1:)`, and `(:)` forms. The grammar models these optional bounds
    through `substring_range`'s `substr_start_expr?` / `substr_end_expr?`
    pairing.

Lexer:

- `STRING_LITERAL` is defined as a single-quoted string with doubled
  quotes for embedded `'`.
- `CONCAT` token recognizes `//`.

Tests:

- `test_character_expressions` parses simple CHARACTER expressions such
  as `HELLO` and `FIRST // SECOND` using the `character_expr` rule.
- `test_character_string_processing` now parses STRING_LITERAL
  expressions including `'HELLO'`, `'HELLO' // ' WORLD'`,
  `'IT''S A TEST'` (doubled apostrophes), and multi-operand
  concatenations.
- `test_hollerith_literal` and `test_call_statement_with_hollerith_argument`
  verify that the `literal` rule accepts `HOLLERITH` tokens and that
  `call_stmt` can receive them as actual arguments. These tests rely on
  the `tests/fixtures/FORTRAN77/test_fortran77_parser/hollerith_call_argument.f`
  fixture to cover a realistic subroutine call scenario (issue #589).
- `test_character_substring_operations` exercises substring syntax
  `NAME(1:5)`, `STR(I:J)`, and open-ended forms like `LINE(1:)`.
- `test_character_concatenation_combinations` tests mixed-operand
  concatenations such as `'PREFIX' // NAME` and `NAME(1:5) // SUFFIX`.
- `test_character_function_references` verifies character function
  references like `TRIM(NAME)` and `FMT(X, Y, Z)`.
- `test_string_literal_edge_cases` covers edge cases including empty
  strings `''`, whitespace-only `' '`, multiple embedded apostrophes
  `'A''B''C'`, and compound concatenations.
- `test_character_expression_fixture` parses a comprehensive fixture
  file `character_expressions.f` that combines character declarations,
  assignments, concatenations, substrings, and I/O operations.

Fixture coverage:

- `tests/fixtures/FORTRAN77/test_fortran77_parser/character_expressions.f`
  exercises character constants, embedded apostrophes (doubled quotes),
  character declarations, character assignment, and character
  expressions in I/O and IF constructs per ANSI X3.9-1978.

- **Substring assignment integration (ISO 1539:1980 Section 10.3)**
  - `assignment_target` extends the base `variable` rule to allow substring
    references (`IDENTIFIER` optionally followed by subscripts and
    `substring_range`) on the left-hand side, covering `NAME(1:5)` and
    `ARRAY(I)(1:)`, including omitted bounds.
  - `assignment_rhs` accepts both `character_expr` and `expr`, letting
    character/substring expressions and numeric/logical expressions share the
    same assignment statement syntax.
  - `tests/fixtures/FORTRAN77/test_fortran77_parser/substring_assignment.f`
    exercises the new capability, including:
    `NAME(1:5) = 'JANE '`, `NAME = CITY(1:5)`, and omitted-bound writes like
    `NAME(:5) = 'HELLO'` and `NAME(10:) = 'WORLD'`.
  - This resolves issue #466, which tracked the previous gap where
    `assignment_stmt` lacked full character-substring support and only parsed
    `expr` on the RHS. The new fixture and parser rules verify the correction.

## 4. Structured IF and DO constructs

FORTRAN 77’s key innovation is structured programming via block IF and
enhanced DO loops.

### 4.1 Block IF–THEN–ELSE

Grammar:

- `block_if_construct`:
  - `if_then_stmt execution_part_construct* else_if_part* else_part? end_if_stmt`.
  - Supports nested and multi-branch IF/ELSE IF/ELSE blocks.

- `if_then_stmt`, `else_if_stmt`, `else_stmt`, `end_if_stmt`:
  - `IF (logical_expr) THEN`.
  - `ELSE IF (logical_expr) THEN`.
  - `ELSE`.
  - `END IF` or `ENDIF`.

- `execution_part_construct` and `executable_construct`:
  - Define what can appear inside an IF block: assignments, GOTOs,
    arithmetic/logical IFs, calls, returns, STOP/PAUSE/CONTINUE, I/O,
    DO loops and nested block IF constructs.

Tests:

- `test_if_then_else_construct` exercises:
  - Simple IF/THEN.
  - IF/THEN/ELSE.
  - IF/THEN/ELSE IF/ELSE with nested branches.

- `test_fortran77_revolution_features` uses the fixture
  `test_fortran77_parser_extra/nested_if_block.f` to stress nested
  structured IFs, and it passes both targeted tests and the generic
  fixture harness using the `main_program` entry rule.

Interpretation:

- The block IF machinery is fully implemented and passes all tests.
- Nested IF examples in the fixture suite parse correctly as whole
  programs via the `main_program` entry rule.

### 4.2 Enhanced DO loops

Grammar:

- `do_stmt` redefined to allow:
  - Loop control variable:
    - `integer_variable` or `real_variable` (floating point DO
      index).
  - Initial, final, and increment expressions:
    - `integer_expr` or `real_expr`.

- `real_expr` and `real_variable` are syntactic placeholders
  (semantic constraints left to later tools).

Tests:

- `test_enhanced_do_loops` parses examples such as:
  - `DO 100 X = 1.0, 10.0, 0.5`.
  - `DO 200 I = 1, N`.

## 5. Declarations: SAVE, INTRINSIC, EXTERNAL, DATA

New and enhanced declaration statements in FORTRAN 77:

- `SAVE`:
  - Grammar: `save_stmt : SAVE (save_list)?`.
  - `save_list` can include variables or common block names (`/BLOCK/`).
  - Tests: `test_save_statement` exercises `SAVE`, `SAVE A, B, C`,
    `SAVE /COMMON_BLOCK/`.

- `INTRINSIC`:
  - Grammar: `intrinsic_stmt : INTRINSIC intrinsic_procedure_list`.
  - Tests: `test_intrinsic_statement` parses `INTRINSIC SIN, COS, TAN`
    and `INTRINSIC SQRT`.

- `EXTERNAL`:
  - Grammar: `external_stmt : EXTERNAL external_name_list`.
  - Tests: `test_external_statement` parses `EXTERNAL FUNC1, FUNC2`
    and `EXTERNAL MYSUB`.

- `DATA`:
  - Lexer: `DATA` token in `FORTRAN77Lexer.g4`.
  - Grammar: `data_stmt` and related rules (`data_stmt_set`,
    `data_variable_list`, `data_variable`, `implied_do_data`,
    `data_constant_list`, `data_constant`) for initializing variables.
  - Features:
    - Simple variable lists: `DATA A, B, C / values /` (via `data_variable`).
    - Implied-DO lists for array initialization: `DATA (A(I), I=1,10) / values /`
      (via `implied_do_data` rule, ISO 1539:1980 Section 9.2).
    - Nested implied-DO for matrix initialization: `DATA ((B(I,J), J=1,5), I=1,5) / values /`.
    - Repetition factors: `DATA A / 10*0.0 /` (via `data_repetition` rule,
      ISO 1539:1980 Section 9.3).
  - Included twice in `statement_body` (a harmless duplication).
  - Tests: `test_data_implied_do_single_fixture` and
    `test_data_implied_do_nested_fixture` in `tests/FORTRAN77/test_fortran77_parser.py`.

Statement coverage:

- `statement_body` in `FORTRAN77Parser.g4` includes:
  - All relevant FORTRAN 66 statements (assignment, GOTOs, arithmetic
    IF, logical IF, DO, CONTINUE, STOP, PAUSE, READ, PRINT, PUNCH,
    FORMAT, DIMENSION, EQUIVALENCE, COMMON, type declarations,
    RETURN, CALL).
  - Plus FORTRAN 77 additions: `block_if_construct`, `write_stmt`,
    `data_stmt`, `save_stmt`, `intrinsic_stmt`, `external_stmt`.

The core FORTRAN 77 declaration repertoire (SAVE, INTRINSIC, EXTERNAL,
DATA) is therefore present and exercised by tests.

## 6. Statement coverage versus FORTRAN 77 §7

Section 7 of the FORTRAN 77 “Full Language” document (see
`validation/pdfs/FORTRAN77_ISO_1539-1980.html`, around “7.1
Executable_Statements” and “7.2 Nonexecutable_Statements”) classifies:

- **Executable statements** (§7.1):
  1. Arithmetic, logical, statement label (ASSIGN), and character
     assignment statements.
  2. Unconditional GO TO, assigned GO TO, and computed GO TO
     statements.
  3. Arithmetic IF and logical IF statements.
  4. Block IF, ELSE IF, ELSE, and END IF statements.
  5. CONTINUE statement.
  6. STOP and PAUSE statements.
  7. DO statement.
  8. READ, WRITE, and PRINT statements.
  9. REWIND, BACKSPACE, ENDFILE, OPEN, CLOSE, and INQUIRE statements.
  10. CALL and RETURN statements.
  11. END statement.

- **Nonexecutable statements** (§7.2):
  1. PROGRAM, FUNCTION, SUBROUTINE, ENTRY, and BLOCK DATA
     statements.
  2. DIMENSION, COMMON, EQUIVALENCE, IMPLICIT, PARAMETER,
     EXTERNAL, INTRINSIC, and SAVE statements.
  3. INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL, and
     CHARACTER type‑statements.
  4. DATA statement.
  5. FORMAT statement.
  6. Statement function statement.

Mapping that classification to the current grammar:

- **Assignment statements (arithmetic, logical, character, label ASSIGN)**
  - Arithmetic and logical assignment:
    - Implemented by `assignment_stmt : variable EQUALS expr` and the
      FORTRAN 77 expression/logic machinery; arithmetic and logical
      variables are handled in the same way as in FORTRAN 66.
  - Character assignment:
    - Implemented syntactically via `assignment_stmt` when the LHS is
      a CHARACTER variable and the RHS is a `character_expr`. The
      grammar treats `expr` broadly; character semantics are left to
      later phases.
  - Statement label (ASSIGN) assignment:
    - Not implemented:
      - As with FORTRAN 66, there is no GO TO assignment rule
        (`ASSIGN k TO i`) and no statement‑label assignment form.
        This is tracked under the FORTRAN 66 GO TO‑assignment issue
        and is equally applicable to FORTRAN 77.

- **GO TO statements (unconditional, assigned, computed)**
  - Implemented:
    - Unconditional GO TO (`goto_stmt`) and computed GO TO
      (`computed_goto_stmt`) via inherited rules from FORTRAN 66.
  - Not implemented:
    - Assigned GO TO (`GO TO i, (k1, k2, ...)`) is not modeled. Only
      the computed GO TO form `GO TO (k1, k2, ...), i` exists. This
      is the same gap noted for FORTRAN 66 (issue #159).

- **Arithmetic IF and logical IF**
  - Implemented:
    - `arithmetic_if_stmt` and `logical_if_stmt` via the inherited
      FORTRAN 66 rules and `logical_expr`.

- **Block IF, ELSE IF, ELSE, END IF**
  - Implemented:
    - `block_if_construct` with `if_then_stmt`, `else_if_stmt`,
      `else_stmt`, and `end_if_stmt`, wired into
      `execution_part_construct` and `executable_construct`.
    - Targeted tests (including the nested IF fixture via
      `block_if_construct`) pass.
    - The `nested_if_block.f` fixture also passes when parsed through
      the generic harness with `main_program` as entry rule.

- **CONTINUE, STOP, PAUSE, DO**
  - Implemented:
    - `continue_stmt`, `stop_stmt`, `pause_stmt`, `do_stmt` are all
      present and exercised by tests.
  - Deviations:
    - The STOP/PAUSE octal‑digit restriction from the standard is not
      enforced; arguments are treated as generic integer expressions.

- **READ, WRITE, PRINT**
  - Implemented:
    - READ and PRINT are inherited from FORTRAN 66 (`read_stmt`,
      `print_stmt`).
    - Enhanced WRITE is implemented as `write_stmt` with:
      - A `control_info_list` including unit, format identifier, and
        list‑directed `*`.
      - `output_item_list` supporting implied DO constructs.

- **REWIND, BACKSPACE, ENDFILE, OPEN, CLOSE, INQUIRE**
  - Implemented:
    - `OPEN`, `CLOSE`, `INQUIRE`, `BACKSPACE`, `REWIND`, and `ENDFILE`
      tokens exist in `FORTRAN77Lexer.g4`.
    - Corresponding statement rules (`open_stmt`, `close_stmt`,
      `inquire_stmt`, `rewind_stmt`, `backspace_stmt`, `endfile_stmt`)
      are defined in `FORTRAN77Parser.g4` per ISO 1539:1980 Sections
      12.9 and 12.10.
    - All file I/O statements are wired into `statement_body` and
      `executable_construct`.
    - Tests: `test_file_io_statements_fixture` exercises positional
      and keyword-based forms for all file I/O statements.

- **CALL, RETURN, END**
  - Implemented:
    - `call_stmt`, `return_stmt`, and `end_stmt` are inherited and
      wired into `statement_body`.

- **PROGRAM, FUNCTION, SUBROUTINE, ENTRY, BLOCK DATA**
  - Implemented:
    - FUNCTION, SUBROUTINE and BLOCK DATA are supported through the
      imported FORTRAN II/66 rules (`function_subprogram`,
      `subroutine_subprogram`, `block_data_subprogram`).
    - PROGRAM statement:
      - Grammar: `program_stmt : PROGRAM IDENTIFIER NEWLINE`
      - The `main_program` rule is overridden to accept an optional
        `program_stmt` followed by statements and `end_stmt`.
      - Syntax: `PROGRAM name` at the start of a main program unit.
    - ENTRY statement:
      - Grammar: `entry_stmt : ENTRY IDENTIFIER entry_dummy_arg_list?`
      - Per ISO 1539:1980 Section 15.7, provides alternate entry points
        into FUNCTION or SUBROUTINE subprograms.
      - Supports dummy argument lists and alternate return specifiers (*).
      - Tests: `test_entry_statement` and `test_entry_statement_in_subroutine_fixture`
        exercise various ENTRY forms including no-argument, with arguments,
        and alternate return specifiers.

- **DIMENSION, COMMON, EQUIVALENCE, IMPLICIT, PARAMETER, EXTERNAL, INTRINSIC, SAVE**
  - Implemented:
    - DIMENSION, COMMON, EQUIVALENCE, type statements, DATA and
      FORMAT are inherited from FORTRAN 66 and described in the 66
      audit.
    - SAVE, EXTERNAL, and INTRINSIC have dedicated rules:
      `save_stmt`, `external_stmt`, `intrinsic_stmt`, with tests
      proving basic usage.
    - IMPLICIT statement:
      - Grammar: `implicit_stmt : IMPLICIT implicit_spec_list`
      - Supports type specification with letter ranges per ANSI X3.9-1978
        Section 8.4: `IMPLICIT INTEGER (I-N), REAL (A-H, O-Z)`
      - Tests: `test_implicit_statement` and `test_implicit_statement_in_program`
        exercise single and multiple type specs with letter ranges.
      - Note: IMPLICIT NONE is NOT supported (Fortran 90 feature).
    - PARAMETER statement:
      - Grammar: `parameter_stmt : PARAMETER LPAREN parameter_assignment_list RPAREN`
      - Supports named constant definitions per ANSI X3.9-1978 Section 8.5:
        `PARAMETER (PI = 3.14159, MAXSIZE = 100)`
      - Tests: `test_parameter_statement`, `test_parameter_with_expressions`,
        and `test_parameter_statement_in_program` exercise various forms.

- **DATA and FORMAT**
  - Implemented:
    - DATA: via `data_stmt` and related rules in
      `FORTRAN77Parser.g4`, and included in `statement_body` (twice,
      harmlessly).
    - FORMAT: via the inherited FORMAT machinery from FORTRAN 66.

- **Statement functions**
  - Implemented:
    - Parser: `statement_function_stmt` rule defined in
      `FORTRANIIParser.g4` (inherited through the grammar chain).
    - Syntax: `name(dummy-arg-list) = expression` where dummy-arg-list
      is a non-empty comma-separated list of identifier dummy arguments.
    - Wired into `statement_body` in FORTRAN77Parser.g4 as a distinct
      alternative, enabling syntactic differentiation from array
      element assignment.
    - Tests: covered by `test_statement_function_simple`,
      `test_statement_function_multiple_args`,
      `test_statement_function_in_statement_body`, and
      `test_statement_function_fixture` in
      `tests/FORTRAN77/test_fortran77_parser.py`.
    - Semantic constraint: per ANSI X3.9-1978 Section 8, statement
      functions must appear after specification statements and before
      executable statements; this constraint is not enforced at the
      syntax level.

## 7. Fixed-form handling

As documented in `docs/fixed_form_support.md`:

- The FORTRAN 77 grammar:
  - Uses a statement-oriented, layout‑lenient model:
    - Labels and statements are parsed by token stream, not column
      positions.
    - Sequence numbers and column‑6 continuation are not modeled.
  - Treats classic fixed-form comments (`C`/`*` in column 1) as part
    of the fixed-form comment machinery inherited from earlier
    grammars, but without strict column enforcement.

This is sufficient for parsing typical F77 source but does not attempt
full card-image fidelity.

## 8. Tests and XPASS fixtures

The dedicated F77 tests in `tests/FORTRAN77/test_fortran77_parser.py`
cover:

- CHARACTER type declarations and expressions.
- IF‑THEN‑ELSE constructs (including ELSEIF).
- Enhanced DO loops with floating-point control variables.
- SAVE, INTRINSIC, EXTERNAL and DATA statement usage (via
  `test_data_type_revolution_features` and others).
- A structured program example fixture:
  - `FORTRAN77/test_fortran77_parser/structured_program_example.f`.

In the generic fixture harness (`tests/test_fixture_parsing.py`):

- `FORTRAN77/test_fortran77_parser_extra/nested_if_block.f` passes
  when parsed via the `main_program` entry rule with zero syntax
  errors.

Thus:

- The core structured programming features work in both targeted tests
  and generic fixture parsing.
- Complex nested IF examples parse correctly as standalone programs.

## 9. Summary

The FORTRAN 77 grammar in this repository:

- Extends FORTRAN 66 by:
  - Adding `CHARACTER` type and character expressions with
    concatenation and substrings.
  - Implementing structured `IF‑THEN‑ELSE`/`ELSEIF`/`ELSE`/`END IF`
    blocks.
  - Allowing floating-point DO loop control variables.
  - Adding SAVE, INTRINSIC, EXTERNAL and DATA declaration statements.
  - Adding IMPLICIT statement for type specification by letter ranges
    per ANSI X3.9-1978 Section 8.4 (note: IMPLICIT NONE is a Fortran 90
    feature and is not supported).
  - Adding PARAMETER statement for named constant definitions per
    ANSI X3.9-1978 Section 8.5.
  - Enhancing WRITE with list-directed and format-identifier syntax.
- Retains the earlier FORTRAN 66 statements (including COMMON,
  DIMENSION, EQUIVALENCE, arithmetic/logical IFs and BLOCK DATA).
- Uses a layout‑lenient fixed-form model without strict 80‑column
  semantics.
- Models file I/O control statements (OPEN, CLOSE, INQUIRE,
  BACKSPACE, REWIND, ENDFILE) with full parser rules per ISO
  1539:1980 Sections 12.9 and 12.10.
- Nested IF programs (including nested_if_block.f) parse correctly
  under the generic fixture parser with zero syntax errors.
