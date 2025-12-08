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
  - Inherited from `FORTRAN66Parser.g4` as a `statement_list`. The
    `PROGRAM` keyword is provided by the lexer, but there is no
    dedicated `program_stmt` rule in `FORTRAN77Parser.g4`.
  - Effect: programs can still be written without an explicit PROGRAM
    line; explicit `PROGRAM name` is lexed but not modeled as a
    distinct statement form.

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

- `substring_range`:
  - `IDENTIFIER(L:U)` with integer expressions for start/end.

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

Note: Concatenation (`//`) and substring (`(start:end)`) operations
work when parsed via the `character_expr` rule directly (as in the
unit tests), but the `main_program` entry rule's `assignment_stmt`
uses `expr` which does not yet include `character_expr`. This is a
grammar integration gap, not a test gap.

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
  structured IFs, but note that:
  - In `tests/test_fixture_parsing.py`, the same fixture is marked
    XPASS for FORTRAN 77, indicating that when parsed via the generic
    fixture harness (using `main_program` as entry rule), it still
    produces syntax errors.

Interpretation:

- The block IF machinery is implemented and passes targeted unit tests.
- Some realistic nested IF examples in the fixture suite still fail
  when parsed as whole programs, indicating integration or entry-rule
  issues.

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
    `data_constant_list`, `data_constant`) for initializing variables.
  - Included twice in `statement_body` (a harmless duplication).

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
  - Integration gap:
    - The `nested_if_block.f` fixture is still XPASS when parsed
      through the generic harness with `main_program` as entry, as
      tracked by issue #166 and the earlier meta issue #145.

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
  - Missing:
    - Although `OPEN`, `CLOSE`, `INQUIRE`, `BACKSPACE`, and `REWIND`
      tokens exist in `FORTRAN77Lexer.g4`, there are no corresponding
      statement rules (`open_stmt`, `close_stmt`, `inquire_stmt`,
      `rewind_stmt`, `backspace_stmt`, `endfile_stmt`) in
      `FORTRAN77Parser.g4`, and `statement_body` does not mention
      them. There is no ENDFILE token or rule at all.
    - Any standard‑conforming program that uses these file‑control
      statements will be rejected or see them as unknown tokens.
    - This is tracked in issue #163.

- **CALL, RETURN, END**
  - Implemented:
    - `call_stmt`, `return_stmt`, and `end_stmt` are inherited and
      wired into `statement_body`.

- **PROGRAM, FUNCTION, SUBROUTINE, ENTRY, BLOCK DATA**
  - Implemented / partially implemented:
    - FUNCTION, SUBROUTINE and BLOCK DATA are supported through the
      imported FORTRAN II/66 rules (`function_subprogram`,
      `subroutine_subprogram`, `block_data_subprogram`).
    - `PROGRAM` exists as a token in `FORTRAN77Lexer.g4`, but there
      is no `program_stmt` rule and no explicit program‑unit wrapper
      for “PROGRAM name … END”.
    - ENTRY is not modeled at all: no `ENTRY` token or `entry_stmt`
      rule.
    - The top‑level entry rule remains the FORTRAN 66 main program
      (`main_program : statement_list`).
    - These gaps are tracked by issue #164.

- **DIMENSION, COMMON, EQUIVALENCE, IMPLICIT, PARAMETER, EXTERNAL, INTRINSIC, SAVE**
  - Implemented:
    - DIMENSION, COMMON, EQUIVALENCE, type statements, DATA and
      FORMAT are inherited from FORTRAN 66 and described in the 66
      audit.
    - SAVE, EXTERNAL, and INTRINSIC have dedicated rules:
      `save_stmt`, `external_stmt`, `intrinsic_stmt`, with tests
      proving basic usage.
  - Missing:
    - IMPLICIT and PARAMETER statements:
      - IMPLICIT is present only as the legacy token in FORTRAN I;
        there is no FORTRAN 77‑style `IMPLICIT` statement rule.
      - PARAMETER is defined as a token in `FORTRAN77Lexer.g4`, but
        there is no `parameter_stmt` rule in `FORTRAN77Parser.g4`.
      - Neither IMPLICIT nor PARAMETER are currently usable as
        nonexecutable specification statements.

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

- `FORTRAN77/test_fortran77_parser_extra/nested_if_block.f` is marked
  XPASS, with the explanation that it:
  - “exercises complex structured programming patterns that still
    cause {errors} syntax errors in the current FORTRAN 77 grammar.”

Thus:

- The core structured programming features work in targeted tests.
- At least one more complex nested IF example still fails when parsed
  as a standalone program via the fixture harness.

## 9. Summary

The FORTRAN 77 grammar in this repository:

- Extends FORTRAN 66 by:
  - Adding `CHARACTER` type and character expressions with
    concatenation and substrings.
  - Implementing structured `IF‑THEN‑ELSE`/`ELSEIF`/`ELSE`/`END IF`
    blocks.
  - Allowing floating-point DO loop control variables.
  - Adding SAVE, INTRINSIC, EXTERNAL and DATA declaration statements.
  - Enhancing WRITE with list-directed and format-identifier syntax.
- Retains the earlier FORTRAN 66 statements (including COMMON,
  DIMENSION, EQUIVALENCE, arithmetic/logical IFs and BLOCK DATA).
- Uses a layout‑lenient fixed-form model without strict 80‑column
  semantics.
- Does not yet model file I/O control statements (OPEN, CLOSE,
  INQUIRE, BACKSPACE, REWIND, ENDFILE) as full statements in the
  parser, even though some tokens exist.
- Still has at least one realistic nested IF program (nested_if_block.f)
  that fails under the generic fixture parser, indicating remaining
  integration gaps.

Future work should:

- Add parser rules and tests for OPEN/CLOSE/INQUIRE/BACKSPACE/
  REWIND/ENDFILE if full FORTRAN 77 coverage is desired.
- Resolve the XPASS nested IF fixture by refining the block IF
  constructs or adjusting the fixture expectations, and reflect any
  changes back into this audit.
