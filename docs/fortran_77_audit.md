# FORTRAN 77 (ANSI X3.9‑1978) – Grammar Audit (status: in progress)

This document summarizes the **FORTRAN 77** grammar in this repository,
based on:

- `grammars/FORTRAN77Lexer.g4`, `grammars/FORTRAN77Parser.g4`
- Inherited grammars (`FORTRAN66Lexer.g4`, `FORTRAN66Parser.g4`)
- `docs/fixed_form_support.md`
- Tests under `tests/FORTRAN77/`
- XPASS fixtures in `tests/test_fixture_parsing.py`

It is descriptive of the current implementation, not a formal claim of
full conformance to the ANSI X3.9‑1978 standard.

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
- `test_character_string_processing` contains stubbed tests for
  `'HELLO'` and `'HELLO' // ' WORLD'` but currently passes (i.e. does
  nothing) because of earlier concerns about `STRING_LITERAL`. These
  can be turned into real parser tests now that the lexer rule exists.

Limitations:

- The generic fixture parser (`tests/test_fixture_parsing.py`) does not
  yet exercise complex character expressions in FORTRAN 77 fixtures.
  Only targeted unit tests in `tests/FORTRAN77` cover this area.

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

## 6. I/O enhancements

In addition to inherited READ/PRINT/PUNCH and FORMAT statements,
FORTRAN 77 specifies:

- List-directed and formatted WRITE.
- Enhanced file I/O: `OPEN`, `CLOSE`, `INQUIRE`, `BACKSPACE`,
  `REWIND`, `ENDFILE`.

Grammar coverage:

- `write_stmt`:
  - `WRITE (control_info_list) output_item_list?`.
  - Supports:
    - Unit number: an `integer_expr`.
    - Format identifier:
      - Label (FORMAT statement label).
      - Character expression (quoted format).
      - `*` for list-directed I/O.
  - Output list can include implied DO loops.

- `OPEN`, `CLOSE`, `INQUIRE`, `BACKSPACE`, `REWIND`:
  - Tokens exist in `FORTRAN77Lexer.g4`, **but there are no parser
    rules** for `OPEN`/`CLOSE`/`INQUIRE`/`BACKSPACE`/`REWIND` in
    `FORTRAN77Parser.g4`, and they are not referenced in
    `statement_body`.
  - There is no `ENDFILE` token or statement rule at all.

Implications:

- The core WRITE statement and list-directed `WRITE(*,*)` style I/O
  are modeled syntactically.
- File-oriented I/O control statements (`OPEN`, `CLOSE`, `INQUIRE`,
  `BACKSPACE`, `REWIND`, `ENDFILE`) are only partially or not at all
  represented:
  - Code using these statements will either be rejected or treated as
    unknown tokens by the parser.

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
- Turn the `test_character_string_processing` stubs into real tests
  and add more fixtures with CHARACTER expressions and strings.
- Resolve the XPASS nested IF fixture by refining the block IF
  constructs or adjusting the fixture expectations, and reflect any
  changes back into this audit.

