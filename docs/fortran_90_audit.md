# Fortran 90 (1990) – Grammar Audit (status: in progress)

This audit describes what the **Fortran 90** grammar in this repository
implements and compares it against the Fortran 90 standard text we have
locally:

- WG5 N692 “Fortran 90” draft / ISO/IEC 1539:1991 (E), stored as
  `validation/pdfs/Fortran90_WG5_N692.txt`.

The implementation view is based on:

- `grammars/src/Fortran90Lexer.g4`, `grammars/src/Fortran90Parser.g4`
- `docs/fixed_form_support.md`
- `tests/Fortran90/test_fortran_90_comprehensive.py`
- XPASS fixtures recorded in `tests/test_fixture_parsing.py`

It is descriptive and spec‑aware, not a claim of full conformance.

## 1. Program units, modules and procedures

Specification (N692 §§2.2, 5.2.2, 5.5, 12):

- Program units: main program, external subprograms, modules, and
  block data program units.
- Internal procedures contained in a main program, an external
  subprogram or a module.

Grammar:

- Program unit entry:
  - `program_unit_f90` is the top‑level rule, covering:
    - Main program (`program_stmt` + body + `end_program_stmt`).
    - External subroutines/functions (`subroutine_subprogram`,
      `function_subprogram`).
    - Modules (`module`).
    - Block data program units (`block_data_subprogram` per ISO/IEC 1539:1991 R202, inherited from earlier standards).
- Modules:
  - `module` and `end_module_stmt` define a MODULE unit with:
    - `specification_part` (USE/import + declarations).
    - Optional `execution_part` and `internal_subprogram_part`.
  - `use_stmt` and `import_stmt` cover USE and IMPORT forms.
- External and internal procedures:
  - `function_subprogram` and `subroutine_subprogram` include:
    - A function/subroutine header (`function_stmt` / `subroutine_stmt`).
    - Optional `specification_part`, `execution_part`, and
      `internal_subprogram_part` (F90 internal procedures via
      `contains_stmt`).
- Interfaces and procedure statements:
  - `interface_block`, `interface_specification` and `interface_body`
    wire together generic interface declarations, procedure definitions,
    and the `procedure_stmt` entry point used inside interface blocks.
  - `procedure_stmt` now implements `MODULE PROCEDURE procedure-name-list`
    per ISO/IEC 1539:1991 Section 12.3.2.2 (R1206), driven by
    `F90ProcsParser.g4:procedure_stmt` and `procedure_name_list` (fixes
    #594).

Notable simplifications / gaps:

- **Specification-statement ordering validation (N692 §5.2.1)**: Detailed rules
  about which specification statements may appear in which program unit contexts
  and in which order (N692 §5.2.1) are now enforced via the semantic validator
  `tools/f90_specification_ordering_validator.py` (issue #672). The validator
  checks USE statement placement (must be first), IMPLICIT ordering, and
  context-specific constraints (e.g., INTENT/OPTIONAL in procedures only). Full
  implementation includes test suite with 23 test cases covering main programs,
  procedures, modules, and internal subprograms. Implementation status: Phase 1
  complete (main programs and procedures); future phases cover modules, block
  data, and advanced constraints.
- Separate module subprograms (introduced later) are not modeled; F90
  does not require them, so this is historically acceptable.

## 2. Free‑form and fixed‑form source

Specification (N692 §3) defines fixed and free source forms, with strict
rules for columns in fixed form and much more flexible free form.

Grammar / lexer (Fortran90Lexer.g4, `docs/fixed_form_support.md`):

- Free form:
  - `FREE_FORM_COMMENT` for `!` comments.
  - `CONTINUATION` for trailing `&` with optional comment.
  - Free‑form whitespace and case‑insensitive keywords are fully
    supported.
- Fixed form:
  - `FIXED_FORM_COMMENT` treats leading `C`/`c`/`*` as entire‑line
    comments.
  - There is **no** token‑level enforcement of:
    - Labels in columns 1–5.
    - Continuation in column 6.
    - Statement text in columns 7–72.
    - Sequence numbers in 73–80.

Implication:

- The F90 grammar uses a **layout‑lenient** fixed‑form model that is
  intentionally less strict than N692 §3. It is practical for parsing
  typical legacy F77/F90 code but not card‑accurate.

## 3. Types, derived types and declarations

Specification (N692 §§4–5):

- Intrinsic types with kind and length parameters.
- Derived types with TYPE/END TYPE, components and structure
  constructors.
- Specification statements for attributes like ALLOCATABLE, POINTER,
  TARGET, DIMENSION, INTENT, OPTIONAL, SAVE, EXTERNAL, INTRINSIC,
  PUBLIC, PRIVATE.

Grammar:

- Intrinsic type spec (Fortran90Parser.g4):
  - `intrinsic_type_spec_f90` handles:
    - `INTEGER (kind_selector)?`
    - `REAL (kind_selector)?`
    - `DOUBLE PRECISION`
    - `COMPLEX (kind_selector)?`
    - `LOGICAL (kind_selector)?`
    - `CHARACTER (char_selector)?`
  - `kind_selector` and `char_selector` both accept general `expr_f90`
    expressions; KIND/LEN are modeled syntactically, not restricted to
    constant expressions.
- Derived types:
  - `derived_type_def`, `type_stmt`, `component_def_stmt`,
    `component_decl`, `component_attr_spec`, `end_type_stmt` implement
    the F90 TYPE definition.
  - `derived_type_spec_f90` supports `TYPE(type_name)` in type
    declarations.
  - `structure_constructor` allows both positional and keyword
    component specifications.
- Type declarations and attributes:
  - `type_declaration_stmt_f90` combines a type specifier, optional
    attribute specifiers (`attr_spec_f90`) and an entity declaration
    list (`entity_decl_list_f90`).
  - `attr_spec_f90` supports:
    - PARAMETER, DIMENSION, ALLOCATABLE, POINTER, TARGET, PUBLIC,
      PRIVATE, INTENT, OPTIONAL, EXTERNAL, INTRINSIC, SAVE.
  - Separate attribute statements (`allocatable_stmt`, `pointer_stmt`,
    `target_stmt`, `optional_stmt`, `intent_stmt`, `public_stmt`,
    `private_stmt`, `save_stmt`, `external_stmt`, `intrinsic_stmt`)
    are all wired into `declaration_construct`.

Gaps (spec vs grammar):

- The grammar does not syntactically enforce:
  - That KIND and LEN selectors are integer constant expressions.
    **ADDRESSED**: Semantic validator `tools/f90_kind_len_selector_validator.py`
    now enforces KIND/LEN selector constraints per ISO/IEC 1539:1991 Section 4.3
    (issue #674). Test suite validates KIND/LEN selector semantics with 10 test
    cases covering literals, variables, and expressions.
  - Many fine‑grained attribute ordering and consistency rules (e.g.
    not all attribute combinations are valid for every context).

## 4. Arrays, pointers, ALLOCATABLE and dynamic memory

Specification (N692 §§5.1.2.4–5.1.2.7, 6, 13.8):

- Array declarations (explicit‑, assumed‑, deferred‑shape,
  assumed‑size).
- ALLOCATABLE and POINTER arrays, TARGETs.
- ALLOCATE / DEALLOCATE / NULLIFY and pointer association status
  inquiries.

Grammar:

- Array specs:
  - `array_spec_f90` combines:
    - `explicit_shape_spec_list`
    - `assumed_shape_spec_list`
    - `deferred_shape_spec_list`
    - `assumed_size_spec`
- Dynamic memory:
  - `allocate_stmt`, `allocation_list`, `allocation`, `allocate_object`
    and `allocate_shape_spec_list` implement the ALLOCATE statement.
  - `deallocate_stmt` and `deallocate_list` implement DEALLOCATE.
  - `nullify_stmt` and `pointer_object_list` implement NULLIFY.
  - `pointer_assignment_stmt` implements `=>` pointer assignment.
  - `stat_variable` and `io_control_spec` cover `STAT=` and related
    status/control specifiers.
- Intrinsic inquiry functions (subset):
  - `intrinsic_function_f90` includes a broad set of intrinsics:
    - Inquiry forms such as SIZE, SHAPE, LBOUND, UBOUND, ALLOCATED,
      PRESENT, SELECTED_REAL_KIND, SELECTED_INT_KIND.
    - Array reductions, helper operations, and character procedures from
      §13.8 (ALL, ANY, COUNT, DOT_PRODUCT, MATMUL, MAXVAL, MINVAL,
      PRODUCT, PACK, UNPACK, SPREAD, MERGE, TRANSPOSE, REPEAT, TRIM,
      ADJUSTL, ADJUSTR) matching ISO/IEC 1539:1991 coverage for
      intrinsic procedures.

Gaps (spec vs grammar):

- The grammar does not enforce all context restrictions on assumed‑shape/deferred‑shape arrays or POINTER/TARGET combinations.
  **ADDRESSED**: Semantic validator `tools/f90_array_spec_pointer_target_validator.py` now enforces:
  - Assumed-shape arrays restricted to dummy arguments (ISO/IEC 1539:1991 Section 5.1.2.4)
  - POINTER arrays must have deferred-shape specs (Section 5.2.7)
  - ALLOCATABLE arrays must have deferred-shape specs (Section 5.1.2.4)
  - POINTER and TARGET are mutually exclusive (Section 5.2.7-5.2.8)
  - POINTER excludes INTENT attribute (Section 5.2.7)
  - ALLOCATABLE and POINTER are mutually exclusive (Section 5.2.7)
  Test suite validates all constraints with comprehensive test cases covering valid and invalid scenarios.
  Implementation status: Complete (issue #676).

## 5. Control constructs (IF, CASE, DO, WHERE)

Specification (N692 §§8.1.2–8.1.4, 7.5):

- IF construct, CASE construct, DO construct, WHERE construct.

Grammar:

- IF construct:
  - `if_construct`, `if_then_stmt`, `else_if_stmt`, `else_stmt`,
    `end_if_stmt` implement block IF/ELSE IF/ELSE/END IF, including
    named constructs.
- CASE construct:
  - `select_case_construct`, `select_case_stmt`, `case_construct`,
    `case_stmt`, `case_selector`, `case_value_range_list`,
    `case_value_range` support `SELECT CASE` with scalar expression,
    ranges and optional construct names.
- DO construct:
  - `do_construct_f90`, `do_stmt_f90`, `loop_control`, `end_do_stmt`
    implement:
    - Counted DO loops.
    - DO WHILE loops.
    - Named constructs and `CYCLE` / `EXIT` (`cycle_stmt`, `exit_stmt`)
      with optional construct names.
- WHERE construct:
  - `where_construct`, `where_construct_stmt`, `elsewhere_stmt`,
    `end_where_stmt`, `where_stmt`, `logical_expr_f90` implement the
    WHERE construct and single WHERE statement.

Semantic validation (issue #677):

- Semantic validator `tools/f90_cycle_exit_validator.py` implements CYCLE/EXIT
  construct-name validation per §8.1.4.4, enforcing:
  - CYCLE and EXIT must appear within DO construct range (§8.1.4.4.3–4.4)
  - Named CYCLE/EXIT must reference active DO construct by name (§8.1.4.4.3–4.4)
  - No stray construct references allowed (§8.1.4.4.3–4.4)
- Test suite: `tests/Fortran90/test_issue677_cycle_exit_semantics.py` (30+ test cases)
- Fixtures: `tests/fixtures/Fortran90/test_cycle_exit/` with positive and negative cases
- Closes issue #677

## 6. Expressions and literals

Specification (N692 §7, §4.3, §4.5, §13.7–13.8):

- Scalar and array expressions, logical operations, relational
  operations, concatenation, BOZ literals, kind‑ and length‑selected
  literals.

Grammar:

- `expr_f90`:
  - A precedence-aware rule tower that implements N692 §7.1.1 and §7.4
    (Table 7.7), including associativity:
    - Defined unary operators bind tightest (`level_1_expr_f90`).
    - Exponentiation `**` associates right to left (`mult_operand_f90`).
    - `*`/`/`, `+`/`-`, and `//` associate left to right.
    - Relational operators are non-associative and appear at most once
      (`level_4_expr_f90`).
    - Logical precedence `.NOT.` > `.AND.` > `.OR.` > `.EQV.`/`.NEQV.`
      (`level_5_expr_f90`).
    - Defined binary operators have lowest precedence (Table 7.7) and
      are parsed at `expr_f90` (fixes #678).
- `primary_f90`:
  - Includes `literal_f90`, `variable_f90`, `function_reference_f90`,
    `intrinsic_function_f90`, `array_constructor_f90`,
    `structure_constructor`, and parenthesized expressions.
- `defined_unary_op` / `defined_binary_op`:
  - Recognizes `.custom.` names via the `DOP` token so user-defined unary
    and binary operators per ISO/IEC 1539:1991 Section 7.1.2 (R703-R704)
    participate in expression parsing and interface declarations.
- Literals:
  - `literal_f90` supports:
    - Integer and real literals with optional kind markers.
    - Single‑ and double‑quoted character literals.
    - Logical `.TRUE.`/`.FALSE.` (`logical_literal_f90`).
    - BOZ literal constants (`boz_literal_constant` for B/O/Z forms).

Gaps:

- Expression parsing is syntactic only; type, rank and definability
  constraints are deferred to downstream semantic tooling.

## 7. I/O: READ, WRITE, NAMELIST, control lists

Specification (N692 §10):

- READ, WRITE, PRINT statements with formatted and list‑directed I/O.
- NAMELIST I/O.
- Control specifiers: UNIT, FMT, IOSTAT, ERR, END, EOR, ADVANCE,
  SIZE, REC.

Grammar:

- `read_stmt_f90` / `write_stmt_f90`:
  - Implement READ and WRITE with:
    - Positional unit and format forms.
    - General `io_control_spec_list` control lists.
    - NAMELIST WRITE `WRITE namelist_name`.
- `io_control_spec`:
  - Covers UNIT, FMT, IOSTAT, ERR, END, EOR, ADVANCE, SIZE, REC, or a
    positional io-unit/format (expression or `*`).
  - Supports preconnected unit and list-directed forms via `*` (R901/R912).
- `io_unit_f90`:
  - Implements `io-unit` (R901) for `UNIT=` control specs, accepting
    external/internal file units and `*` for preconnected units.
- `format_spec`:
  - Accepts list‑directed `*`, labels, format expressions and NAMELIST
    names.
- `namelist_stmt`:
  - Included in `declaration_construct` as an F90 declaration form.
- File I/O statements (N692 §9):
- `open_stmt_f90`, `close_stmt_f90`, `backspace_stmt_f90`,
  `endfile_stmt_f90`, `rewind_stmt_f90`, `inquire_stmt_f90` are
  all wired into `executable_construct_f90` with proper ISO section
  references (R904, R908, R923, R924, R925, R929).
- `inquire_stmt_f90` now also supports the `INQUIRE(IOLENGTH=...)`
  record-length inquiry (ISO/IEC 1539:1991 Section 9.7.3 R931) paired
  with `output_item_list_f90`, closing issue #601.

## 8. Fixtures and integration status

`tests/Fortran90/test_fortran_90_comprehensive.py` (30+ tests) confirms
that the grammar can parse a broad set of Fortran 90 features when
exercised in focused unit tests. The generic fixture harness in
`tests/test_fixture_parsing.py` validates all F90 fixtures.

**Current status:** All F90 fixtures pass (0 xfail).

- Added `tests/fixtures/Fortran90/test_fortran_90_comprehensive/user_defined_operator.f90`
  to ensure user-defined interface/operator combinations (ISO/IEC 1539:1991 Section 7.1.2, R703-R704) parse with both unary/binary usage (fixes #590).
- Added `tests/fixtures/Fortran90/test_interface_module_procedure/module_procedure_interface.f90`
  to assert that `MODULE PROCEDURE procedure-name-list` declarations (ISO/IEC
  1539:1991 Section 12.3.2.2, R1206) parse successfully (fixes #594).
- Added `tests/fixtures/Fortran90/test_file_io_statements/inquire_iolength.f90`
  to exercise `INQUIRE(IOLENGTH=...)` record-length inquiries with
  `output_item_list_f90` and closing issue #601 (ISO/IEC 1539:1991 Section 9.7.3, R931).

Previous xfail fixtures were corrected to use valid F90 syntax:

- `array_constructor_program.f90`: Updated to use F90 parenthesis array
  constructor syntax `(/.../)` instead of F2003 bracket syntax `[...]`
- `free_form_features_program.f90`: Fixed declaration placement and
  array constructor syntax
- `f90_array_features_program.f90` (renamed from `fortran95_features_program.f90`):
  Replaced FORALL construct (F95 feature) with valid F90 DO loops

The `tests/xpass_fixtures.py` registry is now empty for Fortran 90,
indicating full fixture coverage.

## 9. PURE and ELEMENTAL: forward extensions from Fortran 95

The F90 grammar accepts the PURE and ELEMENTAL keywords as procedure
prefixes (via `prefix_spec` in `Fortran90Parser.g4`), even though these
features are defined in the **Fortran 95** standard (ISO/IEC 1539‑1:1997),
not in Fortran 90 (ISO/IEC 1539:1991).

This is a **deliberate forward extension** for practical parsing of
mixed-standard codebases. Many real‑world Fortran programs use PURE and
ELEMENTAL procedures in code that is otherwise compatible with F90
compilers, and accepting these keywords at the F90 grammar level avoids
forcing users to explicitly select the F95 grammar for such programs.

The grammar comments in `Fortran90Lexer.g4` and `Fortran90Parser.g4`
explicitly document this forward extension and reference issue #182.

The Fortran 95 grammar (`Fortran95Parser.g4`) provides the proper
standard‑compliant handling of PURE/ELEMENTAL via dedicated entry rules:

- `program_unit_f95`: top‑level entry integrating F95 constructs.
- `function_stmt_f95`, `subroutine_stmt_f95`: procedure statements with
  full `prefix_f95` support (RECURSIVE, PURE, ELEMENTAL, type spec).
- `function_subprogram_f95`, `subroutine_subprogram_f95`: complete
  subprogram rules using the F95 procedure statements.

These F95 rules are exercised by `tests/Fortran95/test_fortran_95_features.py`
and referenced via the `program_unit_f95` entry rule in the generic fixture
harness (`tests/test_fixture_parsing.py`).

For users who require strict historical accuracy:

- A pure F90 parser (without F95 extensions) would need to remove PURE
  and ELEMENTAL from `prefix_spec` in `Fortran90Parser.g4`.
- The current design prioritizes practical usability over strict historical
  conformance, which is documented in this audit and in the grammar comments.

## 10. ISO/IEC 1539:1991 Spec-Grammar Cross-Walk

The grammar files now include inline ISO/IEC 1539:1991 (WG5 N692) section
references. This table summarizes the mapping between ISO sections and grammar
rules:

| ISO Section | Topic | Grammar Files and Rules |
|-------------|-------|-------------------------|
| Section 2 | Fortran Terms and Concepts | `Fortran90Parser.g4`: `program_unit_f90` |
| Section 3.3 | Fixed Source Form | `Fortran90Lexer.g4`: `FIXED_FORM_COMMENT`, `STAR_COMMENT` |
| Section 3.4 | Free Source Form | `Fortran90Lexer.g4`: `FREE_FORM_COMMENT`, `CONTINUATION`, `SEMICOLON` |
| Section 4.3 | Intrinsic Types | `F90TypesParser.g4`: `intrinsic_type_spec_f90`, `kind_selector`, `char_selector` |
| Section 4.4 | Derived Types | `F90TypesParser.g4`: `derived_type_def`, `derived_type_stmt`, `end_type_stmt` |
| Section 4.4.4 | Structure Constructors | `F90TypesParser.g4`: `structure_constructor`, `component_spec` |
| Section 4.5 | Array Constructors | `F90ExprsParser.g4`: `array_constructor_f90`, `ac_value_list` |
| Section 5.1 | Type Declarations | `F90TypesParser.g4`: `type_declaration_stmt_f90`, `attr_spec_f90` |
| Section 5.1.2.3 | INTENT Attribute | `F90TypesParser.g4`: `intent_spec`; Lexer: `INTENT`, `IN`, `OUT`, `INOUT` |
| Section 5.1.2.4 | Array Specifications | `F90TypesParser.g4`: `array_spec_f90`, `deferred_shape_spec_list` |
| Section 5.3 | IMPLICIT Statement | `Fortran90Parser.g4`: `implicit_stmt_f90`, `specification_part` |
| Section 5.4 | NAMELIST Statement | `F90IOParser.g4`: `namelist_stmt` |
| Section 6.3 | Dynamic Allocation | `F90MemoryParser.g4`: `allocate_stmt`, `deallocate_stmt`, `nullify_stmt` |
| Section 7 | Expressions | `F90ExprsParser.g4`: `expr_f90`, `primary_f90` |
| Section 7.2.2 | Relational Operators | `Fortran90Lexer.g4`: `EQ_OP`, `NE_OP`, `LT_OP`, `LE_OP`, `GT_OP`, `GE_OP` |
| Section 7.5.2 | Pointer Assignment | `Fortran90Lexer.g4`: `POINTER_ASSIGN`; Parser: `pointer_assignment_stmt` |
| Section 7.5.3 | WHERE Construct | `F90ControlParser.g4`: `where_construct`, `where_stmt`, `elsewhere_stmt` |
| Section 8.1.1 | IF Construct | `F90ControlParser.g4`: `if_construct`, `if_then_stmt_f90`, `else_if_stmt_f90`, `else_stmt_f90`, `end_if_stmt_f90` |
| Section 8.1.3 | CASE Construct | `F90ControlParser.g4`: `select_case_construct`, `case_stmt`, `case_selector` |
| Section 8.1.4 | DO Construct | `F90ControlParser.g4`: `do_construct_f90`, `loop_control`, `cycle_stmt`, `exit_stmt` |
| Section 9.3 | Read Statement | `F90IOParser.g4`: `read_stmt_f90` |
| Section 9.4 | Write Statement | `F90IOParser.g4`: `write_stmt_f90` |
| Section 9.6 | File Positioning | `Fortran90Parser.g4`: `backspace_stmt_f90`, `endfile_stmt_f90`, `rewind_stmt_f90` |
| Section 9.7 | File Connection/Inquiry | `Fortran90Parser.g4`: `open_stmt_f90`, `close_stmt_f90`, `inquire_stmt_f90` |
| Section 11.1 | Main Program | `Fortran90Parser.g4`: `main_program`, `program_stmt`, `end_program_stmt` |
| Section 11.3 | Modules | `F90ModulesParser.g4`: `module`, `module_stmt`, `use_stmt` |
| Section 12.3 | Interface Blocks | `F90ModulesParser.g4`: `interface_block`, `interface_stmt`, `generic_spec` |
| Section 12.5 | Procedures | `F90ProcsParser.g4`: `function_stmt`, `subroutine_stmt`, `prefix`, `suffix` |
| Section 13.8 | Intrinsic Procedures | `F90ExprsParser.g4`: `intrinsic_function_f90`; Lexer: `*_INTRINSIC` tokens |

## 11. Summary

**Implementation Coverage:** 217 of 246 ISO syntax rules (88.2%)

The Fortran 90 grammar in this repository:

- Implements most of the **major language features** described in
  WG5 N692 / ISO/IEC 1539:1991 at the syntactic level, including:
  - Module system and explicit interfaces.
  - Derived types and structure constructors.
  - Dynamic arrays, pointers and ALLOCATABLE objects, plus associated
    intrinsics and statements (ALLOCATE/DEALLOCATE/NULLIFY).
  - Block IF, CASE, DO, DO WHILE, WHERE, named constructs, CYCLE/EXIT.
  - Enhanced expressions and literals (kinded numerics, BOZ, strings).
  - Enhanced I/O (I/O control lists, NAMELIST, list‑directed I/O,
    plus `INQUIRE(IOLENGTH=...)` record-length inquiries per
    ISO/IEC 1539:1991 Section 9.7.3, R931, fixes #601).
  - Internal subprograms and unified program‑unit structure.
- Uses a unified lexer for free‑form and fixed‑form with **lenient
  fixed‑form** handling (no card‑accurate column enforcement).
- Passes a broad comprehensive test suite targeted at individual F90
  features.

**Known Gaps and Limitations:**

| ISO Rule | Description | Status |
|----------|-------------|--------|
| R531–R534 | `data-implied-do` nested forms (DATA implied-DO lists) | Implemented (fixes #378) |
| R703–R704 | `defined-unary-op` / `defined-binary-op` user-defined dotted operators | Implemented (fixes #590) |
| R620 | `section-subscript` with vector subscript (R620-R621) | Implemented (fixes #381) |
| R1206 | `procedure_stmt` (`MODULE PROCEDURE` procedure-name-list) in interface blocks | Implemented (fixes #594) |
| R1219 | `entry-stmt` | Implemented (F90 extension via `entry_stmt_f90`) |

**xfail Fixtures:** 0 (Issue #311 resolved; fixtures corrected)

**Vector Subscript Implementation (Issue #381):**

Added explicit grammar rules per ISO/IEC 1539:1991 Section 6.2.2.1:
- R611 `subscript` → scalar integer expression
- R620 `section-subscript` → subscript | subscript-triplet | vector-subscript
- R621 `vector-subscript` → rank-one integer array expression
- R622 `subscript-triplet` → start:end:stride

Test fixtures in `tests/fixtures/Fortran90/test_vector_subscripts/`:
- `vector_subscript_basic.f90`: Basic usage with simple array variable
- `vector_subscript_multidim.f90`: Multidimensional array with vector subscripts
- `vector_subscript_expression.f90`: Vector subscript using array constructor
- `vector_subscript_mixed.f90`: Mixed subscript types (scalar, triplet, vector)

Test suite: `tests/Fortran90/test_vector_subscripts.py`

**Semantic Validator Implementation (Issue #675):**

Implements semantic validation for vector subscripts per ISO/IEC 1539:1991 Section 6.2.2.1:
- **tools/f90_vector_subscript_validator.py** (220 lines): ANTLR listener-based semantic analyzer
- Enforces rank-one array expression constraint (R621)
- Enforces integer element type constraint
- Validates array bounds and indices
- Test suite: `tests/Fortran90/test_issue675_vector_subscript_semantics.py` (10 test cases)
- Semantic fixtures: `tests/fixtures/Fortran90/test_vector_subscripts_semantic/` (3 positive/negative cases)

Error codes E675-001 through E675-003 with ISO section references (6.2.2.1).

Future work should:

- Tighten module/program‑unit integration and internal procedures
- Keep the grammar and tests in sync with spec‑section annotations (#173)
