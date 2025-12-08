# Fortran 95 (1995) – Grammar Audit (status: in progress)

This audit describes what the **Fortran 95** grammar in this repository
implements and how it relates to the Fortran 95 language as defined in:

- ISO/IEC 1539‑1:1997 (Fortran 95) – not stored in this repo, but used
  as the conceptual reference for the F95 feature set.
- J3/98‑114 “Fortran 95 Request for Interpretation”
  (`validation/pdfs/Fortran95_J3_98-114_RFI.txt`), which clarifies some
  F95 semantics for dummy arguments and function results.

The implementation view is based on:

- `grammars/Fortran95Lexer.g4`, `grammars/Fortran95Parser.g4`
- Inherited F90 grammar:
  - `grammars/Fortran90Lexer.g4`, `grammars/Fortran90Parser.g4`
- Tests in `tests/Fortran95/test_fortran_95_features.py` and fixtures
  under `tests/fixtures/Fortran95/test_fortran_95_features/`

As with the other standards, this document is **descriptive and
spec‑aware**, not a claim of full conformance.

## 1. Fortran 95 in this repository – high‑level picture

Specification‑wise, Fortran 95 is a relatively small revision on top of
Fortran 90. The most visible new or clarified features are:

- FORALL statement and construct for array assignments.
- Enhanced WHERE/ELSEWHERE constructs (including masked ELSEWHERE and
  more systematic nesting).
- PURE and ELEMENTAL procedures, with restrictions on side effects and
  use in specification expressions.
- Default initialization of derived‑type components and improved
  pointer/ALLOCATABLE semantics (including automatic deallocation).
- Extensions and additions to the intrinsic procedure set (e.g.
  `CPU_TIME`, extended CEILING/FLOOR, refinements to MAXLOC/MINVAL).
- Detailed rules for dummy arguments, function results and explicit
  interfaces (as seen in J3/98‑114).

In this repository:

- The **lexer** adds tokens for:
  - `FORALL`, `END_FORALL`.
  - A “modern intrinsic” group:
    `CEILING_INTRINSIC`, `FLOOR_INTRINSIC`, `MODULO_INTRINSIC`,
    `BIT_SIZE_INTRINSIC`, `BTEST_INTRINSIC`, `IAND_INTRINSIC`,
    `IBCLR_INTRINSIC`, `IBITS_INTRINSIC`, `IBSET_INTRINSIC`,
    `IEOR_INTRINSIC`, `IOR_INTRINSIC`, `ISHFT_INTRINSIC`,
    `ISHFTC_INTRINSIC`, `NOT_INTRINSIC`, `TRANSFER_INTRINSIC`,
    `CPU_TIME_INTRINSIC`, `SYSTEM_CLOCK_INTRINSIC`.
- The **parser** defines F95‑labelled rules that sit *beside* the F90
  ones:
  - `forall_stmt`, `forall_construct`, `forall_header`, etc.
  - `where_construct_f95`, `where_stmt_f95`, `logical_expr_f95`.
  - `pure_function_stmt`, `pure_subroutine_stmt`,
    `elemental_function_stmt`, `elemental_subroutine_stmt`.
  - `array_constructor_f95` (with both `(/ ... /)` and `[...]` forms).
  - `type_declaration_stmt_f95`, `entity_decl_f95`,
    `derived_type_def_f95`, `component_def_stmt_f95`.
  - F95‑specific expression and I/O variants such as `expr_f95`,
    `variable_f95`, `read_stmt_f95`, `write_stmt_f95`,
    `io_control_spec_f95`.
- Tests currently exercise only a **small subset** of these rules.

Program entry point:

- A dedicated `program_unit_f95` entry point integrates F95 constructs
  (FORALL, enhanced WHERE) into the program structure. This is provided
  alongside the inherited F90 constructs via `execution_part_f95` and
  `executable_construct_f95`.

The sections below expand this into a feature‑by‑feature audit.

## 2. FORALL constructs

Specification (ISO/IEC 1539‑1:1997, clauses on FORALL):

- Introduces `FORALL` as a new array‑oriented construct and statement:
  - `FORALL (forall-control-list [, scalar-mask-expr]) assignment`
  - `FORALL (forall-control-list [, scalar-mask-expr]) forall-body-construct`
- `forall-control-list` is a comma‑separated list of index‑triplet
  specifications `i = lower:upper[:stride]`.
- The mask expression must be a scalar LOGICAL.
- The body allows assignments and certain intrinsic constructs, with
  restrictions on side effects and dependencies.

Grammar implementation:

- Core rules:
  - `forall_construct`:
    - `forall_construct_stmt forall_assignment_stmt* end_forall_stmt`
  - `forall_construct_stmt`:
    - Optional construct name + `FORALL forall_header`.
  - `forall_stmt`:
    - Single‑statement form `FORALL forall_header forall_assignment_stmt`.
  - `forall_header`:
    - `LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN`.
  - `forall_triplet_spec`:
    - `IDENTIFIER EQUALS expr_f95 COLON expr_f95 (COLON expr_f95)?`.
  - `forall_assignment_stmt`:
    - Allows `assignment_stmt_f95`, `pointer_assignment_stmt`,
      `where_stmt`, `where_construct`, `forall_construct`,
      `forall_stmt`.
  - `end_forall_stmt`:
    - `END_FORALL (IDENTIFIER)?`.
- Grammar faithfully models:
  - Triplet forms and optional stride.
  - Optional scalar mask expression via `scalar_mask_expr`.
  - Nested FORALL and interplay with WHERE constructs.

Tests:

- `tests/Fortran95/test_fortran_95_features.py`:
  - `test_forall_construct_and_stmt` uses fixtures
    `forall_stmt.f90` and `forall_construct.f90` and parses them via
    the dedicated `forall_stmt` / `forall_construct` entry rules.

Integration:

- The `program_unit_f95` entry point integrates FORALL via
  `execution_part_f95` -> `executable_construct_f95` -> `construct_f95`
  -> `forall_construct`. Complete F95 programs containing FORALL can
  now be parsed via the dedicated entry point.

Semantic limitations (not enforced by grammar):

- The requirement that `scalar_mask_expr` be scalar LOGICAL is
  documented but not enforced syntactically (any `expr_f95` is
  accepted).
- Side‑effect and ordering constraints on the FORALL body are not
  modeled; they belong to semantic analysis.

## 3. Enhanced WHERE and ELSEWHERE

Specification:

- Fortran 95 refines the WHERE construct:
  - Clarifies nesting of WHERE constructs.
  - Allows multiple `ELSEWHERE` branches, with optional scalar logical
    masks.
  - Interactions between WHERE and FORALL are defined more precisely.

Grammar implementation:

- `where_construct_f95`:
  - `where_construct_stmt_f95 where_body_construct* end_where_stmt`.
- `where_construct_stmt_f95`:
  - Optional construct name + `WHERE (logical_expr_f95)`.
- `where_body_construct` allows:
  - `where_assignment_stmt`, nested `where_construct_f95`,
    and `elsewhere_part`.
- `elsewhere_part` / `elsewhere_stmt`:
  - Supports masked `ELSEWHERE (logical_expr_f95)` with optional
    construct name.
- `where_assignment_stmt` / `elsewhere_assignment_stmt`:
  - Allow assignment, pointer assignment and simple `where_stmt`.
- `where_stmt_f95`:
  - Single WHERE form: `WHERE (logical_expr_f95) assignment_stmt_f95`.
- `logical_expr_f95`:
  - Aliased to `expr_f95` (with the “must be logical” constraint left
    to semantics).

Tests:

- `test_where_construct` parses `where_construct.f90` through the
  `where_construct_f95` entry rule.

Integration:

- The `program_unit_f95` entry point integrates WHERE via
  `execution_part_f95` -> `executable_construct_f95` -> `construct_f95`
  -> `where_construct_f95`. Complete F95 programs containing enhanced
  WHERE can now be parsed via the dedicated entry point.

Semantic limitations (not enforced by grammar):

- The "logical and scalar" constraints on mask expressions are captured
  by comments, not grammar structure.

## 4. PURE and ELEMENTAL procedures

Specification (ISO/IEC 1539‑1:1997 Section 12.6):

- Fortran 95 introduces PURE and ELEMENTAL procedures with:
  - Restrictions on side effects, argument usage and references to
    global state.
  - Permitting PURE functions in specification expressions and as
    bounds/length expressions.

Grammar implementation:

- F90 forward extension:
  - `prefix_spec : RECURSIVE | PURE | ELEMENTAL | type_spec_f90`.
  - `function_stmt` / `subroutine_stmt` rules that accept this prefix.
  - This is a deliberate forward extension documented in
    `docs/fortran_90_audit.md` section 9 and the F90 grammar comments.
- Fortran 95 grammar provides proper standard‑compliant handling via:
  - `prefix_f95`: `prefix_spec_f95+` combining RECURSIVE, PURE,
    ELEMENTAL and type specifications.
  - `function_stmt_f95`:
    - `(prefix_f95)? FUNCTION IDENTIFIER (...) (suffix)?`.
  - `subroutine_stmt_f95`:
    - `(prefix_f95)? SUBROUTINE IDENTIFIER (...)`.
  - `function_subprogram_f95`, `subroutine_subprogram_f95`: complete
    subprogram rules using the F95 procedure statements.
- Program entry point:
  - `program_unit_f95` integrates F95 procedure subprograms alongside
    F95 constructs (FORALL, enhanced WHERE) into the program structure.
  - Full F95 programs with PURE/ELEMENTAL procedures parse via this
    entry point.
- Tests:
  - `test_pure_and_elemental_procedures` parses
    `pure_function_stmt.f90` and `elemental_subroutine_stmt.f90` using
    the `function_stmt_f95` and `subroutine_stmt_f95` entry rules.
  - The generic fixture harness (`tests/test_fixture_parsing.py`) uses
    `program_unit_f95` for Fortran95 fixtures.

Historical notes:

- **Historical attribution** (resolved in issue #182):
  - PURE and ELEMENTAL are accepted in the F90 grammar (via `prefix_spec`)
    as a deliberate forward extension for practical parsing of mixed-standard
    code, even though they are Fortran 95 language features. This is now
    explicitly documented in the F90 grammar comments and in
    `docs/fortran_90_audit.md` section 9.

Semantic constraints (not enforced by grammar):

- No syntactic checks exist for the "no side‑effects" or
  specification‑expression restrictions described in the standard and
  clarified in J3/98‑114. Those constraints are out of scope for this
  grammar and need to be enforced by later stages.

## 5. Type declarations and default initialization

Specification:

- Fortran 95 refines type declarations by:
  - Allowing default initialization of derived‑type components and
    certain entities.
  - Tightening rules around POINTER, ALLOCATABLE and TARGET attributes.

Grammar implementation:

- New F95‑labelled rules:
  - `type_declaration_stmt_f95`:
    - Mirrors `type_declaration_stmt_f90` but uses `type_spec_f95`,
      `attr_spec_f95`, `entity_decl_list_f95`.
  - `entity_decl_f95`:
    - `IDENTIFIER (LPAREN array_spec_f95 RPAREN)? (MULTIPLY char_length)? (ASSIGN initialization_expr)?`.
  - `initialization_expr`:
    - Alias of `expr_f95`.
  - `derived_type_def_f95`:
    - `derived_type_stmt component_def_stmt_f95* end_type_stmt`.
  - `component_def_stmt_f95`:
    - Uses `type_declaration_stmt_f95` and `private_sequence_stmt`.
  - `type_spec_f95`, `intrinsic_type_spec_f95`, `derived_type_spec_f95`,
    `kind_selector_f95`, `char_selector_f95`, `array_spec_f95` and
    related shape‑spec rules mirror their F90 counterparts but are
    wired to `expr_f95`.

Gaps and limitations:

- **Unreachable F95‑type rules in full programs**:
  - F90’s `declaration_construct` (which is what `specification_part`
    uses) still refers to `type_declaration_stmt_f90` and
    `derived_type_def`, not to the F95 variants.
  - None of the F95 `*_f95` type rules are referenced from
    `declaration_construct` or `derived_type_def`, so they are not used
    when parsing real modules or program units.
  - Default initialization of derived‑type components is therefore
    modeled only indirectly via the F90 `entity_decl_f90 ASSIGN expr_f90`
    pattern, not via the dedicated F95 constructs.
- **No syntactic enforcement of initialization‑expression restrictions**:
  - The standard’s distinction between general expressions and
    initialization expressions (constant, specification‑expression, etc.)
    is not represented; `initialization_expr` accepts any `expr_f95`.
- **Pointer and ALLOCATABLE rules**:
  - F90’s pointer/ALLOCATABLE declarations and ALLOCATE/DEALLOCATE/NULLIFY
    statements are reused; F95’s refinements (e.g. automatic
    deallocation) are semantic and not represented in syntax.

## 6. Array constructors and implied‑DO

Specification (ISO/IEC 1539‑1:1997, Section 4.5):

- Fortran 95 continues to use the F90 array constructor syntax:
  - `(/ ac-spec /)`, with possible implied‑DO lists.
- Square‑bracket array constructors `[ ... ]` are a **Fortran 2003**
  feature (ISO/IEC 1539‑1:2004), not part of the Fortran 95 standard.

Grammar implementation:

- `array_constructor_f95`:
  - `LPAREN SLASH ac_spec_f95 SLASH RPAREN`
  - Only the standard `(/ ... /)` form is accepted.
- `ac_value_f95` and `ac_implied_do_f95` mirror the F90 rules but use
  `expr_f95` and `do_variable`.
- Tests:
  - `test_array_constructor` parses `array_constructor.f90` via the
    `array_constructor_f95` rule. The fixture uses the `(/ ... /)` form.
  - `test_bracket_array_constructor_rejected` verifies that square
    bracket syntax `[...]` is rejected by the F95 grammar.

Remaining gaps:

- **Entry‑point integration**:
  - As with other F95 rules, `array_constructor_f95` is not referenced
    from F90's `primary_f90` / `array_constructor_f90`, so full F95
    programs continue to use the F90 constructor rule.

## 7. Intrinsic procedures and F95 additions

Specification:

- Fortran 95 extends the intrinsic procedure set over Fortran 90:
  - Adds `CPU_TIME`, and refines intrinsics like `CEILING`, `FLOOR`,
    `MAXLOC`, `MINLOC`, and some bit intrinsics.
  - Clarifies argument forms and allows additional keyword arguments
    (e.g. `KIND=`).

Lexer and parser implementation:

- Lexer (`Fortran95Lexer.g4`):
  - Defines dedicated tokens for a group of “modern intrinsics”:
    - CEILING/FLOOR/MODULO.
    - BIT_SIZE, BTEST, IAND/IBCLR/IBITS/IBSET/IEOR/IOR, ISHFT/ISHFTC.
    - NOT, TRANSFER, CPU_TIME, SYSTEM_CLOCK.
  - Tests assert that these tokens exist and can be lexed.
- Parser:
  - There is **no** `intrinsic_function_f95` rule.
  - `function_reference_f95` and `variable_f95` both require an
    `IDENTIFIER` as the leading token.
  - Because these intrinsics have their *own* token types (e.g.
    `CPU_TIME_INTRINSIC`), they are not treated as identifiers and
    therefore cannot appear as `procedure_designator_f95`, a
    `variable_f95` or a generic function reference in F95 programs.
  - F90’s `intrinsic_function_f90` covers a different subset of
    intrinsics (SIZE, SHAPE, LBOUND, UBOUND, etc.) and is unaware of
    the F95 additions.

Gaps:

- **Intrinsic calls are not parsed in F95**:
  - Calls such as `CALL CPU_TIME(t)` or `x = CEILING(y)` cannot
    currently be parsed by the F95 grammar, even though the lexer
    recognizes the tokens.
- **No F95‑specific intrinsic argument rules**:
  - The extra `KIND=` or DIM/MASK arguments introduced or clarified in
    F95 are not modeled at the grammar level.
- **Interaction with later standards**:
  - Fortran 2003 introduces `identifier_or_keyword` and additional
    handling that treats some keywords as identifiers. That mechanism
    is not present at the F95 level.

These limitations should be covered by Fortran 95 issues dedicated to:

- Making F95 intrinsics usable in expressions and calls.
- Documenting that argument‑level semantics are left to later
  validation.

## 8. Program units and integration of F95 constructs

Specification:

- Fortran 95 retains Fortran 90's overall program‑unit structure:
  - Main program, external subroutines/functions, modules, BLOCK DATA.
  - F95 constructs (FORALL, enhanced WHERE, PURE/ELEMENTAL, default
    initialization, etc.) are allowed in the same general contexts as
    their F90 precursors, subject to additional semantic rules.

Grammar implementation:

- The F95 parser **imports** the Fortran 90 parser:
  - `parser grammar Fortran95Parser;`
  - `import Fortran90Parser;`
- F95 provides a dedicated `program_unit_f95` entry point that integrates
  F95 constructs into the program structure (resolved in issue #179):
  - `program_unit_f95`: top‑level entry for F95 programs.
  - `main_program_f95`, `module_f95`, `external_subprogram_f95`: F95
    program unit variants using `specification_part_f95` and
    `execution_part_f95`.
  - `function_subprogram_f95`, `subroutine_subprogram_f95`: complete
    subprogram rules with F95 procedure statements (`function_stmt_f95`,
    `subroutine_stmt_f95`) supporting PURE/ELEMENTAL prefixes.
- F95 execution and specification parts:
  - `execution_part_f95` reaches F95 constructs via
    `executable_construct_f95` → `construct_f95` → `forall_construct`,
    `where_construct_f95`.
  - `specification_part_f95` reaches F95 type rules via
    `declaration_construct_f95` → `type_declaration_stmt_f95`,
    `derived_type_def_f95`.

Test coverage:

- `tests/Fortran95/test_fortran_95_features.py` validates individual F95
  constructs (FORALL, WHERE, PURE/ELEMENTAL headers, array constructors,
  intrinsic tokens) via focused entry rules.
- The generic fixture harness (`tests/test_fixture_parsing.py`) uses
  `program_unit_f95` as the entry rule for Fortran95 fixtures.
- Many F95 fragment fixtures (bare tokens, partial constructs) are
  expected to fail with `program_unit_f95` entry since they are not
  complete program units; these are documented as XPASS entries
  referencing issue #148.

Issue #148 ("Fortran 95: expand minimal feature tests to full‑program
coverage") tracks expanding the F95 test suite with complete program
fixtures that exercise the integrated `program_unit_f95` entry point.

## 9. J3/98‑114 and semantic characteristics

Specification context:

- J3/98‑114 discusses Fortran 95 rules for dummy arguments and function
  results with nonconstant bounds in the presence of explicit
  interfaces, clarifying:
  - When bounds expressions are part of the “characteristics” of a
    function result.
  - When it is valid to evaluate bounds expressions on the caller vs
    callee side.

Grammar implications:

- The current grammar:
  - Represents bounds through general `expr_f95` expressions in array
    specifications and function result declarations.
  - Does **not** attempt to model the “characteristics” concept or
    enforce equality of corresponding bounds expressions between
    interface and definition.
- These constraints are inherently semantic and would require a
  separate analysis phase operating on symbol tables and expression
  trees, not on raw parse rules.

Audit stance:

- It is acceptable for the ANTLR grammar not to encode these semantics,
  but the audit must state explicitly that:
  - The grammar alone cannot guarantee compliance with the Fortran 95
    rules described in J3/98‑114.
  - Any future semantic analyzer or linter built on top of this grammar
    must incorporate those rules separately.

## 10. Tests and coverage model

Tests:

- `tests/Fortran95/test_fortran_95_features.py` currently:
  - Validates that the F95 lexer recognizes `FORALL`, `END_FORALL` and
    the F95‑related intrinsic tokens.
  - Parses:
    - PURE and ELEMENTAL headers via `pure_function_stmt` and
      `elemental_subroutine_stmt`.
    - FORALL constructs and statements via `forall_construct` and
      `forall_stmt`.
    - An enhanced WHERE construct via `where_construct_f95`.
    - An array constructor via `array_constructor_f95`.
- There are **no** parser tests that:
  - Use F95 constructs inside complete program units or modules.
  - Exercise F95 default initialization rules in derived types.
  - Call the F95 intrinsic procedures.

Consequences:

- The existing tests confirm that individual F95 rules can be invoked
  and that the lexer tokens exist, but:
  - They do not provide coverage for integrated F95 programs.
  - They do not exercise many F95 areas that are already partially
    modeled in the grammar (type rules, I/O refinements, etc.).

## 11. Summary and issue mapping

The Fortran 95 layer in this repository:

- **Implements**:
  - Core FORALL and enhanced WHERE syntax, including nested constructs
    and scalar mask expressions.
  - PURE and ELEMENTAL procedure prefixes via `prefix_f95` and dedicated
    `function_stmt_f95` / `subroutine_stmt_f95` rules.
  - F95‑flavored type and array‑specification rules and an array
    constructor rule (standard `(/ ... /)` form only).
  - A set of F90/F95 "modern intrinsic" tokens in the lexer.
  - A `program_unit_f95` entry point that integrates F95 constructs into
    the program structure via `execution_part_f95`, `specification_part_f95`,
    `function_subprogram_f95` and `subroutine_subprogram_f95`.
- **Remaining gaps**:
  - F95 intrinsic tokens are recognized by the lexer but not yet fully
    usable as intrinsic calls in all contexts.
- **Historical notes** (all resolved):
  - (Resolved in #182: PURE/ELEMENTAL in F90 grammar is now documented as a
    deliberate forward extension, with clear comments in the grammar files
    and a dedicated section in the F90 audit.)
  - (Resolved: Square‑bracket array constructors were removed from the
    F95 grammar; only the standard `(/ ... /)` form is now accepted.)
  - (Resolved in #179: F95 constructs are now integrated into program
    structure via `program_unit_f95` entry point.)

Existing issues:

- #140 – Standard audits: this document is part of the Fortran 95 slice
  of that work.
- #148 – Fortran 95: expand minimal feature tests to full‑program
  coverage (test/fixture side).
- #174 – Fortran 95: annotate grammar with J3 98‑114 and
  ISO/IEC 1539‑1:1997 references, and ensure every F95 gap discovered
  in the spec→grammar cross‑walk has a dedicated issue.

Outstanding work tracked by issues:

- Making F95 intrinsic tokens usable as function and subroutine calls.

Together with the Fortran 90 audit, this document completes the
spec‑aware audit chain up through Fortran 95 for issue #140, while
honestly recording areas where the implementation is intentionally or
unintentionally divergent from the standard.

