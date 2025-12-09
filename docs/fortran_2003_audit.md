# Fortran 2003 (ISO/IEC 1539‑1:2004) – Grammar Audit (status: in progress)

This audit describes what the **Fortran 2003** grammar in this repository
implements and how it relates to the Fortran 2003 language as defined in:

- ISO/IEC 1539‑1:2004 (Fortran 2003).
- J3/03‑007 “Fortran 2003” draft/final text, stored locally as
  `validation/pdfs/Fortran2003_J3_03-007.pdf`.

Because the OCR’d text file is currently empty
(`validation/pdfs/Fortran2003_J3_03-007.txt`), this audit treats the PDF
itself and the standard’s well‑known feature set as the primary reference
and cross‑checks against:

- `grammars/Fortran2003Lexer.g4`, `grammars/Fortran2003Parser.g4`
- Inherited grammars:
  - `grammars/Fortran95Lexer.g4`, `grammars/Fortran95Parser.g4`
- Tests under `tests/Fortran2003/`
  - `test_fortran_2003_comprehensive.py`
  - `test_simple_f2003.py`, `test_working_f2003.py`
  - TDD/feature tests for specific areas:
    `test_issue22_tdd.py`, `test_issue23_tdd.py`, `test_issue24_semantic_c_interop.py`,
    `test_issue25_tdd.py`, `test_issue26_pdt_basic.py`, `test_issue27_ieee_arithmetic.py`,
    `test_issue68_defined_io.py`, `test_issue69_select_type_semantics.py`,
    `test_issue70_c_interop_extended.py`, `test_issue71_pdt_extended.py`,
    `test_issue72_fixed_form_f2003.py`, `test_issue90_pdt_constructors.py`,
    `test_f2003_polymorphism_and_c_interop.py`, `test_f2003_parse_trees.py`.

It is **spec‑aware and implementation‑driven**: where this grammar
diverges from or simplifies the standard, those gaps are recorded and
backed by explicit GitHub issues.

---

## 1. Program structure and entry points

Specification (J3/03‑007):

- Program units: main program, module, external subprograms, block data.
- F2003 keeps the F95/F90 program‑unit taxonomy but adds new constructs
  within program bodies (OOP, C interoperability, enhanced I/O, etc.).

Grammar implementation:

- Entry rule for F2003 programs:
  - `program_unit_f2003` (Fortran2003Parser.g4) is the top‑level rule:
    - Accepts leading/trailing `NEWLINE*`.
    - Chooses among `main_program_f2003`, `module_f2003`,
      `external_subprogram_f2003`.
- `main_program_f2003`:
  - Overrides the F90 main program to use enhanced F2003 spec/execution
    parts:
    - `program_stmt specification_part_f2003? execution_part_f2003? internal_subprogram_part_f2003? end_program_stmt`.
  - `program_stmt` / `end_program_stmt` accept trailing `NEWLINE*` to
    match Fortran’s line‑oriented style more robustly.
- Modules:
  - `module_f2003`:
    - Explicitly sequences `module_stmt`, `specification_part_f2003?`,
      optional `module_subprogram_part`, and `end_module_stmt`.
  - F90’s `module` rule is overridden to delegate to the same
    F2003‑specific structure.
- External subprograms:
  - `external_subprogram_f2003` uses F2003‑specific function and
    subroutine subprogram rules:
    - `function_subprogram_f2003` and `subroutine_subprogram_f2003`
      incorporate F2003’s `binding_spec` (BIND(C) for procedures) and
      the enhanced specification/execution parts.

Tests:

- `TestFortran2003Unified` in
  `tests/Fortran2003/test_fortran_2003_comprehensive.py` always parses
  via `program_unit_f2003()` and exercises:
  - Main programs, modules, CONTAINS, and external procedures.
  - Both free‑form `.f90` and fixed‑form `.f` code paths.
- `test_simple_f2003.py` and `test_working_f2003.py` further verify
  basic main programs, modules, and OOP examples.

Gaps / simplifications:

- Block data program units:
  - F2003 inherits block data from earlier standards through the
    Fortran 95/F90 chain; the F2003 grammar does not add a dedicated
    F2003‑specific `block_data_subprogram` entry.
  - This is acceptable historically, but the audit should note that
    block data is tested primarily at the F90/F95 level.
- Mixed declarations and executable statements:
  - `execution_part_f2003` explicitly allows `type_declaration_stmt`
    inside the execution part (commented “F2003 allows mixed
    declarations and executable statements”), matching the spirit of
    F2003’s more flexible scoping but without enforcing all detailed
    ordering rules from the standard.
  - Detailed ordering and context restrictions for declarations within
    execution parts remain a semantic concern.

Gaps that warrant explicit issues:

- A dedicated audit of **block data** in F2003 contexts and fixtures.
- Explicit documentation and tests for **mixed
  declarations/executable statements** in all legal contexts
  (currently only covered by a handful of fixtures).

---

## 2. Derived types, OOP, and polymorphism

Specification:

- Fortran 2003 introduces full object‑oriented support:
  - Type extension via `EXTENDS`.
  - Type‑bound procedures with `PROCEDURE` and binding attributes
    (PASS/NOPASS, DEFERRED, NON_OVERRIDABLE, PUBLIC/PRIVATE).
  - Polymorphism with `CLASS`, unlimited polymorphic `CLASS(*)`, and
    `SELECT TYPE` constructs.
  - Finalization via `FINAL` procedures.
  - ABSTRACT types and ABSTRACT INTERFACE.

Grammar implementation:

- Enhanced derived type definition:
  - `derived_type_def_f2003`:
    - `derived_type_stmt_f2003` + optional `type_param_def_stmt`s,
      privacy/SEQUENCE, `component_part`, an optional
      `type_bound_procedure_part`, and `end_type_stmt_f2003`.
  - `derived_type_stmt_f2003`:
    - `TYPE (COMMA type_attr_spec_list DOUBLE_COLON | DOUBLE_COLON)? type_name (LPAREN type_param_name_list RPAREN)?`.
  - `type_attr_spec_list` / `type_attr_spec`:
    - `PUBLIC`, `PRIVATE`, `ABSTRACT`, `EXTENDS(type-name)`, `BIND(C)`
      for derived types.
- Components and procedure components:
  - `component_part`, `data_component_def_stmt`, `proc_component_def_stmt`
    model data components and procedure pointer components.
  - `proc_component_def_stmt` uses `proc_attr_spec_list` with
    `PUBLIC`, `PRIVATE`, `NOPASS`, `PASS`, `DEFERRED`, `POINTER`.
- Type‑bound procedures:
  - `type_bound_procedure_part` contains `type_bound_procedure_stmt`,
    `type_bound_generic_stmt`, and `final_procedure_stmt`.
  - `type_bound_procedure_stmt` covers the different procedure binding
    forms with optional binding attributes.
  - `type_bound_generic_stmt` extends `generic_spec` to allow
    `READ(FORMATTED)` / `WRITE(UNFORMATTED)` for defined derived‑type
    I/O.
  - `final_procedure_stmt` implements `FINAL` with one or more
    subroutine names.
- Polymorphism and SELECT TYPE:
  - `class_declaration_stmt`:
    - `CLASS(type-spec-or-derived), attr-spec-list :: entities`
      and unlimited polymorphic `CLASS(*)`.
  - `select_type_construct`/`select_type_stmt`/`type_guard_stmt`:
    - Model `SELECT TYPE (selector [=> expr])` with `TYPE IS` and
      `CLASS IS` guards, and `CLASS DEFAULT`.
    - `IS` is purposely treated as an `IDENTIFIER` token named `"is"`
      rather than a dedicated keyword token, simplifying the lexer.

Tests:

- OOP and polymorphism:
  - `test_fortran_2003_comprehensive.py` fixtures
    (`oop_code.f90`, `poly_mod.f90`, `advanced_f2003.f90`) exercise:
    - Type extension and inheritance.
    - Type‑bound procedures and generic bindings.
    - SELECT TYPE with polymorphic dummy arguments.
  - `test_f2003_polymorphism_and_c_interop.py` combines polymorphism
    with C interoperability and BIND(C).
  - `test_issue69_select_type_semantics.py` stresses SELECT TYPE
    nesting and multiple guards.
- PDTs and OOP (see also §3 below):
  - `test_issue26_pdt_basic.py`, `test_issue71_pdt_extended.py`,
    `test_issue90_pdt_constructors.py` cover parameterized derived
    types used with extension and type‑bound procedures.

Gaps / simplifications:

- SELECT TYPE guard syntax:
  - The grammar models `TYPE IS` / `CLASS IS` by treating `IS` as a
    plain identifier, not a distinct keyword, for simplicity.
  - This is syntactically adequate for most code but diverges from the
    standard’s explicit treatment of `IS` and could mis‑tokenize
    pathological cases.
- Omitted OOP corners:
  - The grammar does not explicitly separate *type parameter* defaults
    for extended types from those of the parent; many context rules
    are semantic and not enforced.
  - Some exotic combinations of ABSTRACT/EXTENDS/PASS/NOPASS/
    DEFERRED/NON_OVERRIDABLE may parse but lack tests.

Gaps that warrant explicit issues:

- A dedicated issue for the **`TYPE IS`/`CLASS IS` guard representation**
  (lexer treating `IS` as an identifier rather than a keyword) and its
  impact on historical accuracy.
- Additional test coverage for **complex type‑bound procedure
  combinations** (multiple generics, public/private overrides,
  deferred bindings in larger hierarchies) to ensure the grammar
  behaves as intended in all OOP scenarios.

---

## 3. Parameterized derived types (PDTs)

Specification:

- Fortran 2003 extends derived types with *kind*, *len* and possibly
  deferred/assumed parameters:
  - `TYPE :: t(k, n, ...)`
  - Type parameters can have defaults, can be used in component
    declarations, and can appear in constructor calls.

Grammar implementation:

- Type parameter definitions:
  - `type_param_def_stmt`:
    - `INTEGER, type_param_attr_spec :: type_param_decl_list`.
  - `type_param_attr_spec` / `type_param_decl_list` model the allowed
    attribute combinations and declarations.
- Derived type spec and instantiation:
  - `derived_type_spec` accepts type parameter lists with
    `type_param_value` entries (explicit, `:`, `*`).
- PDT structure constructors:
  - `pdt_structure_constructor`:
    - `derived_type_spec LPAREN component_spec_list_f2003? RPAREN`.
  - `component_spec_f2003` allows both named (`name = expr_f2003`) and
    positional component values, mirroring the general constructor
    pattern.

Tests:

- PDT basics and extended cases:
  - `test_issue26_pdt_basic.py`, `test_issue71_pdt_extended.py`,
    `test_issue90_pdt_constructors.py` exercise:
    - PDT declarations and instantiations.
    - PDTs used as components, dummy arguments, and nested types.
    - Structure constructors with positional and named parameters,
      and mixtures of `:`, `*`, default and explicit values.

Gaps / simplifications:

- Semantic constraints such as:
  - Consistency between type parameter attributes and their use in
    components.
  - Restrictions on which parameters may be deferred or assumed.
  - These are not encoded syntactically and are left to downstream
    semantic tools.

Gaps that warrant explicit issues:

- None **purely syntactic** beyond the general “semantic PDT rules are
  out of scope for the grammar”; the existing tests and rules align
  well with the F2003 syntax. Semantic enforcement should be tracked
  under separate analyzer/linter issues rather than grammar changes.

---

## 4. Procedure pointers, interfaces, and binding specifications

Specification:

- Fortran 2003 adds:
  - Procedure pointers (`PROCEDURE, POINTER :: p`).
  - Procedure pointer components in derived types.
  - Abstract interfaces and more flexible `INTERFACE` blocks.
  - BIND(C) for procedures with optional `NAME=` specifier.

Grammar implementation:

- Procedure declarations:
  - `procedure_declaration_stmt` and `proc_attr_spec` model procedure
    pointer declarations with attributes `PUBLIC`, `PRIVATE`, `NOPASS`,
    `PASS`, `DEFERRED`, `POINTER`.
  - `proc_component_def_stmt` is used in the component part of derived
    types.
- Abstract interfaces:
  - `interface_stmt` and `interface_body` override the F90 behaviour to
    support `ABSTRACT_INTERFACE` and F2003 specification parts.
- Binding specifications:
  - `binding_spec`:
    - `BIND(C)` and `BIND(C, NAME="func")` for procedures (and
      derived types via `type_attr_spec`).
  - `string_literal` rule reused for the BIND(C) `NAME=` value.

Tests:

- TDD tests:
  - `test_issue23_tdd.py` covers procedure pointer declarations and
    usage patterns.
  - `test_issue22_tdd.py` and `test_issue25_tdd.py` cover OOP
    constructs that include procedure pointers and abstract interfaces.
- Integration:
  - `test_fortran_2003_comprehensive.py`, `test_f2003_polymorphism_and_c_interop.py`,
    and C‑interop tests (see §7) exercise procedure pointers in real
    contexts.

Gaps / simplifications:

- Some advanced interface semantics from the standard:
  - The grammar does not encode all characteristics matching rules
    between abstract interfaces and procedure pointer declarations;
    those are semantic.
- No explicit modeling of `PROCEDURE (interface-name)` vs `PROCEDURE()`
  beyond the current simplified forms.

Gaps that warrant explicit issues:

- A semantic‑level issue to track **interface/characteristics checks**
  for procedure pointers and abstract interfaces built atop the
  Fortran 2003 parse trees.

---

## 5. ASSOCIATE, BLOCK, and control constructs

Specification:

- F2003 adds:
  - `ASSOCIATE` constructs:
    - `ASSOCIATE (name => expr, ...)` and a corresponding `END ASSOCIATE`.
  - `BLOCK` constructs:
    - `BLOCK`/`END BLOCK` with local declarations and execution blocks.
- Traditional control constructs (IF, DO, SELECT CASE) remain from
  F90/F95 and are augmented by the new constructs.

Grammar implementation:

- `associate_construct` and `block_construct` appear in
  `executable_construct_f2003_inner` alongside inherited IF/DO/SELECT
  CASE constructs.
- Rules include binding lists, inner specification parts, and nested
  executable constructs.

Tests:

- `test_fortran_2003_comprehensive.py`:
  - `associate_construct.f90` and `block_construct.f90` fixtures are
    parsed via `program_unit_f2003`.
- Additional tests combine ASSOCIATE/BLOCK with polymorphism and
  SELECT TYPE (e.g. in `advanced_f2003.f90`).

Gaps:

- None obvious at the purely syntactic level; the major constructs and
  nesting behaviours are exercised by the existing fixtures.

---

## 6. Enhanced ALLOCATE, DEALLOCATE, and dynamic memory

Specification:

- F2003 extends ALLOCATE/DEALLOCATE with:
  - SOURCE= and MOLD= specifiers.
  - Extended STAT and ERRMSG handling.
  - Type‑spec forms for allocating PDTs.

Grammar implementation:

- `allocate_stmt_f2003`:
  - `ALLOCATE ( allocation_list , alloc_opt_list? )`.
  - `allocation` covers both simple object names and PDT allocations
    via `type_spec_allocation`.
- `type_spec_allocation`:
  - `derived_type_spec :: identifier_or_keyword (shape)?` for PDTs.
- `alloc_opt`:
  - `STAT = identifier_or_keyword`, `ERRMSG = identifier_or_keyword`,
    `SOURCE = expr_f2003`, `MOLD = expr_f2003`.
- DEALLOCATE:
  - `deallocate_stmt` reuses `allocation_list` for consistency.

Tests:

- `enhanced_allocate.f90` in
  `test_fortran_2003_comprehensive.py` demonstrates:
  - ALLOCATE with SOURCE, MOLD and PDT type‑spec forms.
  - DEALLOCATE of the same objects.

Gaps:

- As with F90/F95, the grammar does not enforce:
  - All contextual constraints on where SOURCE/MOLD may appear.
  - Semantics of automatic deallocation or interactions with PDT
    parameters. These are left to semantic checks.

---

## 7. Enhanced I/O, WAIT, FLUSH, and defined derived‑type I/O

Specification:

- F2003 substantially enhances I/O:
  - Additional specifiers: ASYNCHRONOUS, STREAM, ID, IOMSG, PENDING.
  - WAIT and FLUSH statements.
  - Defined derived‑type I/O via generic `READ(FORMATTED)` /
    `WRITE(UNFORMATTED)` in type‑bound generics and interface blocks.

Grammar implementation:

- Unified F2003 I/O:
  - `open_stmt`, `close_stmt`, `write_stmt`, `read_stmt` all use
    `f2003_io_spec_list`, which in turn uses `f2003_io_spec`.
  - `f2003_io_spec` covers:
    - All standard F90/F95 specifiers (UNIT, FILE, ACCESS, FORM, STATUS,
      BLANK, POSITION, ACTION, DELIM, PAD, RECL, IOSTAT, ERR, END, EOR,
      ADVANCE, SIZE, REC).
    - F2003 additions: ASYNCHRONOUS, STREAM, PENDING, ID, IOMSG.
    - A generic `IDENTIFIER = primary` form.
- WAIT/FLUSH:
  - `wait_stmt` with `wait_spec_list` (UNIT, ID, IOSTAT, IOMSG, ERR, END,
    EOR, positional unit).
  - `flush_stmt` with `flush_spec_list` (UNIT, IOSTAT, IOMSG, ERR,
    positional unit).
- Defined derived‑type I/O:
  - `generic_spec` is extended to allow `READ(identifier_or_keyword)` and
    `WRITE(identifier_or_keyword)` so that type‑bound generics for
    formatted/unformatted READ/WRITE can be written as:
    - `GENERIC :: READ(FORMATTED) => read_formatted`.
    - `GENERIC :: WRITE(UNFORMATTED) => write_unformatted`.
  - DT edit descriptors **inside FORMAT strings** are *not* parsed as a
    separate sub‑grammar; format strings are treated as character
    literals.

Tests:

- `enhanced_io.f90` in the comprehensive suite exercises:
  - Use of the enhanced I/O specifiers in READ/WRITE/OPEN/CLOSE.
- `test_issue68_defined_io.py` specifically targets:
  - Defined derived‑type I/O generic bindings for READ/WRITE.
  - Basic usage of such generic bindings in user code.
- `test_issue70_c_interop_extended.py` mixes enhanced I/O with C
  interoperability and asynchronous I/O specifiers.

Gaps:

- DT edit descriptors:
  - The syntax of `DT` edit descriptors (inside FORMAT) is not modeled;
    they are treated as opaque character strings, so the grammar does not
    enforce the detailed rules for DT descriptors from the standard.
  - This is an **intentional design decision**: format-specification
    content (including DT descriptors) is accepted as part of character
    literals but not structurally parsed.
  - See issue #185 for the documented decision and test coverage.

Gap issues (resolved):

- Issue #185 explicitly documents that **DT edit descriptors** are left as
  opaque character strings and that only the generic level of defined
  derived-type I/O (READ(FORMATTED)/WRITE(UNFORMATTED) in `generic_spec`)
  is modeled structurally. Tests in
  `tests/Fortran2003/test_issue185_dt_edit_descriptors.py` verify that
  FORMAT strings containing DT descriptors are accepted correctly.

---

## 8. C interoperability and IEEE arithmetic

Specification:

- C interoperability (ISO_C_BINDING) in F2003:
  - `BIND(C)` for procedures and derived types.
  - `VALUE`, `C_PTR`, `C_FUNPTR`, C integer/real/complex kinds, etc.
  - IMPORT and use of the `ISO_C_BINDING` intrinsic module.
- IEEE arithmetic:
  - Intrinsic modules `IEEE_EXCEPTIONS`, `IEEE_ARITHMETIC`,
    `IEEE_FEATURES`.
  - Names for exceptions, special values, rounding modes and feature
    flags.

Grammar implementation:

- Lexer:
  - `Fortran2003Lexer.g4` defines a full set of C interoperability
    tokens (`C_INT`, `C_FLOAT`, `C_DOUBLE_COMPLEX`, `C_PTR`,
    `C_NULL_PTR`, etc.), `BIND`, `VALUE`, `NAME`, and the IEEE module /
    entity names.
- Parser:
  - `binding_spec` accepts `BIND(C)` and
    `BIND(C, NAME="...")` for procedures.
  - `type_attr_spec` supports `BIND(C)` for derived types.
  - `use_stmt` recognizes `USE, INTRINSIC :: IEEE_EXCEPTIONS /
    IEEE_ARITHMETIC / IEEE_FEATURES` with a dedicated `ieee_only_list`
    (`ieee_entity` rule) that includes exception types, special values,
    rounding modes, feature names, and generic identifiers.
  - `c_interop_type` lists all the C interop types for use in IMPORT
    and declarations.
  - `ieee_constant`, `ieee_function_call`, `intrinsic_function_call`
    allow IEEE names in expressions.

Tests:

- C interop:
  - `test_issue24_semantic_c_interop.py`,
    `test_f2003_polymorphism_and_c_interop.py`,
    `test_issue70_c_interop_extended.py`:
    - Validate BIND(C) for procedures and derived types.
    - Use ISO_C_BINDING types and C pointers/funptrs.
    - Combine C interop with polymorphism and type‑bound procedures.
- IEEE arithmetic:
  - `test_issue27_ieee_arithmetic.py` exercises:
    - USE of IEEE intrinsic modules and ONLY lists.
    - Selected IEEE constants and inquiry/value functions in expressions.

Gaps:

- The grammar deliberately stops at *syntactic* recognition of C
  interop and IEEE names:
  - It does not enforce that `BIND(C)` entities use only interoperable
    types.
  - It does not check the full set of restrictions on combinations of
    VALUE, POINTER, C interoperable types, or IEEE flag usage.
  - Those belong to semantic validation layers.
- `ISO_FORTRAN_ENV` intrinsic module (ISO/IEC 1539-1:2004 Section 13.8.2):
  - Although `ISO_FORTRAN_ENV` is a Fortran 2003 feature, the F2003
    grammar's `use_stmt` rule only accepts the three IEEE intrinsic
    modules (`IEEE_EXCEPTIONS`, `IEEE_ARITHMETIC`, `IEEE_FEATURES`) in
    the `USE, INTRINSIC ::` path.
  - This is an intentional simplification: the F2003 grammar focuses on
    the IEEE arithmetic modules which require dedicated token handling
    for their exception types, special values, and rounding modes.
  - `ISO_FORTRAN_ENV` support (with named constants like `INPUT_UNIT`,
    `OUTPUT_UNIT`, `ERROR_UNIT`, `IOSTAT_END`, `IOSTAT_EOR`, and kind
    type parameters from later standards) is modeled in the Fortran
    2018 grammar via the more general `intrinsic_module_name` rule that
    accepts `ieee_module_name | IDENTIFIER`.
  - Fixtures that need `USE, INTRINSIC :: ISO_FORTRAN_ENV` should use
    the Fortran 2018 parser or use a non-intrinsic `USE` statement when
    syntactic validation is the goal.
- Rename syntax in `ONLY` lists:
  - The F2003 grammar's `ieee_only_list` does not support rename syntax
    (`local_name => use_name`) for IEEE module entities.
  - The F2018 grammar's `only_item_f2018` supports renames via the
    `IDENTIFIER POINTER_ASSIGN only_item_target_f2018` alternative.
  - For F2003-level parsing of rename syntax in USE statements, use
    the generic `USE module COMMA ONLY COLON only_list` form with a
    non-intrinsic module.

Gaps that warrant explicit issues:

- **RESOLVED (Issue #186)**: A semantic validation layer for C
  interoperability and IEEE arithmetic has been implemented in
  `tools/f2003_semantic_validator.py`. This module:
  - Validates that `BIND(C)` entities are tracked and reported
  - Checks that C interoperable types require `USE ISO_C_BINDING`
  - Validates `VALUE` attribute usage in C interoperability context
  - Detects IEEE module imports and validates entity usage
  - Provides detailed diagnostics with ISO section references
  - Test coverage in `tests/Fortran2003/test_issue186_semantic_validation.py`
- Issue #244 tracks the documentation of `ISO_FORTRAN_ENV` scope in the
  F2003 grammar (this section).

---

## 9. Fixed‑form vs free‑form and layout

Specification:

- F2003 continues to allow both fixed‑form and free‑form source.
- Fixed‑form retains column‑based rules from earlier standards; F2003
  itself does not radically change layout.

Grammar implementation:

- Lexer:
  - `Fortran2003Lexer.g4`:
    - Inherits free‑form support from `Fortran95Lexer`.
    - Adds fixed‑form comment tokens:
      - `FIXED_FORM_COMMENT`, `FIXED_FORM_COMMENT_STAR`, `STAR_COMMENT`.
    - Defines `CONTINUATION` and overrides `NEWLINE` to support unified
      fixed/free‑form handling.
    - Introduces `LSQUARE`/`RSQUARE` for F90+ array constructors.
- Parser:
  - `program_unit_f2003` and related rules accept leading/trailing
    `NEWLINE*` so that fixed‑form layout is handled more robustly.
  - Fixed‑form layout is otherwise treated leniently (columns are not
    enforced).

Tests:

- `test_issue72_fixed_form_f2003.py` and
  `test_fortran_2003_comprehensive.py::test_fixed_form_compatibility`
  exercise:
  - Fixed‑form `.f` code that uses F2003 features such as OOP, CLASS,
    SELECT TYPE, PDTs, and BIND(C).

Gaps:

- As with earlier standards, the lexer does not enforce strict
  column‑based rules for fixed‑form source (labels, continuation
  column, 72‑column line length, etc.).
  - This is a conscious, documented leniency in favour of practical
    parsing of real‑world code rather than card‑accurate emulation.

Gaps that warrant explicit issues:

- Fixed‑form layout strictness is already tracked at the cross‑standard
  level (see the fixed‑form documentation and earlier issues for
  FORTRAN/F77/F90). No additional F2003‑specific grammar gaps were
  found here.

---

## 10. Expression model, array constructors, and intrinsic calls

Specification:

- Expressions in F2003 extend F90/F95 with:
  - PDT and OOP‑driven constructs (structure constructors, PDT
    constructors, type‑bound procedure calls).
  - IEEE intrinsic functions and constants.
  - The existing F90 precedence and array expression machinery.

Grammar implementation:

- Expression rule:
  - `expr_f2003` builds on the F90 expression model but:
    - Extends primary forms via `primary` to include:
      - `pdt_structure_constructor`.
      - Array constructors `array_constructor` using square brackets.
      - IEEE constants and intrinsic function calls.
      - Prefixed character literals (e.g. `c_char_'Hello'`).
- Array constructors:
  - `array_constructor` is defined with `LSQUARE ... RSQUARE` and
    allows:
    - Nested expressions (`expr_f2003`).
    - Constructor‑like calls (`identifier_or_keyword LPAREN actual_arg_list? RPAREN`).
  - F2003 itself does not introduce new array‑constructor syntax over
    F90; brackets are primarily associated with later standards.

Tests:

- `test_f2003_parse_trees.py` and comprehensive fixtures include:
  - Array constructors with square brackets.
  - PDT structure constructors nested in expressions.
  - IEEE constants and functions in expressions.

Gaps / historical inaccuracies:

- As with Fortran 95:
  - Square‑bracket array constructors are historically more closely
    associated with Fortran 2003+ and later, while Fortran 95 only has
    `(/ ... /)`. In this grammar family:
    - F95 exposes `[ ... ]` as an extension.
    - F2003 relies on those bracket tokens as a practical array
      constructor syntax, aligning more with modern Fortran practice
      than with a strictly historical view of F95 vs F2003.
- The expression rule remains deliberately permissive:
  - Precedence and conformability are not fully encoded; this follows
    the same “syntactic only” philosophy as the F90 expression grammar.

Gaps that warrant explicit issues:

- Bracket constructors have already been called out as historically a
  post‑F95 feature in the F95 audit (see issue #181). No additional
  F2003‑specific grammar change is required, but any future tightening
  should consider the full F90–F2003 chain holistically.

---

## 11. Summary and issue mapping

**xfail Fixtures:** 1 (tracked by Issue #309)

The Fortran 2003 layer in this repository:

- **Implements and tests, for practical use:**
  - Program/module structure with F2003‑specific spec/execution parts.
  - OOP features: type extension, type‑bound procedures, generics,
    FINAL procedures, ABSTRACT types and ABSTRACT INTERFACE.
  - Polymorphism via CLASS, CLASS(*), and SELECT TYPE.
  - Parameterized derived types and PDT structure constructors.
  - Procedure pointers and abstract interfaces.
  - ASSOCIATE and BLOCK constructs.
  - Enhanced ALLOCATE/DEALLOCATE with SOURCE, MOLD, STAT, ERRMSG.
  - F2003 I/O (WAIT, FLUSH, extended OPEN/READ/WRITE/CLOSE) and
    generic defined derived‑type I/O at the READ/WRITE level.
  - C interoperability syntax and IEEE intrinsic modules.
  - Unified fixed/free‑form lexical support for F2003 programs.
- **Intentionally leaves to semantic tooling:**
  - C interop type correctness and VALUE rules.
  - IEEE arithmetic semantics beyond name recognition.
  - PDT and OOP semantic constraints that involve type parameter
    values, abstract vs concrete types, and binding characteristics.
  - J3‑style “characteristics” rules for interfaces and procedure
    pointers.
- **Contains a few deliberate or historical simplifications:**
  - SELECT TYPE guards treat `IS` as a generic identifier.
  - DT edit descriptors inside FORMAT are not parsed as a dedicated
    grammar; only generic READ(FORMATTED)/WRITE(UNFORMATTED) mapping is
    modeled.
  - Square‑bracket array constructors are treated as available in the
    F95/F2003 era even though historically they are later additions.

**Grammar Gap (Issue #309):**

| Fixture | Gap |
|---------|-----|
| `fixed_form_f2003.f` | Column-1 C comment handling in fixed-form |

Existing umbrella issues relevant to this audit:

- #140 – **Standard audits** (this document is the F2003 slice).
- #309 – **Fixed-form source with column-1 C comments not parsed**.
- #175 – **Fortran 2003: annotate grammar with J3/03‑007 sections**:
  - **RESOLVED**: The grammar files `Fortran2003Parser.g4` and
    `Fortran2003Lexer.g4` have been annotated with comprehensive
    ISO/IEC 1539-1:2004 section references throughout.
  - Parser annotations cover:
    - Program structure (Section 2.1, 11)
    - Derived types and OOP (Section 4.5)
    - Parameterized derived types (Section 4.5.3)
    - Type-bound procedures (Section 4.5.4)
    - Type extension and polymorphism (Section 4.5.6)
    - ASSOCIATE and BLOCK constructs (Section 8.1.3, 8.1.4)
    - SELECT TYPE (Section 8.1.5)
    - Procedure pointers and interfaces (Section 12.3.2)
    - Enhanced I/O (Section 9)
    - C interoperability (Section 15)
    - IEEE arithmetic modules (Section 14)
    - Expressions (Section 7)
  - Lexer annotations cover all F2003-specific keywords organized by
    their ISO standard sections.

Additional issues (either existing or to be opened) should cover:

- SELECT TYPE guard/tokenization accuracy for `TYPE IS`/`CLASS IS`.
- #185 – **DT edit descriptors** (resolved): documented decision to keep
  them as opaque character strings, with test coverage added.
- #186 – **C interoperability and IEEE arithmetic semantics** (resolved):
  implemented `tools/f2003_semantic_validator.py` with comprehensive
  validation of BIND(C) entities, ISO_C_BINDING imports, VALUE attribute
  usage, and IEEE module entity tracking. Test suite in
  `tests/Fortran2003/test_issue186_semantic_validation.py` (33 tests).
- Semantic-level checks for PDTs and procedure pointer characteristics.

When those issues and their follow‑ups are addressed, Fortran 2003 in
this repo will have a fully annotated grammar and an audit that
honestly reflects both the supported subset and any deliberate
divergences from ISO/IEC 1539‑1:2004.

