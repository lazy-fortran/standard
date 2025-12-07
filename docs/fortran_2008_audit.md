# Fortran 2008 (ISO/IEC 1539‑1:2010) – Grammar Audit (status: in progress)

This audit describes what the **Fortran 2008** grammar in this repository
implements and how it relates to the Fortran 2008 language as defined in:

- ISO/IEC 1539‑1:2010 (Fortran 2008).
- J3/08‑007 “Fortran 2008” draft/final text, stored as
  `validation/pdfs/Fortran2008_J3_08-007.pdf`.

The OCR’d `.txt` file is currently empty, so this audit uses the known
F2008 feature set plus the local PDF and compares that to:

- `grammars/Fortran2008Lexer.g4`, `grammars/Fortran2008Parser.g4`
- Inherited grammars:
  - `grammars/Fortran2003Lexer.g4`, `grammars/Fortran2003Parser.g4`
- Tests under `tests/Fortran2008/`:
  - `test_basic_f2008_features.py`
  - `test_f2008_coarrays.py`
  - `test_f2008_submodules.py`

As with the earlier standards, this document is **spec‑aware and
implementation‑driven**: every divergence or simplification that we
identify is backed by a GitHub issue.

---

## 1. Program structure and F2008 entry points

Specification:

- Fortran 2008 retains the F2003 program‑unit forms (main program,
  module, external subprograms, block data) and adds:
  - Submodules and separate module subprograms.
  - DO CONCURRENT loops.
  - Coarray constructs and image control statements.

Grammar implementation:

- Entry rule for F2008 programs:
  - `program_unit_f2008`:
    - Accepts leading/trailing `NEWLINE*`.
    - Chooses among:
      - `main_program_f2008`
      - `module_f2008`
      - `submodule_f2008`
      - `external_subprogram_f2008`.
- Main program:
  - `main_program_f2008`:
    - `program_stmt specification_part_f2008? execution_part_f2008? internal_subprogram_part? end_program_stmt`.
  - Overrides F2003’s main program to use F2008 spec/execution parts
    (which know about coarrays, DO CONCURRENT, ERROR STOP, etc.).
- Modules and external subprograms:
  - `module_f2008` and `external_subprogram_f2008` mirror the F2003
    structure, but also use `specification_part_f2008` and
    `execution_part_f2008`.
- Global overrides:
  - The bottom of `Fortran2008Parser.g4` overrides:
    - `specification_part : specification_part_f2008`.
    - `execution_part : execution_part_f2008`.
    - `program_unit : program_unit_f2008`.
  - This ensures that F2008 entry points and execution/specification
    parts are used whenever the F2008 parser is invoked, while still
    inheriting all F2003 features.

Tests:

- `test_basic_f2008_features.py`, `test_f2008_coarrays.py`,
  `test_f2008_submodules.py` all parse via `program_unit_f2008()` and
  exercise:
  - Main programs, modules, and submodules.
  - Coarray constructs and image control.
  - DO CONCURRENT, CONTIGUOUS, enhanced intrinsics, and ERROR STOP.

Gaps:

- Block data remains inherited from F2003/F95; F2008 does not add new
  block data semantics, so no dedicated F2008 block data rule exists.
  This is acceptable historically.

---

## 2. Submodules and separate module subprograms

Specification:

- Fortran 2008 introduces submodules:
  - `SUBMODULE (parent) submodule-name` and `END SUBMODULE [submodule-name]`.
  - Parent identifiers can reference modules and nested submodules.
  - Separate module procedures can be defined in submodules using:
    - `MODULE SUBROUTINE name(...)` and
    - `MODULE FUNCTION name(...)`.

Grammar implementation:

- Submodule definition:
  - `submodule_f2008`:
    - `submodule_stmt specification_part_f2008? module_subprogram_part? end_submodule_stmt`.
  - `submodule_stmt`:
    - `SUBMODULE ( parent_identifier ) submodule_identifier NEWLINE`.
  - `parent_identifier`:
    - `IDENTIFIER (COLON IDENTIFIER)*` to model
      `parent_module[:parent_submodule[:...]]`.
  - `end_submodule_stmt`:
    - `END_SUBMODULE (submodule_identifier)? NEWLINE`.
- Separate module subprograms:
  - `module_subroutine_subprogram_f2008` and
    `module_function_subprogram_f2008`:
    - Use `module_subroutine_stmt_f2008` / `module_function_stmt_f2008`
      with F2008 spec/execution parts, then `end_subroutine_stmt` /
      `end_function_stmt`.
  - `module_subroutine_stmt_f2008`:
    - `MODULE SUBROUTINE name(args...) [binding_spec]`.
  - `module_function_stmt_f2008`:
    - `MODULE FUNCTION name(args...) [suffix] [binding_spec]`.
  - `module_subprogram` is overridden to include these new forms in
    addition to the F2008 regular function/subroutine subprogram rules.

Tests:

- `test_basic_f2008_features.py::test_submodule_basic_syntax` parses a
  simple submodule via F2008’s entry.
- `test_f2008_submodules.py` exercises:
  - Basic SUBMODULE declarations.
  - Parent module + parent submodule hierarchies.
  - Submodules containing module procedure implementations.
  - Nested submodule references.

Gaps:

- The grammar models the basic SUBMODULE hierarchy and separate module
  procedures, but does not encode semantic linkage:
  - It does not verify that a `MODULE SUBROUTINE` in a submodule
    corresponds to a specific interface in a parent module.
  - It does not check that parent identifiers exist and match the
    actual module/submodule tree.
  - These are out of scope for syntax and belong to semantic tooling.

---

## 3. Coarrays and image control

Specification:

- Fortran 2008 extends Fortran with coarrays (SPMD model):
  - Coarray declarations with codimensions `[ * ]`, `[ n ]`, and more
    complex cosubscripts.
  - Image control statements:
    - `SYNC ALL`, `SYNC IMAGES`, `SYNC MEMORY`.
  - Image inquiry functions:
    - `THIS_IMAGE`, `NUM_IMAGES`, etc.
  - Coarray references in variables and array sections.

Grammar implementation:

- Coarray declarations:
  - `coarray_spec`:
    - `LBRACKET cosubscript_list RBRACKET`.
  - `cosubscript_list`:
    - One or more `cosubscript`s, or `*` (`[*]` for “any image”).
  - `cosubscript`:
    - Expressions with `:`, `lower:`, `:upper`, `lower:upper`, or bare
      `:` to denote sets of images.
- Image control statements:
  - `sync_construct`:
    - `sync_all_stmt | sync_images_stmt | sync_memory_stmt`.
  - `sync_all_stmt`:
    - `SYNC ALL [ (sync_stat_list) ]`.
  - `sync_images_stmt`:
    - `SYNC IMAGES ( image_set [, sync_stat_list] )`.
  - `sync_memory_stmt`:
    - `SYNC MEMORY [ (sync_stat_list) ]`.
  - `image_set`:
    - `*`, `expr_f2003`, `[expr, ...]` using `LSQUARE`/`RSQUARE` or
      `LBRACKET`/`RBRACKET` depending on context.
  - `sync_stat_list`/`sync_stat`:
    - `STAT = variable_f90` or `ERRMSG = variable_f90`.
- Coarray‑aware entity and variable references:
  - `entity_decl` is overridden to add an optional trailing
    `coarray_spec`.
  - `lhs_expression` is extended with optional `coarray_spec` on the
    left‑hand side of assignments, covering:
    - Simple variables, array elements/sections, components, and
      combinations thereof.
  - `variable_f2008` introduces an F2008‑specific variable rule which
    overlays IDENTIFIER + optional coarray codimensions.
- Image functions and intrinsics:
  - `image_function_call` within `intrinsic_function_call_f2008`
    covers:
    - `THIS_IMAGE(...)`, `NUM_IMAGES(...)`, and `STORAGE_SIZE(...)`.

Tests:

- `test_basic_f2008_features.py`:
  - Exercises simple coarray declarations and `SYNC ALL` via fixtures.
- `test_f2008_coarrays.py`:
  - Coarray declaration with `[*]`.
  - SYNC ALL / SYNC IMAGES / SYNC MEMORY.
  - THIS_IMAGE and NUM_IMAGES functions.
  - Comprehensive coarray module combining coarrays and sync operations.
  - Coarray array section references with image selectors.

Gaps:

- Image control and coarray semantics:
  - The grammar does not enforce the full coarray semantics from
    Fortran 2008 (e.g. restrictions on where SYNC statements may
    appear, constraints on coarray ranks and codimensions, or ordering
    guarantees). These are semantic and must be handled downstream.
- `variable_f2008` vs F2003 variable rules:
  - `lhs_expression` and `entity_decl` are coarray‑aware, but the
    F2003 `variable_f90`/`variable_f2003` rules are still present for
    non‑coarray contexts. This is acceptable but means that some
    legacy variable forms remain under F2003 rules while F2008
    functions primarily cover coarrays and new intrinsics.

---

## 4. DO CONCURRENT and enhanced DO constructs

Specification:

- Fortran 2008 adds `DO CONCURRENT` as an explicitly parallel loop:
  - `DO CONCURRENT (header [, mask]) ... END DO`.
  - The header uses triplet specifications similar to FORALL.
  - Mask must be a scalar LOGICAL expression.

Grammar implementation:

- `do_construct_f2008`:
  - Extends F2003’s `do_construct` with `do_concurrent_construct`.
- `do_concurrent_construct`:
  - `do_concurrent_stmt execution_part_f2008? end_do_stmt`.
- `do_concurrent_stmt`:
  - Optional construct name + `DO CONCURRENT concurrent_header NEWLINE`.
- `concurrent_header`:
  - `LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? RPAREN`.
  - Reuses the existing FORALL triplet syntax.
- `scalar_mask_expr`:
  - Aliased to a general `logical_expr`.

Tests:

- `test_basic_f2008_features.py::test_do_concurrent_tokens` uses
  `do_concurrent.f90` to parse a basic DO CONCURRENT example via
  `program_unit_f2008`.

Gaps:

- Semantic rules (independence of iterations, restrictions on
  variables, absence of side effects) are not encoded in the grammar
  and are beyond its scope.
- The use of `forall_triplet_spec_list` is deliberate but means the
  grammar relies on F2003 FORALL syntax; any subtle syntactic
  differences between DO CONCURRENT and FORALL control lists are
  not distinguished here.

---

## 5. CONTIGUOUS, enhanced ALLOCATE, and integer kinds

Specification:

- Fortran 2008 adds:
  - The CONTIGUOUS attribute for pointers/allocatables.
  - Additional intrinsic integer and real kinds: INT8/16/32/64 and
    REAL32/64/128.
  - Enhancements to ALLOCATE for coarrays and type‑spec forms.

Grammar implementation:

- CONTIGUOUS:
  - `CONTIGUOUS` keyword in the lexer.
  - `contiguous_stmt`:
    - `CONTIGUOUS :: object_name_list`.
  - `attr_spec` is overridden to include `CONTIGUOUS` as a valid
    attribute in type declarations.
- Enhanced intrinsic declarations:
  - `enhanced_intrinsic_declaration`:
    - `INT8/INT16/INT32/INT64` and `REAL32/REAL64/REAL128` with
      optional attribute specs and entity declaration lists.
  - `type_declaration_stmt_f2008`:
    - Either `type_declaration_stmt` (F2003) or
      `enhanced_intrinsic_declaration`.
- Enhanced ALLOCATE:
  - `allocate_stmt_f2008`:
    - `ALLOCATE ( type_spec :: allocation_list_f2008 ... )` or
      `ALLOCATE ( allocation_list_f2008 ... )`.
  - `allocation_f2008`:
    - `IDENTIFIER coarray_spec? (shape)?` or
      `derived_type_spec :: IDENTIFIER coarray_spec? (shape)?`.
  - This extends F2003’s allocation with explicit coarray codimensions.

Tests:

- `test_basic_f2008_features.py`:
  - `integer_kinds.f90` for INT*/REAL* declarations.
  - `contiguous_attribute.f90` and `contiguous_statement.f90` for the
    CONTIGUOUS attribute in both inline and standalone forms.
  - Allocation forms are covered in the coarray and intrinsic fixtures.

Gaps:

- As in earlier standards, the grammar does not enforce:
  - That specific integer or real kinds are available on the target
    implementation.
  - All constraints on how CONTIGUOUS interacts with POINTER/
    ALLOCATABLE or coarray declarations.
  - These are left to semantic analysis and implementation‑specific
    availability checks.

---

## 6. Enhanced ERROR STOP

Specification:

- Fortran 2008 adds `ERROR STOP` for error termination:
  - With an optional integer or character stop code.

Grammar implementation:

- `ERROR_STOP` token in the lexer.
- `error_stop_stmt`:
  - `ERROR_STOP (INTEGER_LITERAL | string_literal)? NEWLINE`.

Tests:

- `test_basic_f2008_features.py`:
  - `error_stop.f90` and `error_stop_no_code.f90` cover both forms.

Gaps:

- No syntactic gaps; semantics (whether ERROR STOP is permitted in a
  particular context, or how the code is interpreted) are outside the
  grammar.

---

## 7. Enhanced intrinsic functions and image intrinsics

Specification:

- Fortran 2008 adds intrinsic procedures such as:
  - Bessel functions (BESSEL_J0/J1/JN, BESSEL_Y0/Y1/YN).
  - ERF/ERFC, GAMMA/LOG_GAMMA.
  - NORM2, PARITY, FINDLOC, STORAGE_SIZE.
  - Enhanced forms of existing intrinsics and image-related intrinsics.

Grammar implementation:

- Lexer:
  - `BESSEL_J0`, `BESSEL_J1`, `BESSEL_JN`, `BESSEL_Y0`, `BESSEL_Y1`,
    `BESSEL_YN`, `ERF`, `ERFC`, `GAMMA`, `LOG_GAMMA`, `NORM2`,
    `PARITY`, `FINDLOC`, `STORAGE_SIZE`.
- Parser:
  - `intrinsic_function_call_f2008` extends `intrinsic_function_call`
    (F2003) with:
    - `bessel_function_call`
    - `math_function_call`
    - `array_function_call`
    - `image_function_call` (THIS_IMAGE/NUM_IMAGES/STORAGE_SIZE).
  - `primary` is overridden so that intrinsic calls go through
    `intrinsic_function_call_f2008`, which wraps the F2003 set.

Tests:

- `test_basic_f2008_features.py`:
  - `enhanced_intrinsics.f90` and
    `enhanced_intrinsics_named_args.f90` exercise:
    - New intrinsic functions.
    - Named arguments to those intrinsics.
  - `image_intrinsics.f90` covers basic image inquiry intrinsics.

Gaps:

- As with earlier standards, the grammar does not encode:
  - Argument rank, type, or keyword restrictions for these intrinsics.
  - Any semantic invariants about how they should be used (e.g. where
    FINDLOC may appear).
  - These are semantic concerns and must be handled in later passes.

---

## 8. Execution and specification parts integration

Specification:

- F2008 allows:
  - Mixed declarations and executable statements in blocks and main
    program bodies.
  - The new constructs (DO CONCURRENT, coarrays, SYNC, ERROR STOP,
    submodules) to appear in the same contexts as existing constructs
    subject to semantic rules.

Grammar implementation:

- `specification_part_f2008`:
  - Wraps `use_stmt`, `import_stmt`, `implicit_stmt`, and the new
    `declaration_construct_f2008`.
  - `declaration_construct_f2008` includes:
    - F2003 derived types and CLASS declarations.
    - Procedure declarations.
    - Interface blocks.
    - VOLATILE/PROTECTED.
    - `contiguous_stmt` and F2008 type declarations.
    - A fall‑through to `declaration_construct_f2003`.
- `execution_part_f2008` and `executable_construct_f2008`:
  - Extend F2003’s `executable_construct_f2003_inner` with:
    - `error_stop_stmt`
    - `block_construct_f2008`
    - `allocate_stmt_f2008`
    - `sync_construct`
    - `do_construct_f2008`
    - `type_declaration_stmt_f2008` (for mixed declarations).
    - And all F2003 constructs.

Tests:

- The F2008 test suite uses realistic modules and programs that combine:
  - F2003 OOP, PDTs, and C interop.
  - F2008 coarrays, DO CONCURRENT, and enhanced intrinsics.

Gaps:

- Fine‑grained ordering rules for declarations vs executable
  statements (e.g. where certain declaration constructs may appear in
  blocks or internal subprograms) are not enforced syntactically.

---

## 9. Fixed‑form vs free‑form and layout (F2008)

Specification:

- Fortran 2008 continues to support both fixed‑form and free‑form
  source; F2008 does not introduce new layout rules.

Grammar implementation:

- The F2008 lexer inherits fixed/free‑form handling from
  `Fortran2003Lexer.g4`:
  - Fixed‑form comment tokens and continuation handling are reused.
  - No additional fixed‑form constraints are added for F2008.
- The parser’s use of `NEWLINE*` around program units and constructs
  follows the same lenient model as F2003.

Tests:

- There are currently no F2008‑specific fixed‑form fixtures; F2008
  relies on the F2003 fixed‑form support, which is already exercised.

Gaps:

- As with F2003 and earlier, strict card/column enforcement for
  fixed‑form is intentionally not implemented; F2008 inherits this
  leniency.

---

## 10. Summary and issue mapping

The Fortran 2008 layer in this repository:

- **Implements and tests, for practical use:**
  - Submodules and separate module subprograms.
  - Coarrays and image control statements.
  - DO CONCURRENT.
  - CONTIGUOUS attribute and enhanced INTEGER/REAL kinds.
  - Coarray‑aware ALLOCATE and entity declarations.
  - ERROR STOP.
  - New intrinsic procedures (BESSEL family, ERF/ERFC, GAMMA/LOG_GAMMA,
    NORM2, PARITY, FINDLOC, STORAGE_SIZE).
  - Integration of all of the above into F2008 specification and
    execution parts and program units.
- **Intentionally leaves to semantic tooling:**
  - Coarray semantics (synchronization rules, ranks, codimension
    constraints).
  - DO CONCURRENT semantics (independence of iterations, side‑effect
    restrictions).
  - CONTIGUOUS, ALLOCATE, and intrinsic‑procedure argument semantics.
  - Submodule linkage between parents and separate module procedures.
  - All implementation‑dependent aspects of integer/real kinds.

Existing umbrella issues relevant to this audit:

- #140 – **Standard audits** (this document is the Fortran 2008 slice).
- #176 – **Fortran 2008: annotate grammar with J3/08‑007 sections**:
  - Should use this audit as the spec→grammar cross‑walk and ensure
    every F2008 gap identified here has its own issue.

Additional issues (existing or to be opened) should cover:

- Semantic checks for coarrays and image control usage.
- DO CONCURRENT semantic validation.
- CONTIGUOUS/coarray/ALLOCATE semantics beyond syntax.
- Submodule/parent linkage validation.

When those issues and their follow‑ups are addressed, and the grammar
annotations required by #176 are in place, Fortran 2008 will have a
complete, spec‑aware grammar audit comparable in depth to the Fortran
90/95/2003 audits already in this repository.

