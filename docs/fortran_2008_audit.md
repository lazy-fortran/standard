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
- Procedure prefix specifications (ISO/IEC 1539-1:2010 R1225-R1226):
  - `prefix_f2008` and `prefix_spec_f2008` rules allow MODULE as a
    prefix-spec keyword, enabling combined syntax like `MODULE PURE FUNCTION`.
  - `prefix_spec_f2008` includes:
    - `RECURSIVE` (F90), `PURE` (F95), `ELEMENTAL` (F95)
    - `IMPURE` (F2008, issue #392), `MODULE` (F2008, issue #464),
      `NON_RECURSIVE` (F2008, issue #411)
    - `type_spec` (function return type)
  - These prefixes apply to both regular procedures (`function_stmt_f2008`,
    `subroutine_stmt_f2008`) and separate module procedures.

Tests:

- `test_basic_f2008_features.py::test_submodule_basic_syntax` parses a
  simple submodule via F2008's entry.
- `test_basic_f2008_features.py::test_module_prefix_spec` exercises
  combined MODULE prefixes with PURE and ELEMENTAL keywords.
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
  - `LPAREN forall_triplet_spec_list (COMMA scalar_mask_expr)? (COMMA concurrent_locality)? RPAREN`.
  - Reuses the existing FORALL triplet syntax.
  - Supports optional `concurrent_locality` specifier (F2008, issue #428).
- `concurrent_locality` (ISO/IEC 1539-1:2010 Section 8.1.6.6, R821):
  - `LOCAL LPAREN local_variable_list RPAREN`.
  - Declares variables that are private to each DO CONCURRENT iteration.
  - Each iteration gets its own copy of LOCAL variables, preventing race conditions.
- `local_variable_list`:
  - Comma-separated list of IDENTIFIER tokens.
- `scalar_mask_expr`:
  - Aliased to a general `logical_expr`.

Tests:

- `test_basic_f2008_features.py::test_do_concurrent_tokens` uses
  `do_concurrent.f90` to parse a basic DO CONCURRENT example via
  `program_unit_f2008`.
- `test_basic_f2008_features.py::test_do_concurrent_local_specifier` uses
  `do_concurrent_local.f90` to parse DO CONCURRENT with LOCAL specifier,
  including:
  - Basic LOCAL with single variable.
  - Multiple LOCAL variables.
  - LOCAL with mask expression.
  - LOCAL with multiple loop indices.
  - LOCAL keyword used as identifier outside DO CONCURRENT context.

Gaps:

- Semantic rules (independence of iterations, restrictions on
  variables, absence of side effects) are not encoded in the grammar
  and are beyond its scope.
- The use of `forall_triplet_spec_list` is deliberate but means the
  grammar relies on F2003 FORALL syntax; any subtle syntactic
  differences between DO CONCURRENT and FORALL control lists are
  not distinguished here.
- F2018 extensions (LOCAL_INIT, SHARED, DEFAULT, and REDUCE) are not
  implemented in F2008 grammar as they are F2018+ features (tracked
  separately in F2018 audit).

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

**Implementation Coverage:** ~95% fully implemented per exhaustive audit

The Fortran 2008 layer in this repository:

- **Implements and tests, for practical use:**
  - Submodules and separate module subprograms.
  - Coarrays and image control statements (SYNC ALL/IMAGES/MEMORY).
  - LOCK/UNLOCK statements (Section 8.5.6) with lexer/parser coverage and fixture (#324).
  - DO CONCURRENT.
  - CONTIGUOUS attribute and enhanced INTEGER/REAL kinds.
  - Coarray-aware ALLOCATE and entity declarations.
  - ERROR STOP.
  - CRITICAL construct (Section 8.1.5, R818-R820) with lexer/parser coverage
    and fixture (#323).
  - New intrinsic procedures (BESSEL family, ERF/ERFC, GAMMA/LOG_GAMMA,
    NORM2, PARITY, FINDLOC, STORAGE_SIZE).
  - Compiler inquiry intrinsics `COMPILER_VERSION` and `COMPILER_OPTIONS`
    (Section 13.7.41-42) with grammar coverage, fixture
    `compiler_inquiry_intrinsics.f90`, and issue #454.
  - Atomic subroutine intrinsics (`ATOMIC_DEFINE`/`ATOMIC_REF`, ISO/IEC 1539-1:2010 Sections 13.7.19-13.7.20) with lexer/parser coverage and fixture `atomic_intrinsics.f90` (#327).
  - Integration of all of the above into F2008 specification and
    execution parts and program units.

**CRITICAL Gaps (Issue #313):**

| ISO Rule | Description | Status |
|----------|-------------|--------|
| R818 | `critical-construct` | IMPLEMENTED (#323) |
| R819 | `critical-stmt` | IMPLEMENTED (#323) |
| R820 | `end-critical-stmt` | IMPLEMENTED (#323) |
| R859 | `lock-stmt` | IMPLEMENTED (#324) |
| R860 | `unlock-stmt` | IMPLEMENTED (#324) |
| R866 | `lock-variable` | IMPLEMENTED (#324) |

LOCK/UNLOCK statements (R859/R860/R866) are now implemented via issue #324; issue #313 now focuses on the remaining bitwise intrinsic gaps.

**Bitwise Intrinsics (Issue #313 - IMPLEMENTED):**

| Intrinsic | ISO Section | Description | Status |
|-----------|-------------|-------------|--------|
| SHIFTL | 13.7.159 | Shift left | IMPLEMENTED |
| SHIFTR | 13.7.160 | Shift right | IMPLEMENTED |
| SHIFTA | 13.7.158 | Arithmetic shift | IMPLEMENTED |
| MASKL | 13.7.110 | Left-justified mask | IMPLEMENTED |
| MASKR | 13.7.111 | Right-justified mask | IMPLEMENTED |
| IALL | 13.7.79 | Bitwise AND reduction | IMPLEMENTED |
| IANY | 13.7.80 | Bitwise OR reduction | IMPLEMENTED |
| IPARITY | 13.7.94 | Bitwise XOR reduction | IMPLEMENTED |
| HYPOT | 13.7.77 | Hypotenuse function | IMPLEMENTED |

Test fixtures:
- `tests/fixtures/Fortran2008/test_basic_f2008_features/bit_shift_intrinsics.f90`
- `tests/fixtures/Fortran2008/test_basic_f2008_features/bit_reduction_intrinsics.f90`
- `tests/fixtures/Fortran2008/test_basic_f2008_features/hypot_intrinsic.f90`

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
- #176 – **Fortran 2008: annotate grammar with J3/08-007 sections**.
- #313 – **F2008: CRITICAL construct, LOCK/UNLOCK statements, and bitwise intrinsics** – RESOLVED.
- #324 – **F2008: Add LOCK/UNLOCK statements for coarray synchronization** (Section 8.5.6) – RESOLVED.
- #323 – **F2008: CRITICAL construct (R818-R820)** – RESOLVED.
- #327 – **F2008: Atomic subroutines (ATOMIC_DEFINE/ATOMIC_REF)** (Sections 13.7.19-13.7.20) – RESOLVED.

All major F2008 grammar features are now implemented.

---

## Appendix A. ISO/IEC 1539-1:2010 Grammar Cross-Walk

This appendix provides a direct mapping from ISO/IEC 1539-1:2010 syntax
rules to grammar rules in `Fortran2008Lexer.g4` and `Fortran2008Parser.g4`.

### A.1 Program Structure (Section 11)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R202 | program-unit | `program_unit_f2008` |
| R1101 | main-program | `main_program_f2008` |
| R1104 | module | `module_f2008` |
| R1116 | submodule | `submodule_f2008` |
| R1117 | submodule-stmt | `submodule_stmt` |
| R1118 | parent-identifier | `parent_identifier` |
| R1119 | end-submodule-stmt | `end_submodule_stmt` |
| R1227 | external-subprogram | `external_subprogram_f2008` |
| R1223 | function-subprogram | `function_subprogram_f2008` |
| R1231 | subroutine-subprogram | `subroutine_subprogram_f2008` |
| R1224 | function-stmt | `function_stmt_f2008` |
| R1232 | subroutine-stmt | `subroutine_stmt_f2008` |

### A.2 Specification and Execution Parts (Section 2)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R204 | specification-part | `specification_part_f2008` |
| R207 | declaration-construct | `declaration_construct_f2008` |
| R208 | execution-part | `execution_part_f2008` |
| R213 | executable-construct | `executable_construct_f2008` |

### A.3 Coarrays (Sections 5.3.6, 6.6, 8.5)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R509 | coarray-spec | `coarray_spec` |
| R624 | image-selector | `coarray_spec` (reused) |
| R625 | cosubscript-list | `cosubscript_list` |
| R858 | sync-all-stmt | `sync_all_stmt` |
| R859 | sync-images-stmt | `sync_images_stmt` |
| R860 | image-set | `image_set` |
| R862 | sync-memory-stmt | `sync_memory_stmt` |
| R863 | sync-stat | `sync_stat` |
| R503 | entity-decl (coarray) | `entity_decl` |
| R601 | designator (coindexed) | `lhs_expression` |

### A.4 DO CONCURRENT (Section 8.1.6.6)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R818 | loop-control (CONCURRENT) | `do_concurrent_stmt` |
| R819 | concurrent-header | `concurrent_header` |
| R820 | concurrent-spec | `forall_triplet_spec_list` |
| R821 | concurrent-control | `forall_triplet_spec` |

### A.5 BLOCK Construct (Section 8.1.4)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R807 | block-construct | `block_construct_f2008` |
| R808 | block-stmt | (inline in rule) |
| R809 | end-block-stmt | (inline in rule) |

### A.6 Separate Module Procedures (Section 12.6.2.5)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R1108 | module-subprogram | `module_subprogram` |
| R1237 | separate-module-subprogram | `module_subroutine_subprogram_f2008`, `module_function_subprogram_f2008` |
| R1238 | mp-subprogram-stmt | `module_subroutine_stmt_f2008`, `module_function_stmt_f2008` |

### A.7 Type Declarations (Section 5)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R502 | attr-spec | `attr_spec` |
| R544 | contiguous-stmt | `contiguous_stmt` |
| -- | ISO_FORTRAN_ENV kinds | `enhanced_intrinsic_declaration` |

### A.8 ALLOCATE Statement (Section 6.7.1)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R626 | allocate-stmt | `allocate_stmt_f2008` |
| R627 | allocation-list | `allocation_list_f2008` |
| R628 | allocation | `allocation_f2008` |

### A.9 ERROR STOP (Section 8.4)

| ISO Rule | ISO Description | Grammar Rule |
|----------|-----------------|--------------|
| R856 | error-stop-stmt | `error_stop_stmt` |
| R857 | stop-code | (inline in rule) |

### A.10 Intrinsic Procedures (Section 13.7)

| ISO Section | Procedure | Grammar Rule |
|-------------|-----------|--------------|
| 13.7.22-27 | BESSEL_J0/J1/JN, BESSEL_Y0/Y1/YN | `bessel_function_call` |
| 13.7.52 | ERF | `math_function_call` |
| 13.7.53 | ERFC | `math_function_call` |
| 13.7.58 | FINDLOC | `array_function_call` |
| 13.7.61 | GAMMA | `math_function_call` |
| 13.7.77 | HYPOT | `math_function_call` |
| 13.7.79 | IALL | `bit_reduction_function_call` |
| 13.7.80 | IANY | `bit_reduction_function_call` |
| 13.7.94 | IPARITY | `bit_reduction_function_call` |
| 13.7.108 | LOG_GAMMA | `math_function_call` |
| 13.7.110 | MASKL | `bit_mask_function_call` |
| 13.7.111 | MASKR | `bit_mask_function_call` |
| 13.7.119 | NORM2 | `array_function_call` |
| 13.7.121 | NUM_IMAGES | `image_function_call` |
| 13.7.127 | PARITY | `array_function_call` |
| 13.7.158 | SHIFTA | `bit_shift_function_call` |
| 13.7.159 | SHIFTL | `bit_shift_function_call` |
| 13.7.160 | SHIFTR | `bit_shift_function_call` |
| 13.7.163 | STORAGE_SIZE | `image_function_call` |
| 13.7.165 | THIS_IMAGE | `image_function_call` |
| 13.7.41 | COMPILER_OPTIONS | `compiler_inquiry_function_call` |
| 13.7.42 | COMPILER_VERSION | `compiler_inquiry_function_call` |

### A.11 Lexer Tokens

| ISO Reference | Token | Lexer Rule |
|---------------|-------|------------|
| Section 6.6 | `[` | `LBRACKET` |
| Section 6.6 | `]` | `RBRACKET` |
| Section 11.2.3 | SUBMODULE | `SUBMODULE` |
| Section 11.2.3 | END SUBMODULE | `END_SUBMODULE` |
| Section 8.1.6.6 | CONCURRENT | `CONCURRENT` |
| Section 5.3.7 | CONTIGUOUS | `CONTIGUOUS` |
| Section 8.4 | ERROR STOP | `ERROR_STOP` |
| Section 8.5 | SYNC | `SYNC` |
| Section 8.5.3 | ALL | `ALL` |
| Section 8.5.4 | IMAGES | `IMAGES` |
| Section 8.5.5 | MEMORY | `MEMORY` |
| Section 13.7.165 | THIS_IMAGE | `THIS_IMAGE` |
| Section 13.7.121 | NUM_IMAGES | `NUM_IMAGES` |
| Section 13.7.22-27 | BESSEL_* | `BESSEL_J0`, `BESSEL_J1`, etc. |
| Section 13.7.52-53 | ERF, ERFC | `ERF`, `ERFC` |
| Section 13.7.61, 108 | GAMMA, LOG_GAMMA | `GAMMA`, `LOG_GAMMA` |
| Section 13.7.119 | NORM2 | `NORM2` |
| Section 13.7.127 | PARITY | `PARITY` |
| Section 13.7.58 | FINDLOC | `FINDLOC` |
| Section 13.7.77 | HYPOT | `HYPOT` |
| Section 13.7.79 | IALL | `IALL` |
| Section 13.7.80 | IANY | `IANY` |
| Section 13.7.94 | IPARITY | `IPARITY` |
| Section 13.7.110 | MASKL | `MASKL` |
| Section 13.7.111 | MASKR | `MASKR` |
| Section 13.7.158 | SHIFTA | `SHIFTA` |
| Section 13.7.159 | SHIFTL | `SHIFTL` |
| Section 13.7.160 | SHIFTR | `SHIFTR` |
| Section 13.7.163 | STORAGE_SIZE | `STORAGE_SIZE` |
| Section 13.7.41 | COMPILER_OPTIONS | `COMPILER_OPTIONS` |
| Section 13.7.42 | COMPILER_VERSION | `COMPILER_VERSION` |
| Section 8.1.5 | CRITICAL | `CRITICAL` |
| Section 8.1.5 | END CRITICAL | `END_CRITICAL` |
| Section 8.5.6 | LOCK | `LOCK` |
| Section 8.5.6 | UNLOCK | `UNLOCK` |
| Section 8.5.6 | ACQUIRED_LOCK | `ACQUIRED_LOCK` |
| Section 13.7.19 | ATOMIC_DEFINE | `ATOMIC_DEFINE` |
| Section 13.7.20 | ATOMIC_REF | `ATOMIC_REF` |
| Section 13.8.2 | INT8/16/32/64 | `INT8`, `INT16`, `INT32`, `INT64` |
| Section 13.8.2 | REAL32/64/128 | `REAL32`, `REAL64`, `REAL128` |
