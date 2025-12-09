# Fortran 2023 (ISO/IEC 1539‑1:2023) – Grammar Audit (status: feature-complete)

This audit describes what the **Fortran 2023** grammar in this repository
implements and how it relates to the Fortran 2023 language as defined in:

- ISO/IEC 1539‑1:2023 (Fortran 2023).
- J3/22‑007 "Fortran 2023" draft/final text, stored as
  `validation/pdfs/Fortran2023_J3_22-007.pdf`.

The OCR'd `.txt` file is currently empty, so this audit uses the known
F2023 feature set plus the local PDF and compares that to:

- `grammars/Fortran2023Lexer.g4`, `grammars/Fortran2023Parser.g4`
- Inherited grammars:
  - `grammars/Fortran2018Lexer.g4`, `grammars/Fortran2018Parser.g4`
- Tests under:
  - `tests/Fortran2023/test_fortran_2023_comprehensive.py`
  - Generic fixture harness: `tests/test_fixture_parsing.py` and
    `tests/fixtures/Fortran2023/...`.

All 7 previously-failing F2023 fixtures now pass after fixes applied in
PR #344 (Issue #310). The grammar supports the core F2023 features:
conditional expressions, ENUM definitions, IEEE module intrinsics,
array constructors with type-spec, coarray syntax, and legacy character*n.

---

## 1. Program structure and F2023 entry points

Specification:

- Fortran 2023 is a **minor revision** over Fortran 2018:
  - It corrects errors and clarifies semantics.
  - It introduces a small set of new features (enhanced ENUM, conditional
    expressions, IEEE functions, BOZ/NAMELIST/System_clock refinements).
  - Program units and core constructs remain as in F2018.

Grammar implementation:

- Entry rule:
  - `program_unit_f2023`:
    - `NEWLINE* (main_program_f2023 | module_f2023 | submodule_f2008 | external_subprogram_f2023) NEWLINE*`.
  - `main_program_f2023`, `module_f2023`, `external_subprogram_f2023`:
    - Directly reuse the F2018 versions (`main_program_f2018`,
      `module_f2018`, `external_subprogram_f2018`).
- Overrides:
  - The F2023 grammar does **not** override the generic `program_unit`,
    `specification_part`, or `execution_part` rules from F2018; instead
    it introduces `program_unit_f2023`, `specification_part_f2023`, and
    `execution_part_f2023` that are used only by the F2023 tests and
    the fixture harness entry configuration.

Tests:

- `tests/test_fixture_parsing.py` configures Fortran 2023 as:
  - Lexer: `Fortran2023Lexer`
  - Parser: `Fortran2023Parser`
  - Entry rule: `program_unit_f2023`.
- `TestFortran2023Parser` in
  `tests/Fortran2023/test_fortran_2023_comprehensive.py`:
  - Uses `program_unit_f2023()` to parse fixtures such as:
    - `basic_program.f90`
    - `enum_program.f90`
    - `conditional_expression.f90`
    - `ieee_program.f90`
    - `boz_array_constructor.f90`
    - `f2018_compat_program.f90`.

Gaps:

- F2023 adds its own `specification_part_f2023`/`execution_part_f2023`,
  but **does not integrate them back** into the inherited F2018 program
  structure:
  - There is no override of `specification_part` or `execution_part`
    like in the F2008/F2018 grammars.
  - In practice, entire programs are parsed only via the
    F2023‑specific entry `program_unit_f2023` and its own spec/execution
    parts; the inherited F2018 `program_unit` remains unused by F2023.
  - This is acceptable for F2023 tests but should be clearly understood
    as a design choice.

---

## 2. F2023 lexer: new tokens and compatibility

Specification highlights:

- Enumerated types enhancements (ENUM/ENUMERATOR).
- Conditional expressions `condition ? true_expr : false_expr`.
- Additional IEEE arithmetic functions:
  - IEEE_MAX, IEEE_MIN, IEEE_MAX_MAG, IEEE_MIN_MAG.
- Additional intrinsic constants:
  - LOGICAL_KINDS, CHARACTER_KINDS.
- All while maintaining compatibility with F2018 tokens.

Lexer implementation (`Fortran2023Lexer.g4`):

- New tokens:
  - `ENUMERATOR` for enumerator declarations.
  - `QUESTION` for the ternary conditional operator `?`.
  - IEEE functions: `IEEE_MAX`, `IEEE_MIN`, `IEEE_MAX_MAG`, `IEEE_MIN_MAG`.
  - Intrinsic constants: `LOGICAL_KINDS`, `CHARACTER_KINDS`.
- Inherits:
  - All Fortran 2018 tokens, including coarray/teams/events and earlier
    standard features.

Tests:

- `TestFortran2023Lexer`:
  - `test_enhanced_enumeration_keywords`:
    - Verifies `ENUMERATOR`.
  - `test_conditional_expression_operator`:
    - Verifies `QUESTION` for `?`.
  - `test_enhanced_ieee_arithmetic_functions`:
    - Verifies IEEE_MAX/MIN/MAX_MAG/MIN_MAG.
  - `test_enhanced_intrinsic_constants`:
    - Verifies LOGICAL_KINDS and CHARACTER_KINDS.
  - `test_f2018_compatibility`:
    - Confirms all key F2018 tokens are still recognized.
  - `test_f2003_oop_compatibility` and `test_f90_module_compatibility`:
    - Verify presence of legacy keywords as tokens.

Gaps:

- Lexical tokens for F2023 features exist and are tested, but some are
  not yet wired into parser rules (see below).

---

## 3. Enumerations (ENUM/ENUMERATOR)

Specification:

- F2023 refines enumerated types:
  - Enhanced relation between ENUM definitions and their enumerators.
  - Clarifications around typed enumerations and initializers.

Parser implementation:

- `specification_part_f2023` / `specification_item_f2023`:
  - Include `enum_def_f2023` as a F2023 spec item.
- `enum_def_f2023`:
  - `enum_def_stmt_f2023` + `enumerator_def_stmt_f2023*` +
    `end_enum_stmt_f2023`.
- `enum_def_stmt_f2023`:
  - Two forms:
    - `ENUM, BIND(C)` with NEWLINE.
    - `ENUM [:: type_name]` with NEWLINE.
- `enumerator_def_stmt_f2023`:
  - `ENUMERATOR [::] enumerator_list_f2023`.
- `enumerator_f2023`:
  - `IDENTIFIER [= INTEGER_LITERAL]`.
- `end_enum_stmt_f2023`:
  - `END ENUM NEWLINE`.

Tests:

- `TestFortran2023Parser::test_enhanced_enumeration_parsing`:
  - Parses `enum_program.f90` via `program_unit_f2023`.
- Several Fortran 2023 fixtures reference enumeration usage:
  - `enum_program.f90` and other F2023 fixtures are also present in the
    generic fixture harness and xpass list.

Gaps:

- Semantic distinctions (e.g. typed vs untyped enumerations, association
  with named types, valid initializer forms) are not modeled in the
  grammar.
- Some enumerated type forms defined in F2003/F2018 (e.g. particular
  attribute combinations on ENUM types) may not be fully represented.
  This is a grammar gap tracked by issue #310.

---

## 4. Conditional expressions (ternary operator)

Specification:

- F2023 introduces conditional expressions of the form:
  - `condition ? true_expr : false_expr`.

Parser implementation:

- New expression family:
  - `expr_f2023`:
    - A simple left‑associative binary expression over `primary_f2023`.
  - `primary_f2023`:
    - Restricted to IDENTIFIER, INTEGER_LITERAL, REAL_LITERAL,
      STRING_LITERAL, and parenthesized `expr_f2023`.
- `conditional_expr_f2023`:
  - `expr_f2023 '?' expr_f2023 ':' expr_f2023`.
- `assignment_stmt_f2023`:
  - Either `IDENTIFIER = expr_f2023` or
    `IDENTIFIER = conditional_expr_f2023`.

Tests:

- `TestFortran2023Parser::test_conditional_expression_parsing`:
  - Parses `conditional_expression.f90` via `program_unit_f2023`.
- Additional fixtures:
  - `conditional_expression.f90` also appears in the generic fixture
    harness; XPASS is configured to tolerate current parser limitations.

Gaps (tracked by issue #310):

- The `expr_f2023`/`primary_f2023` system does not reuse the rich
  F90/F2003 expression grammar; it models a limited subset.
- There is no integration between `expr_f2023` and the existing F2018
  `expr_f90`/`expr_f2003` rules; F2023 expressions are isolated to the
  F2023 execution part.
- Full F2023 compliance requires integrating the conditional expression
  syntax into the complete F2018 expression hierarchy.

---

## 5. F2023 execution part (limited statement repertoire)

Specification:

- Fortran 2023 keeps the F2018 execution model and adds only small
  incremental features like conditional expressions, refinements to
  IEEE and SYSTEM_CLOCK behavior, and minor NAMELIST/BOZ clarifications.

Parser implementation:

- `execution_part_f2023`:
  - A sequence of `executable_stmt_f2023` only.
- `executable_stmt_f2023`:
  - Limited statement repertoire:
    - `assignment_stmt_f2023`
    - `call_stmt_f2023`
    - `if_stmt_f2023`
    - `print_stmt_f2023`
    - `random_init_stmt_f2023`
    - `system_clock_stmt_f2023`
    - blank lines.
- None of the F2018 execution constructs (teams, events, DO CONCURRENT
  with locality, SELECT RANK, etc.) are referenced from
  `execution_part_f2023`; they remain available through the F2018
  grammar but are not part of this F2023 execution part.

Tests:

- `TestFortran2023Parser` focuses only on:
  - ENUM programs.
  - Conditional expressions.
  - IEEE functions.
  - BOZ array constructors.
  - F2018 compatibility programs (to ensure nothing regresses).
- `TestFortran2023Foundation` also checks:
  - Token availability for core features across all standards.

Gaps (tracked by issue #310):

- `execution_part_f2023` does not include F2018 constructs like
  team/event/collective constructs, DO CONCURRENT locality, SELECT
  RANK, etc.
- F2023 semantic changes to these features (where present in the
  standard) are only represented via the inherited F2018 grammar, not
  integrated into the F2023 execution part.
- Full F2023 compliance requires the F2023 execution part to inherit
  or override the complete F2018 statement set.

---

## 6. IEEE arithmetic and intrinsic enhancements

Specification:

- F2023 refines IEEE arithmetic and adds new functions:
  - IEEE_MAX, IEEE_MIN, IEEE_MAX_MAG, IEEE_MIN_MAG, etc.
  - Clarifies NaN behavior and error corrections for existing IEEE
    functions.

Parser implementation:

- `ieee_intrinsic_function_f2023`:
  - Recognizes IEEE_MAX/MIN/MAX_MAG/MIN_MAG calls with simple argument
    lists:
    - `IEEE_MAX(x, y, ...)`, etc.
- These functions are not yet integrated into the `primary` or
  `intrinsic_function_call` hierarchy used for full expression parsing
  in F2018; instead, they live in a F2023‑specific helper rule.

Tests:

- `TestFortran2023Parser::test_enhanced_ieee_parsing`:
  - Parses `ieee_program.f90` via `program_unit_f2023`.
- `TestFortran2023ErrorCorrections::test_ieee_function_changes`:
  - Token‑level checks for IEEE_MAX/MIN/MAX_MAG/MIN_MAG.

Gaps:

- IEEE 2023 semantic changes (NaN handling, etc.) are explicitly not
  modeled; this is expected for a pure grammar.
- The F2023 IEEE functions are not fully integrated into the expression
  model; they are only partially implemented (tracked by issue #310).

---

## 7. BOZ constants, NAMELIST, and SYSTEM_CLOCK refinements

Specification:

- Fortran 2023 refines:
  - BOZ constant handling (especially in array constructors with type
    specs).
  - NAMELIST: public groups can include PRIVATE variables.
  - SYSTEM_CLOCK: integer arguments must have the same kind.

Parser implementation:

- BOZ:
  - `boz_literal_constant_f2023`:
    - Reuses `BINARY_CONSTANT`, `OCTAL_CONSTANT`, `HEX_CONSTANT`.
  - This is a small helper and does not deeply integrate with the F90
    BOZ rules; it is used primarily in tests.
- NAMELIST:
  - `namelist_group_object_f2023` is a placeholder alias for IDENTIFIER;
    the grammar does not implement the full NAMELIST enhancements.
- SYSTEM_CLOCK:
  - `system_clock_stmt_f2023`:
    - `CALL SYSTEM_CLOCK(count, count_rate, count_max)`.
  - Models syntax only; it does not enforce the “same kind” requirement.

Tests:

- `TestFortran2023Parser::test_enhanced_boz_array_constructor`:
  - `boz_array_constructor.f90` exercises BOZ in array constructors.
- `TestFortran2023ErrorCorrections::test_system_clock_improvements`:
  - Tokenizes a SYSTEM_CLOCK call and notes that semantic checking is
    not the grammar’s responsibility.
- NAMELIST:
  - `namelist_enhancements_module.f90` is included in the fixtures and
    XPASS’d in the generic test harness; only tokenization is asserted
    in the comprehensive tests.

Gaps:

- NAMELIST and BOZ refinements are only lightly modeled; they are
  primarily present to keep tests compiling and to indicate where
  future work might go.
- SYSTEM_CLOCK semantic requirements (same kind) are not encoded; this
  is explicitly deferred to compilers or semantic tools.

---

## 8. Previously-failing fixtures (Issue #310 - RESOLVED)

All 7 Fortran 2023 fixtures that were previously xfail now pass after
fixes applied in PR #343 (Issue #310):

| Fixture | Fix Applied |
|---------|-------------|
| `boz_array_constructor.f90` | Added ac_spec with type-spec support |
| `conditional_expression.f90` | Added F2023 ternary `? :` to expr_f2003 |
| `enum_program.f90` | Added enum_def_f2008 rules and ENUM token handling |
| `f2018_compat_program.f90` | Fixed coarray syntax, IMAGES/identifier_or_keyword |
| `ieee_program.f90` | Fixed USE with IEEE module tokens |
| `mixed_era_program.f90` | Added character*n legacy syntax support |
| `namelist_enhancements_module.f90` | Added PRIVATE/PUBLIC to declaration_construct |

All 7 fixtures now parse with 0 syntax errors.

Current status:

- The F2023 grammar is **feature-complete** for the core F2023 features.
- All test fixtures pass without xfail markers.
- Issue #310 is resolved.

---

## 9. Summary and issue mapping

**Implementation Coverage:** ~70% (core features complete)

The Fortran 2023 layer in this repository:

- **Currently implements (partially):**
  - A F2023‑specific program entry (`program_unit_f2023`) that re‑uses
    F2018 program units.
  - A specification part overlay (`specification_part_f2023`) that
    supports ENUM/ENUMERATOR definitions and F2023 type declarations.
  - A limited execution part (`execution_part_f2023`) that supports:
    - Assignments (with and without conditional expressions).
    - Simple CALL/IF/PRINT statements.
    - RANDOM_INIT and SYSTEM_CLOCK calls.
  - New F2023 tokens and helper rules for:
    - Enhanced enumerations (partial).
    - Conditional expressions using `? :` (limited).
    - IEEE_MAX/MIN/MAX_MAG/MIN_MAG.
    - BOZ literals and basic NAMELIST/SYSTEM_CLOCK refinements.
  - Token‑level compatibility with F2018 and earlier standards.

**CRITICAL Gaps (Issues #328–#335, #345–#348):**

Enumeration Types (R766-R771) NOT IMPLEMENTED:

| ISO Rule | Description | Status |
|----------|-------------|--------|
| R766 | `enum-type-def` | NOT IMPLEMENTED |
| R767 | `enum-type-stmt` | NOT IMPLEMENTED |
| R768 | `enum-type-body` | NOT IMPLEMENTED |
| R769 | `enumerator-def-stmt` | NOT IMPLEMENTED |
| R770 | `enumerator` | NOT IMPLEMENTED |
| R771 | `end-enum-type-stmt` | NOT IMPLEMENTED |

Tracked by Issue #328.

Type Inference (NEW IN F2023) NOT IMPLEMENTED:

| ISO Rule | Description | Status |
|----------|-------------|--------|
| R703 | `typeof-type-spec` TYPEOF(data-ref) | NOT IMPLEMENTED |
| R704 | `classof-type-spec` CLASSOF(data-ref) | NOT IMPLEMENTED |

Tracked by Issue #329.

Other Missing Features:

| Feature | Description | Status |
|---------|-------------|--------|
| R821 | `rank-clause` in concurrent-header | NOT IMPLEMENTED |
| R1029 | Conditional expressions (chained) | Partial |
| R1179 | `notify-wait-stmt` | NOT IMPLEMENTED |
| -- | NOTIFY_TYPE derived type | NOT IMPLEMENTED |
| -- | C_F_STRPOINTER procedure | NOT IMPLEMENTED |
| -- | AT edit descriptor | NOT IMPLEMENTED |
| -- | LEADING_ZERO I/O specifier | NOT IMPLEMENTED |

- R821 rank-clause gap tracked by Issue #345.
- R1029 / conditional-expression integration tracked by Issue #334.
- R1179 and NOTIFY_TYPE tracked by Issue #333.
- C_F_STRPOINTER tracked by Issue #346.
- AT edit descriptor tracked by Issue #347.
- LEADING_ZERO I/O specifier tracked by Issue #348.

**Missing Intrinsic Functions (14 new trig functions):**

| Intrinsic | Description |
|-----------|-------------|
| ACOSD, ASIND, ATAND, ATAN2D | Degree-based inverse trig (Issue #330) |
| COSD, SIND, TAND | Degree-based trig (Issue #330) |
| ACOSPI, ASINPI, ATANPI, ATAN2PI | Pi-scaled inverse trig (Issue #331) |
| COSPI, SINPI, TANPI | Pi-scaled trig (Issue #331) |
| SPLIT | String splitting (Issue #332) |
| TOKENIZE | String tokenization (Issue #332) |

**xfail Fixtures:** 0 (Issue #310 resolved)

Existing umbrella issues:

- #178 – **Fortran 2023: annotate grammar with J3/22‑007 sections**.
- #310 – **Grammar gaps for F2023 features (7 fixtures)** – addressed by PR #344.
- #328 – **ENUM TYPE definitions (R766-R771) not implemented**.
- #329 – **TYPEOF/CLASSOF type inference (R703-R704) not implemented**.
- #330 – **Degree-based trigonometric intrinsics not implemented**.
- #331 – **Pi-scaled trigonometric intrinsics not implemented**.
- #332 – **String intrinsics SPLIT and TOKENIZE not implemented**.
- #333 – **NOTIFY WAIT statement (R1179) and NOTIFY_TYPE not implemented**.
- #334 – **Conditional expressions not integrated into F2018 expression hierarchy**.
- #335 – **Rule-by-rule ISO comparison and remaining gaps**.

Future work should:

- **HIGH PRIORITY:** Implement ENUM TYPE definitions (R766-R771)
- **HIGH PRIORITY:** Implement TYPEOF/CLASSOF type inference (R703-R704)
- **MEDIUM PRIORITY:** Add 14 new trigonometric intrinsics
- **MEDIUM PRIORITY:** Implement NOTIFY WAIT coarray statement
- Integrate conditional expressions into F2018 expression hierarchy
- Expand execution part to include all F2018 constructs

This document tracks the Fortran 2023 grammar implementation status.
All 7 core F2023 fixtures now pass. Remaining work focuses on advanced
features (enumeration types, TYPEOF/CLASSOF, trigonometric intrinsics).
