# Fortran 2003 Grammar Audit – Implementation vs. Specification (Issue #73)

This document summarizes the current Fortran 2003 grammar implementation in
this repository and maps it to major language areas from ISO/IEC 1539‑1:2004.
It is intentionally conservative: where functionality has not been exercised
by tests or is only approximated, it is recorded as such.

The goal is to give an honest, spec‑driven overview of what this grammar
supports for practical use, without claiming full ISO conformance.

---

## 1. Program Structure and Scoping

- **Main programs, modules, external subprograms**
  - **Status:** Implemented and tested.
  - **Notes:** `program_unit_f2003` covers program units and modules; tests in
    `tests/Fortran2003/test_fortran_2003_comprehensive.py` and
    `tests/Fortran2003/test_working_f2003.py` exercise simple programs and
    modules with CONTAINS sections.

- **CONTAINS, internal subprograms, interface blocks**
  - **Status:** Implemented and tested.
  - **Notes:** Interface blocks (including ABSTRACT INTERFACE) and module
    subprograms appear in multiple tests, including
    `test_issue22_tdd.py`, `test_issue23_tdd.py` and
    `test_issue24_semantic_c_interop.py`.

- **IMPLICIT NONE / IMPLICIT typing**
  - **Status:** Implemented (simplified) and tested.
  - **Notes:** Grammar provides `implicit_stmt` with `IMPLICIT NONE` and
    simple `IMPLICIT` specifications; usage is tested in most F2003 examples.

---

## 2. Derived Types, OOP and Polymorphism

- **Derived types, TYPE/END TYPE, components**
  - **Status:** Implemented and widely tested.
  - **Notes:** Used throughout the test suite (e.g. `test_simple_f2003.py`,
    `test_fortran_2003_comprehensive.py`, PDT tests, and C interop tests).

- **Type extension and inheritance (`EXTENDS`)**
  - **Status:** Implemented and tested.
  - **Notes:** Inheritance and abstract base types are exercised in the OOP
    tests in `test_fortran_2003_comprehensive.py` and
    `test_f2003_polymorphism_and_c_interop.py`.

- **Type-bound procedures, GENERIC, FINAL, DEFERRED**
  - **Status:** Implemented and tested (Issues #22, #25, #26, #59).
  - **Notes:** The type-bound procedure machinery (`type_bound_procedure_stmt`,
    `type_bound_generic_stmt`, `final_procedure_stmt`) is used in multiple
    OOP tests and passes the targeted TDD suites.

- **Polymorphism: CLASS, CLASS(*) and SELECT TYPE**
  - **Status:** Implemented and well tested (Issues #59, #69).
  - **Notes:** `class_declaration_stmt` and `select_type_construct` handle
    polymorphic declarations and `SELECT TYPE` with TYPE IS / CLASS IS /
    CLASS DEFAULT. Tests:
    - `test_f2003_polymorphism_and_c_interop.py`
    - `test_fortran_2003_comprehensive.py`
    - `test_issue69_select_type_semantics.py`

---

## 3. Parameterized Derived Types (PDTs)

- **PDT definitions (`type :: t(k, n, ...)`) and parameter attributes**
  - **Status:** Implemented and tested (Issues #26, #71).
  - **Notes:** Kind and len parameters, default values and dependent components
    are covered by:
    - `test_issue26_pdt_basic.py`
    - `test_issue71_pdt_extended.py`

- **PDT instantiation and usage**
  - **Status:** Implemented and tested for representative patterns.
  - **Notes:** The grammar supports positional and keyword-like parameter
    specifications via `derived_type_spec` and `type_param_spec_list`. Tests
    exercise:
    - PDTs as components and dummy arguments.
    - PDTs nested inside other derived types.
    - PDTs with deferred (`:`) and assumed (`*`) parameters.

- **Structure constructors for PDTs**
  - **Status:** Partially implemented; lightly tested.
  - **Notes:** Structure constructors are handled via general array/constructor
    rules; PDT-specific constructor corner cases are not fully exercised.

---

## 4. Procedure Pointers and Interfaces

- **Procedure pointers and abstract interfaces**
  - **Status:** Implemented and tested (Issue #23).
  - **Notes:** `procedure_declaration_stmt`, `proc_component_def_stmt` and
    related rules are covered by
    `test_issue23_tdd.py` and
    `test_f2003_polymorphism_and_c_interop.py`.

- **Extended CALL and function references**
  - **Status:** Implemented and tested.
  - **Notes:** Component procedure calls and calls through procedure pointers
    are exercised in the comprehensive F2003 tests.

---

## 5. ASSOCIATE, BLOCK and Control Constructs

- **ASSOCIATE construct**
  - **Status:** Implemented and tested (Issue #25).
  - **Notes:** `associate_construct` is covered in
    `test_fortran_2003_comprehensive.py` and the SELECT TYPE tests.

- **BLOCK construct**
  - **Status:** Implemented and tested (Issue #25).
  - **Notes:** `block_construct` is used in complex combined examples
    (`test_fortran_2003_comprehensive.py`).

- **IF/DO/SELECT CASE constructs**
  - **Status:** Inherited from F90/F95 and tested via multiple F2003 programs.

---

## 6. I/O, DT= and Fixed-form

- **Enhanced I/O specifiers (ASYNCHRONOUS, STREAM, ID, IOMSG, etc.)**
  - **Status:** Implemented and tested.
  - **Notes:** `f2003_io_spec_list` and related rules are exercised in
    `test_fortran_2003_comprehensive.py` and
    `test_issue70_c_interop_extended.py`.

- **Defined derived-type I/O (DT=)**
  - **Status:** Partially implemented and tested (Issue #68).
  - **Notes:** Generic forms `READ(FORMATTED)` / `WRITE(FORMATTED|UNFORMATTED)`
    and their use in type-bound GENERIC statements and interface blocks are
    covered in `test_issue68_defined_io.py`. DT edit descriptors inside
    format strings are treated as opaque character literals; the descriptor
    syntax is not parsed as a sub‑grammar.

- **Fixed-form F2003 support**
  - **Status:** Implemented and tested for representative code (Issue #72).
  - **Notes:** The unified lexer handles fixed-form comments and upper-case
    layout; tests in
    `test_fortran_2003_comprehensive.py::test_fixed_form_compatibility` and
    `test_issue72_fixed_form_f2003.py` combine fixed-form layout with F2003
    OOP, CLASS(*), SELECT TYPE, PDTs and BIND(C).

---

## 7. C Interoperability and IEEE Arithmetic

- **C interoperability (ISO_C_BINDING, BIND(C), C types, VALUE)**
  - **Status:** Substantially implemented and tested (Issues #24, #59, #70).
  - **Notes:** See `grammars/fortran_2003_limitations.md` section 7 and tests:
    - `test_issue24_semantic_c_interop.py`
    - `test_f2003_polymorphism_and_c_interop.py`
    - `test_issue70_c_interop_extended.py`

- **IEEE arithmetic**
  - **Status:** Implemented and tested (Issue #27).
  - **Notes:** Tokens and intrinsic modules for IEEE arithmetic are covered
    by `test_issue27_ieee_arithmetic.py`.

---

## 8. Summary and Remaining Gaps

Fortran 2003 support in this repository can be summarized as:

- **Implemented and well tested for practical use:**
  - Program/module structure, interfaces and CONTAINS.
  - OOP (type extension, type-bound procedures, polymorphism, FINAL).
  - PDTs in the most common patterns (definitions, nested usage, arguments).
  - Procedure pointers and abstract interfaces.
  - ASSOCIATE, BLOCK and modern control structures.
  - Enhanced I/O and key parts of C interoperability.
  - IEEE arithmetic tokens and basic usage.
  - Representative fixed-form F2003 code paths.

- **Implemented but only partially or lightly tested:**
  - Full richness of PDT structure constructors.
  - Exotic SELECT TYPE nesting patterns beyond the current test matrix.
  - Highly nested or vendor-specific C interop patterns.

- **Intentionally out of scope or not yet implemented:**
  - Full semantic validation of interoperability (beyond syntax).
  - Exact reconstruction of all Fortran 2003 edit descriptors and low-level
    FORMAT grammars.
  - Strict historical column semantics for fixed-form source.

This audit, combined with the updated `grammars/fortran_2003_limitations.md`,
forms the spec‑driven checklist requested in Issue #73 and provides a clear
picture of which parts of Fortran 2003 are supported, tested, or explicitly
left outside the current subset.

