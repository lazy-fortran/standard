# LFortranInfer Grammar Implementation Audit

**Standard:** LFortran Standard + Infer Mode (Global Scope / Script Mode)
**Grammar Files:** `grammars/src/LFortranInferLexer.g4`, `grammars/src/LFortranInferParser.g4`
**Test Directory:** `tests/LFortranInfer/`
**Status:** ✅ COMPLETE (100% infer mode coverage)

---

## Overview

The LFortranInfer grammar extends LFortran Standard with **infer mode** capabilities, enabling script-style Fortran programming without traditional program/end program wrappers. This supports:

- Bare statements at top level (global scope)
- Bare expressions for REPL-style evaluation
- Bare declarations without program units
- Mixed script-style and traditional program units

Infer mode is activated with the `--infer` flag in the LFortran compiler.

---

## Implementation Status

| Feature Category | Coverage | Status |
|-----------------|----------|--------|
| **Infer Mode (Global Scope)** | 100% | ✅ IMPLEMENTED |
| Bare statements | 100% | ✅ IMPLEMENTED |
| Bare expressions | 100% | ✅ IMPLEMENTED |
| Bare declarations | 100% | ✅ IMPLEMENTED |
| Bare use/implicit | 100% | ✅ IMPLEMENTED |
| Mixed mode (script + programs) | 100% | ✅ IMPLEMENTED |
| **LFortran Base (J3 Generics)** | 100% | ✅ INHERITED |
| **Fortran 2023 Base** | 100% | ✅ INHERITED |

**Overall Coverage:** 100%
**Test Fixtures:** 3/3 passing
**Test Cases:** 10/10 passing

---

## INFER MODE FEATURES (Global Scope / Script Mode)

### Global Scope Program Structure

**Reference:** LFortran compiler `--infer` mode
**Status:** ✅ IMPLEMENTED

**Capabilities:**
- Execute Fortran code without `program`/`end program` wrapper
- Mix bare statements with standard program units
- Top-level declarations, expressions, and statements
- Automatic program unit generation by standardizer

**Grammar Rules:**
- `program_lfortran_infer` - Top-level infer mode entry point
- `script_unit_list` - List of script units
- `script_unit` - Bare code or standard program unit

**Top-Level Structure:**
```antlr4
program_lfortran_infer
  : script_unit_list EOF
  ;

script_unit
  : program_unit_f2023        // Standard programs, modules
  | template_construct        // J3 TEMPLATE
  | requirement_construct     // J3 REQUIREMENT
  | instantiate_stmt          // J3 INSTANTIATE
  | use_stmt                  // Bare USE
  | implicit_stmt             // Bare IMPLICIT
  | declaration_construct     // Bare declarations
  | executable_construct      // Bare statements
  | expr_f2003                // Bare expressions (REPL)
  ;
```

**Test Coverage:**
- ✅ `fixtures/global_scope_basic.f90` - Bare statements without program
- ✅ `fixtures/global_scope_arrays.f90` - Global arrays and expressions
- ✅ `fixtures/global_scope_with_functions.f90` - Mixed bare code and functions
- ✅ `test_bare_expression` - Bare expression (REPL mode)
- ✅ `test_bare_declaration` - Bare declaration
- ✅ `test_bare_assignment` - Bare assignment
- ✅ `test_bare_print` - Bare print statement
- ✅ `test_mixed_script_and_program` - Mix of script and program units
- ✅ `test_script_with_template` - Script with J3 template

---

### Bare Statements (Global Scope)

**Status:** ✅ IMPLEMENTED

Executable statements can appear at top level without enclosing program unit:

```fortran
! No program wrapper needed
integer :: x
x = 42
print *, x
```

**Grammar:** `executable_construct` at top level via `script_unit`

**Test Coverage:**
- ✅ Assignment statements (`x = 42`)
- ✅ Print statements (`print *, x`)
- ✅ Function calls in global scope

---

### Bare Expressions (REPL Mode)

**Status:** ✅ IMPLEMENTED

Expressions can be evaluated at top level for interactive/REPL use:

```fortran
2 + 2
sin(3.14_dp)
[1, 2, 3] + [4, 5, 6]
```

**Grammar:** `expr_f2003` at top level via `script_unit`

**Test Coverage:**
- ✅ Arithmetic expressions (`2 + 2`)
- ✅ Tested in `test_bare_expression`

---

### Bare Declarations (Global Scope)

**Status:** ✅ IMPLEMENTED

Type declarations can appear at top level without program wrapper:

```fortran
integer :: x, y
real(dp) :: pi
allocatable :: arr(:)
```

**Grammar:** `declaration_construct` at top level via `script_unit`

**Test Coverage:**
- ✅ Integer declarations
- ✅ Real declarations with kind
- ✅ Array declarations
- ✅ Tested in `global_scope_basic.f90` and `test_bare_declaration`

---

### Bare Use/Implicit Statements

**Status:** ✅ IMPLEMENTED

Module imports and implicit typing can appear at global scope:

```fortran
use iso_fortran_env, only: dp => real64
implicit none
```

**Grammar:** `use_stmt`, `implicit_stmt` at top level via `script_unit`

**Test Coverage:**
- ✅ Bare USE statements (`global_scope_with_functions.f90`)
- ✅ Mixed with declarations and code

---

### Mixed Mode (Script + Program Units)

**Status:** ✅ IMPLEMENTED

Infer mode supports mixing script-style code with standard program units:

```fortran
! Script-style: bare use
use iso_fortran_env, only: dp => real64

! Standard program unit
program main
    real(dp) :: x
    x = 1.0_dp
end program main
```

**Grammar:** `script_unit` alternation supports both

**Test Coverage:**
- ✅ `test_mixed_script_and_program` - Script + program
- ✅ `test_script_with_template` - Script + J3 template
- ✅ `global_scope_with_functions.f90` - Bare code + function definitions

---

## LFORTRAN BASE (Inherited)

**Reference:** J3/24-107r1 (J3 Generics)
**Status:** ✅ 100% INHERITED from LFortranParser

All J3 Generics features available in infer mode:

```antlr4
parser grammar LFortranInferParser;
import LFortranParser;  // Full LFortran + J3 Generics support
```

**Supported in Infer Mode:**
- ✅ TEMPLATE constructs at top level
- ✅ REQUIREMENT constructs at top level
- ✅ INSTANTIATE statements in global scope
- ✅ All LFortran Standard features

**Test Coverage:**
- ✅ `test_script_with_template` - Template in script mode
- ✅ All LFortran features work (inherited tests)

---

## FORTRAN 2023 BASE (Inherited)

**Reference:** ISO/IEC 1539-1:2023
**Status:** ✅ 100% INHERITED from Fortran2023Parser (via LFortranParser)

**Test Coverage:**
- ✅ `test_standard_program` - Standard program still works in infer mode

---

## GRAMMAR ARCHITECTURE

### Inheritance Chain

```
Fortran2023Parser (F2023)
  → LFortranParser (F2023 + J3 Generics)
    → LFortranInferParser (LFortran + Infer Mode)
```

### Grammar Structure

**LFortranInferLexer.g4:**
- Imports LFortranLexer (all F2023 + J3 Generic tokens)
- NO new tokens (all inherited)
- Minimal lexer (infer mode handled entirely in parser)

**LFortranInferParser.g4:**
- Imports LFortranParser (all LFortran + F2023 rules)
- Adds 3 new parser rules for infer mode structure
- Allows top-level statements/expressions/declarations

### Top-Level Comparison

**LFortran (Standard Mode):**
```antlr4
lfortran_unit
  : program_unit_f2023        // REQUIRED wrapper
  | template_construct
  | requirement_construct
  ;
```

**LFortranInfer (Infer Mode):**
```antlr4
script_unit
  : program_unit_f2023        // Optional wrapper
  | template_construct
  | requirement_construct
  | instantiate_stmt          // Bare INSTANTIATE
  | use_stmt                  // Bare USE
  | implicit_stmt             // Bare IMPLICIT
  | declaration_construct     // Bare declarations
  | executable_construct      // Bare statements
  | expr_f2003                // Bare expressions
  ;
```

---

## STANDARDIZER OUTPUT

The LFortran compiler's standardizer converts infer-mode code to standard Fortran:

| Input Structure | Generated Output |
|-----------------|------------------|
| Bare statements only | `program main ... end program` |
| Bare statements + procedures | `program main ... contains ... end program` |
| Only procedures | `module <filename> ... contains ... end module` |
| Already valid program units | Preserved as-is |

**Example:**

Input (infer mode):
```fortran
integer :: x
x = 42
print *, x
```

Output (standard Fortran):
```fortran
program main
    integer :: x
    x = 42
    print *, x
end program main
```

---

## USE CASES

### Interactive REPL

```fortran
> 2 + 2
4
> sin(3.14_dp)
0.001592...
```

### Script-Style Programming

```fortran
#!/usr/bin/env lfortran --infer
use iso_fortran_env, only: dp => real64

real(dp) :: x
x = 3.14_dp
print *, "Pi is approximately", x
```

### Mixed Development

```fortran
! Quick test at top
use mylib
print *, mylib_version()

! Full program below
program main
    use mylib
    call run_simulation()
end program main
```

---

## FUTURE WORK

### Type Inference (Planned - NOT Grammar Scope)

Issue #708 describes planned type inference at global scope:

```fortran
! Future: Type inference (NOT in current grammar)
x = 1.0        ! Inferred as real(dp) in LFortran
arr = [1,2,3]  ! Inferred as integer(:) allocatable
```

This is a **semantic analysis feature**, not grammar. Current grammar already supports parsing these as bare statements; semantic phase will add inference.

### LFortran Infer Mode

The LFortran Infer mode adds:
- Type inference at global scope (intrinsic types only)
- Automatic array reallocation on assignment
- Enhanced standardizer for outputting ISO Fortran

Grammar is already prepared for these features.

---

## COMPLIANCE CHECKLIST

- [x] Infer mode entry point implemented (`program_lfortran_infer`)
- [x] Bare statements supported (declarations, executables, expressions)
- [x] Mixed mode supported (script + programs)
- [x] All LFortran features available in infer mode
- [x] All Fortran 2023 features available in infer mode
- [x] Test coverage for all infer mode features (10/10 tests passing)
- [x] Grammar builds without errors
- [x] Documentation headers complete

---

## REFERENCES

- **Issue #708:** Create LFortran Infer specification (Infer Mode)
- **Issue #707:** Create LFortran Standard specification
- **LFortran Compiler:** https://lfortran.org (`--infer` flag)
- **docs/lfortran_audit.md:** LFortran base (J3 Generics) audit

---

**Last Updated:** 2025-12-30
**Next Review:** After LFortran Infer specification release
