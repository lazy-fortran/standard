# LFortran Design Document

**Status:** Draft
**Base standard:** Fortran 2023 (ISO/IEC 1539-1:2023)

## Overview

LFortran extends Fortran 2023 with J3 Generics, sensible defaults, and modern syntax. All standard Fortran 2023 programs remain valid LFortran programs.

For normative specifications, see:
- [LFortran Standard](lfortran-standard.md) - Stricter defaults
- [LFortran Infer](lfortran-infer.md) - Type inference mode
- [Design Rationale](design-rationale.md) - Why we made these choices

## Design Principles

1. **Strongly typed generics** - Templates with explicit type bounds, no whole-program inference
2. **No whole-program analysis** - All features are single-pass or module-local
3. **Scientific computing focus** - 8-byte reals, bounds checking ON by default
4. **Modern safety patterns** - Immutable by default, no silent overflow
5. **Standard compatibility** - All code compiles with standard Fortran compilers

## Feature Classification

All features are **single-pass** (local or module-local analysis only):

- Dot notation transformation (`a.b` -> `a%b`)
- Default `intent(in)` for arguments
- Default `implicit none` injection
- Default precision (real=8 bytes, integer=4 bytes)
- Template/requirement instantiation
- Type inference (`:=` syntax and `--infer` first assignment)

**Explicitly NOT supported** (would require whole-program analysis):
- Implicit monomorphization from call sites
- Function result type inference from body
- Automatic specialization across compilation units

## Compiler Modes

| Mode | Flag | Purpose |
|------|------|---------|
| **Strict** | `--std=lf` (default) | Production code with safety checks |
| **Standard** | `--std=f23` | ISO Fortran 2023 compatibility |
| **Infer** | `--infer` | Interactive/prototyping with type inference |

### Build Modes (orthogonal)

| Build Mode | Bounds Checking | Optimization |
|------------|-----------------|--------------|
| **Debug** (default) | ON | OFF |
| **ReleaseFast** | OFF | ON |
| **ReleaseSafe** | ON | ON |

## Standardizer

The standardizer transforms LFortran code (.lf) to standard Fortran (.f90):

1. Dot notation: `a.b` -> `a%b`
2. Program wrapping: Bare statements -> `program main`
3. Declaration generation: Inferred types -> explicit declarations
4. Kind promotion: Unqualified types -> 8-byte kind specifiers
5. Intent injection: Default `intent(in)` made explicit
6. Template expansion: Instantiations -> concrete procedures

### Example

Input (script.lf with `--infer`):
```fortran
x = 5
y = 3.14
print *, x, y
```

Output (script.f90):
```fortran
program main
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: x
    real(dp) :: y
    x = 5
    y = 3.14_dp
    print *, x, y
end program main
```

## LFortran Implementation

LFortran features are implemented in [LFortran](https://github.com/lfortran/lfortran).

| Feature | Status |
|---------|--------|
| Bounds checking (default ON) | Implemented |
| Global scope / infer mode | Implemented |
| Unsigned integers | ASR support |
| Default real = 8 bytes | Planned |
| Default `intent(in)` | Planned |
| Dot notation | Planned |
| J3 Generics | In progress |

## References

- ISO/IEC 1539-1:2023 (Fortran 2023)
- [LFortran Compiler](https://lfortran.org)
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/)
