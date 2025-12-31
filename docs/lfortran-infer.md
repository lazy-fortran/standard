# LFortran Infer Mode

**Status:** Draft
**Derived from:** [LFortran Standard](lfortran-standard.md)
**Compiler flag:** `--infer`

## Overview

LFortran Infer mode extends LFortran Standard with features for rapid prototyping and interactive use. Enabled with `--infer` flag or implicitly in the REPL.

## Feature Comparison

| Feature | LFortran Standard | LFortran Infer |
|---------|-------------------|----------------|
| Default real | 8 bytes | 8 bytes |
| Default integer | 4 bytes | 4 bytes |
| `dp` predefined | YES | YES |
| Bounds checking | ON | ON |
| Default intent | `intent(in)` | `intent(in)` |
| Implicit typing | OFF (error) | **Type inference** |
| Array realloc | OFF (error) | **ON** |
| Global scope | No | **YES** |

## Type Inference

### First Assignment Rule

Variables at global scope get their type from the first value assigned:

```fortran
x = 42             ! integer (4 bytes)
y = 3.14           ! real(8)
z = (1.0, 2.0)     ! complex(8)
flag = .true.      ! logical
s = "hello"        ! character(:), allocatable
```

### Restrictions

- **Intrinsic types only** - integer, real, complex, logical, character
- **Derived types require explicit declaration**
- **Global scope only** - Inside procedures, standard declaration rules apply
- **Compile error on ambiguity**

## Global Scope

Bare statements allowed without `program`/`end program`:

```fortran
x = 5.0
y = 3.0
print *, x + y
```

## Automatic Array Reallocation

Allocatable arrays reallocate on shape mismatch:

```fortran
real, allocatable :: arr(:)
arr = [1.0, 2.0, 3.0]   ! size 3
arr = [7.0, 8.0]        ! reallocates to size 2
```

Equivalent to `--realloc-lhs-arrays` flag.

## Dot Notation

Both `.` and `%` supported for member access:

```fortran
particle.x = 1.0           ! Modern style
particle%x = 1.0           ! Standard Fortran
```

User-defined operators require spaces: `a .op. b`

## Unsigned Integers

```fortran
integer, unsigned :: count          ! 4-byte unsigned
integer(8), unsigned :: big_count   ! 8-byte unsigned
```

No implicit mixing of signed/unsigned. Explicit conversion required:
```fortran
u + uint(i)                 ! OK
i + int(u)                  ! OK
```

Overflow behavior:
- Debug mode: Runtime error
- `--fast` mode: Wraparound
- Explicit: `wrap_add`, `wrap_sub`, `wrap_mul`

## Standardizer

Transforms LFortran Infer code to ISO Fortran 2023:

| Input Structure | Output |
|-----------------|--------|
| Bare statements only | `program main ... end program` |
| Bare statements + procedures | `program main ... contains ... end program` |
| Only procedures | `module <filename> ... end module` |

## Compiler Modes

| Mode | Flag | Type Inference | Array Realloc |
|------|------|----------------|---------------|
| **Strict** | `--std=lf` (default) | OFF | OFF |
| **Standard** | `--std=f23` | OFF | ON |
| **Infer** | `--infer` | ON | ON |

## References

- [LFortran Standard](lfortran-standard.md)
- [Design Rationale](design-rationale.md)
- [LFortran Compiler](https://lfortran.org)
