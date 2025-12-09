# Lazy Fortran 2025

**Status:** Draft
**Base standard:** Fortran 2023 (ISO/IEC 1539-1:2023)

---

## Summary

Lazy Fortran 2025 extends Fortran 2023 with automatic type inference, intent deduction, and generic programming. The goal is reduced boilerplate while maintaining compatibility with standard Fortran compilers through a source-to-source standardizer.

Key features:
- **Type inference** - variables get their type from first assignment
- **Intent inference** - argument intents derived from usage analysis
- **Generics** - templates and traits for type-safe polymorphism
- **Monomorphization** - automatic generation of specialized code

All standard Fortran 2023 programs remain valid Lazy Fortran programs.

---

## Type Inference

### First Assignment Rule

Variables get their type from the first value assigned:

```fortran
x = 42             ! integer
y = 3.14           ! real
z = (1.0, 2.0)     ! complex
flag = .true.      ! logical
s = "hello"        ! character(len=5)
p = particle_t()   ! derived type from constructor
```

Subsequent assignments use standard Fortran coercion rules.

### Arrays

Array types come from constructors or allocate statements:

```fortran
arr = [1, 2, 3]           ! rank-1 integer array, 3 elements
allocate(matrix(n, m))    ! rank-2, runtime bounds
```

### Interaction with implicit none

When `implicit none` is present, undeclared names are errors and inference is disabled. This preserves compatibility with strict coding styles.

### Open Issues

| Issue | Question | Options |
|-------|----------|---------|
| 1 | Default numeric kinds? | A: ISO default (real(4)) B: Double precision (real(8)) C: Context-dependent |
| 10 | Character length handling? | A: First assignment wins B: Maximum length seen C: Allocatable deferred-length |
| 15 | Fallback when type unclear? | A: Compile error B: Default to real(8) C: ISO default kind |
| 2 | Infer from intent(out)? | A: No (assignment only) B: Yes (inspect callee signature) |
| 3 | Declaration placement? | A: Block beginning only B: Anywhere in scope |
| 11 | Infer pointer attribute? | A: No B: From pointer assignment C: Both pointer and target |
| 12 | Infer allocatable? | A: No B: From allocate statements |
| 13 | Function result types? | A: Body only B: Call site only C: Body first, call site fallback D: Must match |
| 14 | Derived type scope? | A: Explicit USE required B: Auto-USE C: Explicit declaration required |

---

## Intent Inference

Procedure argument intents are derived from usage analysis:

```fortran
subroutine process(x, y, z)
    ! x only read     -> intent(in)
    ! y modified      -> intent(inout)
    ! z only written  -> intent(out)
    y = x + 1
    z = y * 2
end subroutine
```

### Open Issue

| Issue | Question | Options |
|-------|----------|---------|
| 4 | Default intent when usage is inconclusive? | A: intent(in) B: intent(inout) C: Require explicit |

---

## Generic Programming

Two complementary approaches are available.

### Templates (J3 Direction)

The TEMPLATE construct defines parameterized procedures:

```fortran
template swap_t(T)
    type, deferred :: T
contains
    subroutine swap(x, y)
        type(T), intent(inout) :: x, y
        type(T) :: tmp
        tmp = x; x = y; y = tmp
    end subroutine
end template

! Explicit instantiation
instantiate swap_t(integer), only: swap_int => swap
instantiate swap_t(real), only: swap_real => swap

! Inline instantiation
call swap{integer}(a, b)
```

Requirements define reusable type constraints:

```fortran
requirement r_comparable(T, less_than)
    type, deferred :: T
    interface
        pure logical function less_than(a, b)
            type(T), intent(in) :: a, b
        end function
    end interface
end requirement
```

### Traits (Swift/Rust Style)

Type sets specify constraints as unions:

```fortran
abstract interface :: INumeric
    integer | real(real64)
end interface INumeric
```

Types declare trait conformance:

```fortran
type, implements(ISum) :: simple_sum_t
contains
    procedure, nopass :: sum
end type

! Retroactive conformance
implements IComparable :: integer
    procedure :: less_than => builtin_less_than
end implements
```

### Combining Approaches

| Use Case | Recommended |
|----------|-------------|
| Generic containers | J3 TEMPLATE |
| Numeric algorithms | Traits type sets |
| Retroactive conformance | Traits IMPLEMENTS |
| Runtime polymorphism | Traits with class(ITrait) |
| Explicit instantiation | J3 TEMPLATE |
| Ergonomic call sites | Traits automatic inference |

### Open Issues

| Issue | Question | Options |
|-------|----------|---------|
| 5 | Which generics system? | A: J3 TEMPLATE only B: Traits only C: Hybrid (both) |
| 6 | Generic parameter delimiter? | A: Braces {T} B: Parentheses (T) C: Angle brackets <T> |
| 7 | Dispatch mechanism? | A: Static only B: Dynamic only C: Both |
| 16 | Support @ annotations? | A: No B: Yes C: Both styles |

---

## Monomorphization

Generic procedures are automatically specialized for each type combination used:

```fortran
function add(a, b)
    add = a + b
end function

x = add(5, 3)       ! generates add__i32_i32
y = add(2.5, 1.5)   ! generates add__r64_r64
```

User-written specific procedures take precedence over generated specializations. Ambiguity is a compile-time error.

### Open Issue

| Issue | Question | Options |
|-------|----------|---------|
| 8 | Specialization scope? | A: Per-module B: Per-program (LTO) C: Lazy (on-demand) |

---

## Application Binary Interface

### gfortran Compatibility

Module procedures follow gfortran conventions:

```
__<module-name>_MOD_<procedure-name>
```

Main program entry is `MAIN__`.

### Specialization Mangling

Specialized procedures use kind suffixes:

```
<procedure-name>__<kind-suffix-1>_<kind-suffix-2>_...
```

Kind suffix table (bits convention):

| Type | Kind | Suffix |
|------|------|--------|
| integer | 4 | i32 |
| integer | 8 | i64 |
| real | 4 | r32 |
| real | 8 | r64 |
| complex | 8 | c128 |
| logical | 4 | l32 |
| character | N | chN |

Array rank uses `rank<n>` suffix: `sum__r64rank1`

Multiple specializations are wrapped in a generic interface within an auto-generated module:

```fortran
module auto_add
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface
end module
```

### Open Issue

| Issue | Question | Options |
|-------|----------|---------|
| 9 | Kind suffix convention? | A: Bits (i32, r64) B: Bytes (i4, r8) |

---

## Standardizer

The standardizer transforms Lazy Fortran (.lf) to standard Fortran (.f90).

### Transformations

1. **Program wrapping** - Bare statements become `program main`
2. **Declaration generation** - Inferred types become explicit declarations
3. **implicit none injection** - Added to all program units
4. **Intent generation** - Inferred intents become explicit
5. **Monomorphization** - Generic calls become specialized procedures

### Example

Input (script.lf):
```fortran
x = 5
print *, x
```

Output (script.f90):
```fortran
program main
    implicit none
    integer :: x
    x = 5
    print *, x
end program main
```

---

## References

- ISO/IEC 1539-1:2023 (Fortran 2023)
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/) - TEMPLATE/INSTANTIATE syntax
