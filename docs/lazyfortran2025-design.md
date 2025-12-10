# Lazy Fortran 2025

**Status:** Draft
**Base standard:** Fortran 2023 (ISO/IEC 1539-1:2023)

---

## Summary

Lazy Fortran 2025 extends Fortran 2023 with automatic type inference, intent deduction, and generic programming. The goal is reduced boilerplate while maintaining compatibility with standard Fortran compilers through a source-to-source standardizer.

All standard Fortran 2023 programs remain valid Lazy Fortran programs.

### Feature Classification

Features are classified by compilation complexity:

**Single-pass (local analysis only):**
- Type inference from first assignment
- Default `intent(in)` for arguments
- Expression type rules
- Declaration placement
- Explicit template/trait declarations

**Semantic analysis (multi-pass or whole-program):**
- Monomorphization (collect all call sites)
- Function result type inference (with monomorphization)
- Infer from `intent(out)` arguments (Open Issue 2)
- Infer pointer/allocatable attributes (Open Issues 11, 12)
- Auto-USE for derived types (Open Issue 14)

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

### Expression Type Rules

Expression types follow ISO/IEC 1539-1:2023 Clause 10.1.5 (Numeric intrinsic operations). Mixed numeric array constructors use type promotion: `[1, 2.0, 3]` promotes all elements to real.

### Interaction with implicit none

When `implicit none` is present, undeclared names are errors and inference is disabled. This preserves compatibility with strict coding styles.

### Open Issues

**Single-pass (local analysis):**

| Issue | Question | Options |
|-------|----------|---------|
| 1 | Default numeric kinds? | A: ISO default (real(4)) B: Double precision (real(8)) C: Context-dependent |
| 3 | Declaration placement? | A: Block beginning only B: Anywhere in scope |
| 10 | Character length handling? | A: First assignment wins B: Maximum length seen C: Allocatable deferred-length |
| 15 | Fallback when type unclear? | A: Compile error B: Default to real(8) C: ISO default kind |

**Semantic analysis (multi-pass):**

| Issue | Question | Options |
|-------|----------|---------|
| 2 | Infer from intent(out)? | A: No (assignment only) B: Yes (inspect callee signature) |
| 11 | Infer pointer attribute? | A: No B: From pointer assignment C: Both pointer and target |
| 12 | Infer allocatable? | A: No B: From allocate statements |
| 14 | Derived type scope? | A: Explicit USE required B: Auto-USE C: Explicit declaration required |

### Function Result Types

Function result types are inferred from the body expression given the input types. With monomorphization, each specialization gets its return type from evaluating the body:

```fortran
function add(a, b)
    add = a + b       ! return type = type of (a + b)
end function

x = add(5, 3)         ! e.g. add__i32_i32 / add__i4_i4 (Issue 9)
y = add(2.5d0, 1.5d0) ! e.g. add__r64_r64 / add__r8_r8 (Issue 9)
```

The call site provides argument types, which determines which specialization to generate. That specialization's return type comes from the body expression.

---

## Default Intent

Lazy Fortran uses `intent(in)` as the default for all procedure arguments. This is stricter than standard Fortran, which has no default intent (arguments without explicit intent can be read and modified).

To modify an argument, explicitly specify `intent(inout)` or `intent(out)`:

```fortran
subroutine process(x, y, z)
    integer, intent(in) :: x       ! Explicit (same as default)
    integer, intent(inout) :: y    ! Explicit override - can read and modify
    integer, intent(out) :: z      ! Explicit override - output only
    y = x + 1
    z = y * 2
end subroutine
```

Without explicit intent, `y` and `z` would default to `intent(in)` and the assignments would be compile-time errors.

### Rationale

Standard Fortran allows unrestricted modification of arguments without explicit intent, which can lead to subtle bugs. The `intent(in)` default follows the principle of least privilege: arguments are read-only unless explicitly declared otherwise. This aligns with modern language design (e.g., Rust immutable-by-default).

### Standardizer Behavior

The standardizer generates explicit `intent(in)` declarations for all arguments that lack explicit intent:

```fortran
! Input: script.lf
subroutine scale(x, factor)
    x = x * factor   ! ERROR: x has intent(in) by default
end subroutine

! Must be written as:
subroutine scale(x, factor)
    real, intent(inout) :: x    ! Explicit override required
    x = x * factor
end subroutine
```

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

Traits can specify required procedure signatures:

```fortran
abstract interface :: ISum
    function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
    end function
end interface ISum
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

### Trait Annotation Syntax

An alternative `@` annotation syntax for trait declarations:

```fortran
@IComparable
integer function compare(a, b)
    integer, intent(in) :: a, b
    compare = a - b
end function
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

x = add(5, 3)       ! generates a specialized integer version (see ABI)
y = add(2.5, 1.5)   ! generates a specialized real version (see ABI)
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

Kind suffix table (Option A: bits convention):

| Type | Kind | Storage | Suffix |
|------|------|---------|--------|
| integer | 1 | 1 byte | i8 |
| integer | 2 | 2 bytes | i16 |
| integer | 4 | 4 bytes | i32 |
| integer | 8 | 8 bytes | i64 |
| integer | 16 | 16 bytes | i128 |
| real | 4 | 4 bytes | r32 |
| real | 8 | 8 bytes | r64 |
| real | 16 | 16 bytes | r128 |
| complex | 4 | 8 bytes | c64 |
| complex | 8 | 16 bytes | c128 |
| complex | 16 | 32 bytes | c256 |
| logical | 1 | 1 byte | l8 |
| logical | 4 | 4 bytes | l32 |
| character | N | N bytes | chN |

Alternative byte-based convention (Option B: matches Fortran kind parameters):

| Type | Kind | Suffix |
|------|------|--------|
| integer | 1 | i1 |
| integer | 2 | i2 |
| integer | 4 | i4 |
| integer | 8 | i8 |
| real | 4 | r4 |
| real | 8 | r8 |
| complex | 4 | c4 |
| complex | 8 | c8 |
| logical | 1 | l1 |
| logical | 4 | l4 |

Array rank uses `rank<n>` suffix: e.g. `sum__r64rank1` (Option A) or `sum__r8rank1` (Option B).

For concreteness, examples below assume Option A (bits); if Option B (bytes) is chosen in Issue 9, `i32`/`r64` would become `i4`/`r8` consistently.

Examples (assuming Option A):
- `add(integer, integer)` -> `add__i32_i32`
- `add(real(8), real(8))` -> `add__r64_r64`
- `sum(real(8), dimension(:))` -> `sum__r64rank1`

Multiple specializations are wrapped in a generic interface within an auto-generated module:

```fortran
module auto_add
    interface add
        module procedure add__i32_i32, add__r64_r64  ! assuming Option A (bits)
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

1. **Program wrapping** - Bare statements become `program main`; files with only procedures become `module <filename>`
2. **Declaration generation** - Inferred types become explicit declarations
3. **implicit none injection** - Added to all program units
4. **Intent generation** - Default `intent(in)` becomes explicit
5. **Monomorphization** - Generic calls become specialized procedures

### Simple Example

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

### Monomorphization Example

Input (script.lf):
```fortran
function add(a, b)
    add = a + b
end function

x = add(5, 3)
y = add(2.5, 1.5)
```

Output (script.f90):
```fortran
module auto_add
    implicit none
    interface add
        module procedure add__i32_i32, add__r64_r64  ! assuming Option A (bits)
    end interface add
contains
    integer function add__i32_i32(a, b)
        integer, intent(in) :: a, b
        add__i32_i32 = a + b
    end function

    real(8) function add__r64_r64(a, b)
        real(8), intent(in) :: a, b
        add__r64_r64 = a + b
    end function
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real(8) :: y
    x = add(5, 3)
    y = add(2.5d0, 1.5d0)
end program main
```

---

## References

- ISO/IEC 1539-1:2023 (Fortran 2023)
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/) - TEMPLATE/INSTANTIATE syntax
