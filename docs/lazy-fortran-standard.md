# Lazy Fortran Standard

**Status:** Draft
**Derived from:** [LFortran Standard](lfortran-standard.md)
**Compiler flag:** `--infer`

---

## Overview

The Lazy Fortran Standard extends the [LFortran Standard](lfortran-standard.md) with features for rapid prototyping and interactive use. It is enabled with the `--infer` flag or implicitly in the LFortran interactive REPL.

Lazy Fortran maintains all the safety guarantees of LFortran Standard while adding:

1. **Type inference** at global scope (intrinsic types only)
2. **Global scope** for bare statements without `program`/`end program`
3. **Automatic array reallocation** on assignment
4. **Standardizer** for outputting ISO Fortran 2023

---

## Feature Comparison

| Feature | LFortran Standard | Lazy Fortran (Infer) |
|---------|-------------------|----------------------|
| Default real | 8 bytes | 8 bytes |
| Default integer | 4 bytes | 4 bytes |
| `dp` predefined | YES | YES |
| Bounds checking | ON | ON |
| Default intent | `intent(in)` | `intent(in)` |
| Implicit typing | OFF (error) | **Type inference** |
| Array realloc | OFF (error) | **ON** |
| Global scope | No | **YES** |

---

## Type Inference

### First Assignment Rule

In infer mode, variables at global scope get their type from the first value assigned:

```fortran
x = 42             ! integer (4 bytes)
y = 3.14           ! real(8)
z = (1.0, 2.0)     ! complex(8)
flag = .true.      ! logical
s = "hello"        ! character(:), allocatable
```

### Restrictions

- **Intrinsic types only** - integer, real, complex, logical, character
- **Derived types require explicit declaration:**
  ```fortran
  type(particle_t) :: p     ! Required
  p = particle_t(x=1.0)     ! OK
  ! p = particle_t(x=1.0)   ! ERROR without declaration
  ```
- **Global scope only** - Inside `program`, `module`, `function`, `subroutine`: standard declaration rules apply
- **Compile error on ambiguity** - If type cannot be determined, explicit declaration required

### Expression Type Rules

Expression types follow standard Fortran type promotion rules for numeric intrinsic operations:

- Mixed numeric array constructors use type promotion: `[1, 2.0, 3]` promotes all to `real(8)`
- Real literals are `real(8)` by default
- Integer literals are default integer (4 bytes)

---

## Global Scope

Bare statements are allowed without `program`/`end program` wrapper:

```fortran
! Valid Lazy Fortran file
x = 5.0
y = 3.0
print *, x + y
```

The standardizer wraps these appropriately for standard Fortran output.

---

## Automatic Array Reallocation

**Allocatable arrays** are automatically reallocated on assignment when shapes differ:

```fortran
real, allocatable :: arr(:)
arr = [1.0, 2.0, 3.0]   ! Allocates size 3
arr = [4.0, 5.0, 6.0]   ! OK: same shape
arr = [7.0, 8.0]        ! OK: automatically reallocates to size 2
```

This is equivalent to the `--realloc-lhs-arrays` flag which is enabled implicitly in infer mode.

**Non-allocatable arrays** (fixed-size, automatic, pointer) follow standard Fortran rules - shapes must match:

```fortran
real :: fixed(3)
fixed = [1.0, 2.0, 3.0]   ! OK: same shape
fixed = [4.0, 5.0]        ! ERROR: shape mismatch
```

**Note:** In LFortran Standard (strict mode), allocatable array shape mismatch raises a runtime error. Use infer mode only for prototyping; switch to strict mode for production code.

---

## Standardizer

The standardizer transforms Lazy Fortran to ISO Fortran 2023.

### Output Structure Rules

| Input Structure | Generated Output |
|-----------------|------------------|
| Bare statements only | `program main ... end program` |
| Bare statements + procedures | `program main ... contains ... end program` |
| Only procedures | `module <filename> ... contains ... end module` |
| Already valid program units | Preserved as-is |

**Filename transformation for module names:**
- File extension removed: `utils.lf` → `module utils`
- Hyphens replaced with underscores: `my-utils.lf` → `module my_utils`
- Leading digits prefixed with underscore: `3dmath.lf` → `module _3dmath`

**Edge cases:**
- Mixed `module` + bare statements: Error - invalid structure
- Multiple `program` units: Error - only one program allowed per file
- `submodule`: Preserved as-is (already a valid program unit)
- `block data`: Preserved as-is (already a valid program unit)

### Transformation Example

Input (script.lf with `--infer`):
```fortran
x = 5
y = 3.14
s = "hello"
print *, x, y, s
```

Output (script.f90):
```fortran
program main
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: x
    real(dp) :: y
    character(:), allocatable :: s
    x = 5
    y = 3.14_dp
    s = "hello"
    print *, x, y, s
end program main
```

### Transformations Applied

1. **Program wrapping** - Bare statements become `program main`
2. **Declaration generation** - Inferred types become explicit declarations
3. **Kind promotion** - `real` becomes `real(dp)`, literals get kind suffixes
4. **Implicit none injection** - Added to all program units
5. **Intent generation** - Default `intent(in)` becomes explicit
6. **Dot notation** - `a.b` becomes `a%b`

---

## Dot Notation

Both `.` (dot) and `%` are supported for derived type member access:

```fortran
particle.x = 1.0           ! Dot notation (modern style)
particle%x = 1.0           ! Standard Fortran (also supported)
```

The standardizer converts dot notation to `%` for standard Fortran output.

### Disambiguation from User-Defined Operators

User-defined operators require spaces:

```fortran
a .op. b     ! User-defined operator (spaces required)
a.member     ! Member access (no spaces)
```

---

## Unsigned Integers

Lazy Fortran adds an `unsigned` attribute for integers:

```fortran
integer, unsigned :: count          ! 4-byte unsigned (0 to 4,294,967,295)
integer(8), unsigned :: big_count   ! 8-byte unsigned
```

### No Implicit Mixing

Signed and unsigned integers cannot be mixed without explicit conversion:

```fortran
integer :: i = 5
integer, unsigned :: u = 10

! u + i                     ! ERROR: mixed signed/unsigned
u + uint(i)                 ! OK: explicit conversion to unsigned
i + int(u)                  ! OK: explicit conversion to signed
```

### Overflow Behavior (Rust-like)

| Build Mode | Overflow Behavior | Use Case |
|------------|-------------------|----------|
| **Debug** (default) | Runtime error | Catch bugs during development |
| **ReleaseSafe** | Runtime error | Production with safety |
| **ReleaseFast** (`--fast`) | Wraparound (modular) | Maximum performance |

Unlike C/C++ where overflow is undefined behavior, all modes have **defined semantics**:

```fortran
integer, unsigned :: u = 0
u = u - 1                   ! Debug/ReleaseSafe: runtime error
                            ! ReleaseFast: wraps to 4294967295
```

**Explicit modular arithmetic** for intentional wraparound in any mode:

```fortran
u = wrap_sub(u, 1)          ! Always wraps, no error in any mode
```

### Modular Arithmetic Intrinsics

```fortran
wrap_add(a, b)              ! a + b with wraparound
wrap_sub(a, b)              ! a - b with wraparound
wrap_mul(a, b)              ! a * b with wraparound
```

### Use Cases

- **Array indexing:** Zero-based indexing natural with unsigned
- **Bit manipulation:** Clear semantics for bitwise operations
- **C interoperability:** `size_t`, `uint32_t`, etc.

---

## Generic Programming

Lazy Fortran uses **strongly typed generics/traits** with explicit instantiation. No implicit monomorphization or whole-program type inference.

### Design Principles

1. **No whole-program analysis** - All features are single-pass or module-local
2. **Explicit instantiation** - Generic code must be explicitly instantiated
3. **Strongly typed** - All type parameters must be bounded by traits

### Templates (J3 Direction)

The `TEMPLATE` construct defines parameterized procedures:

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

! Explicit instantiation required
instantiate swap_t(integer), only: swap_int => swap
instantiate swap_t(real(8)), only: swap_real => swap

! Inline instantiation (still explicit)
call swap{integer}(a, b)
```

### Requirements

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

### Traits (Strongly Typed)

Traits define explicit type constraints:

```fortran
trait INumeric
    integer | real(8) | complex(8)
end trait

trait IComparable(T)
    pure logical function less_than(a, b)
        type(T), intent(in) :: a, b
    end function
end trait
```

Types declare trait conformance:

```fortran
type, implements(IComparable) :: my_type_t
contains
    procedure :: less_than => my_less_than
end type

! Retroactive conformance for intrinsic types
implements IComparable :: integer
    procedure :: less_than => builtin_less_than
end implements
```

### Generic Procedures with Trait Bounds

```fortran
function min_value{IComparable :: T}(a, b) result(res)
    type(T), intent(in) :: a, b
    type(T) :: res
    if (less_than(a, b)) then
        res = a
    else
        res = b
    end if
end function

! Must instantiate explicitly
instantiate min_value{integer}, only: min_int => min_value
```

### Dispatch Mechanism

| Syntax | Dispatch | Use case |
|--------|----------|----------|
| `type(T)` | Static | Zero overhead, inlinable |
| `class(Trait)` | Dynamic | Runtime polymorphism |

### Syntax Clarification

Lazy Fortran uses **curly braces `{}`** for generic type parameters (following the Traits-for-Fortran proposal):

| Construct | Syntax | Example |
|-----------|--------|---------|
| Template instantiation | `INSTANTIATE template(type)` | `instantiate swap_t(integer)` |
| Inline template call | `call proc{type}(args)` | `call swap{integer}(a, b)` |
| Trait-bounded generic | `func{Trait :: T}(args)` | `min_value{IComparable :: T}(a, b)` |

**Note:** The J3 TEMPLATE proposal uses `^()` syntax for inline instantiation (`call sub^(integer)(x)`). Lazy Fortran's `{}` syntax is an alternative that avoids ambiguity with exponentiation operators and aligns with the Traits-for-Fortran proposal.

### Why No Implicit Monomorphization

```fortran
! WRONG (would require whole-program analysis):
function add(a, b)      ! What types? Unknown until call site
    add = a + b
end function
x = add(5, 3)           ! Would need to infer types and generate code

! CORRECT (explicit instantiation):
instantiate add_t(integer), only: add_int => add
x = add_int(5, 3)       ! No whole-program analysis needed
```

Benefits:
- Avoids C++ template compilation explosion
- Avoids Julia-style recompilation on new types
- No whole-program analysis needed

---

## Compiler Modes Summary

| Mode | Flag | Type Inference | Array Realloc | Use Case |
|------|------|----------------|---------------|----------|
| **Strict** | `--std=lf` (default) | OFF | OFF | Production code |
| **Standard** | `--std=f23` | OFF | ON | ISO compatibility |
| **Infer** | `--infer` | ON | ON | Interactive/prototyping |

The **interactive mode** (REPL) implicitly uses `--infer` mode.

---

## Related Standards

- **[LFortran Standard](lfortran-standard.md)** - Base standard (stricter than ISO)
- **ISO/IEC 1539-1:2023** - ISO Fortran 2023

---

## References

- [LFortran Compiler](https://lfortran.org)
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [Traits-for-Fortran Proposal](https://github.com/difference-scheme/Traits-for-Fortran)
