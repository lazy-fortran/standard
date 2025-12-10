---
marp: true
theme: default
paginate: true
style: |
  section {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
    background: #fafafa;
    color: #1a1a1a;
  }
  h1 {
    color: #2563eb;
  }
  h2 {
    color: #374151;
  }
  code {
    background: #e5e7eb;
    border-radius: 4px;
    padding: 2px 6px;
  }
  pre {
    background: #f3f4f6;
    border: 1px solid #d1d5db;
    border-radius: 8px;
  }
  table {
    font-size: 0.85em;
  }
  th {
    background: #e5e7eb;
  }
  blockquote {
    border-left: 4px solid #2563eb;
    background: #eff6ff;
    padding: 0.5em 1em;
    margin: 0.5em 0;
  }
  .question {
    background: #fef3c7;
    border: 1px solid #f59e0b;
    border-radius: 8px;
    padding: 0.5em 1em;
    margin-top: 0.5em;
  }
---

# Lazy Fortran 2025
## Draft Standard Review

Base: ISO/IEC 1539-1:2023

---

# Overview

Lazy Fortran extends Fortran 2023 with a source-to-source front-end that:

- Uses strongly typed generics/traits with explicit instantiation
- **No whole-program analysis** - avoids C++ template hell and Julia recompilation
- Provides sensible defaults (real=8 bytes, int=4 bytes, `intent(in)`, `implicit none`)
- Adds unsigned integers with Rust-like safety
- Offers modern syntax (dot notation `a.b` instead of `a%b`)
- Optional infer mode for rapid prototyping (intrinsic types only)
- Emits standard-conforming Fortran 2023 for any back-end compiler

---

# Design Decisions (Resolved)

| Decision | Resolution |
|----------|------------|
| Generics approach | Strongly typed traits, explicit instantiation |
| Whole-program analysis | **NOT supported** - all features single-pass |
| Type inference | Infer mode only (intrinsic types) |
| Default precision | real=8 bytes, int=4 bytes (like Rust) |
| Default intent | `intent(in)` |
| Implicit typing | `implicit none` default |
| Member access | Dot notation `a.b` |

---

# Feature Classification

**All features are single-pass (local or module-local):**

| Feature | Analysis |
|---------|----------|
| Dot notation (`a.b` -> `a%b`) | Local |
| Default `intent(in)` | Local |
| Default `implicit none` | Local |
| Default precision (real=8, int=4) | Local |
| Infer mode (intrinsic types) | Local |
| Explicit template instantiation | Module-local |

**Explicitly NOT supported (would require whole-program analysis):**
- Implicit monomorphization from call sites
- Function result type inference from body
- Automatic specialization across compilation units

---

# 1. Dot Notation (RESOLVED)

**Member access uses `.` instead of `%`:**

```fortran
! Lazy Fortran
particle.x = 1.0
particle.velocity.vx = 2.0
call particle.move(dt)

! Standardized output
particle%x = 1.0
particle%velocity%vx = 2.0
call particle%move(dt)
```

**Disambiguation:** User-defined operators require spaces: `a .op. b`

---

# 2. Default Precision (RESOLVED)

**Different defaults for reals vs integers (like Rust):**

```fortran
x = 1.0              ! real(8) - 64-bit for precision
y = 1.0e3            ! real(8)
n = 42               ! integer - 32-bit for performance
integer :: count     ! integer (4 bytes)
real :: value        ! real(8)

! Explicit sizes when needed
integer(8) :: big_count    ! 64-bit for large arrays/counts
real(4) :: single_precision
```

- **Reals at 8 bytes:** Scientific precision, matches Python/Julia/NumPy
- **Integers at 4 bytes:** Better cache/SIMD, use `integer(8)` when needed

---

# Unsigned Integers (NEW)

**Attribute syntax with Rust-like safety:**

```fortran
integer, unsigned :: count          ! 4-byte unsigned
integer(8), unsigned :: big_count   ! 8-byte unsigned

! No implicit mixing - explicit conversion required
integer :: i = 5
integer, unsigned :: u = 10
u + uint(i)                 ! OK
i + int(u)                  ! OK
! u + i                     ! ERROR
```

**Overflow:** Wrap around (or runtime error with `-fcheck=overflow`)

**Use cases:** Array indexing, bit manipulation, C interop

---

# 3. Type Inference (Infer Mode Only)

**RESOLVED:** Type inference is optional, enabled with `--infer` flag.

```fortran
! Infer mode only - intrinsic types
x = 42           ! integer (4 bytes)
y = 3.14         ! real(8)
s = "hello"      ! character(:), allocatable
arr = [1, 2, 3]  ! integer, allocatable - NOT reallocated on assignment

! NOT inferred - requires explicit declaration
p = particle_t() ! ERROR in infer mode
```

**Key restrictions:**
- Only intrinsic types (int, real, complex, logical, character)
- Derived types always require explicit declaration
- Arrays are NOT reallocated on assignment (unlike standard Fortran)
- Derived type instances ARE reallocated on assignment

---

# Type Inference: Resolved Issues (Infer Mode)

| Issue | Decision |
|-------|----------|
| Declaration placement | **Anywhere in scope** |
| Fallback for unresolved type | **Compile error** |

No implicit defaults - if the type cannot be determined, the programmer must provide an explicit declaration.

---

# Why No Implicit Monomorphization?

**Problem with C++/Julia approach:**
```fortran
! WRONG - requires whole-program analysis
function add(a, b)      ! What types? Unknown
    add = a + b
end function
x = add(5, 3)           ! Must scan all call sites
```

**Lazy Fortran approach - explicit instantiation:**
```fortran
template add_t(T)
    type, deferred :: T
contains
    function add(a, b) result(res)
        type(T), intent(in) :: a, b
        type(T) :: res
        res = a + b
    end function
end template

instantiate add_t(integer), only: add_int => add
x = add_int(5, 3)       ! No whole-program analysis needed
```

---

# 4. Default Intent (RESOLVED)

**All arguments default to `intent(in)`:**

```fortran
subroutine process(x, y, z)
    integer, intent(in) :: x       ! Explicit (same as default)
    integer, intent(inout) :: y    ! Explicit override required
    integer, intent(out) :: z      ! Explicit override required
    y = x + 1
    z = y * 2
end subroutine
```

Without explicit intent, `y` and `z` would be `intent(in)` and assignments would error.

**Principle of least privilege:** arguments are read-only unless explicitly declared otherwise (like Rust).

---

# 5. Implicit None (RESOLVED)

**`implicit none` is the default in all program units.**

```fortran
! Lazy Fortran - no implicit none needed
subroutine example()
    integer :: x
    x = 1
end subroutine

! Standardized output
subroutine example()
    implicit none
    integer :: x
    x = 1
end subroutine
```

This is already best practice in modern Fortran - Lazy Fortran makes it mandatory.

---

# 6. Generic Programming (RESOLVED)

**RESOLVED:** Strongly typed generics/traits with **explicit instantiation only**.

No implicit monomorphization - avoids:
- C++ template compilation explosion
- Julia-style recompilation on new types
- Whole-program analysis requirements

Two complementary approaches:

1. **J3 Procedural Templates** - Explicit instantiation required
2. **Traits (Swift/Rust style)** - Strongly typed constraints

---

# Generics: J3 Templates

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

! Inline instantiation
call swap{integer}(a, b)
```

---

# Generics: Strongly Typed Traits

```fortran
trait IComparable(T)
    pure logical function less_than(a, b)
        type(T), intent(in) :: a, b
    end function
end trait

! Retroactive conformance for intrinsic types
implements IComparable :: integer
    procedure :: less_than => builtin_less_than
end implements
```

All type parameters must be bounded - no unconstrained generics.

---

# Generics: Syntax Status

**J3 TEMPLATE** (official proposal for Fortran 202Y):
- `TEMPLATE foo(T)` with standard parentheses
- `^()` for inline instantiation: `CALL sub^(INTEGER)(x)`
- Explicit `INSTANTIATE` statements

**Traits** ([Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran)):
- `abstract interface` for trait definitions
- `implements` blocks for conformance
- **Curly braces `{T}`** for generic parameters:
  ```fortran
  function sum{INumeric :: T}(x) result(s)
  ```

Lazy Fortran adopts curly braces `{T}` for generic parameters.

---

# Generics: Resolved Issues

**Issue 7 - Dispatch mechanism: BOTH**

| Syntax | Dispatch | Use case |
|--------|----------|----------|
| `type(T)` | Static | Zero overhead, inlinable |
| `class(Trait)` | Dynamic | Runtime polymorphism |

**Issue 16 - @ annotations: NO**

Use curly braces `{Constraint :: T}` syntax instead:

```fortran
! Lazy Fortran - curly braces
function sum{INumeric :: T}(x) result(s)

! NOT supported
@INumeric
function sum(x) result(s)
```

---

# Summary: Resolved Decisions

| Decision | Resolution |
|----------|------------|
| Whole-program analysis | **NOT supported** |
| Generics | Strongly typed traits, explicit instantiation |
| Type inference | Infer mode only (intrinsic types) |
| Default precision | real=8 bytes, int=4 bytes (like Rust) |
| Default intent | `intent(in)` |
| Implicit typing | `implicit none` default |
| Member access | Dot notation `a.b` |
| Arrays (infer mode) | NOT reallocated on assignment |
| Types (infer mode) | ARE reallocated on assignment |

---

# Summary: All Issues Resolved

| Decision | Resolution |
|----------|------------|
| Whole-program analysis | **NOT supported** |
| Generics | Strongly typed traits, explicit instantiation |
| Generic syntax | Curly braces `{Constraint :: T}` |
| Dispatch | **Both** - `type(T)` static, `class(Trait)` dynamic |
| @ annotations | **No** - use curly braces syntax |
| Type inference | Infer mode only (intrinsic types) |
| Default precision | real=8 bytes, int=4 bytes (like Rust) |
| Unsigned integers | `integer, unsigned` attribute, Rust-like safety |
| Default intent | `intent(in)` |
| Implicit typing | `implicit none` default |
| Member access | Dot notation `a.b` |
| Declaration placement | Anywhere in scope |
| Fallback for unclear type | Compile error |
| Arrays (infer mode) | NOT reallocated on assignment |
| Types (infer mode) | ARE reallocated on assignment |

---

# Resources

- **Design Document:** [lazyfortran2025-design.md](../lazyfortran2025-design.md)
- **Base Standard:** ISO/IEC 1539-1:2023

**Document Status:** DRAFT - All provisions subject to change
