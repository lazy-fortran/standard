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

Lazy Fortran extends Fortran 2023 with:

1. **Type Inference** - No explicit declarations needed
2. **Strict Intent Default** - `intent(in)` by default, explicit override required
3. **Generic Programming** - Templates and traits
4. **Monomorphization** - Automatic specialization

---

# Feature Classification

| Single-pass (local) | Semantic analysis (multi-pass) |
|---------------------|-------------------------------|
| Type inference (first assignment) | Monomorphization |
| Default `intent(in)` | Function result types |
| Expression type rules | Infer from `intent(out)` (Issue 2) |
| Declaration placement | Infer pointer/allocatable (Issues 11, 12) |
| Explicit templates/traits | Auto-USE derived types (Issue 14) |

Single-pass features work like traditional Fortran. Semantic analysis features require collecting information across the program.

---

# 1. Type Inference

Variables get their type from first assignment:

```fortran
x = 42           ! integer
y = 3.14         ! real
z = (1.0, 2.0)   ! complex
flag = .true.    ! logical
s = "hello"      ! character(len=5)
```

No `implicit none` required. First assignment wins.

---

# Type Inference: Default Kinds

<div class="question">

**OPEN ISSUE 1: What kind for inferred literals?**

| Option | Types | Trade-off |
|--------|-------|-----------|
| A | real(4), int(4) | ISO compatible, less memory |
| B | real(8), int(8) | Safe for scientific computing |
| C | Context-dependent | Adapts but unpredictable |

</div>

---

# Type Inference: Characters

```fortran
s = "hello"      ! len=5
s = "goodbye"    ! len=7 - what happens?
```

<div class="question">

**OPEN ISSUE 10: How to handle different string lengths?**

| Option | Behavior | Trade-off |
|--------|----------|-----------|
| A | First wins | Truncation possible |
| B | Maximum length | May waste memory |
| C | Deferred-length | Dynamic, modern idiom |

</div>

---

# Type Inference: Fallback

What if type cannot be determined?

```fortran
x = unknown_function()   ! Type unclear
```

<div class="question">

**OPEN ISSUE 15: Fallback for unresolved inference?**

| Option | Behavior |
|--------|----------|
| A | Compile error |
| B | Default to real(8) |
| C | Default to ISO kind |

</div>

---

# Type Inference: Declarations

Where can explicit declarations appear?

```fortran
subroutine example()
    integer :: x       ! Here only? (Option A)
    x = 1
    integer :: y       ! Or anywhere? (Option B)
    y = 2
end subroutine
```

<div class="question">

**OPEN ISSUE 3: Declaration placement?**
- A: Block beginning only (clean structure)
- B: Anywhere (declare near use)

</div>

---

# Type Inference: Derived Types

```fortran
p = particle_t(1.0, 2.0, 3.0)
! p inferred as type(particle_t)
```

<div class="question">

**OPEN ISSUE 14: Module scope for derived types?**

| Option | Behavior |
|--------|----------|
| A | Type must be in scope (explicit USE) |
| B | Auto-USE if type found |
| C | Derived types require explicit declaration |

</div>

---

# Type Inference: Function Results

```fortran
function add(a, b)
    add = a + b   ! Return type from expression
end function

x = add(5, 3)         ! add__i32_i32 returns integer
y = add(2.5d0, 1.5d0) ! add__r64_r64 returns real(8)
```

With monomorphization, return type is inferred from body expression given input types. Call site provides argument types -> determines specialization -> return type comes from body.

---

# Type Inference: intent(out)

```fortran
subroutine init(particle)
    type(particle_t), intent(out) :: particle
end subroutine

call init(p)    ! Auto-declare p?
```

<div class="question">

**OPEN ISSUE 2: Infer from intent(out) arguments?**

| Option | Analysis |
|--------|----------|
| A | No (assignment only) | Local |
| B | Yes (inspect callee) | Cross-procedure |

</div>

---

# Type Inference: Pointer Attribute

```fortran
p => x    ! Pointer assignment
```

<div class="question">

**OPEN ISSUE 11: Infer pointer/target attributes?**

| Option | Behavior |
|--------|----------|
| A | No inference (explicit required) |
| B | Infer pointer only |
| C | Infer both pointer and target |

</div>

---

# Type Inference: Allocatable Attribute

```fortran
allocate(arr(n))    ! Allocate array
```

<div class="question">

**OPEN ISSUE 12: Infer allocatable from allocate?**

| Option | Behavior |
|--------|----------|
| A | No (require explicit attribute) |
| B | Yes (infer from allocate statement) |

</div>

---

# 2. Default Intent

All arguments default to `intent(in)` - stricter than ISO Fortran:

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

---

# Default Intent: Rationale

**No inference** - just a stricter default than ISO Fortran.

| Standard Fortran | Lazy Fortran |
|-----------------|--------------|
| No default intent | `intent(in)` default |
| Arguments modifiable | Read-only unless explicit |

**Principle of least privilege:** arguments are read-only unless explicitly declared otherwise.

Aligns with modern language design (Rust immutable-by-default).

---

# 3. Generic Programming

Two approaches available:

**J3 TEMPLATE** (official Fortran direction):
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
```

**Traits** (Swift/Rust style):
```fortran
type, implements(IComparable) :: my_type_t
```

---

# Generics: Which Approach?

<div class="question">

**OPEN ISSUE 5: Which generics system?**

| Option | Description |
|--------|-------------|
| A | J3 TEMPLATE only |
| B | Traits only |
| C | Hybrid (both) |

</div>

---

# Generics: Parameter Syntax

```fortran
! Option A: Braces (J3 inline)
call swap{integer}(a, b)

! Option B: Parentheses
call swap(integer)(a, b)    ! Ambiguous

! Option C: Angle brackets
call swap<integer>(a, b)    ! Conflicts with .lt.
```

<div class="question">

**OPEN ISSUE 6: Delimiter for type parameters?**
- A: Braces `{T}` - J3 direction, unambiguous
- B: Parentheses `(T)` - Fortran-like but ambiguous
- C: Angle brackets `<T>` - Familiar but conflicts

</div>

---

# Generics: Dispatch Mechanism

```fortran
! Static dispatch (monomorphization)
call add_i4(x, y)     ! Compiled for integer(4)

! Dynamic dispatch (vtable)
class(IAddable), pointer :: obj
call obj%add(x, y)    ! Runtime lookup
```

<div class="question">

**OPEN ISSUE 7: Static vs dynamic dispatch?**

| Option | Trade-off |
|--------|-----------|
| A | Static only - zero overhead |
| B | Dynamic only - runtime flexibility |
| C | Both - user chooses |

</div>

---

# Generics: Annotation Syntax

```fortran
! Standard style
type, implements(IComparable) :: my_type_t

! Annotation style
@IComparable
integer function compare(a, b)
    integer, intent(in) :: a, b
    compare = a - b
end function
```

<div class="question">

**OPEN ISSUE 16: Support @ annotations?**

| Option | Trade-off |
|--------|-----------|
| A | No @ - Fortran-like |
| B | @ syntax - familiar to Java/Python |
| C | Both supported |

</div>

---

# 4. Monomorphization

Generic code specialized for each type used:

```fortran
function add(a, b)
    add = a + b
end function

x = add(5, 3)       ! Generates add_i4
y = add(2.5, 1.5)   ! Generates add_r4
```

---

# Monomorphization: Scope

<div class="question">

**OPEN ISSUE 8: Where to generate specializations?**

| Option | Scope | Trade-off |
|--------|-------|-----------|
| A | Per-module | May duplicate |
| B | Per-program (LTO) | No duplication |
| C | Lazy (on-demand) | Complex build |

</div>

---

# 5. ABI: Name Mangling

Specialized functions need unique names:

```
add(integer, integer)  ->  add_i4_i4
add(real(8), real(8))  ->  add_r8_r8
```

---

# ABI: Kind Suffix Convention

```fortran
integer(4) function add_??(a, b)
! Bits:  add_i32  (32 bits)
! Bytes: add_i4   (4 bytes = kind parameter)
```

<div class="question">

**OPEN ISSUE 9: Bits or bytes for kind suffixes?**

| Option | Example | Rationale |
|--------|---------|-----------|
| A | i32, r64 | C/Rust convention |
| B | i4, r8 | Matches Fortran kind |

Note: complex(8) has two real(8) -> c8 not c128

</div>

---

# Summary: Single-pass Open Issues

These require only local analysis (traditional Fortran style):

| # | Topic | Key Question |
|---|-------|--------------|
| 1 | Numeric kinds | real(4) vs real(8) default? |
| 3 | Declarations | Block start vs anywhere? |
| 10 | Char length | First vs max vs deferred? |
| 15 | Fallback type | Error vs default? |
| 5 | Generics | Template vs Traits vs Both? |
| 6 | Generic syntax | Braces vs parens vs angles? |
| 9 | Kind suffix | Bits vs bytes? |
| 16 | @ syntax | Support annotations? |

---

# Summary: Semantic Analysis Open Issues

These require multi-pass or whole-program analysis:

| # | Topic | Key Question |
|---|-------|--------------|
| 2 | Intent(out) | Auto-declare from callee? |
| 7 | Dispatch | Static vs dynamic vs both? |
| 8 | Specialization | Module vs program scope? |
| 11 | Pointer | Infer attribute? |
| 12 | Allocatable | Infer from allocate? |
| 14 | Derived types | Auto-USE modules? |

**Resolved:** Issue 4 (default intent = `intent(in)`), Issue 13 (function result types from body + monomorphization)

---

# Discussion Order Suggestion

**Single-pass issues first (simpler to implement):**
- Issue 9: Kind suffix (bits vs bytes)
- Issue 3: Declaration placement
- Issue 16: @ annotation syntax
- Issue 1: Default numeric kinds
- Issue 10: Character length
- Issue 15: Fallback type
- Issues 5-6: Generics syntax

**Then semantic analysis issues:**
- Issues 7-8: Dispatch and specialization scope
- Issues 11-12: Pointer/allocatable inference
- Issue 14: Derived type auto-USE
- Issue 2: Intent(out) inference

---

# Resources

- **Design Document:** [lazyfortran2025-design.md](../lazyfortran2025-design.md)
- **Base Standard:** ISO/IEC 1539-1:2023

**Document Status:** DRAFT - All provisions subject to change
