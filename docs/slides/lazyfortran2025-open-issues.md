---
marp: true
theme: default
paginate: true
backgroundColor: #0b1020
color: #f5f7ff
style: |
  section {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
  }
  h1, h2 {
    color: #ffcc66;
  }
  h3 {
    color: #9ca3af;
  }
  code {
    background: #111827;
    border-radius: 4px;
    padding: 2px 6px;
  }
  pre {
    background: #111827;
    border: 1px solid #374151;
    border-radius: 8px;
  }
  table {
    font-size: 0.85em;
  }
  th {
    background: #1f2937;
  }
  blockquote {
    border-left: 4px solid #ffcc66;
    background: rgba(255, 204, 102, 0.1);
    padding: 0.5em 1em;
    margin: 0.5em 0;
  }
  .tier1 { color: #4ade80; }
  .tier2 { color: #facc15; }
  .tier3 { color: #f87171; }
  .recommendation {
    background: rgba(74, 222, 128, 0.15);
    border: 1px solid #4ade80;
    border-radius: 8px;
    padding: 0.5em 1em;
    margin-top: 0.5em;
  }
---

# Lazy Fortran 2025
## Open Issues Discussion

**Draft Standard Review**

Base: ISO/IEC 1539-1:2023 (Fortran 2023)

---

# Agenda

### <span class="tier1">Tier 1: Quick Decisions (5 min each)</span>
- ISSUE 9: Kind suffix convention
- ISSUE 10: Character length inference
- ISSUE 16: @ annotation syntax

### <span class="tier2">Tier 2: Design Implications (10 min each)</span>
- ISSUE 1: Default numeric kinds
- ISSUE 2: Intent(out) inference
- ISSUE 5: Generics approach
- ISSUE 15: Fallback type

### <span class="tier3">Tier 3: Deeper Discussion (15+ min)</span>
- ISSUES 3, 4, 6, 7, 8, 11-14

---

<!-- _class: lead -->

# Tier 1: Quick Decisions

---

# ISSUE 9: Kind Suffix Convention

**Bytes vs Bits for mangled names?**

| Option | Example | Rationale |
|--------|---------|-----------|
| **A: Bits** | `add_i32`, `add_r64` | C/Rust convention, current fortfront |
| **B: Bytes** | `add_i4`, `add_r8` | Matches Fortran kind parameter |

```fortran
! With bits (i32 = 32 bits = 4 bytes)
integer(4) function add_i32(a, b)

! With bytes (i4 = 4 bytes)
integer(4) function add_i4(a, b)
```

<div class="recommendation">

**Recommendation:** Option B (Bytes) - Consistency with `integer(4)`, `real(8)`

</div>

---

# ISSUE 9: Kind Suffix - Full Table

| Type | Kind | Bits | Bytes |
|------|------|------|-------|
| integer | 1 | i8 | i1 |
| integer | 4 | i32 | i4 |
| integer | 8 | i64 | i8 |
| real | 4 | r32 | r4 |
| real | 8 | r64 | r8 |
| complex | 4 | c64 | c4 |
| complex | 8 | c128 | c8 |

**Key insight:** `complex(8)` contains two `real(8)` → should be `c8` not `c128`

---

# ISSUE 10: Character Length Inference

**How to handle multiple string assignments?**

```fortran
s = "hello"      ! len=5
s = "goodbye"    ! len=7 - what happens?
```

| Option | Description | Pros | Cons |
|--------|-------------|------|------|
| A | First wins | Consistent | Truncation |
| B | Maximum | No truncation | Memory |
| C | Deferred-length | Dynamic | Overhead |

<div class="recommendation">

**Recommendation:** Option C (Deferred-length) - Modern Fortran idiom

</div>

---

# ISSUE 16: @ Annotation Syntax

**Should trait annotations use `@` prefix?**

```fortran
! Option A: No annotations (verbose)
TYPE, IMPLEMENTS(IComparable) :: MyType

! Option B: @ syntax (concise)
@IComparable
integer function compare(a, b)
    integer, intent(in) :: a, b
    compare = a - b
end function
```

| Option | Pros | Cons |
|--------|------|------|
| A: No @ | Fortran-like | Verbose |
| B: @ syntax | Familiar (Java/Python) | New symbol |
| C: Both | Flexibility | Two ways |

---

<!-- _class: lead -->

# Tier 2: Design Implications

---

# ISSUE 1: Default Numeric Kinds

**What kind for inferred numeric literals?**

```fortran
x = 42          ! integer(??)
y = 3.14159     ! real(??)
```

| Option | Types | Pros | Cons |
|--------|-------|------|------|
| A | real(4), int(4) | ISO compatible | Precision loss |
| B | real(8), int(8) | Scientific safe | 2x memory |
| C | Context | Adapts | Unpredictable |

<div class="recommendation">

**Recommendation:** Option B (Double) for safety, or Option A with promotion rules

</div>

---

# ISSUE 2: Inference from intent(out)

**Should `call init(p)` auto-declare `p`?**

```fortran
subroutine init(particle)
    type(particle_t), intent(out) :: particle
    ...
end subroutine

! In calling code:
call init(p)    ! Should p be auto-declared as particle_t?
```

| Option | Description | Analysis Required |
|--------|-------------|-------------------|
| A | No (assignment only) | Local only |
| B | Yes (inspect callee) | Cross-procedure |

---

# ISSUE 5: Generics Approach

**Which generics system?**

| Option | Description | Example |
|--------|-------------|---------|
| A | J3 TEMPLATE | `TEMPLATE swap_t(T)` |
| B | Traits only | `IMPLEMENTS IComparable` |
| C | Hybrid | Both available |

```fortran
! J3 style
INSTANTIATE swap_t(integer), ONLY: swap_int => swap

! Traits style
@IComparable
TYPE :: MyType
```

<div class="recommendation">

**Recommendation:** Option C (Hybrid) - Future-proof for J3, ergonomic for users

</div>

---

# ISSUE 15: Fallback Type

**What if type inference fails?**

```fortran
x = some_unresolvable_expression()
! Cannot determine type - what now?
```

| Option | Behavior | Pros | Cons |
|--------|----------|------|------|
| A | Compile error | Catches issues | May reject valid code |
| B | Default real(8) | fortfront compat | Silent assumption |
| C | ISO default | Standard | Precision loss |

<div class="recommendation">

**Recommendation:** Option A (Error) - Fail fast, explicit is better

</div>

---

<!-- _class: lead -->

# Tier 3: Deeper Discussion

---

# ISSUE 3: Declaration Placement

**Where can explicit declarations appear?**

```fortran
! Option A: Block beginning only
subroutine foo()
    integer :: x, y    ! Must be here
    x = 1
    y = 2
end subroutine

! Option B: Anywhere
subroutine bar()
    x = 1
    integer :: y       ! Near first use
    y = 2
end subroutine
```

**Trade-off:** Clean structure vs. declare-near-use

---

# ISSUE 4: Default Intent

**What intent when not specified?**

```fortran
subroutine process(data)
    ! No intent specified - what default?
end subroutine
```

| Option | Default | ISO Behavior |
|--------|---------|--------------|
| A | intent(in) | Different (safer) |
| B | intent(inout) | Closer to ISO |
| C | Required explicit | Forces clarity |

**Key consideration:** Breaking change from ISO Fortran

---

# ISSUE 6: Generic Parameter Syntax

**Delimiter for type parameters?**

```fortran
! Option A: Braces (J3 inline)
call swap{integer}(a, b)

! Option B: Parentheses (Fortran-like)
call swap(integer)(a, b)    ! Ambiguous!

! Option C: Angle brackets (C++/Rust)
call swap<integer>(a, b)    ! Conflicts with .lt.
```

<div class="recommendation">

**Recommendation:** Option A (Braces) - Unambiguous, J3 direction

</div>

---

# ISSUE 7: Dispatch Mechanism

**Static vs Dynamic dispatch for generics?**

| Option | Runtime Cost | Flexibility |
|--------|--------------|-------------|
| A: Static only | Zero overhead | No runtime poly |
| B: Dynamic only | Vtable | Full polymorphism |
| C: Both | User choice | Complex |

```fortran
! Static (monomorphization)
call add_i32(x, y)

! Dynamic (vtable)
class(IAddable), pointer :: obj
call obj%add(x, y)
```

---

# ISSUE 8: Specialization Scope

**Where to generate specialized code?**

| Option | Scope | Trade-off |
|--------|-------|-----------|
| A | Per-module | May duplicate |
| B | Per-program (LTO) | No duplication |
| C | Lazy (on-demand) | Complex build |

**Consideration:** Build system integration, separate compilation

---

# ISSUES 11-12: Pointer/Allocatable Inference

**Auto-infer attributes from usage?**

```fortran
! ISSUE 11: Pointer
p => x    ! Auto-declare p as pointer, x as target?

! ISSUE 12: Allocatable
allocate(arr(n))    ! Auto-declare arr as allocatable?
```

| Approach | Pros | Cons |
|----------|------|------|
| Auto-infer | Concise | Hides semantics |
| Explicit | Clear intent | Verbose |

---

# ISSUES 13-14: Advanced Inference

### ISSUE 13: Function Result Type
```fortran
function add(a, b)
    add = a + b   ! Infer return type from body?
end function
```

### ISSUE 14: Derived Type Inference
```fortran
p = particle_t(1.0, 2.0, 3.0)
! Auto-USE module containing particle_t?
```

**Trade-off:** Convenience vs. explicit dependencies

---

<!-- _class: lead -->

# Summary: Recommended Decisions

---

# Quick Wins (Tier 1)

| Issue | Recommendation |
|-------|----------------|
| **9: Kind suffix** | Bytes (i4, r8) - Fortran consistency |
| **10: Char length** | Deferred-length - Modern idiom |
| **16: @ syntax** | Support both - Flexibility |

---

# Design Decisions (Tier 2)

| Issue | Recommendation |
|-------|----------------|
| **1: Numeric kinds** | Double precision default |
| **2: Intent(out)** | Yes, inspect callee |
| **5: Generics** | Hybrid (J3 + Traits) |
| **15: Fallback** | Compile error |

---

# Discussion Items (Tier 3)

| Issue | Key Question |
|-------|--------------|
| **3: Declaration** | Structure vs convenience? |
| **4: Default intent** | Safety vs ISO compat? |
| **6: Generic syntax** | Braces recommended |
| **7: Dispatch** | Performance vs flexibility? |
| **8: Scope** | Build system constraints? |
| **11-14: Inference** | How much magic? |

---

<!-- _class: lead -->

# Questions?

**Next Steps:**
1. Vote on Tier 1 items
2. Discuss Tier 2 trade-offs
3. Schedule deep-dives for Tier 3

---

# Resources

- **Design Document:** [lazyfortran2025-design.md](lazyfortran2025-design.md)
- **GitHub:** [lazy-fortran/standard](https://github.com/lazy-fortran/standard)
- **Base Standard:** ISO/IEC 1539-1:2023

**Document Status:** DRAFT - All provisions subject to change
