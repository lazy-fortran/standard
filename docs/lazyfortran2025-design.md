# Lazy Fortran 2025 Design

> **Status:** Work in progress. Details may change as implementation evolves.

Lazy Fortran 2025 builds on the standard-grounded grammars for FORTRAN 1957 through Fortran 2023, adding:

- Type inference for `.lf` sources
- World-wide automatic specializations
- Strict alignment with ISO Fortran generic resolution rules

This is a **design document**, not a standard. The ISO text is the normative reference for any ambiguity.

---

## LF-TYP: Automatic Type Inference (#52, #53)

Lazy Fortran 2025 uses **automatic type inference** - a modern approach that eliminates the legacy I-N naming convention while remaining predictable.

**First assignment wins:**
- The type of an undeclared variable is determined by its **first assignment**
- `x = 4` makes `x` an integer
- `y = 3.14` makes `y` a real
- `z = (1.0, 2.0)` makes `z` a complex
- `obj = create_particle()` makes `obj` whatever type `create_particle` returns

**Subsequent assignments follow standard coercion:**
- After type is established, later assignments use normal Fortran coercion rules
- `x = 5.3` after `x = 4` is valid (real truncated to integer)
- `y = 7` after `y = 3.14` is valid (integer promoted to real)

**Interaction with `implicit none`:**
- With `implicit none`: undeclared names are errors; inference is disabled
- This preserves compatibility with strict coding styles

**Expression promotion rules:**

| Expression | Result |
|------------|--------|
| integer + integer | integer |
| integer + real | real |
| real + complex | complex |
| complex + complex | complex (dominant kind) |

---

## LF-GEN: Traits and Generics

Lazy Fortran 2025 will adopt a generics system. Two main proposals exist:

### Approach A: J3 TEMPLATE/REQUIREMENT (Official Fortran 202Y Direction)

The J3 committee's approach uses explicit TEMPLATE constructs with deferred type parameters:

```fortran
! Template definition
TEMPLATE swap_t(T)
   TYPE, DEFERRED :: T
CONTAINS
   SUBROUTINE swap(x, y)
      TYPE(T), INTENT(INOUT) :: x, y
      TYPE(T) :: tmp
      tmp = x; x = y; y = tmp
   END SUBROUTINE
END TEMPLATE

! Explicit instantiation
INSTANTIATE swap_t(integer), ONLY: swap_int => swap
INSTANTIATE swap_t(real), ONLY: swap_real => swap

! Inline instantiation (simple template procedures)
CALL swap{integer}(a, b)
```

**REQUIREMENT construct** for reusable constraints:

```fortran
REQUIREMENT R_comparable(T, less_than)
   TYPE, DEFERRED :: T
   INTERFACE
      PURE LOGICAL FUNCTION less_than(a, b)
         TYPE(T), INTENT(IN) :: a, b
      END FUNCTION
   END INTERFACE
END REQUIREMENT

TEMPLATE sort_t(T, less_than)
   REQUIRES R_comparable(T, less_than)
CONTAINS
   SUBROUTINE sort(arr)
      TYPE(T), INTENT(INOUT) :: arr(:)
      ...
   END SUBROUTINE
END TEMPLATE
```

### Approach B: Traits-for-Fortran (Swift/Rust-inspired)

Named abstract interfaces (traits) with type sets and implicit instantiation:

```fortran
! Trait with type set constraint
ABSTRACT INTERFACE :: INumeric
   integer | real(real64)
END INTERFACE INumeric

! Trait with generic procedure signature
ABSTRACT INTERFACE :: ISum
   FUNCTION sum{INumeric :: T}(x) RESULT(s)
      TYPE(T), INTENT(IN) :: x(:)
      TYPE(T) :: s
   END FUNCTION
END INTERFACE ISum

! Type implementing trait
TYPE, SEALED, IMPLEMENTS(ISum) :: SimpleSum
CONTAINS
   PROCEDURE, NOPASS :: sum
END TYPE

! Retroactive trait implementation
IMPLEMENTS (INumeric + IPrintable) :: MyType
END IMPLEMENTS MyType

! Usage - automatic instantiation (no manual INSTANTIATE)
y = average(x)  ! T inferred from x
```

### Key Differences

| Aspect | J3 TEMPLATE | Traits-for-Fortran |
|--------|-------------|-------------------|
| Instantiation | Explicit `INSTANTIATE` or inline `{T}` | Automatic inference (Swift-like) |
| Constraints | `REQUIREMENT` + `REQUIRES` | Type sets or explicit signatures in traits |
| Scope | Module-level templates | Traits can be retroactively implemented |
| Dispatch | Compile-time only | Both static (`type(T)`) and dynamic (`class(ITrait)`) |

---

## LF-SYN: World-Wide Automatic Specializations (#51)

Lazy Fortran 2025 introduces world-wide specialization for generic procedures, aligned with ISO/IEC 1539-1:2018 Section 15.4.3.4.

**Resolution policy:**
1. **User-written specifics win** - explicit procedures always take precedence over generated specializations
2. **Most specific candidate wins** - follows ISO generic resolution rules
3. **Ambiguity is an error** - incomparable candidates trigger compile-time errors with ISO rule references

This enables Lazy Fortran to add optimized implementations (BLAS-backed kernels, vectorized loops) while remaining standard-compliant.

---

## LF-WLD: ISO Generic Resolution Alignment (#54)

Lazy Fortran specializations are **standard-compatible decorations** that never override ISO generic behavior:

- Operates *on top* of ISO semantics
- Never changes which specific procedures exist from ISO perspective
- Resolution per ISO/IEC 1539-1:2018 Section 15.4.3.4

---

## Open Questions

Design decisions that need further discussion before finalizing.

### OQ-1: Default numeric kinds

| Option | Pros | Cons |
|--------|------|------|
| A: ISO defaults (`real(4)`, `integer(4)`) | Compatible with existing code, predictable, smaller memory | Precision loss, integer overflow at ~2B |
| B: Double precision (`real(8)`, `integer(8)`) | Safer for scientific computing, no precision surprises | Breaks ISO expectations, 2x memory |

### OQ-2: Inference from `intent(out)` arguments

Fortran idiomatically uses `intent(out)` arguments instead of function return values.

```fortran
call init_particle(p)  ! Should this declare p automatically?
```

| Option | Pros | Cons |
|--------|------|------|
| A: No (assignment only) | Simple local analysis, predictable | Doesn't support idiomatic Fortran patterns |
| B: Yes (inspect callee) | Supports `intent(out)` patterns | Requires cross-procedure analysis, interface files |

### OQ-3: Declaration placement

Should declarations be allowed anywhere in the code, or only at block beginning?

```fortran
x = 1
y = 2
integer :: z  ! Allowed mid-block?
z = x + y
```

| Option | Pros | Cons |
|--------|------|------|
| A: Block beginning only | Clean structure, all declarations visible | Verbose, variables far from first use |
| B: Anywhere | Declare near use, familiar to C/Rust devs | Scattered declarations, redundant with inference |

### OQ-4: Generics approach

Which generics system to adopt?

| Option | Pros | Cons |
|--------|------|------|
| A: J3 TEMPLATE/REQUIREMENT | Official Fortran direction, type-safe, explicit | Verbose, manual instantiation |
| B: Traits-for-Fortran | Concise, automatic inference, retroactive impl | Not official, more complex semantics |
| C: Hybrid | Best of both, flexible | Implementation complexity |

### OQ-5: Generic parameter syntax

What delimiter for generic type parameters?

```fortran
function sum{T}(x)    ! Curly braces (J3 inline, Traits-for-Fortran)
function sum(T)(x)    ! Parentheses (J3 TEMPLATE)
function sum<T>(x)    ! Angle brackets (C++/Rust)
```

| Option | Pros | Cons |
|--------|------|------|
| A: Curly braces `{T}` | Distinct from arrays, used in J3 inline syntax | Not traditional Fortran |
| B: Parentheses `(T)` | Used in J3 TEMPLATE, Fortran-like | Ambiguous with function calls |
| C: Angle brackets `<T>` | Familiar to C++/Rust devs | Conflicts with relational operators |

### OQ-6: Instantiation model

How should generic code be instantiated?

| Option | Pros | Cons |
|--------|------|------|
| A: Explicit only (`INSTANTIATE`) | Clear, predictable, J3 approach | Verbose, boilerplate |
| B: Automatic inference | Concise, Swift-like ergonomics | Magic, harder to debug |
| C: Both (explicit + inline) | Flexibility, J3 supports both | Two ways to do same thing |

### OQ-7: Constraint specification

How to specify type constraints for generics?

| Option | Pros | Cons |
|--------|------|------|
| A: REQUIREMENT construct (J3) | Reusable, explicit, type-safe | Verbose |
| B: Type sets (`integer \| real`) | Concise for intrinsics | Only works for listed types |
| C: Named traits with signatures | OO-style, extensible | Requires trait implementations |
| D: All of the above | Maximum flexibility | Complex specification |

### OQ-8: Dispatch mechanism

Should generics support both compile-time and run-time polymorphism?

| Option | Pros | Cons |
|--------|------|------|
| A: Static only (J3 approach) | Zero overhead, simple | No runtime flexibility |
| B: Dynamic only (`class(ITrait)`) | Runtime polymorphism | Overhead, no monomorphization |
| C: Both (`type(T)` vs `class(ITrait)`) | User chooses tradeoff | Complex implementation |

---

## Issue Tracking

| Issue | ID | Description |
|-------|-----|-------------|
| #51 | LF-SYN | World-wide automatic specializations |
| #52, #53 | LF-TYP | Automatic type inference |
| #54 | LF-WLD | ISO generic resolution alignment |
| #55 | LF-WLD | Interoperability with legacy Fortran code |
| #56 | LF-CODE | Tooling integration and diagnostics |
| #57 | LF-DOC | Documentation guarantees |
