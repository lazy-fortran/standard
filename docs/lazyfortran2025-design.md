# Lazy Fortran 2025 Design

> **Status:** Work in progress. Details may change as implementation evolves.

Lazy Fortran 2025 builds on the standard-grounded grammars for FORTRAN 1957 through Fortran 2023, adding:

- Type inference for `.lf` sources
- World-wide automatic specializations (monomorphization)
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

### Open Questions: Type Inference

#### TYP-OQ-1: Default numeric kinds

What kind should inferred numeric literals have?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: ISO defaults (`real(4)`, `integer(4)`) | Compatible with existing code, predictable, smaller memory | Precision loss, integer overflow at ~2B | Generates smaller specialized code |
| B: Double precision (`real(8)`, `integer(8)`) | Safer for scientific computing, no precision surprises | Breaks ISO expectations, 2x memory | Larger but more precise specializations |
| C: Context-dependent (match surrounding code) | Adapts to usage context | Unpredictable, harder to reason about | May generate multiple specializations |

#### TYP-OQ-2: Inference from `intent(out)` arguments

Fortran idiomatically uses `intent(out)` arguments instead of function return values.

```fortran
call init_particle(p)  ! Should this declare p automatically?
```

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: No (assignment only) | Simple local analysis, predictable | Doesn't support idiomatic Fortran patterns | Simpler - types known locally |
| B: Yes (inspect callee) | Supports `intent(out)` patterns | Requires cross-procedure analysis, interface files | Requires interface analysis before specialization |

#### TYP-OQ-3: Declaration placement

Should explicit declarations be allowed anywhere in the code, or only at block beginning?

```fortran
x = 1
y = 2
integer :: z  ! Allowed mid-block?
z = x + y
```

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Block beginning only | Clean structure, all declarations visible | Verbose, variables far from first use | No impact - types resolved same either way |
| B: Anywhere | Declare near use, familiar to C/Rust devs | Scattered declarations, redundant with inference | No impact |

---

## LF-GEN: Traits and Generics

Lazy Fortran 2025 will adopt a generics system. Two main proposals exist, both supporting **automatic monomorphization** - the compiler generates specialized code for each concrete type used.

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

! Explicit instantiation - triggers monomorphization
INSTANTIATE swap_t(integer), ONLY: swap_int => swap
INSTANTIATE swap_t(real), ONLY: swap_real => swap

! Inline instantiation (simple template procedures)
CALL swap{integer}(a, b)  ! Also triggers monomorphization
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

**Monomorphization:** Explicit `INSTANTIATE` statements define exactly which specializations to generate. Inline `{type}` syntax generates on-demand.

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
y = average(x)  ! T inferred from x, monomorphized automatically
```

**Monomorphization:** Compiler automatically generates specializations based on call-site types. No explicit instantiation needed.

### Comparison

| Aspect | J3 TEMPLATE | Traits-for-Fortran |
|--------|-------------|-------------------|
| Instantiation | Explicit `INSTANTIATE` or inline `{T}` | Automatic inference |
| Constraints | `REQUIREMENT` + `REQUIRES` | Type sets or trait signatures |
| Scope | Module-level templates | Traits retroactively implementable |
| Dispatch | Compile-time only (static) | Both static and dynamic |
| Monomorphization control | Explicit - user controls what's generated | Implicit - compiler decides |

### Open Questions: Generics

#### GEN-OQ-1: Which generics approach?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: J3 TEMPLATE only | Official Fortran direction, predictable | Verbose, manual instantiation | User explicitly controls all specializations |
| B: Traits-for-Fortran only | Concise, automatic, retroactive | Not official, complex | Compiler auto-generates; may over-specialize |
| C: Hybrid (both) | Maximum flexibility | Two systems to learn | User chooses explicit or automatic per case |

#### GEN-OQ-2: Generic parameter syntax

What delimiter for generic type parameters in procedure signatures?

```fortran
SUBROUTINE swap{T}(x, y)      ! Curly braces
SUBROUTINE swap(T)(x, y)      ! Parentheses (J3 TEMPLATE style)
SUBROUTINE swap<T>(x, y)      ! Angle brackets
```

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Curly braces `{T}` | Distinct from arrays, J3 inline syntax | Not traditional Fortran | None - syntax only |
| B: Parentheses `(T)` | J3 TEMPLATE style, Fortran-like | Ambiguous with function calls | None |
| C: Angle brackets `<T>` | Familiar to C++/Rust devs | Conflicts with `<` and `>` operators | None |

#### GEN-OQ-3: Instantiation model

How should generic code be instantiated/monomorphized?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Explicit only | Predictable, no surprises, J3 approach | Verbose boilerplate | Only requested specializations generated |
| B: Automatic only | Concise, Swift-like | Magic, harder to debug | Compiler generates all used specializations |
| C: Automatic with explicit override | Ergonomic defaults, control when needed | Two mechanisms | Auto-generate, but allow manual control |

#### GEN-OQ-4: Constraint specification

How to specify type constraints for generics?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: REQUIREMENT construct (J3) | Reusable, explicit, type-safe | Verbose | Constraints checked before monomorphization |
| B: Type sets (`integer \| real`) | Concise for intrinsics | Only listed types | Generates specialization per type in set |
| C: Named traits with signatures | OO-style, extensible, user types | Requires implementations | Generates for all implementing types |
| D: All of the above | Maximum flexibility | Complex spec | Mixed - depends on which mechanism used |

#### GEN-OQ-5: Dispatch mechanism

Should generics support both compile-time and run-time polymorphism?

```fortran
! Static dispatch - monomorphized, zero overhead
TYPE(T), INTENT(IN) :: x

! Dynamic dispatch - vtable, runtime flexibility
CLASS(INumeric), ALLOCATABLE :: x
```

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Static only (J3) | Zero overhead, predictable | No runtime flexibility | All generic code monomorphized |
| B: Dynamic only | Runtime polymorphism | Overhead, no specialization | No monomorphization - vtable dispatch |
| C: Both (`type(T)` vs `class(ITrait)`) | User chooses tradeoff | Complex implementation | `type(T)` monomorphized, `class()` uses vtable |

---

## LF-SYN: World-Wide Automatic Specializations (#51)

Lazy Fortran 2025 introduces world-wide specialization (automatic monomorphization) for generic procedures, aligned with ISO/IEC 1539-1:2018 Section 15.4.3.4.

**How it works:**
1. Generic procedures are defined with type parameters
2. At each call site, concrete types are known
3. Compiler generates specialized (monomorphized) code for each unique type combination
4. Specialized code is optimized for the concrete types (inlining, SIMD, etc.)

**Resolution policy:**
1. **User-written specifics win** - explicit procedures always take precedence over generated specializations
2. **Most specific candidate wins** - follows ISO generic resolution rules
3. **Ambiguity is an error** - incomparable candidates trigger compile-time errors with ISO rule references

This enables Lazy Fortran to add optimized implementations (BLAS-backed kernels, vectorized loops) while remaining standard-compliant.

### Open Questions: Specialization

#### SYN-OQ-1: Specialization scope

At what scope should specializations be generated?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Per-module | Smaller compilation units | May duplicate across modules | Each module has own specializations |
| B: Per-program (link-time) | No duplication, optimal | Requires LTO, longer link | Single copy of each specialization |
| C: Lazy (on-demand) | Only generates what's used | Complex build system | Minimal code size |

#### SYN-OQ-2: Specialization for optimized libraries

Should the compiler auto-generate BLAS/LAPACK-backed specializations?

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Yes, always | Automatic optimization | Binary size, BLAS dependency | Extra specializations for numeric types |
| B: Yes, opt-in | User controls | Requires annotation | Only when requested |
| C: No, user provides | Maximum control | More work for user | User writes specialized implementations |

---

## LF-WLD: ISO Generic Resolution Alignment (#54)

Lazy Fortran specializations are **standard-compatible decorations** that never override ISO generic behavior:

- Operates *on top* of ISO semantics
- Never changes which specific procedures exist from ISO perspective
- Resolution per ISO/IEC 1539-1:2018 Section 15.4.3.4
- Monomorphized code behaves identically to hand-written specific procedures

### Open Questions: ISO Alignment

#### WLD-OQ-1: Handling ambiguous resolutions

When automatic specialization creates ambiguity with user code:

| Option | Pros | Cons | Monomorphization Impact |
|--------|------|------|------------------------|
| A: Error at compile time | Safe, predictable | May reject valid-seeming code | Prevents problematic specializations |
| B: User code always wins | Compatible | May hide optimization | User specific blocks auto-specialization |
| C: Most specific wins | Optimal dispatch | Complex rules | May choose auto over user if more specific |

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

---

## References

- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [J3 Paper 18-281r1](https://j3-fortran.org/doc/year/18/18-281r1.txt) - Simple templates proposal
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/) - TEMPLATE/INSTANTIATE syntax
- [Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran) - Swift/Rust-inspired proposal
- ISO/IEC 1539-1:2018 Section 15.4.3.4 - Generic resolution rules
