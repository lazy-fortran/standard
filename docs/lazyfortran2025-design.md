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

Lazy Fortran 2025 adopts a traits-based generics system inspired by Swift, Rust, and Go. This enables both compile-time and run-time polymorphism through a unified interface mechanism.

### Traits (Named Abstract Interfaces)

Traits define collections of procedure signatures that types can implement:

```fortran
abstract interface :: INumeric
   integer | real(real64)              ! Type set syntax
end interface INumeric

abstract interface :: ISum
   function sum{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
   end function sum
end interface ISum
```

**Key features:**
- **Type sets:** `integer | real(real64)` admits multiple types as constraint
- **Generic parameters:** `{INumeric :: T}` constrains type parameter T
- **Wildcard kinds:** `real(*)` matches all real kinds
- **Associated types:** `itself` refers to the implementing type

### Implements Statement

Types declare trait conformance via `implements`:

```fortran
type, sealed, implements(ISum) :: SimpleSum
contains
   procedure, nopass :: sum
end type SimpleSum

! Or retroactively:
implements (INumeric + IPrintable) :: MyType
end implements MyType
```

### Generic Procedures

Procedures parameterized by constrained type parameters:

```fortran
function average{INumeric :: T}(x) result(a)
   type(T), intent(in) :: x(:)
   type(T)             :: a
   a = sum(x) / T(size(x))
end function average
```

**Instantiation:** Lazy Fortran infers type arguments automatically (no manual instantiation required, as in Swift).

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

**Chris' preference:** Option A - standard compliance is paramount.

### OQ-2: Inference from `intent(out)` arguments

Fortran idiomatically uses `intent(out)` arguments instead of function return values.

```fortran
call init_particle(p)  ! Should this declare p automatically?
```

| Option | Pros | Cons |
|--------|------|------|
| A: No (assignment only) | Simple local analysis, predictable | Doesn't support idiomatic Fortran patterns |
| B: Yes (inspect callee) | Supports `intent(out)` patterns | Requires cross-procedure analysis, interface files |

**Chris' preference:** No clear preference - both options have merit.

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
| A: Block beginning only | Clean structure, all declarations visible at once | Verbose, variables far from first use |
| B: Anywhere | Declare near use, familiar to C/Rust devs | Scattered declarations, redundant with inference |

**Chris' preference:** Option A - type inference handles convenience; if you want structure, declare at top.

### OQ-4: Generic parameter syntax

What syntax for generic type parameters?

```fortran
! Option A: Curly braces (as in Traits-for-Fortran proposal)
function sum{INumeric :: T}(x) result(s)

! Option B: Square brackets (Go-style)
function sum[INumeric :: T](x) result(s)

! Option C: Angle brackets (C++/Rust-style)
function sum<INumeric :: T>(x) result(s)
```

| Option | Pros | Cons |
|--------|------|------|
| A: Curly braces `{}` | Distinct from arrays, familiar from Swift | Not used elsewhere in Fortran |
| B: Square brackets `[]` | Used in Go, clean | Conflicts with array syntax |
| C: Angle brackets `<>` | Familiar to C++/Rust devs | Conflicts with operators `<` and `>` |

**Chris' preference:** Option A - curly braces are unambiguous in Fortran context.

### OQ-5: Type sets vs explicit trait signatures

How to define generics constraints?

```fortran
! Option A: Type sets (Go-style, implicit signatures)
abstract interface :: INumeric
   integer | real(real64)
end interface INumeric

! Option B: Explicit signatures (Rust-style)
abstract interface :: INumeric
   function init(n)
      integer, intent(in) :: n
   end function init
   function operator(+)(lhs, rhs) result(res)
      type(itself), intent(in) :: lhs, rhs
      type(itself)             :: res
   end function operator(+)
end interface INumeric
```

| Option | Pros | Cons |
|--------|------|------|
| A: Type sets | Concise, leverages intrinsic operations | Implicit, harder to extend to user types |
| B: Explicit signatures | Clear contract, works for any type | Verbose, requires explicit trait implementations |

**Chris' preference:** Support both - type sets for intrinsic-heavy code, explicit for OO patterns.

### OQ-6: Trait dispatch mechanism

Should traits support both static (compile-time) and dynamic (run-time) dispatch?

```fortran
! Static dispatch (zero-cost, monomorphized)
type(T), intent(in) :: x   ! T known at compile time

! Dynamic dispatch (vtable, existential type)
class(INumeric), allocatable :: x   ! Type erased, runtime dispatch
```

| Option | Pros | Cons |
|--------|------|------|
| A: Static only | Zero runtime overhead, simpler | No runtime polymorphism |
| B: Dynamic only | Flexible, OO-style | Runtime overhead, no monomorphization |
| C: Both (Swift/Rust model) | Best of both worlds | More complex implementation |

**Chris' preference:** Option C - let user choose `type(T)` for static, `class(ITrait)` for dynamic.

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
