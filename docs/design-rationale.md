# Design Rationale

This document explains the design decisions behind LFortran Standard and Lazy Fortran, summarizing discussions and providing rationale for key choices.

---

## Why LFortran Standard is Stricter than ISO Fortran

ISO Fortran 2023 allows many practices that can lead to subtle bugs:

| Practice | ISO Fortran | LFortran Standard | Rationale |
|----------|-------------|-------------------|-----------|
| Implicit typing | Allowed | Error | Undeclared variables should be caught at compile time |
| Implicit interface | Allowed | Error | All procedures need explicit interfaces for type safety |
| Implicit argument casting | Allowed | Error | Type mismatches should be explicit |
| Array bounds checking | OFF by default | ON by default | Catch buffer overflows early |
| Argument intent | No default | `intent(in)` default | Principle of least privilege |

These restrictions can all be satisfied by valid Fortran 2023 code. Code written to LFortran Standard compiles with any Fortran 2023 compiler - it simply enforces best practices that are optional in the ISO standard.

---

## Why 8-Byte Default Real

### Problem with 4-byte default

Most Fortran compilers default to 4-byte (32-bit) reals, which provides only ~7 decimal digits of precision. This causes:

- Accumulation of rounding errors in iterative calculations
- Loss of precision in large-scale simulations
- Subtle numerical bugs that are hard to track down

### Solution: 8-byte default

LFortran Standard uses 8-byte (64-bit) reals by default, providing ~15 decimal digits:

```fortran
real :: x           ! real(8) in LFortran Standard
x = 1.0             ! 1.0_dp semantically
```

### Why not 4-byte integers too?

Integers default to 4 bytes (32-bit), matching Rust/C/Java:

- Most integers do not need 64-bit range
- 4-byte integers provide better cache utilization
- Twice as many integers per SIMD register
- Use `integer(8)` explicitly when needed

This asymmetry (8-byte real, 4-byte integer) matches what most scientific codes actually need: precise floating-point, efficient integer indexing.

---

## Why Default intent(in)

### Problem with no default intent

Standard Fortran allows procedure arguments without explicit intent, meaning they can be read and modified freely. This leads to:

- Accidental modification of inputs
- Unclear API contracts
- Harder-to-trace bugs

### Solution: intent(in) default

LFortran Standard makes all arguments `intent(in)` unless explicitly declared otherwise:

```fortran
subroutine process(x, y)
    integer :: x              ! intent(in) by default
    integer, intent(inout) :: y  ! explicit override required
    ! x = 1                   ! ERROR: cannot modify intent(in)
    y = x + 1                 ! OK
end subroutine
```

This follows the **principle of least privilege**: arguments are read-only unless you explicitly say otherwise. This aligns with modern language design (Rust's immutable-by-default) and makes code intent clearer.

---

## Why No Whole-Program Analysis for Generics

### Problem with C++/Julia approach

C++ templates and Julia generics use implicit monomorphization:

```fortran
! WRONG (would require whole-program analysis):
function add(a, b)      ! What types? Unknown until call site
    add = a + b
end function
x = add(5, 3)           ! Must scan all call sites to infer types
```

This approach causes:

- **C++ template explosion**: Each unique type combination generates new code, leading to long compile times and bloated binaries
- **Julia recompilation**: First call to a function with new types triggers JIT compilation, causing unpredictable latency
- **Whole-program analysis**: Cannot compile modules separately without knowing all call sites

### Solution: Explicit instantiation

Lazy Fortran requires explicit template instantiation:

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

! Explicit instantiation - no guessing needed
instantiate add_t(integer), only: add_int => add
instantiate add_t(real(8)), only: add_real => add

x = add_int(5, 3)       ! Type is known at this point
```

Benefits:
- **No whole-program analysis**: Each module compiles independently
- **Predictable compilation**: No hidden code generation at call sites
- **Clear dependencies**: You know exactly what types are instantiated

---

## Why Type Inference is Infer Mode Only

### Problem with mandatory type inference

Languages like Python infer types everywhere, which creates problems:

- Types are not documented in the source
- Refactoring is harder without type annotations
- IDE support is weaker without explicit types
- Runtime type errors instead of compile-time

### Solution: Opt-in type inference

Lazy Fortran restricts type inference to:

1. **Infer mode only** (`--infer` flag or REPL)
2. **Global scope only** (not inside procedures)
3. **Intrinsic types only** (not derived types)

```fortran
! Infer mode - types inferred at global scope
x = 42             ! integer
y = 3.14           ! real(8)

! But NOT for derived types
type(particle_t) :: p   ! Required explicit declaration
p = particle_t(x=1.0)   ! OK after declaration
```

This provides the convenience of type inference for prototyping while maintaining explicit typing for production code.

---

## Why Both Static and Dynamic Dispatch

### Problem with single dispatch model

Some languages force a choice:

- **Static only** (C++): Maximum performance but no runtime polymorphism
- **Dynamic only** (Java interfaces): Runtime flexibility but overhead on every call

### Solution: Both dispatch mechanisms

Lazy Fortran provides both:

| Syntax | Dispatch | Use case |
|--------|----------|----------|
| `type(T)` | Static | Zero overhead, inlinable |
| `class(Trait)` | Dynamic | Runtime polymorphism |

```fortran
! Static dispatch - resolved at compile time
function min_value{IComparable :: T}(a, b) result(res)
    type(T), intent(in) :: a, b  ! Concrete type, inlinable
    ...
end function

! Dynamic dispatch - resolved at runtime
subroutine sort_any(items)
    class(IComparable), intent(inout) :: items(:)  ! Any type implementing trait
    ...
end subroutine
```

You choose the appropriate dispatch mechanism based on performance requirements and flexibility needs.

---

## Why Unsigned Integers Have No Implicit Wraparound

### Problem with C-style unsigned arithmetic

C/C++ unsigned integers wrap around silently:

```c
unsigned int u = 0;
u = u - 1;  // Silently becomes 4294967295 (UINT_MAX)
```

This causes security vulnerabilities and subtle bugs.

### Solution: Rust-like overflow behavior

Lazy Fortran unsigned integers do not wrap by default:

```fortran
integer, unsigned :: u = 0
u = u - 1                   ! ERROR: unsigned underflow (debug mode)
u = wrap_sub(u, 1)          ! OK: explicit wraparound when intended
```

- **Debug mode**: Runtime error on overflow/underflow
- **`--fast` mode**: Wraparound (modular arithmetic, well-defined)
- **Explicit modular arithmetic**: `wrap_add`, `wrap_sub`, `wrap_mul` when wraparound is intentional

This catches accidental overflows while allowing intentional wraparound when explicitly requested.

---

## Why Dot Notation Requires Spacing for User-Defined Operators

### Problem with ambiguous dot notation

Both `.` for member access and `.op.` for user-defined operators use dots:

```fortran
a.b          ! Member access or partial operator?
a .lt. b     ! User-defined operator
```

### Solution: Spacing rules

Lazy Fortran disambiguates by requiring spaces around user-defined operators:

```fortran
a .op. b     ! User-defined operator (spaces required)
a.member     ! Member access (no spaces)
```

The standardizer enforces this rule and rejects ambiguous code at compile time.

---

## Summary of Key Design Principles

1. **Strictness by default**: Catch bugs at compile/run time, not in production
2. **Explicit over implicit**: No hidden behavior or whole-program analysis
3. **Scientific computing focus**: 8-byte reals, bounds checking, precise numerics
4. **Modern safety patterns**: Immutable by default, no silent overflow
5. **Gradual adoption**: Strict mode for production, infer mode for prototyping
6. **Standard compatibility**: All code compiles with standard Fortran compilers

---

## References

- [LFortran Standard](lfortran-standard.md)
- [Lazy Fortran Standard](lazy-fortran-standard.md)
- [LFortran Compiler](https://lfortran.org)
- [ISO/IEC 1539-1:2023](https://www.iso.org/standard/82170.html) - Fortran 2023
