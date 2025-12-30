# Lazy Fortran 2025

**Status:** Draft
**Implementation:** [LFortran](https://lfortran.org) compiler
**Base:** LFortran Standard (derived from Fortran 2023)

---

## Standards Hierarchy

```
ISO Fortran 2023 (ISO/IEC 1539-1:2023)
    │
    ▼
LFortran Standard (--std=lf)
    │   - Stricter than ISO Fortran
    │   - Bounds checking ON by default
    │   - Default real = 8 bytes, integer = 4 bytes
    │   - Default intent(in)
    │   - dp predefined
    │
    ▼
Lazy Fortran / LFortran Infer Mode (--infer)
        - Adds type inference at global scope
        - Adds automatic array reallocation
        - Adds global scope (bare statements)
        - Interactive REPL uses this implicitly
```

**Terminology:**
- **LFortran Standard** = `--std=lf` mode (default) - strict, production-ready
- **Lazy Fortran** / **Infer Mode** = `--infer` mode - interactive, prototyping
- **Standard Fortran** = `--std=f23` mode - ISO Fortran 2023 compatibility

---

## Summary

Lazy Fortran (LFortran Infer Mode) extends the LFortran Standard with type inference and quality-of-life features for interactive use and rapid prototyping. All features are implemented directly in the LFortran compiler.

**Key principle:** No separate transpiler. LFortran handles both:
1. **Direct compilation** → LLVM → executable
2. **Fortran output** → ASR → standard Fortran source (via built-in printer)

---

## Compiler Modes

### Mode Summary

| Mode | Flag | Base | Purpose |
|------|------|------|---------|
| **LFortran Standard** | `--std=lf` (default) | Fortran 2023 | Production code, stricter than ISO |
| **ISO Standard** | `--std=f23` | Fortran 2023 | Compatibility with gfortran/ifort |
| **Infer (Lazy Fortran)** | `--infer` | LFortran Standard | Interactive, prototyping |

### LFortran Standard (`--std=lf`) - Default

LFortran's default mode is **stricter than ISO Fortran** with sensible defaults for scientific computing:

| Feature | LFortran Standard | ISO Fortran 2023 |
|---------|-------------------|------------------|
| Default real | **8 bytes** (64-bit) | Implementation-dependent |
| Default integer | 4 bytes (32-bit) | Implementation-dependent |
| `dp` predefined | **YES** | No |
| Bounds checking | **ON** by default | OFF by default |
| Implicit typing | **OFF** (error) | Allowed |
| Implicit interface | **OFF** (error) | Allowed |
| Implicit argument casting | **OFF** (error) | Allowed |
| Array realloc on LHS | **OFF** (runtime error) | Compiler-dependent |
| Default intent | **`intent(in)`** | No default |

### ISO Standard Mode (`--std=f23`)

Matches ISO Fortran 2023 behavior for compatibility with other compilers:

- Implicit typing allowed (unless `implicit none` specified)
- Implicit interfaces allowed
- Implicit argument casting allowed
- Array reallocation on LHS enabled
- No default intent
- Default real/integer per implementation (typically 4 bytes)

### Infer Mode (`--infer`) - Lazy Fortran

Designed for **interactive use and rapid prototyping**. Extends LFortran Standard with:

1. **Type inference** - Variables get types from first assignment (intrinsic types only)
2. **Global scope** - Top-level statements, declarations, and expressions allowed
3. **Automatic array reallocation** - Enables `--realloc-lhs-arrays` automatically
4. **Predefined symbols** - `dp` for double precision (inherited from LFortran Standard)

```fortran
! Valid in --infer mode (file or REPL)
x = 5.0           ! real(8), inferred
y = [1, 2, 3]     ! integer array, inferred
print *, x + sum(y)
```

**Inherited from LFortran Standard:**
- Default real = 8 bytes, default integer = 4 bytes
- Default `intent(in)` for arguments
- `dp` predefined for double precision
- Bounds checking ON

**Infer mode additions:**
- Type inference at global scope
- Global scope (bare statements without `program`/`end program`)
- Automatic array reallocation on shape mismatch

**Important restrictions:**
- Type inference only at global scope level
- Inside `program`, `module`, `function`, `subroutine`: full declarations required
- Derived types always require explicit declaration

### Build Modes (Orthogonal)

Each compiler mode can be combined with build modes:

| Build Mode | Bounds Checking | Optimization | Use Case |
|------------|-----------------|--------------|----------|
| **Debug** (default) | ON | OFF | Development, testing |
| **ReleaseFast** (`--fast`) | OFF | ON | Maximum performance |
| **ReleaseSafe** | ON | ON | Production with safety |

---

## Feature Comparison

| Feature | LFortran (`--std=lf`) | ISO (`--std=f23`) | Infer (`--infer`) |
|---------|----------------------|-------------------|-------------------|
| Default real | **8 bytes** | 4 bytes | **8 bytes** |
| Default integer | 4 bytes | 4 bytes | 4 bytes |
| `dp` predefined | **YES** | No | **YES** |
| Bounds checking | **ON** | ON | **ON** |
| Default intent | **`intent(in)`** | None | **`intent(in)`** |
| Implicit typing | OFF (error) | Allowed | **Type inference** |
| Array realloc | OFF (error) | ON | **ON** |
| Global scope | No | No | **YES** |

---

## Standardizer (ASR to Fortran)

LFortran includes a **Fortran printer** that converts ASR back to standard Fortran source. This enables:

1. **Transpilation** - Write in infer mode, output ISO Fortran for other compilers
2. **Code inspection** - See generated declarations and transformations
3. **Portability** - Share code with users of gfortran/ifort/ifx

### Automatic Program Unit Wrapping

When outputting standard Fortran from infer mode, the printer wraps bare statements:

| Input Structure | Generated Output |
|-----------------|------------------|
| Bare statements only | `program main ... end program` |
| Bare statements + procedures | `program main ... contains ... end program` |
| Only procedures (no executable) | `module <filename> ... contains ... end module` |
| Already valid program units | Preserved as-is |

### Example Transformation

**Input (infer mode):**
```fortran
x = 5.0
y = x * 2

subroutine helper(a)
    real(8), intent(inout) :: a
    a = a + 1
end subroutine
```

**Output (standard Fortran via `--show-fortran`):**
```fortran
program main
    implicit none
    real(8) :: x
    real(8) :: y
    x = 5.0_8
    y = x * 2
contains
    subroutine helper(a)
        real(8), intent(inout) :: a
        a = a + 1
    end subroutine
end program main
```

---

## Default Precision

**LFortran Standard and Infer Mode use:**
- **Reals:** 8 bytes (64-bit) - precision for scientific computing
- **Integers:** 4 bytes (32-bit) - like Rust/C/Java, better performance

```fortran
x = 1.0              ! real(8)
n = 42               ! integer (4 bytes)
real :: value        ! real(8) in LFortran Standard
integer(8) :: big    ! explicit 64-bit when needed
```

### Rationale

**Reals at 8 bytes:**
- Scientific computing requires precision
- Prevents accumulation of rounding errors
- Matches Python/Julia/NumPy defaults

**Integers at 4 bytes (like Rust):**
- Most integers don't need 64-bit range
- Better cache utilization for integer arrays
- Twice as many per SIMD register

---

## Unsigned Integers

Lazy Fortran adds an `unsigned` attribute for integers with Rust-like safety:

```fortran
integer, unsigned :: count          ! 4-byte unsigned
integer(8), unsigned :: big_count   ! 8-byte unsigned
```

### Semantics

**No implicit mixing of signed and unsigned:**
```fortran
integer :: i = 5
integer, unsigned :: u = 10
! u + i                     ! ERROR: mixed signed/unsigned
u + uint(i)                 ! OK: explicit conversion
i + int(u)                  ! OK: explicit conversion
```

**Overflow behavior (Rust-like):**
- Default: **NO wraparound** - debug-time error on overflow/underflow
- With `--fast` mode: undefined behavior (optimizations assume no overflow)
- Explicit modular arithmetic via intrinsics when wraparound is intended

```fortran
integer, unsigned :: u = 0
u = u - 1                   ! ERROR: unsigned underflow (debug mode)
u = wrap_sub(u, 1)          ! OK: explicit wraparound
```

---

## Default Intent

LFortran Standard uses `intent(in)` as the default for all procedure arguments:

```fortran
subroutine process(x, y, z)
    integer, intent(in) :: x       ! Explicit (same as default)
    integer, intent(inout) :: y    ! Override required to modify
    integer, intent(out) :: z      ! Override required for output
    y = x + 1
    z = y * 2
end subroutine
```

**Principle of least privilege:** arguments are read-only unless explicitly declared otherwise.

---

## Implementation Status (LFortran)

| Feature | Status | Notes |
|---------|--------|-------|
| Global scope / interactive | ✅ Implemented | `FortranEvaluator` class |
| Type inference (global scope) | ✅ Implemented | In `ast_to_asr.cpp` |
| `--realloc-lhs-arrays` | ✅ Implemented | Flag exists |
| Bounds checking (default ON) | ✅ Implemented | Default in `--std=lf` |
| ASR to Fortran printer | ✅ Implemented | `asr_to_fortran.cpp` |
| Unsigned integers (ASR) | ✅ Partial | ASR support, syntax pending |
| **`--infer` flag** | ❌ Needed | Composite flag |
| **Default real = 8** | ❌ Needed | Currently hardcoded to 4 |
| **`dp` predefined** | ❌ Needed | Symbol table initialization |
| **Default `intent(in)`** | ❌ Needed | Semantic analysis |
| **Dot notation `a.b`** | ❌ Needed | Parser extension |

---

## Resolved Design Decisions

| Decision | Resolution |
|----------|------------|
| Implementation target | **LFortran only** (no separate transpiler) |
| Terminology | **Infer mode** (LFortran), **Lazy Fortran** (working title) |
| Whole-program analysis | **NOT supported** - all features single-pass |
| Generics | Strongly typed traits, explicit instantiation |
| Generic syntax | Curly braces `{Constraint :: T}` |
| Dispatch | **Both** - `type(T)` static, `class(Trait)` dynamic |
| @ annotations | **No** - use curly braces syntax |
| Type inference | Infer mode only (intrinsic types at global scope) |
| Default real | 8 bytes (64-bit) |
| Default integer | 4 bytes (32-bit, like Rust) |
| Unsigned integers | `integer, unsigned` attribute, Rust-like safety |
| Unsigned overflow | **NO wraparound** by default (debug error) |
| Default intent | `intent(in)` |
| Implicit typing | `implicit none` default (LFortran Standard) |
| Member access | Both `a.b` and `a%b` (dot notation pending) |
| Declaration placement | Anywhere in scope (infer mode) |
| Fallback for unclear type | Compile error |
| Arrays (infer mode) | ARE reallocated on assignment |
| Arrays (LFortran Standard) | NOT reallocated (runtime error) |

---

## References

- [LFortran Compiler](https://lfortran.org) - Implementation
- [LFortran GitHub](https://github.com/lfortran/lfortran)
- ISO/IEC 1539-1:2023 (Fortran 2023)
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
