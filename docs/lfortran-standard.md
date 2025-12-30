# LFortran Standard

**Status:** Draft
**Derived from:** ISO Fortran 2023 (ISO/IEC 1539-1:2023)
**Compiler flag:** `--std=lf` (default in LFortran)

---

## Overview

The LFortran Standard defines a stricter dialect of Fortran 2023 with sensible defaults for scientific computing. It is the default mode of the [LFortran compiler](https://lfortran.org).

LFortran Standard enforces additional compile-time and run-time checks that catch common errors. Code written to LFortran Standard compiles with any ISO Fortran 2023 compiler, but not all ISO Fortran 2023 code compiles under LFortran Standard (see [Compatibility](#compatibility)).

---

## Feature Comparison

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

---

## Default Precision

### Real Numbers (8 bytes / 64-bit)

Unqualified `real` declarations and real literals default to 8 bytes (64-bit, double precision):

```fortran
real :: x           ! real(8) - 64-bit
x = 1.0             ! real(8) literal
y = 1.0e3           ! real(8) literal
```

**Rationale:** Scientific computing requires precision. 8-byte default prevents accumulation of rounding errors and matches Python/Julia/NumPy defaults.

### Integer Numbers (4 bytes / 32-bit)

Integers default to 4 bytes (32-bit), matching Rust/C/Java:

```fortran
integer :: n        ! integer (4 bytes)
n = 42              ! integer literal
```

**Rationale:** Most integers do not need 64-bit range. 4-byte integers provide better cache utilization and twice as many values per SIMD register. Use `integer(8)` explicitly when needed for large array indexing or big counts.

### Predefined `dp` Symbol

The symbol `dp` is predefined as `real64` from `iso_fortran_env`:

```fortran
real(dp) :: x       ! Equivalent to real(8)
x = 1.0_dp          ! Explicit kind suffix
```

**Implementation:** The `dp` symbol is implicitly defined as if every program unit began with:

```fortran
use, intrinsic :: iso_fortran_env, only: dp => real64
```

This implicit import:
- **Can be overridden** by explicit `use iso_fortran_env` statements in the same scope
- **Is NOT a reserved keyword** - user code can redefine `dp` with a local declaration
- **Follows normal Fortran scoping rules** - inner scopes can shadow outer definitions

---

## Strictness Checks

### Bounds Checking (ON by default)

Array bounds are checked at runtime in debug and release-safe modes:

```fortran
integer :: arr(10)
arr(11) = 0         ! Runtime error: index out of bounds
```

This is disabled only in `--fast` mode for maximum performance.

### Implicit Typing (OFF / error)

Undeclared variables are compile-time errors:

```fortran
x = 1.0             ! ERROR: x not declared
```

Equivalent to having `implicit none` in every program unit.

### Implicit Interface (OFF / error)

External procedures must have explicit interfaces:

```fortran
call external_sub(x)    ! ERROR: no explicit interface
```

All procedures must be in modules, `contains` blocks, or have interface blocks.

### Implicit Argument Casting (OFF / error)

Type mismatches between actual and dummy arguments are errors:

```fortran
subroutine process(x)
    real(8), intent(in) :: x
end subroutine

call process(1.0)       ! ERROR if 1.0 is real(4)
```

### Array Reallocation on LHS (OFF / runtime error)

Assigning to an allocatable array with mismatched shape raises a runtime error:

```fortran
real, allocatable :: arr(:)
arr = [1.0, 2.0, 3.0]   ! OK: allocates
arr = [4.0, 5.0]        ! ERROR: shape mismatch
```

To resize, use explicit `deallocate`/`allocate`:

```fortran
deallocate(arr)
allocate(arr(2))
arr = [4.0, 5.0]        ! OK
```

---

## Default Intent

Arguments without explicit intent default to `intent(in)`:

```fortran
subroutine process(x, y)
    integer :: x        ! intent(in) by default
    integer, intent(inout) :: y  ! explicit override
    ! x = 1             ! ERROR: intent(in)
    y = x + 1           ! OK
end subroutine
```

**Rationale:** Principle of least privilege. Arguments are read-only unless explicitly declared otherwise, like Rust's immutable-by-default approach.

---

## Build Modes

LFortran Standard can be combined with build modes:

| Build Mode | Bounds Checking | Optimization | Use Case |
|------------|-----------------|--------------|----------|
| **Debug** (default) | ON | OFF | Development, testing |
| **ReleaseFast** (`--fast`) | OFF | ON | Maximum performance |
| **ReleaseSafe** | ON | ON | Production with safety |

**ReleaseSafe** is the Rust-equivalent fast mode - optimized code that still catches runtime errors.

---

## Compatibility

### With ISO Fortran 2023

LFortran Standard has **one-way compatibility** with ISO Fortran 2023:

**LFortran Standard → ISO Fortran 2023 (YES):**
- Code written to LFortran Standard compiles with any Fortran 2023 compiler
- All LFortran Standard restrictions can be satisfied by valid Fortran 2023 code
- The standardizer emits explicit declarations and kind specifiers

**ISO Fortran 2023 → LFortran Standard (NOT ALWAYS):**
- ISO code using implicit typing will fail (LFortran requires explicit declarations)
- ISO code using implicit interfaces will fail (LFortran requires explicit interfaces)
- ISO code relying on implicit argument casting will fail
- ISO code relying on LHS array reallocation may fail at runtime

LFortran Standard is a **strict subset** of valid Fortran 2023 - it accepts only code that follows best practices.

### With Other Compilers

Use `--std=f23` flag to compile in ISO Fortran 2023 mode for compatibility with gfortran, ifort, etc.

---

## Related Standards

- **[Lazy Fortran Standard](lazy-fortran-standard.md)** - Extends LFortran Standard with type inference and modern features
- **ISO/IEC 1539-1:2023** - Base standard

---

## References

- [LFortran Compiler](https://lfortran.org)
- [ISO/IEC 1539-1:2023](https://www.iso.org/standard/82170.html) - Fortran 2023
