# Lazy Fortran 2025

**Status:** Draft
**Base standard:** Fortran 2023 (ISO/IEC 1539-1:2023)

---

## Summary

Lazy Fortran 2025 extends Fortran 2023 with strongly typed generics/traits, sensible defaults, and modern syntax. The goal is reduced boilerplate while maintaining compatibility with standard Fortran compilers through a source-to-source standardizer.

All standard Fortran 2023 programs remain valid Lazy Fortran programs.

### Design Principles

1. **Strongly typed generics** - Traits with explicit type bounds. No weak generics or whole-program type inference. Procedural templates from standard Fortran are included when available.
2. **No whole-program analysis** - All features are single-pass or module-local. No C++ template hell or Julia-style recompilation.
3. **Infer mode** - Optional mode for interactive use and rapid prototyping that infers intrinsic types (int, real, string, etc.) from first assignment. Arrays ARE automatically reallocated on assignment (via `--realloc-lhs-arrays` flag). In strict mode, shape mismatch triggers a runtime error.
4. **Default precision** - Reals default to 8 bytes (64-bit) for scientific precision. Integers default to 4 bytes (32-bit) like Rust/C/Java for performance.
5. **intent(in) default** - Arguments are read-only unless explicitly declared otherwise.
6. **implicit none default** - All program units have `implicit none` by default.
7. **Dot notation** - Member access supports `a.b` in addition to standard `a%b`, with spacing rules to disambiguate user-defined operators.
8. **Unsigned integers** - `integer, unsigned` attribute for unsigned integers with safe Rust-like semantics.

### Feature Classification

All features are **single-pass (local or module-local analysis only)**:

- Dot notation transformation (`a.b` -> `a%b`)
- Default `intent(in)` for arguments
- Default `implicit none` injection
- Expression type rules (real=8 bytes, integer=4 bytes)
- Declaration placement
- Explicit template/trait instantiation
- Infer mode: type inference from first assignment (intrinsic types only)

**Explicitly NOT supported (would require whole-program analysis):**
- Implicit monomorphization from call sites
- Function result type inference from body
- Automatic specialization across compilation units

---

## Dot Notation for Member Access

**RESOLVED:** Lazy Fortran supports both `.` (dot) and `%` for derived type member access:

```fortran
! Both syntaxes are valid
particle.x = 1.0           ! Dot notation (modern style)
particle%x = 1.0           ! Standard Fortran (also supported)

particle.velocity.vx = 2.0
particle%velocity%vx = 2.0  ! Can even mix (though not recommended)

call particle.move(dt)
call particle%move(dt)
```

The standardizer converts dot notation to `%` for standard Fortran output.

### Disambiguation from User-Defined Operators

User-defined operators like `.add.` could conflict with member access. Lazy Fortran uses spacing rules:

```fortran
a .op. b     ! User-defined operator (spaces required)
a.member     ! Member access (no spaces)
```

The standardizer enforces this by requiring spaces around user-defined operators. Violations are compile-time errors.

### Rationale

The dot notation is familiar from most modern languages and improves readability. Supporting both syntaxes ensures full backward compatibility with standard Fortran code while allowing modern style for new code.

---

## Type Inference (Infer Mode)

Type inference is available in **infer mode**, designed for interactive use and rapid prototyping. In standard mode, all variables require explicit declarations.

### First Assignment Rule (Infer Mode Only)

In infer mode, variables get their type from the first value assigned:

```fortran
x = 42             ! integer (default integer, 4 bytes)
y = 3.14           ! real(8)
z = (1.0, 2.0)     ! complex(8)
flag = .true.      ! logical
s = "hello"        ! character(:), allocatable
p = particle_t()   ! ERROR: derived types require explicit declaration
```

**Key restrictions:**
- Only intrinsic types (integer, real, complex, logical, character) are inferred
- Derived types always require explicit declaration
- Inferred strings are deferred-length allocatable
- Inferred arrays ARE automatically reallocated on assignment (infer mode sets `--realloc-lhs-arrays`)
- Derived type instances ARE reallocated on assignment

Subsequent assignments use standard Fortran coercion rules.

### Allocation Semantics

**Mode-dependent array reallocation:**

| Mode | Array Reallocation | Behavior on Shape Mismatch |
|------|-------------------|---------------------------|
| **Infer** | Automatic (`--realloc-lhs-arrays`) | Silently reallocates |
| **Strict** | Disabled | Runtime error |
| **Standard** | Standard Fortran behavior | Depends on allocatable status |

**Infer mode** (interactive/prototyping):

```fortran
arr = [1, 2, 3]    ! integer, allocatable, size 3
arr = [4, 5, 6]    ! OK: same shape
arr = [7, 8]       ! OK: automatically reallocates to size 2
```

**Strict mode** (production):

```fortran
arr = [1, 2, 3]    ! integer, allocatable, size 3
arr = [4, 5, 6]    ! OK: same shape
arr = [7, 8]       ! ERROR: shape mismatch (runtime error)
```

The strict mode prevents accidental reallocation and the associated performance/memory issues. To resize an array, use explicit `deallocate`/`allocate` or `reshape`.

**Derived type instances ARE reallocated on assignment:**

```fortran
type(particle_t), allocatable :: p
p = particle_t(x=1.0, y=2.0)    ! allocates
p = other_particle              ! reallocates if needed
```

**Strings are deferred-length allocatable:**

```fortran
s = "hello"        ! character(:), allocatable, len=5
s = "goodbye"      ! reallocates to len=7 (standard Fortran behavior)
```

### Expression Type Rules

Expression types follow ISO/IEC 1539-1:2023 Clause 10.1.5 (Numeric intrinsic operations). Mixed numeric array constructors use type promotion: `[1, 2.0, 3]` promotes all elements to real(8).

### Interaction with implicit none

`implicit none` is the default in Lazy Fortran. In standard mode, undeclared names are errors. In infer mode, undeclared intrinsic-typed variables are inferred; derived types still require explicit declaration.

### Resolved Issues (Infer Mode)

| Issue | Decision |
|-------|----------|
| 3 | Declaration placement: **Anywhere in scope** |
| 15 | Fallback when type unclear: **Compile error** |

---

## Default Precision

**RESOLVED:** Lazy Fortran uses different defaults for reals and integers:

- **Reals:** 8 bytes (64-bit) - precision matters for scientific computing
- **Integers:** 4 bytes (32-bit) - like Rust/C/Java, better performance

This applies to:

1. **Literal values** - `1.0`, `1.0e3` are real(8); `42` is default integer
2. **Type inference** - Reals inferred as 8-byte, integers as 4-byte
3. **Unqualified declarations** - `real :: x` means `real(8) :: x`; `integer :: n` stays default

```fortran
! Lazy Fortran
x = 1.0              ! real(8)
y = 1.0e3            ! real(8)
n = 42               ! integer (4 bytes)
integer :: count     ! integer (4 bytes)
real :: value        ! real(8)

! Explicit larger/smaller types supported
integer(8) :: big_count    ! 64-bit integer when needed
real(4) :: single_precision
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
- Use `integer(8)` explicitly when needed (large array indexing, big counts)

### Standardizer Behavior

The standardizer generates explicit kind specifiers for reals:

```fortran
! Input (Lazy Fortran)
x = 1.0
n = 42

! Output (Standard Fortran)
real(8) :: x
integer :: n
x = 1.0_8
n = 42
```

---

## Unsigned Integers

**NEW:** Lazy Fortran adds an `unsigned` attribute for integers:

```fortran
integer, unsigned :: count          ! 4-byte unsigned (0 to 4,294,967,295)
integer(8), unsigned :: big_count   ! 8-byte unsigned
```

### Semantics (Rust-like Safety)

**No implicit mixing of signed and unsigned:**

```fortran
integer :: i = 5
integer, unsigned :: u = 10

! u + i                     ! ERROR: mixed signed/unsigned
u + uint(i)                 ! OK: explicit conversion to unsigned
i + int(u)                  ! OK: explicit conversion to signed
```

**Overflow behavior (Rust-like):**

- Default: **NO wraparound** - debug-time error on overflow/underflow
- With `--fast` mode: undefined behavior (optimizations assume no overflow)
- Explicit modular arithmetic via intrinsics when wraparound is intended

```fortran
integer, unsigned :: u = 0
u = u - 1                   ! ERROR: unsigned underflow (debug mode)
u = wrap_sub(u, 1)          ! OK: explicit wraparound to 4,294,967,295
```

**Modular arithmetic intrinsics** (when wraparound is intentional):

```fortran
wrap_add(a, b)              ! a + b with wraparound
wrap_sub(a, b)              ! a - b with wraparound
wrap_mul(a, b)              ! a * b with wraparound
```

### Use Cases

**Array indexing** - the primary use case:

```fortran
integer, unsigned :: idx
do idx = 0, n-1             ! Zero-based indexing natural with unsigned
    arr(idx+1) = ...
end do
```

**Bit manipulation:**

```fortran
integer, unsigned :: flags = 0
flags = ior(flags, bit_mask)
```

**Interoperability with C:**

```fortran
integer(c_size_t), unsigned :: size    ! size_t equivalent
```

### Standardizer Behavior

The standardizer emits unsigned operations using appropriate intrinsics or compiler extensions where available, or emulates with range checks where necessary.

---

## Default Intent

Lazy Fortran uses `intent(in)` as the default for all procedure arguments. This is stricter than standard Fortran, which has no default intent (arguments without explicit intent can be read and modified).

To modify an argument, explicitly specify `intent(inout)` or `intent(out)`:

```fortran
subroutine process(x, y, z)
    integer, intent(in) :: x       ! Explicit (same as default)
    integer, intent(inout) :: y    ! Explicit override - can read and modify
    integer, intent(out) :: z      ! Explicit override - output only
    y = x + 1
    z = y * 2
end subroutine
```

Without explicit intent, `y` and `z` would default to `intent(in)` and the assignments would be compile-time errors.

### Rationale

Standard Fortran allows unrestricted modification of arguments without explicit intent, which can lead to subtle bugs. The `intent(in)` default follows the principle of least privilege: arguments are read-only unless explicitly declared otherwise. This aligns with modern language design (e.g., Rust immutable-by-default).

### Standardizer Behavior

The standardizer generates explicit `intent(in)` declarations for all arguments that lack explicit intent:

```fortran
! Input: script.lf
subroutine scale(x, factor)
    x = x * factor   ! ERROR: x has intent(in) by default
end subroutine

! Must be written as:
subroutine scale(x, factor)
    real, intent(inout) :: x    ! Explicit override required
    x = x * factor
end subroutine
```

---

## Generic Programming

**RESOLVED:** Lazy Fortran uses **strongly typed generics/traits** with explicit instantiation. No implicit monomorphization or whole-program type inference.

Two complementary approaches are available:

1. **J3 Procedural Templates** - When available in standard Fortran, will be supported directly with explicit instantiation
2. **Traits (Swift/Rust style)** - Strongly typed constraints with explicit type bounds

### Key Design Decision: No Implicit Monomorphization

Unlike C++ templates or Julia, Lazy Fortran does NOT automatically generate specializations from call sites. All generic instantiation is explicit:

```fortran
! WRONG (would require whole-program analysis):
function add(a, b)      ! What types? Unknown until call site
    add = a + b
end function
x = add(5, 3)           ! Would need to infer types and generate code

! CORRECT (explicit instantiation):
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
instantiate add_t(real(8)), only: add_real => add

x = add_int(5, 3)       ! Explicit, no whole-program analysis needed
```

### Templates (J3 Direction)

The TEMPLATE construct defines parameterized procedures with explicit instantiation:

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
instantiate swap_t(real), only: swap_real => swap

! Inline instantiation (still explicit)
call swap{integer}(a, b)
```

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

Traits define explicit type constraints. All type parameters must be bounded:

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
! T must satisfy IComparable - explicit instantiation required
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

### Combining Approaches

| Use Case | Recommended |
|----------|-------------|
| Generic containers | J3 TEMPLATE with explicit instantiation |
| Numeric algorithms | Traits with INumeric bounds |
| Retroactive conformance | Traits IMPLEMENTS |
| Runtime polymorphism | Traits with class(ITrait) |

### Resolved Issues

| Issue | Decision |
|-------|----------|
| 7 | Dispatch mechanism: **Both** - `type(T)` for static, `class(Trait)` for dynamic |
| 16 | @ annotations: **No** - use curly braces `{Constraint :: T}` syntax |

### Syntax Alignment

**J3 TEMPLATE** (official proposal for Fortran 202Y):
- Uses `^()` for inline instantiation: `CALL sub^(INTEGER)(x)`
- Standard parentheses for template parameters: `TEMPLATE foo(T)`
- Explicit `INSTANTIATE` statements

**Traits** ([Traits-for-Fortran proposal](https://github.com/difference-scheme/Traits-for-Fortran)):
- Uses `abstract interface` for trait definitions
- Uses `implements` blocks for trait conformance
- Uses **curly braces `{T}`** for generic type parameters: `function sum{INumeric :: T}(x)`
- Type constraints precede parameter name: `{ITrait :: T}` or inline `{integer | real :: T}`

Lazy Fortran adopts the traits syntax with curly braces for generic parameters.

---

## Standardizer

The standardizer transforms Lazy Fortran (.lf) to standard Fortran (.f90).

### Transformations

1. **Dot notation** - `a.b` becomes `a%b`
2. **Program wrapping** - Bare statements become `program main`; files with only procedures become `module <filename>`
3. **Declaration generation** - Inferred types become explicit declarations (infer mode)
4. **Kind promotion** - Unqualified types get 8-byte kind specifiers
5. **Literal promotion** - Numeric literals get 8-byte kind suffixes
6. **implicit none injection** - Added to all program units
7. **Intent generation** - Default `intent(in)` becomes explicit
8. **Template expansion** - Explicit template instantiations become concrete procedures

### Simple Example (Standard Mode)

Input (script.lf):
```fortran
type(particle_t) :: p
p.x = 5.0
p.y = 3.0
print *, p.x + p.y
```

Output (script.f90):
```fortran
program main
    implicit none
    type(particle_t) :: p
    p%x = 5.0_8
    p%y = 3.0_8
    print *, p%x + p%y
end program main
```

### Simple Example (Infer Mode)

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
    implicit none
    integer :: x
    real(8) :: y
    character(:), allocatable :: s
    x = 5
    y = 3.14_8
    s = "hello"
    print *, x, y, s
end program main
```

### Template Instantiation Example

Input (script.lf):
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
instantiate add_t(real(8)), only: add_real => add

x = add_int(5, 3)
y = add_real(2.5, 1.5)
```

Output (script.f90):
```fortran
module script_templates
    implicit none
contains
    integer function add_int(a, b)
        integer, intent(in) :: a, b
        add_int = a + b
    end function

    real(8) function add_real(a, b)
        real(8), intent(in) :: a, b
        add_real = a + b
    end function
end module script_templates

program main
    use script_templates
    implicit none
    integer :: x
    real(8) :: y
    x = add_int(5, 3)
    y = add_real(2.5_8, 1.5_8)
end program main
```

---

## Compiler Modes

Lazy Fortran 2025 extends the [LFortran compiler](https://lfortran.org). LFortran provides three compilation modes:

### Mode Summary

| Mode | Flag | Purpose | Key Characteristics |
|------|------|---------|---------------------|
| **Strict** | `--std=lf` (default) | Production code | Bounds checking ON, implicit typing OFF, no array realloc |
| **Standard** | `--std=f23` | ISO Fortran 2023 compatibility | Matches standard compiler behavior |
| **Infer** | `--infer` | Interactive/prototyping | Type inference, auto array realloc, global scope |

The **interactive mode** (REPL) implicitly uses `--infer` mode. The `--infer` flag allows using infer mode when compiling files, not just interactively.

### Strict Mode (Default)

LFortran's default mode is **stricter than standard Fortran** with sensible defaults:

| Feature | Strict Mode | Standard Fortran |
|---------|-------------|------------------|
| Default real | **8 bytes** (64-bit) | Implementation-dependent (often 4) |
| Default integer | 4 bytes (32-bit) | Implementation-dependent |
| Bounds checking | **ON** by default | OFF by default |
| Implicit typing | **OFF** (error) | OFF but allowed |
| Implicit interface | **OFF** (error) | Allowed |
| Implicit argument casting | **OFF** (error) | Allowed |
| Array realloc on LHS | **OFF** (runtime error) | Compiler-dependent |
| Default intent | `intent(in)` | No default |
| `dp` predefined | **YES** | No |

This mode catches bugs at compile/run time that would silently pass in standard Fortran, while providing sensible defaults for scientific computing.

### Standard Mode (`--std=f23`)

Matches ISO Fortran 2023 behavior for compatibility with other compilers:

- Implicit typing allowed (unless `implicit none` specified)
- Implicit interfaces allowed
- Implicit argument casting allowed
- Array reallocation on LHS enabled
- No default intent (arguments can be modified)

Use this mode when you need code to work identically across LFortran, gfortran, ifort, etc.

### Infer Mode (`--infer`)

Designed for **interactive use and rapid prototyping**. The `--infer` flag enables this mode for file compilation; interactive mode (REPL) uses it implicitly.

**Infer mode features:**
1. **Type inference** - Variables get types from first assignment (intrinsic types only)
2. **Global scope** - Top-level statements, declarations, and expressions allowed
3. **Automatic array reallocation** - Sets `--realloc-lhs-arrays` automatically
4. **Predefined symbols** - `dp` for double precision available by default

```fortran
! Valid in infer mode (file or REPL)
x = 5.0           ! real(8), inferred
y = [1, 2, 3]     ! integer array, inferred
print *, x + sum(y)
```

**Inherited from strict mode:**
- Default real = 8 bytes, default integer = 4 bytes
- Default `intent(in)` for arguments
- `dp` predefined for double precision

**Additional Lazy Fortran 2025 features (all modes):**
- Dot notation `a.b` for member access

**Important restrictions:**
- Type inference only at global scope level
- Inside `program`, `module`, `function`, `subroutine`: standard declaration rules apply
- Derived types always require explicit declaration

### Standardizer (Transpiler)

In `--infer` mode, a **standardizer** transforms Lazy Fortran code into valid standard Fortran. This works for both interactive (REPL) and file compilation.

**Automatic program unit wrapping:**

The standardizer analyzes the code structure and infers the appropriate wrapper:

| Input Structure | Generated Output |
|-----------------|------------------|
| Bare statements only | `program main ... end program` |
| Bare statements + procedures | `program main ... contains ... end program` |
| Only procedures (no executable statements) | `module <filename> ... contains ... end module` |
| Already valid program units | Preserved as-is |

**Example transformation:**

```fortran
! Input (infer mode)
x = 5.0
y = x * 2

subroutine helper(a)
    real(8), intent(inout) :: a
    a = a + 1
end subroutine
```

```fortran
! Output (standard Fortran)
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

The standardizer also:
- Generates explicit type declarations from inferred types
- Adds `implicit none` to all program units
- Converts dot notation (`a.b`) to standard syntax (`a%b`)
- Adds kind specifiers to literals (`1.0` → `1.0_8`)

### Build Modes (Orthogonal)

Each compiler mode can be combined with build modes:

| Build Mode | Bounds Checking | Optimization | Use Case |
|------------|-----------------|--------------|----------|
| **Debug** (default) | ON | OFF | Development, testing |
| **ReleaseFast** (`--fast`) | OFF | ON | Maximum performance |
| **ReleaseSafe** | ON | ON | Production with safety |

**ReleaseSafe** is the Rust-equivalent fast mode - optimized code that still catches runtime errors. LFortran aims to never segfault with bounds checking enabled.

---

## LFortran Implementation

Lazy Fortran 2025 features are implemented in [LFortran](https://github.com/lfortran/lfortran), a modern LLVM-based Fortran compiler.

### Current Implementation Status

| Feature | LFortran Status | Notes |
|---------|-----------------|-------|
| Bounds checking (default ON) | Implemented | `--array-bounds-checking` |
| Global scope / infer mode | Implemented | Type inference at top level |
| Unsigned integers | ASR support | Syntax not yet exposed |
| Default real = 8 bytes | Not yet | Currently follows standard (4 bytes) |
| Default integer = 4 bytes | Implemented | Standard behavior |
| Default `intent(in)` | Not yet | Planned for strict mode |
| Dot notation | Not yet | Planned |
| Templates/Traits | Partial | J3 template syntax in progress |

### LFortran Design Choices vs Standard Fortran

LFortran already makes several choices that differ from gfortran/ifort defaults:

| Feature | LFortran Default | gfortran/ifort Default |
|---------|-----------------|------------------------|
| Bounds checking | **ON** | OFF |
| Carriage control | **OFF** (modern) | ON (legacy) |
| Implicit typing | OFF | OFF |
| Implicit interface | OFF | Allowed |
| Implicit argument casting | OFF | Allowed |

### Architecture

LFortran uses a multi-stage pipeline:

1. **Parsing** - Source → AST (Abstract Syntax Tree)
2. **Semantic Analysis** - AST → ASR (Abstract Semantic Representation)
3. **ASR Passes** - High-level optimizations
4. **Code Generation** - ASR → LLVM IR
5. **Machine Code** - LLVM → executable/JIT

The ASR is the key innovation: it represents **only valid Fortran code**. Invalid code is rejected at semantic analysis, making downstream stages simpler and more reliable.

### Interactive/Jupyter Support

LFortran can execute code interactively:

```bash
$ lfortran
Interactive Fortran.
>>> x = 5.0
>>> print *, x * 2
   10.0000000000000
```

The global scope extension allows statements, declarations, and expressions at the top level, with type inference from first assignment.

---

## Resolved Design Decisions

| Decision | Resolution |
|----------|------------|
| Whole-program analysis | **NOT supported** - all features single-pass |
| Generics | Strongly typed traits, explicit instantiation |
| Generic syntax | Curly braces `{Constraint :: T}` |
| Dispatch | **Both** - `type(T)` static, `class(Trait)` dynamic |
| @ annotations | **No** - use curly braces syntax |
| Type inference | Infer mode only (intrinsic types) |
| Default real | 8 bytes (64-bit) |
| Default integer | 4 bytes (32-bit, like Rust) |
| Unsigned integers | `integer, unsigned` attribute, Rust-like safety |
| Unsigned overflow | **NO wraparound** by default (debug error) |
| Default intent | `intent(in)` |
| Implicit typing | `implicit none` default |
| Member access | Both `a.b` and `a%b` supported |
| Declaration placement | Anywhere in scope |
| Fallback for unclear type | Compile error |
| Arrays (infer mode) | ARE reallocated on assignment |
| Arrays (strict mode) | NOT reallocated (runtime error on mismatch) |
| Types (all modes) | ARE reallocated on assignment |

---

## References

- ISO/IEC 1539-1:2023 (Fortran 2023)
- [LFortran Compiler](https://lfortran.org) - Implementation target
- [LFortran Design Document](https://docs.lfortran.org/design/) - Architecture details
- [J3 Generics Repository](https://github.com/j3-fortran/generics)
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/) - TEMPLATE/INSTANTIATE syntax
