# Lazy Fortran 2025

**DRAFT - Working Document**

> **Document type:** Working Draft
> **Base standard:** ISO/IEC 1539-1:2023 (Fortran 2023)
> **Status:** This document is a draft specification. All provisions are subject to change.

---

## Foreword

This document specifies Lazy Fortran 2025, a superset extension to the Fortran programming language. It is designed to be read in conjunction with ISO/IEC 1539-1:2023 (Fortran 2023), which serves as the normative base standard.

This document does not duplicate the text of ISO/IEC 1539-1:2023. Where this document is silent, the provisions of ISO/IEC 1539-1:2023 apply. All syntax and semantics defined in ISO/IEC 1539-1:2023 remain in effect unless explicitly modified by this document.

Readers should have access to ISO/IEC 1539-1:2023 for complete understanding of the base language features.

---

## Contents

[Foreword](#foreword)

1. [Scope](#1-scope)
2. [Normative references](#2-normative-references)
3. [Terms and definitions](#3-terms-and-definitions)
4. [Conformance](#4-conformance)
5. [Type system](#5-type-system)
6. [Generic programming](#6-generic-programming)
7. [Automatic specialization](#7-automatic-specialization)
8. [Application binary interface](#8-application-binary-interface)
9. [Standardizer](#9-standardizer)

Annex A. [References](#annex-a-references)

---

## 1 Scope

1.1 This document specifies extensions to the Fortran programming language as defined by ISO/IEC 1539-1:2023.

1.2 The extensions specified in this document constitute Lazy Fortran 2025, a strict superset of ISO/IEC 1539-1:2023.

1.3 This document adds the following facilities to the base language:
  - (a) automatic type inference (Section 5);
  - (b) generic programming constructs (Section 6);
  - (c) automatic specialization (monomorphization) (Section 7).

1.4 Lazy Fortran source files use the `.lf` extension and can be transformed to standard-conforming Fortran by a standardizer.

1.5 This document does not specify:
  - (a) the mechanism by which Lazy Fortran programs are transformed or executed;
  - (b) the method of transcription of Lazy Fortran programs for execution;
  - (c) the operations required for setup and control of the use of Lazy Fortran programs.

1.6 For all matters not explicitly addressed in this document, ISO/IEC 1539-1:2023 applies.

---

## 2 Normative references

2.1 The following documents are referred to in the text in such a way that some or all of their content constitutes requirements of this document. For dated references, only the edition cited applies. For undated references, the latest edition of the referenced document (including any amendments) applies.

2.2 ISO/IEC 1539-1:2023, *Information technology — Programming languages — Fortran — Part 1: Base language*

> **NOTE:** ISO/IEC 1539-1:2023 is the base standard for this document. All provisions of ISO/IEC 1539-1:2023 apply to Lazy Fortran 2025 unless explicitly modified by this document.

---

## 3 Terms and definitions

3.1 For the purposes of this document, the terms and definitions given in ISO/IEC 1539-1:2023 Clause 3 apply, together with the following.

3.2 ISO/IEC 1539-1:2023 Clause 3 contains terms and definitions that are applicable but not duplicated here.

3.3 **automatic type inference**
determination of variable types from their usage context without explicit declaration

3.4 **first assignment rule**
the rule that an undeclared variable's type is determined by its first assignment

3.5 **monomorphization**
generation of specialized code for each concrete type combination used with a generic procedure

3.6 **specialization**
a specific instantiation of a generic procedure for particular concrete types

3.7 **standardizer**
a tool that transforms Lazy Fortran source to standard-conforming Fortran

3.8 **trait**
a named collection of procedure signatures that types can implement

3.9 **type set**
a constraint specifying a set of types that a generic parameter may take

---

## 4 Conformance

4.1 **Fortran compatibility**

4.1.1 A conforming Lazy Fortran processor shall be capable of processing any standard-conforming Fortran 2023 program.

4.1.2 The output of a conforming standardizer shall be a standard-conforming Fortran program.

4.2 **ISO behavior changes**

4.2.1 The following features differ from standard Fortran behavior:

| Section | Feature | ISO Fortran Behavior | Lazy Fortran Behavior |
|---------|---------|---------------------|----------------------|
| 5.1 | Type inference | Explicit declarations or I-N naming | First assignment determines type |
| 5.3 | Default intent | No default (arguments modifiable) | See 5.3.3 |

---

## 5 Type system

### 5.1 Automatic type inference

5.1.1 **General**

5.1.1.1 Lazy Fortran 2025 uses automatic type inference to determine variable types from usage context.

5.1.1.2 This replaces the legacy implicit typing rules (I-N integer, otherwise real) with a first-assignment-wins rule.

> **NOTE 1:** This is an **ISO BEHAVIOR CHANGE**. Standard Fortran requires explicit type declarations or uses the I-N naming convention.

5.1.2 **First assignment rule**

5.1.2.1 The type of an undeclared variable is determined by its first assignment.

5.1.2.2 The kind of inferred numeric types follows the rules below.

> **OPEN ISSUE 1: Default numeric kinds**
>
> What kind should inferred numeric literals have?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | ISO defaults (`real(4)`, `integer(4)`) | Compatible, predictable, smaller memory | Precision loss, overflow at ~2B |
> | B | Double precision (`real(8)`, `integer(8)`) | Safer for scientific computing | Breaks ISO expectations, 2x memory |
> | C | Context-dependent | Adapts to usage | Unpredictable |

5.1.2.3 The following assignments establish types:

```fortran
x = 4              ! x is integer
y = 3.14           ! y is real
z = (1.0, 2.0)     ! z is complex
flag = .true.      ! flag is logical
s = "hello"        ! s is character(len=5)
obj = create_particle()  ! obj has return type of create_particle
```

5.1.2.4 Subsequent assignments to the same variable use standard Fortran coercion rules.

5.1.2.5 Logical literals (`.true.`, `.false.`) establish logical type.

5.1.2.6 Complex literals of the form `(real-part, imag-part)` establish complex type. The kind is determined by the component literals according to ISO/IEC 1539-1:2023.

5.1.2.7 String literals establish character type. The handling of length is subject to the following consideration.

> **OPEN ISSUE 10: Character length inference**
>
> How should character length be handled when multiple assignments have different lengths?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | First assignment wins | Consistent with other types | Truncation on longer strings |
> | B | Maximum length seen | No truncation | May waste memory |
> | C | Allocatable deferred-length | Fully dynamic | More complex, allocation overhead |

5.1.3 **Array bounds inference**

5.1.3.1 Allocatable arrays have bounds inferred from `allocate` statements or array constructors.

5.1.3.2 Examples:

```fortran
arr = [1, 2, 3]           ! rank-1, 3 elements
allocate(matrix(n, m))    ! rank-2, runtime bounds
```

5.1.4 **Interaction with implicit none**

5.1.4.1 When `implicit none` is present, undeclared names are errors and inference is disabled.

5.1.4.2 This preserves compatibility with strict coding styles.

5.1.5 **Inference from intent(out) arguments**

5.1.5.1 Whether undeclared variables passed to `intent(out)` arguments are automatically declared is subject to the following consideration.

> **OPEN ISSUE 2: Inference from intent(out) arguments**
>
> Should `call init(p)` automatically declare `p` based on `intent(out)` signature?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | No (assignment only) | Simple local analysis | Doesn't support idiomatic patterns |
> | B | Yes (inspect callee) | Supports `intent(out)` patterns | Requires cross-procedure analysis |

5.1.6 **Declaration placement**

5.1.6.1 Whether explicit declarations may appear anywhere in a block or only at the beginning is subject to the following consideration.

> **OPEN ISSUE 3: Declaration placement**
>
> Should explicit declarations be allowed anywhere or only at block beginning?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | Block beginning only | Clean structure | Variables far from first use |
> | B | Anywhere | Declare near use | Scattered, redundant with inference |

### 5.2 Expression type rules

5.2.1 Expression type determination follows ISO/IEC 1539-1:2023 Clause 10.1.5 (Numeric intrinsic operations).

5.2.2 When automatic type inference determines a variable's type from an expression, the type is determined according to the rules specified in ISO/IEC 1539-1:2023.

### 5.3 Intent inference

5.3.1 **General**

5.3.1.1 Procedure argument intents may be inferred from usage analysis.

> **NOTE 2:** This is an **ISO BEHAVIOR CHANGE**. Standard Fortran has no default intent - arguments without explicit intent can be read and modified.

5.3.2 **Usage-based inference**

5.3.2.1 Arguments that are only read within a procedure are inferred as `intent(in)`.

5.3.2.2 Arguments that are modified are inferred as `intent(inout)` or `intent(out)` based on whether the input value is used.

5.3.3 **Default intent**

5.3.3.1 The default intent when usage analysis is inconclusive is subject to the following consideration.

> **OPEN ISSUE 4: Default intent for procedure arguments**
>
> What should be the default intent when not explicitly specified?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | `intent(in)` default | Safe, prevents accidents | Breaks code relying on implicit inout |
> | B | `intent(inout)` default | Closer to ISO | Still error-prone |
> | C | No default (require explicit) | Forces clarity | Verbose |

---

## 6 Generic programming

### 6.1 Overview

6.1.1 Lazy Fortran 2025 supports two approaches to generic programming that may be used together.

6.1.2 Approach A (J3 TEMPLATE) follows the official Fortran 202Y direction.

6.1.3 Approach B (Traits) provides Swift/Rust-inspired ergonomics.

6.1.4 Which approach(es) to adopt is subject to the following consideration.

> **OPEN ISSUE 5: Generics approach**
>
> Which generics system to adopt?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | J3 TEMPLATE only | Official direction | Verbose |
> | B | Traits only | Concise, automatic | Not official |
> | C | Hybrid (both) | Maximum flexibility | Two systems |

6.1.5 The syntax for generic type parameters is subject to the following consideration.

> **OPEN ISSUE 6: Generic parameter syntax**
>
> What delimiter for generic type parameters?
>
> | Option | Syntax | Pros | Cons |
> |--------|--------|------|------|
> | A | `{T}` | Distinct, J3 inline | Not traditional |
> | B | `(T)` | Fortran-like | Ambiguous with calls |
> | C | `<T>` | Familiar to C++/Rust | Conflicts with operators |

### 6.2 TEMPLATE construct (J3 approach)

6.2.1 **Syntax**

6.2.1.1 A template defines a parameterized scope containing procedures:

```fortran
TEMPLATE template-name ( template-parameter-list )
   [ TYPE, DEFERRED :: type-parameter ]...
   [ REQUIRES requirement-name ( args ) ]...
CONTAINS
   procedure-definitions
END TEMPLATE [ template-name ]
```

6.2.2 **INSTANTIATE statement**

6.2.2.1 Templates are instantiated explicitly:

```fortran
INSTANTIATE template-name ( type-arguments ) [ , rename-list ]
```

6.2.2.2 Example:

```fortran
TEMPLATE swap_t(T)
   TYPE, DEFERRED :: T
CONTAINS
   SUBROUTINE swap(x, y)
      TYPE(T), INTENT(INOUT) :: x, y
      TYPE(T) :: tmp
      tmp = x; x = y; y = tmp
   END SUBROUTINE
END TEMPLATE

INSTANTIATE swap_t(integer), ONLY: swap_int => swap
INSTANTIATE swap_t(real), ONLY: swap_real => swap
```

6.2.3 **Inline instantiation**

6.2.3.1 Simple template procedures may be instantiated inline:

```fortran
CALL swap{integer}(a, b)
```

### 6.3 REQUIREMENT construct

6.3.1 **Syntax**

6.3.1.1 A requirement defines reusable type constraints:

```fortran
REQUIREMENT requirement-name ( parameter-list )
   TYPE, DEFERRED :: type-parameter
   INTERFACE
      interface-body
   END INTERFACE
END REQUIREMENT
```

6.3.2 **Example**

```fortran
REQUIREMENT R_comparable(T, less_than)
   TYPE, DEFERRED :: T
   INTERFACE
      PURE LOGICAL FUNCTION less_than(a, b)
         TYPE(T), INTENT(IN) :: a, b
      END FUNCTION
   END INTERFACE
END REQUIREMENT
```

### 6.4 Traits (Swift/Rust approach)

6.4.1 **Type sets**

6.4.1.1 A type set specifies a constraint as a union of types:

```fortran
ABSTRACT INTERFACE :: INumeric
   integer | real(real64)
END INTERFACE INumeric
```

6.4.2 **Trait signatures**

6.4.2.1 A trait may specify required procedure signatures:

```fortran
ABSTRACT INTERFACE :: ISum
   FUNCTION sum{INumeric :: T}(x) RESULT(s)
      TYPE(T), INTENT(IN) :: x(:)
      TYPE(T) :: s
   END FUNCTION
END INTERFACE ISum
```

6.4.3 **IMPLEMENTS statement**

6.4.3.1 Types declare trait conformance:

```fortran
TYPE, IMPLEMENTS(ISum) :: SimpleSum
CONTAINS
   PROCEDURE, NOPASS :: sum
END TYPE
```

6.4.3.2 Retroactive implementation:

```fortran
IMPLEMENTS IComparable :: integer
   PROCEDURE :: less_than => builtin_less_than
END IMPLEMENTS
```

### 6.5 Compatibility of approaches

6.5.1 The two approaches are not mutually exclusive and may be combined.

6.5.2 Whether generics support static dispatch, dynamic dispatch, or both is subject to the following consideration.

> **OPEN ISSUE 7: Dispatch mechanism**
>
> Should generics support both static and dynamic dispatch?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | Static only | Zero overhead | No runtime flexibility |
> | B | Dynamic only | Runtime polymorphism | Overhead |
> | C | Both | User chooses | Complex |

6.5.3 Complementary strengths:

| Use Case | Recommended Approach |
|----------|---------------------|
| Generic containers | J3 TEMPLATE |
| Numeric algorithms | Traits type sets |
| Retroactive conformance | Traits IMPLEMENTS |
| Runtime polymorphism | Traits with `class(ITrait)` |
| Explicit instantiation control | J3 TEMPLATE |
| Ergonomic call sites | Traits automatic inference |

---

## 7 Automatic specialization

### 7.1 General

7.1.1 Lazy Fortran automatically generates specialized (monomorphized) code for each concrete type combination used with a generic procedure.

7.1.2 Generic resolution follows the rules specified in ISO/IEC 1539-1:2023 Clause 15.4.3.4.

### 7.2 Resolution policy

7.2.1 The following resolution rules apply in order:

7.2.1.1 User-written specific procedures take precedence over generated specializations.

7.2.1.2 Among remaining candidates, the most specific candidate wins according to ISO/IEC 1539-1:2023 Clause 15.4.3.4.

7.2.1.3 If two or more candidates remain after applying the rules of ISO/IEC 1539-1:2023, a compile-time ambiguity error is raised.

### 7.3 Specialization scope

7.3.1 Specializations may be generated at module scope, program scope, or link-time depending on implementation.

7.3.2 The scope at which specializations are generated is subject to the following consideration.

> **OPEN ISSUE 8: Specialization scope**
>
> At what scope should specializations be generated?
>
> | Option | Description | Pros | Cons |
> |--------|-------------|------|------|
> | A | Per-module | Smaller units | May duplicate |
> | B | Per-program (link-time) | No duplication | Requires LTO |
> | C | Lazy (on-demand) | Minimal size | Complex build |

---

## 8 Application binary interface

### 8.1 General

8.1.1 This section specifies the name mangling conventions for interoperability.

8.1.2 The ABI is designed to be compatible with gfortran's conventions where applicable.

### 8.2 gfortran ABI reference

8.2.1 **Module procedures**

8.2.1.1 gfortran mangles module procedures as:

```
__<module-name>_MOD_<procedure-name>
```

8.2.1.2 Example: `add` in module `test_mod` becomes `__test_mod_MOD_add`.

8.2.2 **Internal (contained) procedures**

8.2.2.1 gfortran mangles internal procedures as:

```
<procedure-name>.<unique-number>
```

8.2.2.2 The unique number ensures distinctness within the compilation unit.

8.2.3 **Main program**

8.2.3.1 The main program entry point is `MAIN__`.

### 8.3 Lazy Fortran specialization mangling

8.3.1 **Naming convention**

8.3.1.1 Specialized procedures use the following name mangling:

```
<procedure-name>__<kind-suffix-1>_<kind-suffix-2>_...
```

8.3.1.2 Each kind suffix encodes the type and kind of a parameter.

8.3.2 **Kind suffixes**

8.3.2.1 The naming convention for kind suffixes is subject to the following consideration.

> **OPEN ISSUE 9: Kind suffix convention**
>
> Should kind suffixes use bits (C-style) or bytes (Fortran kind parameter)?
>
> | Option | Example | Pros | Cons |
> |--------|---------|------|------|
> | A | `i32`, `r64` (bits) | Familiar to C/Rust programmers, current fortfront | Inconsistent with Fortran kind parameters |
> | B | `i4`, `r8` (bytes) | Matches `integer(4)`, `real(8)` exactly | Less familiar to polyglot programmers |
>
> Option B aligns with Fortran's kind system where `integer(4)` means 4 bytes, not 4 bits.

8.3.2.2 Kind suffixes (using current bit-based convention):

| Type | Kind | Storage | Suffix |
|------|------|---------|--------|
| integer | 1 | 1 byte | i8 |
| integer | 2 | 2 bytes | i16 |
| integer | 4 | 4 bytes | i32 |
| integer | 8 | 8 bytes | i64 |
| integer | 16 | 16 bytes | i128 |
| real | 4 | 4 bytes | r32 |
| real | 8 | 8 bytes | r64 |
| real | 16 | 16 bytes | r128 |
| complex | 4 | 8 bytes | c64 |
| complex | 8 | 16 bytes | c128 |
| complex | 16 | 32 bytes | c256 |
| logical | 1 | 1 byte | l8 |
| logical | 4 | 4 bytes | l32 |
| character | N | N bytes | chN |

8.3.2.3 Kind suffixes (alternative byte-based convention):

| Type | Kind | Storage | Suffix |
|------|------|---------|--------|
| integer | 1 | 1 byte | i1 |
| integer | 2 | 2 bytes | i2 |
| integer | 4 | 4 bytes | i4 |
| integer | 8 | 8 bytes | i8 |
| integer | 16 | 16 bytes | i16 |
| real | 4 | 4 bytes | r4 |
| real | 8 | 8 bytes | r8 |
| real | 16 | 16 bytes | r16 |
| complex | 4 | 8 bytes | c4 |
| complex | 8 | 16 bytes | c8 |
| complex | 16 | 32 bytes | c16 |
| logical | 1 | 1 byte | l1 |
| logical | 4 | 4 bytes | l4 |
| character | N | N bytes | chN |

8.3.2.4 Array rank is indicated by `rank<n>` suffix:

```
matmul__r64rank2_r64rank2
```

8.3.3 **Examples**

8.3.3.1 `add(integer, integer)` → `add__i32_i32`

8.3.3.2 `add(real(8), real(8))` → `add__r64_r64`

8.3.3.3 `sum(real(8), dimension(:))` → `sum__r64rank1`

### 8.4 Module wrapping

8.4.1 When multiple specializations are generated, they are wrapped in an auto-generated module.

8.4.2 The module name follows the pattern `auto_<procedure-name>`.

8.4.3 Example for procedure `add`:

```fortran
module auto_add
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface add
end module
```

8.4.4 The resulting ABI names follow gfortran convention:

```
__auto_add_MOD_add__i32_i32
__auto_add_MOD_add__r64_r64
```

---

## 9 Standardizer

### 9.1 General

9.1.1 The standardizer transforms Lazy Fortran (`.lf`) to standard-conforming Fortran (`.f90`).

9.1.2 This section describes the transformation mechanics.

### 9.2 Program structure wrapping

9.2.1 **Default program unit**

9.2.1.1 When no program unit is specified, the default is `program main`.

9.2.1.2 Bare statements without program structure are wrapped in `program main ... end program`.

9.2.1.3 When the source file contains only procedure definitions (no executable statements at file level), the default is a module named after the file (without extension).

9.2.1.4 Example: `mathlib.lf` containing only functions becomes `module mathlib`.

9.2.2 **Contained procedures**

9.2.2.1 Functions and subroutines at file level are placed in the `contains` section of the default program unit.

9.2.2.2 When the default is a module (9.2.1.3), procedures are placed in the module's `contains` section.

9.2.3 Example:

```fortran
! Input: script.lf
x = 5
print *, x

! Output: script.f90
program main
    implicit none
    integer :: x
    x = 5
    print *, x
end program main
```

### 9.3 Declaration generation

9.3.1 Explicit declarations are generated for all inferred variables.

9.3.2 Declarations are placed at block beginning, before executable statements.

9.3.3 Example:

```fortran
! Inferred declarations
integer :: x
real :: y
integer, dimension(3) :: arr
```

### 9.4 Implicit none injection

9.4.1 The standardizer injects `implicit none` into all program units.

9.4.2 This ensures type safety in the generated code.

### 9.5 Intent generation

9.5.1 Intent attributes are generated based on usage analysis (see 5.3).

9.5.2 Example:

```fortran
integer function add(a, b)
    integer, intent(in) :: a, b   ! Generated
    add = a + b
end function
```

### 9.6 Monomorphization output

9.6.1 **Multiple specializations**

9.6.1.1 When a procedure is called with multiple type signatures, the standardizer:

  - (a) generates specialized procedures with mangled names (see 8.3);
  - (b) creates a generic interface binding all specializations;
  - (c) wraps in a module (see 8.4);
  - (d) injects `use` statement in the calling code.

9.6.1.2 Example:

```fortran
! Input: script.lf
function add(a, b)
    add = a + b
end function
x = add(5, 3)
y = add(2.5, 1.5)

! Output: script.f90
module auto_add
    implicit none
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface add
contains
    integer function add__i32_i32(a, b)
        integer, intent(in) :: a, b
        add__i32_i32 = a + b
    end function

    real function add__r64_r64(a, b)
        real, intent(in) :: a, b
        add__r64_r64 = a + b
    end function
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real :: y
    x = add(5, 3)
    y = add(2.5, 1.5)
end program main
```

9.6.2 **Single specialization optimization**

9.6.2.1 When a procedure has only one type signature, no interface or module wrapping is generated.

9.6.2.2 The procedure is placed directly in the `contains` section.

---

## Annex A References

### A.1 Normative references

- ISO/IEC 1539-1:2023, *Information technology — Programming languages — Fortran — Part 1: Base language*

### A.2 Informative references

The following documents are referenced for background information only. They do not constitute requirements of this document.

- [J3 Generics Repository](https://github.com/j3-fortran/generics) - Official J3 committee work on generics
- [J3 Paper 18-281r1](https://j3-fortran.org/doc/year/18/18-281r1.txt) - Simple templates proposal
- [J3 Paper 24-107r1](https://j3-fortran.org/doc/year/24/) - TEMPLATE/INSTANTIATE syntax
- [Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran) - Swift/Rust-inspired generics proposal
