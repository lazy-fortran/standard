# LFortran Infer Mode

**Status:** Draft  
**Derived from:** [LFortran Standard](lfortran-standard.md)  
**Compiler flag:** `--infer`

## Overview

LFortran Infer mode extends LFortran Standard for interactive and rapid-prototyping workflows. It keeps standard Fortran syntax and adds inference-oriented behavior.

## Inference Forms

### 1. Walrus declaration (`:=`)

`:=` declares a new variable and infers its type from the right-hand side.

```fortran
x := 42
coeffs := [1.0d0, 0.5d0]
p := point_t(1.0, 2.0)
```

Rules:
- `:=` declares, it does not assign to an existing symbol.
- Redeclaring in the same scope is an error.
- Inner scopes may shadow outer variables.
- Walrus syntax is an LFortran extension.

### 2. First-assignment inference (`=` with `--infer`)

With `--infer`, a first plain assignment to an undeclared name declares it.

```fortran
x = 42
y = 3.14
```

This is intended for scripts, REPL sessions, and notebooks.

## Inference Coverage

Current LFortran behavior includes inference for:
- Intrinsic scalars (`integer`, `real`, `complex`, `logical`, `character`)
- Arrays inferred from RHS expressions
- Derived types inferred from structure constructors or typed expressions

The inferred kind follows the RHS expression kind.

## Global Scope

Infer mode allows bare statements and declarations without a `program ... end program` wrapper:

```fortran
x := 5
print *, x
```

## Automatic Array Reallocation

Allocatable arrays reallocate on shape mismatch in infer mode:

```fortran
real, allocatable :: arr(:)
arr = [1.0, 2.0, 3.0]
arr = [7.0, 8.0]
```

Equivalent to enabling `--realloc-lhs-arrays` behavior.

## Compiler Modes

| Mode | Flag | `:=` | First `=` inference | Array Realloc |
|------|------|------|----------------------|---------------|
| **Strict** | `--std=lf` (default) | YES (extension) | OFF | OFF |
| **Standard** | `--std=f23` | YES (extension) | OFF | ON |
| **Infer** | `--infer` | YES | ON | ON |

## Standardizer

The standardizer can lower infer-mode source to explicit standard Fortran by:
- Wrapping bare statements in program units
- Emitting explicit declarations for inferred symbols
- Preserving explicit declaration semantics

## References

- [LFortran Standard](lfortran-standard.md)
- [Design Rationale](design-rationale.md)
- [LFortran Compiler](https://lfortran.org)
