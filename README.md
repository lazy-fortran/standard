# Fortran Standard Specifications

> **Note**: This project is experimental and subject to major changes.

This repository contains:
1. **ANTLR4 grammars** for all Fortran standards from 1957 to 2023
2. **LFortran extensions**: J3 Generics and type inference
3. **Specifications** for LFortran Standard and LFortran Infer mode

## Standards Hierarchy

```
ISO Fortran 2023 (ISO/IEC 1539-1:2023)
    |
    v
LFortran Standard (--std=lf)
    |   - Fortran 2023 + J3 Generics
    |   - Stricter defaults (bounds checking ON, implicit none)
    |   - 8-byte reals, 4-byte integers
    |
    v
LFortran Infer (--infer)
    - Type inference (`:=` and first assignment with `--infer`)
    - Automatic array reallocation
    - Interactive REPL mode
```

## Implementation Status

All grammars are **complete and tested**.

| Standard | Grammar | Tests | Key Features |
|----------|---------|-------|--------------|
| FORTRAN 1957 | Complete | Yes | Arithmetic IF, DO loops, FORMAT I/O |
| FORTRAN II | Complete | Yes | SUBROUTINE, FUNCTION, COMMON |
| FORTRAN 66 | Complete | Yes | First ANSI standard, LOGICAL/COMPLEX |
| FORTRAN 77 | Complete | Yes | CHARACTER, IF-THEN-ELSE, PARAMETER |
| Fortran 90 | Complete | Extensive | Free-form, modules, derived types, arrays |
| Fortran 95 | Complete | Yes | FORALL, PURE/ELEMENTAL |
| Fortran 2003 | Complete | Extensive | OOP, C interop, PDTs, IEEE |
| Fortran 2008 | Complete | Yes | Coarrays, submodules, DO CONCURRENT |
| Fortran 2018 | Complete | Yes | Teams, events, atomics |
| Fortran 2023 | Complete | Yes | Conditional expressions, TYPEOF/CLASSOF |
| LFortran | Complete | Yes | J3 Generics (TEMPLATE, REQUIREMENT, INSTANTIATE) |
| LFortran Infer | Complete | Yes | Type inference (`:=`, `--infer`), global scope |

## Quick Start

```bash
# Clone
git clone git@github.com:lazy-fortran/standard.git
cd standard

# Build grammars
make all

# Run tests
make test
```

### Prerequisites
- Python 3.8+
- ANTLR4
- Git

## Documentation

| Document | Description |
|----------|-------------|
| [LFortran Standard](docs/lfortran-standard.md) | Stricter Fortran 2023 dialect specification |
| [LFortran Infer](docs/lfortran-infer.md) | Type inference and infer mode specification |
| [Design Rationale](docs/design-rationale.md) | Explains key design decisions |
| [Implementation Notes](docs/implementation-notes.md) | Status and known limitations |

## Directory Structure

```
standard/
├── docs/               # Specifications and documentation
├── grammars/src/       # ANTLR4 grammar files (.g4)
├── tests/              # Test suites by standard
├── tools/              # Semantic validators
└── validation/         # Standard PDFs
```

## Grammar Inheritance

Each grammar extends its predecessor, defining only NEW features:

```
FORTRAN 1957 -> FORTRAN II -> FORTRAN 66 -> FORTRAN 77
                                                 |
                                            Fortran 90
                                                 |
                              Fortran 95 -> 2003 -> 2008 -> 2018 -> 2023
                                                                      |
                                                              LFortran -> Infer
```

## License

[MIT License](LICENSE)

## Related Projects

- [LFortran](https://lfortran.org) - Modern Fortran compiler
- [J3 Generics](https://github.com/j3-fortran/generics) - Fortran 202Y generics proposal
