# Fortran Standard Specifications

> **Note**: This project is experimental and subject to major changes.

This repository contains:
1. **ANTLR4 grammars** for all Fortran standards from 1957 to 2028 (working draft)
2. **LFortran extensions**: F2028 generics base, inline instantiation, and infer mode
3. **Specifications** for LFortran Standard and LFortran Infer mode

## Standards Hierarchy

```
ISO Fortran 2023 (ISO/IEC 1539-1:2023)
    |
    v
Fortran 2028 Working Draft (J3/26-007)
    |
    v
LFortran Standard (--std=lf)
    |   - Fortran 2028 base + LFortran extensions
    |   - Inline instantiation: `name{T}(...)` and `name^(T)(...)`
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
This table tracks repository parser/grammar coverage; semantic/runtime behavior is
tracked separately and is out of scope for this parser repository.

Status source of truth:
- [Implementation Notes: Grammar Status](docs/implementation-notes.md#grammar-status)
- [Implementation Notes: Fortran 2028 Delta Audit](docs/implementation-notes.md#fortran-2028-delta-audit-j326-007)
- [LFortran Design: Deferred Trait Items (Semantic Phase)](docs/lfortran-design.md#deferred-trait-items-semantic-phase)

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
| Fortran 2028 (WD) | Complete | Yes | TEMPLATE/REQUIREMENT/REQUIRE(S)/INSTANTIATE facility |
| LFortran | Complete | Yes | F2028 base + inline instantiation (`{}` and `^()`); traits syntax (`implements`, `sealed`, `initial`) |
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
| [LFortran Standard](docs/lfortran-standard.md) | Stricter Fortran dialect specification (F2028 base) |
| [LFortran Infer](docs/lfortran-infer.md) | Type inference and infer mode specification |
| [Traits Proposal](docs/traits-proposal.md) | Traits, nominal conformance, and `{T}` procedure generics |
| [External Sources](docs/external-sources.md) | Sync policy for Traits repo + Fortran 2028 draft |
| [Design Rationale](docs/design-rationale.md) | Explains key design decisions |
| [Implementation Notes](docs/implementation-notes.md) | Status and known limitations |

## Directory Structure

```
standard/
├── docs/               # Specifications and documentation
├── grammars/src/       # ANTLR4 grammar files (.g4)
├── tests/              # Test suites by standard
├── tools/              # Semantic validators
└── validation/         # Reference corpora, standards PDFs, and validation tooling
```

## Grammar Inheritance

Each grammar extends its predecessor, defining only NEW features:

```
FORTRAN 1957 -> FORTRAN II -> FORTRAN 66 -> FORTRAN 77
                                                 |
                                            Fortran 90
                                                 |
                              Fortran 95 -> 2003 -> 2008 -> 2018 -> 2023 -> 2028
                                                                              |
                                                                      LFortran -> Infer
```

## License

[MIT License](LICENSE)

## Related Projects

- [LFortran](https://lfortran.org) - Modern Fortran compiler
- [J3 Generics](https://github.com/j3-fortran/generics) - Fortran 202Y generics proposal
- [Traits for Types (J3/20-109)](https://github.com/j3-fortran/fortran_proposals/blob/master/proposals/traits/20-109.txt) - Trait proposal track reflected in the LFortran grammar
- [Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran) - Community traits/generics design document
