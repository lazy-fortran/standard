# Fortran Standard Specifications

This repository contains specifications for LFortran Standard and LFortran Infer mode, plus historic ANTLR4 grammars for Fortran language standards from 1957 to 2023.

## Standards Hierarchy

```
ISO Fortran 2023 (ISO/IEC 1539-1:2023)
    |
    v
LFortran Standard (--std=lf)
    |   - Stricter than ISO Fortran
    |   - Bounds checking ON by default
    |   - Default real = 8 bytes, integer = 4 bytes
    |   - Default intent(in)
    |   - dp predefined
    |   - J3 Generics (TEMPLATE, REQUIREMENT, INSTANTIATE)
    |
    v
LFortran Infer Mode (--infer)
        - Adds type inference at global scope
        - Adds automatic array reallocation
        - Adds global scope (bare statements)
        - Interactive REPL uses this implicitly
```

## Specifications

| Document | Description |
|----------|-------------|
| [LFortran Standard](docs/lfortran-standard.md) | Stricter Fortran 2023 dialect with sensible defaults |
| [LFortran Infer](docs/lfortran-infer.md) | Extends LFortran with type inference and infer mode features |
| [Design Document](docs/lfortran-design.md) | Detailed feature descriptions and examples |
| [Design Rationale](docs/design-rationale.md) | Explains key design decisions and trade-offs |

## Historic ANTLR4 Grammars

The `grammars/` directory contains ANTLR4 grammars for historic Fortran versions (1957-2023). These serve as **reference implementations** for understanding Fortran language evolution and are useful for:

- Understanding Fortran syntax across different eras
- Educational purposes and historical research
- Testing and validation against the ISO standards

**Note:** LFortran Standard and LFortran Infer are NOT defined by ANTLR grammars. They are prose specifications implemented directly in the [LFortran compiler](https://lfortran.org), which uses its own bison-based parser.

### Grammar Inheritance Chain

```
FORTRAN (1957) -> FORTRAN II (1958) -> FORTRAN 66 (1966) -> FORTRAN 77 (1977)
                                                                      |
                                                                 Fortran 90 (1990)
                                                                      |
                                          Fortran 95 -> 2003 -> 2008 -> 2018 -> 2023
```

The ANTLR4 grammars are designed with:
- **Modular inheritance** - Each standard builds upon its predecessor
- **Reuse** - Later grammars import and extend earlier ones
- **Historical orientation** - Grammars are organized by language standard
- **Clean architecture** - Source/build separation, proper dependencies

This repository currently implements grammars for:

- FORTRAN (1957, IBM 704)
- FORTRAN II (1958)
- FORTRAN 66 (ANSI X3.9‑1966)
- FORTRAN 77 (1977)
- Fortran 90, 95, 2003, 2008, 2018 and 2023

FORTRAN IV (1962) is not implemented as a separate grammar; its functionality is represented in the FORTRAN 66 grammar, reflecting the fact that FORTRAN 66 largely standardized the then‑current FORTRAN IV practice.

## Directory Structure

```
standard/
├── docs/               # Specifications and documentation
│   ├── lfortran-standard.md      # LFortran Standard spec
│   ├── lfortran-infer.md         # LFortran Infer spec
│   ├── lfortran-design.md        # Design rationale
│   └── fortran_*_audit.md        # Grammar audit documents
├── grammars/           # Historic ANTLR4 grammar files (.g4)
├── tests/              # Test suites grouped by standard
├── scripts/            # Helper / validation scripts
├── tools/              # Extra utilities
└── validation/         # External grammar validation tools
```

## Quick Start

### Prerequisites
- Python 3.8+
- ANTLR4 (Java version)
- Git

### Installation

```bash
# Clone the repository
git clone https://github.com/lazy-fortran/standard.git
cd standard

# Install ANTLR4 (Arch Linux)
sudo pacman -S antlr4

# Install Python dependencies
pip install antlr4-python3-runtime pytest
```

### Building Grammars

The project includes a comprehensive Makefile for building all grammars:

```bash
# Build all grammars in dependency order
make all

# Build specific standard
make Fortran2003

# Clean generated files
make clean

# Show available targets and help
make help

# Run tests after building
make test
```

### Running Tests

```bash
# Run all tests
python -m pytest tests/ -v
```

## Implementation Status

This project provides grammars and tests for many Fortran standards, but it does **not** implement every feature from each ISO standard and does **not** contain a separate FORTRAN IV grammar. The table below describes the state of the implementation in this repository (not the full language specifications):

| Standard | Implemented here? | Tests present? | Notes |
|----------|-------------------|----------------|-------|
| FORTRAN (1957, IBM 704) | Yes (historical core subset) | Yes (`tests/FORTRAN`) | Fixed‑form syntax; arithmetic IF; DO loops; GOTO; I/O. Grammar is an explicitly documented historical stub rather than a full reconstruction of the original compiler. |
| FORTRAN II (1958) | Yes | Yes (`tests/FORTRANII`) | Adds user‑written subroutines and functions (`SUBROUTINE`, `FUNCTION`, `CALL`, `RETURN`) and `COMMON` blocks, matching the historical FORTRAN II enhancements. |
| FORTRAN 66 (1966) | Yes | Yes (`tests/FORTRAN66`) | Represents the first ANSI Fortran standard, incorporating the FORTRAN IV features such as LOGICAL, DOUBLE PRECISION and COMPLEX into a machine‑independent standard. |
| FORTRAN 77 (1977) | Yes | Yes (`tests/FORTRAN77`) | Adds the CHARACTER type, block `IF ... THEN ... ELSE ... ENDIF`, PARAMETER, SAVE and other features; coverage is representative but not exhaustive. |
| Fortran 90 (1990) | Yes | Extensive (`tests/Fortran90`) | Modern foundation: free‑form source, modules, derived types, array operations, dynamic arrays, enhanced control constructs (`SELECT CASE`, `WHERE`), etc. |
| Fortran 95 (1995) | Yes | No dedicated suite yet | Grammar extends F90 with FORALL, enhancements to WHERE and additional intrinsics; tests are still to be written. |
| Fortran 2003 (2003) | Yes | Extensive (`tests/Fortran2003`) | Adds object‑oriented features, C interoperability, procedure pointers, IEEE arithmetic support, etc. Remaining gaps are tracked in `docs/fortran_2003_audit.md` and spec‑grounded issues such as #90. |
| Fortran 2008 (2008) | Yes | Present (`tests/Fortran2008`) | Introduces coarrays, submodules, `DO CONCURRENT`, new intrinsics and kinds. Remaining gaps are tracked in spec‑grounded Fortran 2008 issues such as #83. |
| Fortran 2018 (2018) | Yes | Present (`tests/Fortran2018`) | Extends coarray parallelism with teams, events and related features. Remaining gaps are tracked in spec‑grounded Fortran 2018 issues such as #88. |
| Fortran 2023 (2023) | Yes | Present (`tests/Fortran2023`) | Adds features such as improved enumerations, conditional expressions and further intrinsic enhancements. Current support is intentionally minimal and evolving. |

At the moment the test suite consists of roughly 270 tests across these standards, and all of them pass after generating the grammars with ANTLR.

## Key Features

### FORTRAN (1957)

Historically, the original IBM 704 FORTRAN provided fixed‑form source code, arithmetic expressions, DO loops, conditional branching with arithmetic IF, computed GOTOs, FORMAT‑driven I/O and related features. In this repository the FORTRAN grammar focuses on that core statement set and serves as the base of the inheritance chain.

### FORTRAN II (1958)

FORTRAN II added user‑written subprograms and shared storage:

- `SUBROUTINE` / `FUNCTION` / `CALL` / `RETURN` statements for separately compiled procedures
- `COMMON` blocks for sharing storage between program units
- Hollerith constants (`nHtext`) for representing character data

## Development

### Test-Driven Development
All features implemented using TDD:
1. **RED** - Write failing test
2. **GREEN** - Implement to pass
3. **REFACTOR** - Clean up

### Contributing
See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Architecture Principles
1. Each grammar only defines NEW features
2. Import from predecessor in chain
3. No duplication of rules or tokens
4. Historical accuracy maintained
5. Clean separation of concerns

## Comprehensive Validation

The project includes a growing test suite:
- On the order of 270 unit tests across the implemented standards (all currently passing)
- Historical code examples from different eras
- Operator‑precedence validation in the older dialects
- Parse‑tree checks for selected modern constructs
- Cross‑standard compatibility testing where inheritance is involved

## License

[MIT License](LICENSE)

## Acknowledgments

- Original FORTRAN team at IBM (John Backus et al.)
- ANTLR4 project for grammar tooling
- Historical FORTRAN documentation from IBM archives

## Related Projects

- [LFortran Compiler](https://lfortran.org) - Implementation target for LFortran Standard and LFortran Infer
- [LFortran GitHub](https://github.com/lfortran/lfortran) - Source code for LFortran

## Contact

- GitHub Issues: [Report bugs or request features](https://github.com/lazy-fortran/standard/issues)
- Project Lead: @krystophny

---
*Building the future of Fortran through understanding its past.*
