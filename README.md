# LazyFortran Standard Grammar Implementation

A comprehensive, modular ANTLR4-based implementation of FORTRAN/Fortran language standards from 1957 to LazyFortran2025.

## Project Overview

This repository implements a complete grammar hierarchy for all FORTRAN/Fortran standards, designed with:
- **Modular inheritance** - Each standard builds upon its predecessor
- **No duplication** - Shared constructs defined once in SharedCore
- **Historical accuracy** - Each grammar reflects its era's capabilities
- **Clean architecture** - Source/build separation, proper dependencies

## Grammar Inheritance Chain

```
SharedCore (Universal constructs 1957-2025+)
    ↓
FORTRAN (1957) → FORTRAN_II (1958) → FORTRAN_IV (1962) → FORTRAN66 → FORTRAN77
                                                                         ↓
                                                                    Fortran90
                                                                         ↓
                                                    Fortran95 → 2003 → 2008 → 2018 → 2023
                                                                                      ↓
                                                                              LazyFortran2025
```

## Directory Structure

```
standard/
├── grammars/           # Grammar source files (.g4)
│   ├── shared_core/    # Universal constructs
│   ├── FORTRAN/        # Original 1957 FORTRAN
│   └── ...             # Other standards
├── tests/              # Test suites
│   └── shared_core/    # Comprehensive validation tests
├── scripts/            # Build and utility scripts
│   └── build_grammar.sh
├── build/              # Generated parser/lexer files (git-ignored)
└── validation/         # Reference grammars and validation tools
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

Alternative manual build (if needed):
```bash
# Build SharedCore (foundation)
./scripts/build_grammar.sh shared_core

# Build FORTRAN (1957)
./scripts/build_grammar.sh FORTRAN

# Generated files appear in build/
```

### Running Tests

```bash
# Run all tests
python -m pytest tests/ -v

# Run specific test suite
python tests/shared_core/test_comprehensive_validation.py
```

## Implementation Status

| Standard | Status | Description |
|----------|--------|-------------|
| SharedCore | ✅ Complete | Universal constructs, fully tested |
| FORTRAN (1957) | ✅ Complete | Original IBM 704 language |
| FORTRAN II | 🔄 Planned | Adds FUNCTION, SUBROUTINE |
| FORTRAN IV | 🔄 Planned | Adds LOGICAL type, logical operators |
| FORTRAN 66 | 🔄 Planned | First ANSI standard |
| FORTRAN 77 | 🔄 Planned | Adds CHARACTER, IF-THEN-ELSE |
| Fortran 90 | ✅ Complete | Free-form, modules, arrays |
| Fortran 95 | ✅ Complete | Minor enhancements |
| **Fortran 2003** | **🔶 45% Complete** | **Basic OOP working, advanced features pending** |
| Fortran 2008-2023 | 🔄 Future | Modern features |
| LazyFortran2025 | 🔄 Future | Type inference extensions |

## Key Features

### SharedCore
- Arithmetic operators (+, -, *, /, **)
- Relational operators (.EQ., .NE., .LT., .LE., .GT., .GE.)
- Control flow (IF, GOTO, DO, CONTINUE, STOP, END)
- I/O operations (READ, WRITE)
- Correct operator precedence and associativity

### FORTRAN (1957)
- Fixed-form source (punch cards)
- Arithmetic IF (three-way branch)
- Computed GOTO
- FORMAT statements
- Hollerith constants (nHtext)
- DIMENSION, EQUIVALENCE, COMMON
- FREQUENCY optimization hints

### Fortran 2003 (45% Complete) 🔶
**✅ Working Features:**
- Basic object-oriented programming (types, inheritance, abstract types)
- CLASS declarations and SELECT TYPE constructs
- Module system with CONTAINS, INTERFACE blocks, IMPORT statements
- VOLATILE, PROTECTED, PARAMETER attributes
- Complete lexical analysis (100% tokens recognized)

**⏳ Pending Features (Separate Issues):**
- Type-bound procedures and method binding (Issue #22)
- ASSOCIATE and BLOCK constructs (Issue #25)
- Procedure pointers (Issue #23)
- Full C interoperability (Issue #24)
- Advanced parameterized derived types (Issue #26)

**Production Ready For:**
Basic OOP projects, module-based programs, F90/F95 migration

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

## Validation

The project includes comprehensive validation:
- 42+ unit tests for core functionality
- Operator precedence validation
- Historical code example testing
- Parse tree semantic analysis

## License

[MIT License](LICENSE)

## Acknowledgments

- Original FORTRAN team at IBM (John Backus et al.)
- ANTLR4 project for grammar tooling
- Historical FORTRAN documentation from IBM archives

## Related Projects

- [LazyFortran Compiler](https://github.com/lazy-fortran/compiler)
- [LazyFortran Type Inference](https://github.com/lazy-fortran/type-inference)

## Contact

- GitHub Issues: [Report bugs or request features](https://github.com/lazy-fortran/standard/issues)
- Project Lead: @krystophny

---
*Building the future of Fortran through understanding its past.*