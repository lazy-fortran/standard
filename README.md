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

| Standard | Status | Tests | Description |
|----------|--------|-------|-------------|
| FORTRAN (1957) | ✅ Complete | 65/65 ✅ | Original IBM 704 language - foundational constructs |
| FORTRAN II (1958) | ✅ Complete | 5/5 ✅ | Independent compilation, FUNCTION/SUBROUTINE |
| FORTRAN IV (1962) | ✅ Complete | 13/13 ✅ | LOGICAL data type, enhanced operators |
| FORTRAN 66 (1966) | ✅ Complete | 6/6 ✅ | First ANSI standard - machine independence |
| FORTRAN 77 (1977) | ✅ Complete | 11/11 ✅ | CHARACTER type, IF-THEN-ELSE, structured programming |
| Fortran 90 (1990) | ✅ Complete | 20/20 ✅ | Free-form, modules, dynamic arrays, WHERE constructs |
| Fortran 95 (1995) | ✅ Complete | 26/26 ✅ | FORALL constructs, enhanced intrinsics |
| Fortran 2003 (2003) | ✅ Complete | 64/64 ✅ | Object-oriented programming, C interoperability |
| Fortran 2008 (2008) | ✅ Complete | 20/20 ✅ | Coarrays, submodules, DO CONCURRENT |
| Fortran 2018 (2018) | ✅ Complete | 8/8 ✅ | Teams, events, ERROR STOP enhancements |
| Fortran 2023 (2023) | ✅ Complete | 16/16 ✅ | Generics, conditional expressions, enhanced parallel features |
| **Total** | **✅ ALL COMPLETE** | **254/254 ✅** | **Complete FORTRAN/Fortran grammar hierarchy** |

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

### Complete Implementation Status
**✅ ALL STANDARDS FULLY IMPLEMENTED:**
- **FORTRAN (1957)**: Punch card era constructs, arithmetic IF, computed GOTO
- **FORTRAN II-IV**: Enhanced data types, independent compilation 
- **FORTRAN 66-77**: Standardization, structured programming, CHARACTER type
- **Fortran 90**: Revolutionary free-form syntax, modules, dynamic arrays
- **Fortran 95**: FORALL constructs, enhanced WHERE statements
- **Fortran 2003**: Complete object-oriented programming, C interoperability
- **Fortran 2008**: Coarrays, submodules, DO CONCURRENT parallelism
- **Fortran 2018**: Teams, events, enhanced error handling
- **Fortran 2023**: Generics, conditional expressions, latest ISO standard

**Production Ready For:**
All FORTRAN/Fortran code from 1957 to 2023 - complete historical coverage

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

The project includes complete test coverage:
- **254 unit tests** covering all standards (ALL PASSING ✅)
- **65 FORTRAN (1957) tests** - foundational constructs validated
- **64 Fortran 2003 tests** - complete OOP and modern features 
- **Historical accuracy testing** - real code from each era
- **Operator precedence validation** across all standards
- **Parse tree semantic analysis** for correctness
- **Cross-standard compatibility** testing

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