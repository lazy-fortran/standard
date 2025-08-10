# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LazyFortran2025 is a comprehensive ANTLR4-based grammar implementation covering all FORTRAN/Fortran standards from 1957 to 2023+. The project uses a revolutionary modular inheritance architecture where each standard only defines NEW features and imports from its predecessor.

## Essential Build Commands

```bash
# Build all grammars in dependency order
make all

# Build specific standard (builds dependencies automatically)
make Fortran2003

# Clean all generated files
make clean

# Run comprehensive test suite
make test

# Run specific standard tests
make test-fortran2003

# Show all available targets
make help
```

## Architecture Deep Dive

### Grammar Inheritance Chain
```
FORTRAN(1957) → FORTRAN_II → FORTRAN_IV → FORTRAN66 → FORTRAN77 → Fortran90 → F95 → F2003 → F2008 → F2018 → F2023 → LazyFortran2025
```

### Critical Implementation Details

**File Structure:**
- `grammars/*.g4` - Source grammars (version controlled)
- `grammars/*.py` - Generated files (git-ignored, never commit)
- `tests/` - Comprehensive test suites by standard
- `Makefile` - Dependency-aware build system

**Unified Format Architecture:**
- Each F90+ parser handles BOTH fixed-form (.f) and free-form (.f90) in a single grammar
- Format detection via file extension and content analysis
- Dual comment handling: `!` (free-form) and `C/*` (fixed-form)

### Build Dependencies

ANTLR4 builds must follow strict inheritance order:
1. FORTRAN (foundation) must build first
2. Each subsequent standard imports its predecessor
3. Missing dependencies cause cascading build failures
4. Use `make force-rebuild` for clean slate

## Testing Philosophy

**TDD Requirements:**
- RED: Write failing test first
- GREEN: Implement minimal code to pass
- REFACTOR: Clean up while keeping tests green

**Test Validation:**
- Tests must actually parse real Fortran code
- Verify semantic content, not just "PASS" messages
- Parse tree analysis for correctness
- Historical code examples for accuracy

## Current Implementation Status

**✅ ALL STANDARDS COMPLETE (254/254 tests passing):**
- FORTRAN (1957): 65 tests ✅ - Complete IBM 704 original language
- FORTRAN II (1958): 5 tests ✅ - Independent compilation, FUNCTION/SUBROUTINE
- FORTRAN IV (1962): 13 tests ✅ - LOGICAL data type, enhanced operators  
- FORTRAN 66 (1966): 6 tests ✅ - First ANSI standard, machine independence
- FORTRAN 77 (1977): 11 tests ✅ - CHARACTER type, IF-THEN-ELSE structures
- Fortran 90 (1990): 20 tests ✅ - Free-form, modules, dynamic arrays
- Fortran 95 (1995): 26 tests ✅ - FORALL constructs, enhanced intrinsics
- Fortran 2003 (2003): 64 tests ✅ - Complete OOP, C interoperability
- Fortran 2008 (2008): 20 tests ✅ - Coarrays, submodules, DO CONCURRENT
- Fortran 2018 (2018): 8 tests ✅ - Teams, events, enhanced error handling
- Fortran 2023 (2023): 16 tests ✅ - Generics, conditional expressions

**Project Status: PRODUCTION READY** - Complete FORTRAN/Fortran coverage 1957-2023

## LazyFortran2025 Extension

LazyFortran2025 extends Fortran2023 with syntactic relaxations for more concise, modern code:

### Key Features:

1. **Optional Program/Module Blocks**: Top-level code can omit explicit `program` or `module` blocks. The compiler pipeline determines context.

2. **Implicit None Default**: `implicit none` is the default behavior (compiler-enforced, not parser-relevant).

3. **Optional Contains**: The `contains` keyword becomes optional when procedures follow the main code section.

4. **Type Inference**: Variables can be used without prior type declarations if the compiler can infer types from context (similar to Julia/Python). No `auto` keyword needed.

### Grammar Impact:

Most LazyFortran2025 features are semantic/compiler-level rather than syntactic:
- Parser accepts more flexible structures (optional blocks)
- Type checking deferred to semantic analysis phase
- Backward compatibility maintained through inheritance from Fortran2023

## Development Workflows

### Implementing LazyFortran2025

1. **Grammar Extension**: Create `LazyFortran2025.g4` importing from `Fortran2023.g4`
2. **Relaxed Rules**: Make `program`, `module`, and `contains` optional in appropriate contexts
3. **Test Coverage**: Write tests demonstrating all four key features
4. **Validation**: Ensure standard Fortran2023 code still parses correctly

### Debugging Parser Issues

```bash
# Test specific parsing
python debug_parsing.py <fortran_code>

# Examine token stream  
python debug_tokens.py <fortran_code>

# Run targeted test suite
python -m pytest tests/Fortran2003/ -v
```

### Common Pitfalls

- **Never commit generated .py files** - they're git-ignored for a reason
- **Build order matters** - dependencies cascade through inheritance
- **Test semantically** - verify actual parsing, not just absence of errors
- **Historical accuracy** - each standard reflects its era's capabilities

## Key Architecture Principles

**Inheritance Not Duplication:**
- Each grammar defines only NEW features
- Shared constructs live in parent grammars
- ANTLR4 import mechanism handles inheritance

**Format Unification:**
- Single parser per standard handles both fixed/free form
- Eliminates dual-parser complexity
- Maintains backward compatibility

**Standards Accuracy:**
- FORTRAN (1957): Punch card era, arithmetic IF, computed GOTO
- Fortran90: Revolutionary free-form, modules, dynamic arrays  
- Fortran2003: Object-oriented programming, C interoperability
- Modern standards: Parallel features, generics, type safety

Remember: We're building the future of Fortran by understanding and respecting its past.