# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LazyFortran2025 is a comprehensive ANTLR4-based grammar implementation covering all FORTRAN/Fortran standards from 1957 to 2023+. The project uses a revolutionary modular inheritance architecture where each standard only defines NEW features and imports from its predecessor.

LazyFortran2025 now ships as an ANTLR4 grammar pair (`LazyFortran2025Lexer.g4` and `LazyFortran2025Parser.g4`) that imports the Fortran 2023 lexer and parser. The parser exposes two entry points:
- `traditional_entry` for strict Fortran 2023 program units (standard `.f90+` files)
- `lazy_entry` for `.lf` files with optional PROGRAM/MODULE wrappers, optional CONTAINS, and relaxed type declarations to support type inference at the semantic layer.

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
FORTRAN(1957) → FORTRAN_II → FORTRAN66 → FORTRAN77 → Fortran90 → F95 → F2003 → F2008 → F2018 → F2023 → LazyFortran2025
```

**Note**: FORTRAN IV (1962) and FORTRAN 66 (1966) have been merged since FORTRAN 66 was just the ANSI standardization of FORTRAN IV features.

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

Grammars exist for FORTRAN (1957), FORTRAN II, FORTRAN 66, FORTRAN 77 and for the modern standards Fortran 90, 95, 2003, 2008, 2018 and 2023. Test coverage is uneven and spec‑grounded GitHub issues (for example #83, #88, #90) describe known gaps in the Fortran 2003/2008/2018 subsets.

As of this branch the test suite contains roughly 270 passing tests across all standards. The implementation is suitable for experimentation and research, but it should not be treated as a complete, production‑quality implementation of every feature from each Fortran standard.

## Development Workflows

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
