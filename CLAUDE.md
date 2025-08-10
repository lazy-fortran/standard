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
FORTRAN(1957) → FORTRAN_II → FORTRAN_IV → FORTRAN66 → FORTRAN77 → Fortran90 → F95 → F2003 → F2008 → F2018 → F2023
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

**Production Ready:**
- FORTRAN (1957): Complete foundational constructs
- Fortran2003: 45% complete - basic OOP, modules, inheritance working
- F2008/F2018: Advanced features implemented

**In Development:**
- FORTRAN_IV through FORTRAN77: Historical middle standards
- Type-bound procedures (Issue #22)
- ASSOCIATE/BLOCK constructs (Issue #25)

## Development Workflows

### Adding New Language Features

1. **Research**: Study historical documentation and standards
2. **Plan**: Use TodoWrite tool for multi-step features
3. **Test First**: Write failing tests for new constructs
4. **Implement**: Add minimal grammar rules
5. **Validate**: Ensure inheritance chain intact
6. **Document**: Update limitations files if needed

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