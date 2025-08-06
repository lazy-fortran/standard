# Claude Development Guidelines for LazyFortran2025 Standard

## Project Context
You are working on the LazyFortran2025 standard grammar implementation, a comprehensive ANTLR4-based parser for all FORTRAN/Fortran standards from 1957 to LazyFortran2025.

## Architecture Overview

### Modular Grammar Hierarchy
```
SharedCore → FORTRAN → FORTRAN_II → FORTRAN_IV → FORTRAN66 → FORTRAN77 → Fortran90 → ... → LazyFortran2025
```

Each grammar:
- Only defines NEW features (no duplication)
- Imports from its predecessor
- Maintains historical accuracy

### Directory Structure
- `grammars/` - Source .g4 files (commit these)
- `build/` - Generated files (never commit)
- `tests/` - Test suites (comprehensive validation)
- `scripts/` - Build automation

## Development Principles

### Strict Requirements
1. **Test-Driven Development (TDD)**: RED → GREEN → REFACTOR
2. **No shortcuts**: Full implementation, no stubs
3. **Clean code**: Self-documenting, no unnecessary comments
4. **Small commits**: Incremental, well-documented changes
5. **Historical accuracy**: Respect each standard's era

### Grammar Rules
- SharedCore contains ONLY universal constructs (1957-2025+)
- Each standard grammar imports and extends its predecessor
- Use ANTLR4 import mechanism for inheritance
- Maintain correct operator precedence

### Naming Conventions
- Classic standards: All caps (FORTRAN, FORTRAN_II, FORTRAN77)
- Modern standards: Mixed case (Fortran90, Fortran2003)
- Latest extension: LazyFortran2025

## Build Process

```bash
# Build grammars in dependency order
./scripts/build_grammar.sh shared_core
./scripts/build_grammar.sh FORTRAN
./scripts/build_grammar.sh FORTRAN_II  # When implemented
```

## Testing Strategy

### Required Test Coverage
- Operator precedence validation
- Parse tree semantic analysis
- Historical code examples
- Edge cases and error conditions

### Test Execution
```bash
# Run all tests
python -m pytest tests/ -v

# Run specific suite
python tests/shared_core/test_comprehensive_validation.py
```

## Current Status

### Completed
- ✅ SharedCore grammar (universal constructs)
- ✅ FORTRAN (1957) grammar
- ✅ Modular inheritance architecture
- ✅ Comprehensive test suite
- ✅ Build automation

### In Progress
- 🔄 FORTRAN_II implementation
- 🔄 Additional historical standards

### Future Work
- Fortran90 free-form revolution
- Modern Fortran features (2003-2023)
- LazyFortran2025 type inference

## Resources
- Fortran standards: https://gcc.gnu.org/wiki/GFortranStandards
- ANTLR4 documentation: https://www.antlr.org/
- Historical FORTRAN manuals: IBM archives

## Important Notes

### DO NOT
- Commit generated files (build/ directory)
- Duplicate rules between grammars
- Take shortcuts or create stubs
- Mix concerns between standards

### ALWAYS
- Follow TDD methodology
- Maintain backward compatibility
- Document historical context
- Test comprehensively

## GitHub Integration

### Issues
Update issues with progress, maintain accurate status:
- Issue #6: SharedCore (Complete)
- Issue #10: FORTRAN 1957 (Complete)
- Issue #2: LazyFortran2025 (Future)

### Pull Requests
- Small, focused changes
- Comprehensive commit messages
- Address review feedback promptly
- Include test coverage

## LazyFortran2025 Vision

The ultimate goal is LazyFortran2025:
- Full type inference
- Modern syntax extensions
- Backward compatible with all FORTRAN/Fortran
- Performance optimizations
- Seamless C/Python interop

Remember: We're building the future of Fortran by understanding and respecting its past.