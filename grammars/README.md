# FORTRAN Grammar Modular Architecture

This directory contains the modular FORTRAN grammar implementations, designed with a clear inheritance chain that reflects the historical evolution of the language.

## Inheritance Chain

```
SharedCore (Universal constructs 1957-2023+)
    ↓
FORTRAN (Original IBM 704, 1957)
    ↓
FORTRAN II (1958) - inherits from FORTRAN
    ↓
FORTRAN IV (1962) - inherits from II
    ↓
FORTRAN 66 (1966) - inherits from IV
    ↓
FORTRAN 77 (1977) - inherits from 66
    ↓
Fortran 90 (1990) - inherits from 77
    ↓
Fortran 95 (1995) - inherits from 90
    ↓
Fortran 2003 (2003) - inherits from 95
    ↓
Fortran 2008 (2008) - inherits from 2003
    ↓
Fortran 2018 (2018) - inherits from 2008
    ↓
Fortran 2023 (2023) - inherits from 2018
    ↓
LazyFortran2025 (2025+) - inherits from 2023
```

## Module Structure

### shared_core/
**Foundation Module** - Contains universal constructs present in ALL standards:
- Basic keywords (IF, GOTO, DO, END, CONTINUE, STOP)
- I/O operations (READ, WRITE)
- Arithmetic operators (+, -, *, /, **)
- Relational operators (.EQ., .NE., .LT., .LE., .GT., .GE.)
- Basic expression parsing with correct precedence
- Universal delimiters and literals

### FORTRAN/
**Historical Foundation** - Original IBM 704 FORTRAN (1957):
- Imports: `SharedCore`
- Adds: PAUSE, PRINT, PUNCH, FORMAT
- Adds: DIMENSION, EQUIVALENCE, FREQUENCY, COMMON
- Adds: Hollerith constants (nHtext)
- Adds: Arithmetic IF (three-way branch)
- Adds: Computed GOTO

### FORTRAN_II/ (Future)
**First Extension** - FORTRAN II (1958):
- Imports: `FORTRAN`
- Adds: FUNCTION keyword
- Adds: SUBROUTINE keyword
- Adds: Improved COMMON blocks

### fortran_iv/ (Future)
**Major Enhancement** - FORTRAN IV (1962):
- Imports: `FORTRAN_II`
- Adds: LOGICAL type
- Adds: Logical operators (.AND., .OR., .NOT.)
- Adds: Logical IF
- Adds: Block IF structure

### fortran66/ (Future)
**First Standard** - FORTRAN 66 (ANSI X3.9-1966):
- Imports: `FORTRAN_IV`
- Standardizes existing features
- Adds: DATA statement
- Adds: Multiple entries to procedures

### fortran77/ (Future)
**Modern Foundation** - FORTRAN 77:
- Imports: `FORTRAN66`
- Adds: CHARACTER type
- Adds: IF-THEN-ELSE-ENDIF
- Adds: DO WHILE
- Adds: IMPLICIT statement
- Adds: PARAMETER statement

### fortran90/ (Future)
**Free-Form Revolution** - Fortran 90:
- Imports: `FORTRAN77`
- Adds: Free-form source
- Adds: Modules
- Adds: Derived types
- Adds: Array operations
- Adds: Pointers
- Adds: Recursive procedures

## Building Grammars

Each grammar builds upon its predecessor:

```bash
# Build shared core first
./scripts/build_grammar.sh shared_core

# Then build FORTRAN (imports SharedCore)
./scripts/build_grammar.sh FORTRAN

# Future: Build FORTRAN II (will import FORTRAN)
./scripts/build_grammar.sh FORTRAN_II
```

## Design Principles

1. **No Duplication**: Each grammar only defines what's NEW in that standard
2. **Clear Evolution**: The inheritance chain shows language evolution
3. **Backward Compatibility**: Later standards include all earlier features
4. **Modular Testing**: Each standard can be tested independently
5. **Historical Accuracy**: Each grammar reflects its era's capabilities

## Grammar Files

Each grammar module contains:
- `<Standard>Lexer.g4` - Lexical tokens specific to that standard
- `<Standard>Parser.g4` - Parsing rules specific to that standard
- Both import from their predecessor in the chain

## Implementation Status

- ✅ **SharedCore** - Complete, fully tested
- ✅ **FORTRAN** - Complete, historical accuracy verified (1957)
- 🔄 **FORTRAN II** - Planned
- 🔄 **FORTRAN IV** - Planned
- 🔄 **FORTRAN 66** - Planned
- 🔄 **FORTRAN 77** - Planned
- 🔄 **Fortran 90** - Planned (priority after 77)
- 🔄 **Later standards** - Future work

## Testing Strategy

Each grammar level has its own test suite:
- `tests/shared_core/` - Tests universal constructs
- `tests/FORTRAN/` - Tests original FORTRAN features
- Future test directories for each standard

Tests validate:
1. New features work correctly
2. Inherited features still work
3. Historical code examples parse correctly
4. Precedence and associativity are preserved