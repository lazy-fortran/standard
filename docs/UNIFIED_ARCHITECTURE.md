# Unified Grammar Architecture for LazyFortran2025

## Overview

The LazyFortran2025 standard implements a **revolutionary unified grammar architecture** that supports both fixed-form and free-form Fortran syntax within a single lexer/parser per standard. This eliminates the complexity of maintaining separate format-specific implementations while providing complete backward compatibility.

> **Note:** This document describes the *intended* architecture. In the current repository there is no separate `SharedCoreLexer` / `SharedCoreParser` grammar; instead the existing grammars (starting from `FORTRANLexer.g4` and continuing through Fortran 90 and later) play that role in practice. The inheritance diagrams below should therefore be read as design guidance rather than a literal description of the files on disk.

## Architecture Principles

### 1. Unified Format Support

Each Fortran standard (F90, F95, F2003+) has **one lexer and one parser** that handles both formats:
- **Fixed-form**: `.f`, `.for` files (column-based, F77 compatibility)  
- **Free-form**: `.f90+` files (flexible layout, modern syntax)

### 2. Clean Inheritance Chain

```
SharedCoreLexer → Fortran90Lexer → Fortran95Lexer → F2003+ standards
SharedCoreParser → Fortran90Parser → Fortran95Parser → F2003+ standards  
```

Each grammar:
- **Only defines NEW features** (no duplication)
- **Imports from its predecessor** using ANTLR4 import mechanism
- **Maintains historical accuracy** for each standard's era

### 3. Format Detection Strategy (Current Subset)

The unified approach uses **dual comment handling** and **context-sensitive parsing**:

```antlr
// Free-form comments (priority - most common in F90+)
FREE_FORM_COMMENT: '!' ~[\r\n]* -> channel(HIDDEN);

// Fixed-form comments (subset – layout‑lenient, not full 80-column)
FIXED_FORM_COMMENT: [Cc*] ~[\r\n]* -> channel(HIDDEN);

// Free-form continuation
CONTINUATION: '&' [ \t]* FREE_FORM_COMMENT? -> channel(HIDDEN);
```

This reflects the **implemented subset** rather than full historical
column semantics. In particular:

- Classic 80‑column layout (labels in 1–5, continuation in 6, text in
  7–72, sequence numbers in 73–80) is **not** enforced.
- Column‑6 continuation is modeled via the F90+ `&` convention, not
  punch‑card continuation rules.
- Strict column‑accurate fixed-form support remains out of scope and is
  tracked under umbrella Issue **#91** (see `docs/fixed_form_support.md`).

**Format detection** is handled at the **driver/parser level** based on:
1. **File extension** (`.f`/`.for` = fixed, `.f90+` = free)
2. **Content analysis** (presence of free-form specific constructs)
3. **User specification** (explicit format override)

## Implementation Details

### Token Inheritance Hierarchy

**SharedCoreLexer** contains universal tokens (1957-2025+):
```antlr
// Universal keywords
INTEGER, REAL, PROGRAM, FUNCTION, SUBROUTINE
IF, THEN, ELSE, DO, END, CONTINUE, STOP

// Universal operators  
ASSIGN, PLUS, MINUS, MULTIPLY, DIVIDE, POWER
DOT_EQ, DOT_NE, DOT_LT, DOT_LE, DOT_GT, DOT_GE
```

**Fortran90Lexer** adds F90-specific tokens:
```antlr
// F90 revolutionary features
MODULE, USE, INTERFACE, TYPE, ALLOCATABLE, POINTER
SELECT, CASE, WHERE, RECURSIVE, PURE

// F90 data types
DOUBLE, PRECISION, COMPLEX

// F90 operators
DOUBLE_COLON ('::'), POINTER_ASSIGN ('=>'), PERCENT ('%')
```

**Fortran95Lexer** adds F95 enhancements:
```antlr  
// F95 additions (on top of F90)
FORALL, END_FORALL
CEILING_INTRINSIC, FLOOR_INTRINSIC, MODULO_INTRINSIC
BIT_SIZE_INTRINSIC, TRANSFER_INTRINSIC
CPU_TIME_INTRINSIC, SYSTEM_CLOCK_INTRINSIC
```

### Parser Rule Organization

**SharedCoreParser** defines fundamental constructs:
- Program units, procedures, statements
- Expressions, operators, literals  
- Control flow (IF, DO, GOTO)

**Fortran90Parser** extends with F90 features:
- Module system, interfaces, derived types
- Dynamic arrays, pointers, allocatable arrays
- Enhanced control flow (SELECT CASE, WHERE)
- Array operations and intrinsics

**Fortran95Parser** adds F95 enhancements:
- FORALL constructs for parallel array operations
- Enhanced WHERE with multiple ELSEWHERE blocks  
- PURE/ELEMENTAL procedure enhancements

## Benefits

### 1. **Architectural Elegance**
- Single grammar per standard vs. complex dual-parser systems
- Clean inheritance eliminates code duplication
- Unified approach simplifies maintenance

### 2. **Complete Compatibility**  
- **Backward Compatible**: F90+ supports all F77 constructs through inheritance
- **Forward Compatible**: Clean extension points for modern standards
- **Mixed Codebases**: Seamless support for projects using both `.f` and `.f90` files

### 3. **Extensibility**
The architecture provides a **proven foundation** for implementing:
- **F2003**: Object-oriented programming, C interoperability
- **F2008**: Coarrays, submodules, DO CONCURRENT  
- **F2018**: Teams, events, atomic operations
- **F2023**: Generics, conditional expressions
- **LazyFortran2025**: Type inference extensions

## Usage Guidelines

### Building Grammars

```bash
# Build in dependency order
./scripts/build_grammar.sh SharedCore
./scripts/build_grammar.sh Fortran90  
./scripts/build_grammar.sh Fortran95
```

### Using the Parser

```python
# For .f90 files (free-form)
lexer = Fortran90Lexer(InputStream(fortran_code))
parser = Fortran90Parser(CommonTokenStream(lexer))
tree = parser.program_unit_f90()

# For .f files (fixed-form) - same parser!
lexer = Fortran90Lexer(InputStream(fixed_form_code))
parser = Fortran90Parser(CommonTokenStream(lexer))  
tree = parser.program_unit_f90()
```

### Error Handling

The unified architecture provides **consistent error reporting** across formats:
- **Lexical errors**: Invalid tokens, malformed literals
- **Syntax errors**: Grammar rule violations, missing constructs
- **Format errors**: Mixed format usage or layout issues within the
  **implemented subset** of fixed-form support (see
  `docs/fixed_form_support.md` for details). Strict column‑violation
  checks are not currently implemented and are tracked by Issue #91.

## Performance Considerations

### Parsing Efficiency
- **Single pass**: Unified lexer eliminates format detection overhead
- **Optimized tokens**: Prioritized token ordering (FREE_FORM_COMMENT first)
- **Inherited rules**: No rule duplication across standards

### Memory Usage
- **Shared token vocabulary**: Common tokens inherited, not duplicated
- **Incremental parsing**: Each standard adds minimal overhead
- **Efficient AST**: Clean inheritance reduces node complexity

## Testing Strategy

### Comprehensive Coverage
- **Real Fortran code**: Scientific applications, legacy codebases
- **Format validation**: Mixed fixed/free-form scenarios  
- **Standard compliance**: Each F90/F95 feature validated
- **Error scenarios**: Invalid syntax properly rejected
- **Performance tests**: Large file parsing benchmarks

### Regression Testing
- **Backward compatibility**: F77 constructs through F90 inheritance
- **Cross-standard**: F95 imports F90, F2003 imports F95
- **Format consistency**: Same code parses identically in both formats

## Future Extensions

### LazyFortran2025 Vision
The unified architecture enables **type inference extensions**:

```fortran
! LazyFortran2025 with type inference
auto x = 42        ! inferred: integer
auto y = 3.14      ! inferred: real  
auto z = [1,2,3]   ! inferred: integer array

! Still fully compatible with explicit typing
integer :: explicit_int = 42
```

### Implementation Path
1. **F2003**: Object-oriented features (complete inheritance chain)
2. **F2008-F2023**: Modern features (parallel constructs, generics)
3. **LazyFortran2025**: Type inference (minimal grammar extensions)

The **unified architecture** provides the solid foundation needed to implement these advanced features while maintaining **complete backward compatibility** with all existing Fortran code.

---

*This architecture represents a paradigm shift in Fortran parser design, providing elegance, performance, and extensibility for the next generation of Fortran development.*
