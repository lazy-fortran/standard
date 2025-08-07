# FORTRAN (1957) - Historical Documentation Stub

## Overview

This directory contains a **HISTORICAL STUB** implementation of the original 1957 FORTRAN language developed for IBM 704 computers by John Backus and his team at IBM. This is the world's first high-level programming language for scientific computation.

## Implementation Status: HISTORICAL STUB

### Current Phase: Documentation and Compilation Stub
- ✅ **Comprehensive historical documentation**: Complete record of 1957 FORTRAN features
- ✅ **ANTLR4 compilation**: Grammar compiles and integrates with SharedCoreLexer/Parser
- ✅ **Basic syntax recognition**: Minimal parsing functionality for grammar validation
- ✅ **Educational resource**: Detailed explanations of revolutionary 1957 features

### Future Phases (Low Priority)
- **Phase 2**: Full fixed-form parser with semantic validation
- **Phase 3**: Historical code compatibility and educational tools

## Historical Significance

### Revolutionary Achievement (1957)
The original FORTRAN was a groundbreaking achievement that:

- **First high-level language**: Made programming accessible to scientists and engineers
- **Mathematical notation**: "FORTRAN" = "FORmula TRANslation" - natural mathematical syntax
- **Compiler efficiency**: Proved high-level languages could match hand-coded assembly
- **Industry transformation**: Created the software industry and modern programming
- **18 person-years**: Massive engineering effort (1954-1957) for its time

### Technical Innovation (IBM 704 Era)
- **Punch card format**: 80-column fixed-form source code input
- **Memory constraints**: Compiler + runtime fit in 32K of IBM 704 memory
- **Optimization**: Often produced better code than human assembly programmers
- **Batch processing**: No interactive computing - pure batch environment
- **Vacuum tubes**: Unreliable hardware required robust software design

## Language Features (1957)

### Core Language Elements
```fortran
C     FORTRAN 1957 EXAMPLE - QUADRATIC EQUATION SOLVER
      READ 100, A, B, C
      DISC = B*B - 4.0*A*C
      IF (DISC) 10, 20, 30
10    PRINT 200
      GO TO 40
20    ROOT = -B/(2.0*A)
      PRINT 300, ROOT
      GO TO 40
30    SQRT_DISC = SQRT(DISC)
      ROOT1 = (-B + SQRT_DISC)/(2.0*A)
      ROOT2 = (-B - SQRT_DISC)/(2.0*A)
      PRINT 400, ROOT1, ROOT2
40    STOP
100   FORMAT (3F10.2)
200   FORMAT (12HNO REAL ROOTS)
300   FORMAT (11HSINGLE ROOT, F10.4)
400   FORMAT (10HTWO ROOTS:, 2F10.4)
```

### Revolutionary Features
- **Mathematical expressions**: Natural notation `C = A + B` (revolutionary in 1957!)
- **Variable names**: Up to 6 characters, implicit typing (I-N integer, else real)
- **Data types**: INTEGER and REAL only (no CHARACTER, LOGICAL, COMPLEX)
- **Arrays**: Multi-dimensional arrays with subscripts `A(I,J,K)`
- **Operators**: Full arithmetic including exponentiation `**` (from day one!)

### Control Flow (1957 Style)
- **Arithmetic IF**: Three-way branch `IF (X-Y) 10, 20, 30`
- **GOTO statements**: Unrestricted jumping `GO TO 100`
- **Computed GOTO**: Multi-way branch `GO TO (10, 20, 30), I`
- **DO loops**: Counted iteration `DO 100 I = 1, 10, 2`
- **Labels**: Primary control mechanism (1-99999)

### I/O Operations (Batch Era)
- **READ**: Punch card and magnetic tape input
- **PRINT**: Line printer output (primary output device)
- **PUNCH**: Card punch output (data storage method!)
- **FORMAT**: Precise I/O formatting `100 FORMAT (I5, F10.2, 5HHELLO)`

### Unique 1957 Features
- **PAUSE statement**: Operator intervention `PAUSE 1234`
- **FREQUENCY statement**: Optimization hints `FREQUENCY 10 (25, 3, 1)`
- **Hollerith constants**: String literals `5HHELLO`
- **EQUIVALENCE**: Memory overlay `EQUIVALENCE (A, B(1))`

## Punch Card Format (Fixed-Form)

```
Columns:  1    6    7                                                72    80
         |    |    |                                                |    |
Content: Label Cont  FORTRAN Statement Text                         Seq#
Example: 100   C     THIS IS A COMMENT                              001234
         200        A = B + C                                       001235  
         201     +     * D                                          001236
```

- **Columns 1-5**: Statement labels (1-99999)
- **Column 6**: Continuation character (any non-zero, non-space)
- **Columns 7-72**: FORTRAN statement text
- **Columns 73-80**: Sequence numbers (ignored by compiler)
- **Comments**: 'C' in column 1

## File Structure

```
grammars/FORTRAN/
├── FORTRANLexer.g4      # Historical token definitions
├── FORTRANParser.g4     # Historical grammar rules  
└── README.md           # This file
```

### Generated Files (build/FORTRAN/)
- `FORTRANLexer.py` - Generated lexer (do not edit manually)
- `FORTRANParser.py` - Generated parser (do not edit manually)
- `*.tokens` - Token definitions (generated)

## Usage

### Building the Grammar
```bash
./scripts/build_grammar.sh FORTRAN
```

### Integration with Shared Core
The FORTRAN grammar imports universal constructs:
```antlr
import SharedCoreLexer;   // Basic tokens and operators
import SharedCoreParser;  // Expression parsing and universal rules
```

## Historical Context

### Computing Environment (1957)
- **IBM 704**: 36-bit word, magnetic drum storage, vacuum tube technology
- **Memory**: 4K-32K words (expensive and limited)
- **Input**: 80-column punch cards (batch processing only)
- **Output**: Line printer, card punch for data storage
- **No terminals**: No interactive computing capabilities

### Programming Before FORTRAN
Before 1957, scientific programmers wrote:
```assembly
CLA A        # Clear and add A
ADD B        # Add B to accumulator
STO C        # Store result in C
```

FORTRAN allowed the revolutionary notation:
```fortran
C = A + B
```

### Impact on Computer Science
- **Language design**: Established patterns for all subsequent languages
- **Compiler technology**: First optimizing compiler
- **Software industry**: Created the concept of portable software
- **Scientific computing**: Made programming accessible to scientists
- **Education**: Became the foundation for computer science education

## Educational Value

### Learning Objectives
This historical stub serves as an educational resource for:

1. **Computer science history**: Understanding programming language evolution
2. **Language design**: Learning from the first high-level language decisions
3. **Compiler construction**: Studying pioneering optimization techniques
4. **Software engineering**: Appreciating modern programming conveniences

### Research Applications
- **Historical programming language studies**
- **Computer science museum exhibits**
- **Academic courses on programming language evolution**
- **Documentation of computing history**

## Future Development

### Phase 2: Full Implementation (Future)
When historical completeness becomes important:
- Complete fixed-form lexer for punch card format
- Full semantic validation and type checking
- Column-sensitive parsing (1-5 labels, 6 continuation, 7-72 code)
- Historical accuracy validation against IBM 704 specifications

### Phase 3: Educational Tools (Future)
- Interactive 1957 FORTRAN interpreter
- Historical code examples and tutorials
- Integration with computer science museums
- Educational programming environment

## Relationship to Modern Standards

### Inheritance Chain
```
FORTRAN (1957) → FORTRAN II (1958) → FORTRAN IV (1962) → FORTRAN 66 → FORTRAN 77
                                                                          ↓
                                              Fortran 90 ← ← ← ← ← ← ← ← ←
```

### Foundation for LazyFortran2025
This historical stub:
- Documents the complete evolutionary path
- Preserves the mathematical focus of FORTRAN
- Provides foundation for FORTRAN II extension
- Serves as educational contrast to modern Fortran features

## Contributing

### Historical Accuracy
Improvements to historical accuracy are welcome:
- Original IBM 704 FORTRAN manual references
- Historical code examples and their sources  
- Corrections based on computer science historical research
- Documentation of features unique to specific time periods

### Phase 2/3 Implementation
Full implementation contributions should:
- Maintain historical accuracy as primary goal
- Include comprehensive test suites
- Document sources for all historical claims
- Provide educational value for computer science students

## References

### Primary Sources
- IBM 704 FORTRAN Preliminary Operator's Manual (April 1957)
- IBM 704 FORTRAN Programmer's Reference Manual
- Original IBM FORTRAN development team publications

### Historical Research
- John Backus and the FORTRAN development team
- IBM corporate archives and technical reports
- Computer History Museum collections
- ACM and IEEE historical publications

---

**Note**: This is a HISTORICAL STUB providing comprehensive documentation with minimal functionality. Full implementation is deferred to future phases when historical completeness becomes a priority.