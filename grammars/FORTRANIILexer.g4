// FORTRAN II (1958) - Procedural Programming Revolution
// Adds user-written subroutines and functions to FORTRAN I (1957)
lexer grammar FORTRANIILexer;

import FORTRANLexer;  // Import FORTRAN I (1957) constructs

// ====================================================================
// FORTRAN 1957 HISTORICAL DOCUMENTATION [COMPREHENSIVE STUB]
// ====================================================================
// 
// This is a HISTORICAL STUB documenting the original 1957 FORTRAN language
// developed for IBM 704 computers by John Backus and his team at IBM.
//
// REVOLUTIONARY SIGNIFICANCE (1957):
// - First high-level programming language for scientific computation
// - Mathematical Formula Translation: "FORTRAN" = "FORmula TRANslation"
// - Paradigm shift from machine code/assembly to mathematical expressions
// - Foundation for all subsequent FORTRAN/Fortran development
// - Made programming accessible to scientists and engineers worldwide
//
// TECHNICAL CONTEXT (IBM 704 Era):
// - Punch card input: 80-column fixed-format source code
// - Limited memory: Severe constraints on program size and complexity
// - Magnetic drum storage: Early form of secondary storage
// - Vacuum tube technology: Required robust software design
// - Batch processing: No interactive computing capabilities
//
// PUNCH CARD FORMAT (1957 Fixed-Form):
// - Columns 1-5:   Statement labels (numeric, 1-99999)
// - Column 6:      Continuation character (any non-zero, non-space)
// - Columns 7-72:  Statement text (FORTRAN code)
// - Columns 73-80: Sequence numbers (ignored by compiler)
// - Comments:      'C' in column 1 makes entire line a comment
// - Case:          Only uppercase letters supported
//
// LANGUAGE CHARACTERISTICS (1957):
// - Variable names: Maximum 6 characters, start with letter, uppercase only
// - No underscores: Variable names were simple alphanumeric only
// - Data types: Only INTEGER and REAL (no CHARACTER, LOGICAL, COMPLEX)
// - Arrays: Multi-dimensional arrays with subscripts (revolutionary!)
// - Expressions: Full arithmetic with +, -, *, /, ** (exponentiation)
// - Control flow: GOTO, arithmetic IF, DO loops with labels
// - I/O: READ (cards/tape), PRINT (line printer), PUNCH (card punch)
// - Subprograms: Basic FUNCTION definitions and CALL statements
//
// ====================================================================

// ====================================================================
// FORTRAN II (1958) FEATURES - Independent Compilation Revolution
// ====================================================================
//
// FORTRAN II (1958) major innovation:
// - Independent compilation of subroutines (vs monolithic in FORTRAN I)
// - Improved COMMON and EQUIVALENCE statement implementation
// - Bug fixes and enhanced reliability over FORTRAN I
//
// Note: CALL, SUBROUTINE, FUNCTION, RETURN, COMMON were present in FORTRAN I
// but FORTRAN II enabled separate compilation, making modular programming practical.
//
// FORTRAN II added no major new language features - its achievement was
// engineering: reliable separate compilation and improved implementation.

// ====================================================================
// FORTRAN 1957 ORIGINAL FEATURES (via SharedCoreLexer)
// ====================================================================
// The following are inherited from SharedCoreLexer:
// - IF, GOTO, DO, END, CONTINUE, STOP (control flow)
// - READ, WRITE (I/O operations)  
// - INTEGER, REAL (data types)
// - Arithmetic operators: +, -, *, /, **
// - Comparison operators, delimiters, literals

// HISTORICAL KEYWORDS UNIQUE TO 1957 (not in SharedCore)
//
// Note: FORTRAN I (1957) features now inherited from SharedCoreLexer:
// PAUSE, PRINT, PUNCH, FORMAT, DIMENSION, EQUIVALENCE, FREQUENCY

// Note: Core keywords inherited from SharedCoreLexer:
//       IF, GOTO, DO, END, CONTINUE, STOP, READ, WRITE, INTEGER, REAL
//       All arithmetic operators: +, -, *, /, **, =
//       All delimiters: parentheses, comma
//       Literals: INTEGER_LITERAL, REAL_LITERAL, IDENTIFIER

// ====================================================================
// HISTORICAL 1957-SPECIFIC TOKEN PATTERNS [DOCUMENTED STUB]
// ====================================================================

// Statement labels (1957): 1-5 digits, cannot start with 0
// Used extensively for GOTO targets and DO loop termination
// Example: 100 CONTINUE
// Range: 1-99999 (exactly as specified in 1957 FORTRAN manual)
LABEL : [1-9] ([0-9] ([0-9] ([0-9] [0-9]?)?)?)? ;

// Hollerith constants (1957 string literals)
// Format: nHcharacters where n = number of characters following H
// Example: 5HHELLO represents the string literal "HELLO"
// This was the ONLY way to represent text in 1957 FORTRAN!
// Usage: FORMAT statements, debugging output, data initialization
// Improved pattern: count must match actual character length (semantic validation needed)
HOLLERITH : [1-9] [0-9]* H ~[\r\n]*? ;

// Fixed-form newlines (significant in punch card format)
NEWLINE : '\r'? '\n' ;

// ====================================================================
// STUB IMPLEMENTATION STATUS
// ====================================================================
// 
// CURRENT STATUS: Historical documentation complete, minimal functionality
// COMPILATION: Compiles with ANTLR4, integrates with SharedCoreLexer
// FUNCTIONALITY: Basic token recognition only, no semantic validation
//
// PHASE 1 (CURRENT): Historical documentation and compilation stub
// PHASE 2 (FUTURE): Full fixed-form parser with semantic validation  
// PHASE 3 (FUTURE): Historical code compatibility and educational tools
//
// This stub serves as the foundation for FORTRAN II (1958) extension
// and preserves the complete historical record of 1957 FORTRAN language.
// ====================================================================