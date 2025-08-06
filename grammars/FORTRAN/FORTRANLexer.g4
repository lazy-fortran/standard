// FORTRAN 1957 HISTORICAL STUB - Original IBM 704 Language
// HISTORICAL DOCUMENTATION STUB - Compiles but provides minimal functionality
lexer grammar FORTRANLexer;

import SharedCoreLexer;  // Import universal constructs

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
// FORTRAN 1957-SPECIFIC TOKENS [DOCUMENTED STUB]
// ====================================================================

// HISTORICAL KEYWORDS UNIQUE TO 1957 (not in SharedCore)
//
// PAUSE - For operator intervention (removed in later FORTRAN versions)
//         Used to halt program execution and wait for operator action
//         Example: PAUSE 1234 (displays code 1234 and waits)
PAUSE : P A U S E ;

// RETURN - Return from functions and subroutines
//          Essential for subprogram control flow
RETURN : R E T U R N ;

// PRINT - Line printer output (1957 primary output device)
//         Example: PRINT 100, A, B, C (format 100, variables A, B, C)
PRINT : P R I N T ;

// PUNCH - Output to punch cards (1957 data storage method)
//         Example: PUNCH 200, X, Y (punch variables X, Y with format 200)
//         This was how programs created data files in 1957!
PUNCH : P U N C H ;

// FORMAT - Format specifications for I/O operations
//          Example: 100 FORMAT (I5, F10.2, E15.6)
//          Revolutionary feature allowing formatted input/output
FORMAT : F O R M A T ;

// DIMENSION - Array dimension declarations
//             Example: DIMENSION A(100), B(10,20)
//             Arrays were revolutionary in 1957 high-level languages
DIMENSION : D I M E N S I O N ;

// EQUIVALENCE - Memory sharing between variables
//               Example: EQUIVALENCE (A, B(1)), (C, D(5))
//               Used to overlay variables in memory (memory was precious!)
EQUIVALENCE : E Q U I V A L E N C E ;

// FREQUENCY - Optimization hint unique to 1957 FORTRAN
//             Example: FREQUENCY 10 (25, 3, 1)
//             Told compiler how often statement 10 executed for optimization
//             This feature was removed in later FORTRAN versions
FREQUENCY : F R E Q U E N C Y ;

// COMMON - Shared storage between program units
//          Example: COMMON A, B, C
//          Essential for sharing data between main program and subprograms
COMMON : C O M M O N ;

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
LABEL : [1-9] [0-9]? [0-9]? [0-9]? [0-9]? ;

// Hollerith constants (1957 string literals)
// Format: nHcharacters where n = number of characters following H
// Example: 5HHELLO represents the string literal "HELLO"
// This was the ONLY way to represent text in 1957 FORTRAN!
// Usage: FORMAT statements, debugging output, data initialization
HOLLERITH : [1-9] [0-9]* H ~[\r\n]+ ;

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