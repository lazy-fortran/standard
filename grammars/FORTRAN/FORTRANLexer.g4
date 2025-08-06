// FORTRAN Lexer - Original IBM 704 Language (1957)
// The first high-level programming language for scientific computation
lexer grammar FORTRANLexer;

import SharedCoreLexer;  // Import universal constructs

// ====================================================================
// FORTRAN 1957 HISTORICAL DOCUMENTATION
// ====================================================================
// This grammar represents FORTRAN as it was in 1957 for the IBM 704
// Based on the Preliminary Operator's Manual from April 1957
// 
// Key characteristics:
// - Fixed-form source (columns 1-72 of punch cards)
// - Column 1-5: Statement labels
// - Column 6: Continuation character
// - Column 7-72: Statement text
// - Only uppercase letters
// - Limited keywords (very small language)
// - Arithmetic expressions with +, -, *, /, ** (yes, ** was in 1957!)
// ====================================================================

// ====================================================================
// FORTRAN 1957-SPECIFIC KEYWORDS (not in SharedCore)
// ====================================================================

// Keywords unique to 1957 (not universal)
PAUSE    : P A U S E ;        // PAUSE for operator intervention (removed in later standards)
RETURN   : R E T U R N ;      // RETURN from functions

// I/O statements specific to 1957
PRINT    : P R I N T ;        // Line printer output
PUNCH    : P U N C H ;        // Output to punch cards (1957 only!)
FORMAT   : F O R M A T ;      // FORMAT statements

// Declaration keywords (1957)
DIMENSION : D I M E N S I O N ; // Array dimensions
EQUIVALENCE : E Q U I V A L E N C E ; // Memory sharing
FREQUENCY : F R E Q U E N C Y ; // Optimization hint (unique to 1957!)
COMMON   : C O M M O N ;       // Shared storage

// Note: All basic keywords (IF, GOTO, DO, END, CONTINUE, STOP, READ, WRITE)
// are inherited from SharedCoreLexer

// Note: All operators (+, -, *, /, **, =) are inherited from SharedCoreLexer

// Note: All delimiters (parentheses, comma) are inherited from SharedCoreLexer

// ====================================================================
// FORTRAN 1957-SPECIFIC TOKENS
// ====================================================================

// Note: INTEGER_LITERAL and REAL_LITERAL inherited from SharedCore

// Identifiers in 1957 had stricter rules:
// - Maximum 6 characters (override SharedCore's unlimited length)
// - No underscores allowed (SharedCore allows them)
// This would need special handling in the parser

// Statement labels (FORTRAN - more restrictive than later standards)
LABEL : [1-9] [0-9]? [0-9]? [0-9]? [0-9]? ; // 1-5 digits, can't start with 0

// Hollerith constants (string literals in 1957)
// Format: nHtext where n is the number of characters
// Example: 5HHELLO represents the string "HELLO"
HOLLERITH : [1-9] [0-9]* H ~[\r\n]+ ;  // Simplified - needs semantic validation

// Fixed-form specific handling
NEWLINE : '\r'? '\n' ;  // Newlines are significant in fixed-form

// Comments in 1957 were indicated by 'C' in column 1
// This requires special fixed-form parsing, not handled in lexer

// Note: Case-insensitive fragments inherited from SharedCore