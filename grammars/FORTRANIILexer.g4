// FORTRAN II (1958) - Procedural Programming Revolution
// Introduces separately compiled subroutines and functions
lexer grammar FORTRANIILexer;

import FORTRANLexer;  // Import FORTRAN I (1957) constructs

// ====================================================================
// FORTRAN II (1958) NEW FEATURES
// ====================================================================

// Procedural Programming and shared storage (NEW in FORTRAN II, 1958)
CALL         : C A L L ;             // Subroutine call
SUBROUTINE   : S U B R O U T I N E ; // Subroutine definition
FUNCTION     : F U N C T I O N ;     // Function definition  
RETURN       : R E T U R N ;         // Return from subprogram
COMMON       : C O M M O N ;         // Shared variable storage (COMMON blocks)

// Statement labels: 1-5 digits, cannot start with 0
LABEL : [1-9] ([0-9] ([0-9] ([0-9] [0-9]?)?)?)? ;

// Hollerith constants (string literals)
// Format: nHcharacters where n = number of characters following H
HOLLERITH : [1-9] [0-9]* H ~[\r\n]*? ;

// Fixed-form newlines
NEWLINE : '\r'? '\n' ;
