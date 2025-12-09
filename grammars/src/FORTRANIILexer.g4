/*
 * FORTRANIILexer.g4
 *
 * FORTRAN II (1958) - Procedural Programming Revolution
 * Introduces separately compiled subroutines and functions.
 *
 * This lexer extends FORTRANLexer (1957) with the six new FORTRAN II
 * statement/subprogram forms: SUBROUTINE, FUNCTION, CALL, RETURN,
 * COMMON, and END (as a formal terminator).
 *
 * Reference: IBM FORTRAN II for the IBM 704 Data Processing System
 *            (Form C28-6000-2, 1958)
 *            - Part I, Chapter 1: Introduction and Types of Statements
 *            - Part I, Chapter 3: The New FORTRAN II Statements
 *            - Appendix A: Summary of FORTRAN II Statements
 *
 * Manual available at: Computer History Museum archive
 *   https://archive.computerhistory.org/resources/text/Fortran/
 */

lexer grammar FORTRANIILexer;

import FORTRANLexer;  // Import FORTRAN I (1957) constructs (C28-6003)

// ============================================================================
// FORTRAN II (1958) NEW KEYWORDS
// C28-6000-2 Part I, Chapter 3: The New FORTRAN II Statements
// ============================================================================
// FORTRAN II adds six new statement/subprogram forms to the original 1957
// FORTRAN language. These enable separate compilation of subroutines and
// functions, shared storage via COMMON, and explicit subprogram termination.

// CALL statement - C28-6000-2 Part I, Chapter 3, Section 3.1
// Transfers control to a subroutine with optional arguments
CALL         : C A L L ;

// SUBROUTINE definition - C28-6000-2 Part I, Chapter 3, Section 3.2
// Defines a separately compiled subroutine subprogram
SUBROUTINE   : S U B R O U T I N E ;

// FUNCTION definition - C28-6000-2 Part I, Chapter 3, Section 3.3
// Defines a separately compiled function subprogram
FUNCTION     : F U N C T I O N ;

// RETURN statement - C28-6000-2 Part I, Chapter 3, Section 3.4
// Returns control from a subroutine or function to the caller
RETURN       : R E T U R N ;

// COMMON statement - C28-6000-2 Part I, Chapter 3, Section 3.5
// Declares shared storage between program units
COMMON       : C O M M O N ;

// ============================================================================
// OPTIONAL FUNCTION RETURN TYPE SPECIFIERS
// C28-6000-2 Part I, Chapter 3, Section 3.3 (Function Subprograms)
// ============================================================================
// FORTRAN II allows optional type specification for function subprograms.
// The type (INTEGER or REAL) precedes the FUNCTION keyword in function definitions.
// Example: INTEGER FUNCTION factorial(n)
// Note: These are NOT used for variable declarations (which don't exist in F2)
//       They are only valid as function return type specifiers.
// These will be re-defined in FORTRAN 66 lexer for type declarations.

// INTEGER type - for function return type specification - C28-6000-2 Section 3.3
INTEGER      : I N T E G E R ;

// REAL type - for function return type specification - C28-6000-2 Section 3.3
REAL         : R E A L ;

// ============================================================================
// STATEMENT LABELS
// C28-6000-2 Part I, Chapter 2: Coding for FORTRAN II (columns 1-5)
// ============================================================================
// Statement labels are 1-5 digit integers (1-99999) placed in columns 1-5
LABEL : [1-9] ([0-9] ([0-9] ([0-9] [0-9]?)?)?)? ;

// Note: HOLLERITH constants are inherited from FORTRANLexer (1957).
// Per IBM 704 manual C28-6003, Hollerith was present in original FORTRAN.

// ============================================================================
// SOURCE FORMAT
// C28-6000-2 Part I, Chapter 2: Coding for FORTRAN II
// ============================================================================
// Newlines are significant (statement terminators in fixed-form source)
NEWLINE : '\r'? '\n' ;
