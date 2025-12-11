// FORTRAN 77 Lexer - ISO 1539:1980 / ANSI X3.9-1978
// Reference: ISO 1539:1980 (also published as ANSI X3.9-1978)
//
// This lexer implements tokens for FORTRAN 77 as defined in the ISO/ANSI
// standard. Section references in this file use the ISO 1539:1980 numbering.
//
// Standard Structure Overview:
//   Section 1: Scope
//   Section 2: Processor Characteristics
//   Section 3: Program Form and Source Format (3.1-3.5)
//   Section 4: Data Types and Constants (4.1-4.8)
//   Section 5: Arrays and Substrings (5.1-5.7)
//   Section 6: Expressions (6.1-6.7)
//   Section 7: Executable and Nonexecutable Statements (7.1-7.2)
//   Section 8: Specification Statements (8.1-8.9)
//   Section 9: DATA Statement (9.1-9.3)
//   Section 10: Assignment Statements (10.1-10.4)
//   Section 11: Control Statements (11.1-11.12)
//   Section 12: Input/Output Statements (12.1-12.10)
//   Section 13: Format Specification (13.1-13.5)
//   Section 14: Main Program (14.1)
//   Section 15: Functions and Subroutines (15.1-15.10)
//   Section 16: Block Data Subprogram (16.1-16.3)
//   Section 17: Association (17.1-17.4)
//   Section 18: Scope and Definition (18.1-18.3)
lexer grammar FORTRAN77Lexer;

import FORTRAN66Lexer;  // Import FORTRAN 66 (1966) constructs

// ====================================================================
// FORTRAN 77 (ISO 1539:1980) HISTORICAL OVERVIEW
// ====================================================================
//
// FORTRAN 77 was a revolutionary update that added structured programming:
// - CHARACTER data type with string processing (Section 4.8)
// - Block IF-THEN-ELSE-ENDIF constructs (Section 11.6-11.9)
// - PARAMETER statement for named constants (Section 8.6)
// - PROGRAM statement for main program units (Section 14.1)
// - SAVE statement for persistent local variables (Section 8.7)
// - Enhanced I/O: list-directed, OPEN/CLOSE/INQUIRE (Sections 12.4, 12.10)
//
// Historical Context:
// - ISO 1539:1980 (also ANSI X3.9-1978)
// - Last version requiring uppercase character set (Section 3.1)
// - Dominated scientific computing for decades
//

// ====================================================================
// FORTRAN 77 KEYWORDS - ISO 1539:1980 Section References
// ====================================================================

// --------------------------------------------------------------------
// Program Units - ISO 1539:1980 Sections 14-16
// --------------------------------------------------------------------
// PROGRAM statement (Section 14.1): Identifies the main program unit
// Syntax: PROGRAM program-name
PROGRAM         : P R O G R A M ;

// ENTRY statement (Section 15.7): Alternate entry point in subprograms
// Syntax: ENTRY entry-name [ ( dummy-arg-list ) ]
ENTRY           : E N T R Y ;

// --------------------------------------------------------------------
// CHARACTER Data Type - ISO 1539:1980 Section 4.8
// --------------------------------------------------------------------
// CHARACTER type (Section 4.8.1): String data type for text processing
// Section 4.8.2 defines character constants (literals)
CHARACTER       : C H A R A C T E R ;

// --------------------------------------------------------------------
// Block IF Construct - ISO 1539:1980 Sections 11.6-11.9
// --------------------------------------------------------------------
// Block IF provides structured conditional execution:
// - IF statement (Section 11.6): IF (logical-expr) THEN
// - ELSE IF statement (Section 11.7): ELSE IF (logical-expr) THEN
// - ELSE statement (Section 11.8): ELSE
// - END IF statement (Section 11.9): END IF
THEN            : T H E N ;
ELSE            : E L S E ;
ENDIF           : E N D I F ;

// --------------------------------------------------------------------
// Specification Statements - ISO 1539:1980 Section 8
// --------------------------------------------------------------------
// PARAMETER statement (Section 8.6): Named constants
// Syntax: PARAMETER (name = constant-expr, ...)
PARAMETER       : P A R A M E T E R ;

// SAVE statement (Section 8.7): Variable persistence across calls
// Syntax: SAVE [ save-list ]
SAVE            : S A V E ;

// DATA statement (Section 9): Compile-time initialization
// Inherited from FORTRAN 66 but enhanced syntax
DATA            : D A T A ;

// EXTERNAL statement (Section 8.8): External procedure declaration
// Inherited from FORTRAN 66, Section 7.2.4
EXTERNAL        : E X T E R N A L ;

// INTRINSIC statement (Section 8.9): Intrinsic function declaration
// Syntax: INTRINSIC name-list
INTRINSIC       : I N T R I N S I C ;


// --------------------------------------------------------------------
// File I/O Statements - ISO 1539:1980 Section 12.10
// --------------------------------------------------------------------
// OPEN statement (Section 12.10.1): Connect file to unit
// CLOSE statement (Section 12.10.2): Disconnect file from unit
// INQUIRE statement (Section 12.10.3): Query file/unit properties
// Note: REWIND, BACKSPACE, ENDFILE inherited from FORTRAN 66 (Section 12.9)
OPEN            : O P E N ;
CLOSE           : C L O S E ;
INQUIRE         : I N Q U I R E ;

// ====================================================================
// CHARACTER EXPRESSIONS - ISO 1539:1980 Section 6.2
// ====================================================================

// --------------------------------------------------------------------
// String Concatenation Operator - ISO 1539:1980 Section 6.2.2
// --------------------------------------------------------------------
// Concatenation operator (//): Joins two character operands
// Result length is sum of operand lengths
CONCAT          : '//' ;

// Colon operator - ISO 1539:1980 Section 5.7
// Used in substring notation: NAME(start:end), NAMES(I)(1:5)
COLON           : ':' ;

// --------------------------------------------------------------------
// Character Constants - ISO 1539:1980 Section 4.8.2
// --------------------------------------------------------------------
// Character constant: sequence of characters enclosed in apostrophes
// Embedded apostrophes represented by two consecutive apostrophes
STRING_LITERAL  : '\'' (~'\'' | '\'\'')* '\'' ;

// ====================================================================
// IDENTIFIER LENGTH EXTENSION - ISO 1539:1980 Section 2.3.3
// ====================================================================
// ISO 1539:1980 (FORTRAN 77) relaxed the 6-character restriction from
// FORTRAN 66 (X3.9-1966 Section 2.3) while still limiting names to
// a maximum of 31 characters. This overrides the shorter FORTRAN 66 rule
// to permit longer symbolic names required by ISO 1539:1980 Section 2.3.3.
//
// Per the standard: "A name has 1 to 31 characters."
// Compliance: STANDARD-COMPLIANT with ISO 1539:1980 Section 2.3.3
// Override: Extends the FORTRAN 66 IDENTIFIER token to accept up to 31
// characters (1 leading letter + 30 additional letters/digits/underscores).
//
// Implementation: Matches an alphabetic start followed by at most 30 trailing
// characters (letters, digits, or underscores) to enforce the 31-character limit.
IDENTIFIER
    : LETTER
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
      (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')? (LETTER | DIGIT | '_')?
    ;

// ====================================================================
// ISO 1539:1980 SPEC-GRAMMAR MAPPING (LEXER)
// ====================================================================
//
// This section summarizes the mapping from ISO 1539:1980 sections to
// lexer tokens defined in this file.
//
// Section 3 (Source Form):
//   - Section 3.1: Character set -> inherited from FORTRAN66Lexer
//   - Section 3.2: Lines -> NEWLINE (inherited)
//   - Section 3.4: Statement labels -> LABEL (inherited)
//
// Section 4 (Data Types and Constants):
//   - Section 4.1-4.7: Inherited types (INTEGER, REAL, etc.)
//   - Section 4.8: CHARACTER type -> CHARACTER token
//   - Section 4.8.2: Character constants -> STRING_LITERAL token
//
// Section 6 (Expressions):
//   - Section 6.2.2: Concatenation operator -> CONCAT token
//
// Section 8 (Specification Statements):
//   - Section 8.6: PARAMETER statement -> PARAMETER token
//   - Section 8.7: SAVE statement -> SAVE token
//   - Section 8.8: EXTERNAL statement -> EXTERNAL token (inherited)
//   - Section 8.9: INTRINSIC statement -> INTRINSIC token
//
// Section 9 (DATA Statement):
//   - Section 9.1: DATA statement -> DATA token
//
// Section 11 (Control Statements):
//   - Section 11.6-11.9: Block IF -> THEN, ELSE, ENDIF tokens (ELSE IF is two tokens)
//
// Section 12 (I/O Statements):
//   - Section 12.10.1: OPEN statement -> OPEN token
//   - Section 12.10.2: CLOSE statement -> CLOSE token
//   - Section 12.10.3: INQUIRE statement -> INQUIRE token
//
// Section 14 (Main Program):
//   - Section 14.1: PROGRAM statement -> PROGRAM token
//
// Section 15 (Subprograms):
//   - Section 15.7: ENTRY statement -> ENTRY token
//
// ====================================================================
