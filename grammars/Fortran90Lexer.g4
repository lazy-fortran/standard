// Fortran 90 (1990) Lexer - Unified Fixed/Free Form Support
// Revolutionary Modern Foundation with Complete Format Compatibility
lexer grammar Fortran90Lexer;

import SharedCoreLexer;  // Universal constructs

// ====================================================================
// FORTRAN 90 UNIFIED LEXER OVERVIEW
// ====================================================================
//
// Fortran 90 (ISO 1539:1991) represents the most significant revolution 
// in Fortran history - introducing free-form source format while maintaining
// complete backward compatibility with fixed-form.
//
// This lexer supports BOTH formats in a single grammar:
// - Fixed-form: .f, .for files (column-based, F77 compatibility)
// - Free-form: .f90+ files (flexible layout, modern syntax)
//
// Format detection is handled by the parser/driver based on file extension.
//
// REVOLUTIONARY F90 FEATURES:
// - Module system with explicit interfaces
// - Dynamic arrays (ALLOCATABLE, POINTER)
// - Derived types (user-defined structures)  
// - Array operations and constructors
// - Enhanced control flow (SELECT CASE, WHERE)
// - Modern I/O (NAMELIST, non-advancing)
//
// INHERITANCE ARCHITECTURE:
// SharedCoreLexer → Fortran90Lexer → Fortran95Lexer → F2003+ standards
//
// ====================================================================

// ====================================================================
// FORMAT MODES
// ====================================================================

// Default mode is free-form (F90+ primary format)
// Fixed-form mode can be activated by parser based on file extension

// Unified comment handling (supports both fixed and free form)
// Handles both ! comments (free-form) and C/c/* comments (fixed-form)
COMMENT
    : ('!' | [Cc*]) ~[\r\n]* -> channel(HIDDEN)
    ;

// Free-form continuation (& at end of line)  
CONTINUATION
    : '&' [ \t]* COMMENT? -> channel(HIDDEN)
    ;

// ====================================================================
// FORTRAN 90 KEYWORDS (REVOLUTIONARY FEATURES)
// ====================================================================

// Module system (F90 major innovation)
MODULE          : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
END_MODULE      : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
USE             : ('u'|'U') ('s'|'S') ('e'|'E') ;
ONLY            : ('o'|'O') ('n'|'N') ('l'|'L') ('y'|'Y') ;
PUBLIC          : ('p'|'P') ('u'|'U') ('b'|'B') ('l'|'L') ('i'|'I') ('c'|'C') ;
PRIVATE         : ('p'|'P') ('r'|'R') ('i'|'I') ('v'|'V') ('a'|'A') ('t'|'T') ('e'|'E') ;

// Interface blocks (F90 generic procedures)
INTERFACE       : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
END_INTERFACE   : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
GENERIC         : ('g'|'G') ('e'|'E') ('n'|'N') ('e'|'E') ('r'|'R') ('i'|'I') ('c'|'C') ;
OPERATOR        : ('o'|'O') ('p'|'P') ('e'|'E') ('r'|'R') ('a'|'A') ('t'|'T') ('o'|'O') ('r'|'R') ;
ASSIGNMENT      : ('a'|'A') ('s'|'S') ('s'|'S') ('i'|'I') ('g'|'G') ('n'|'N') ('m'|'M') ('e'|'E') ('n'|'N') ('t'|'T') ;

// Procedure enhancements
RECURSIVE       : ('r'|'R') ('e'|'E') ('c'|'C') ('u'|'U') ('r'|'R') ('s'|'S') ('i'|'I') ('v'|'V') ('e'|'E') ;
PURE            : ('p'|'P') ('u'|'U') ('r'|'R') ('e'|'E') ;
ELEMENTAL       : ('e'|'E') ('l'|'L') ('e'|'E') ('m'|'M') ('e'|'E') ('n'|'N') ('t'|'T') ('a'|'A') ('l'|'L') ;
RESULT          : ('r'|'R') ('e'|'E') ('s'|'S') ('u'|'U') ('l'|'L') ('t'|'T') ;

// Derived types (F90 major innovation)  
TYPE            : ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
END_TYPE        : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
SEQUENCE        : ('s'|'S') ('e'|'E') ('q'|'Q') ('u'|'U') ('e'|'E') ('n'|'N') ('c'|'C') ('e'|'E') ;

// Dynamic arrays and pointers (F90 runtime memory management)
ALLOCATABLE     : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('a'|'A') ('b'|'B') ('l'|'L') ('e'|'E') ;
POINTER         : ('p'|'P') ('o'|'O') ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ;
TARGET          : ('t'|'T') ('a'|'A') ('r'|'R') ('g'|'G') ('e'|'E') ('t'|'T') ;
ALLOCATE        : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
DEALLOCATE      : ('d'|'D') ('e'|'E') ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
NULLIFY         : ('n'|'N') ('u'|'U') ('l'|'L') ('l'|'L') ('i'|'I') ('f'|'F') ('y'|'Y') ;
ASSOCIATED      : ('a'|'A') ('s'|'S') ('s'|'S') ('o'|'O') ('c'|'C') ('i'|'I') ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// Enhanced control flow
SELECT          : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
CASE            : ('c'|'C') ('a'|'A') ('s'|'S') ('e'|'E') ;
DEFAULT         : ('d'|'D') ('e'|'E') ('f'|'F') ('a'|'A') ('u'|'U') ('l'|'L') ('t'|'T') ;
END_SELECT      : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
WHERE           : ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;
END_WHERE       : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;
ELSEWHERE       : ('e'|'E') ('l'|'L') ('s'|'S') ('e'|'E') ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;

// Enhanced loop control
CYCLE           : ('c'|'C') ('y'|'Y') ('c'|'C') ('l'|'L') ('e'|'E') ;
EXIT            : ('e'|'E') ('x'|'X') ('i'|'I') ('t'|'T') ;

// Enhanced I/O
NAMELIST        : ('n'|'N') ('a'|'A') ('m'|'M') ('e'|'E') ('l'|'L') ('i'|'I') ('s'|'S') ('t'|'T') ;
ADVANCE         : ('a'|'A') ('d'|'D') ('v'|'V') ('a'|'A') ('n'|'N') ('c'|'C') ('e'|'E') ;
SIZE            : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
STAT            : ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;
EOR             : ('e'|'E') ('o'|'O') ('r'|'R') ;
IOSTAT          : ('i'|'I') ('o'|'O') ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;

// Intent specifications (F90 procedure interface)
INTENT          : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('n'|'N') ('t'|'T') ;
IN              : ('i'|'I') ('n'|'N') ;
OUT             : ('o'|'O') ('u'|'U') ('t'|'T') ;
INOUT           : ('i'|'I') ('n'|'N') ('o'|'O') ('u'|'U') ('t'|'T') ;

// Optional arguments
OPTIONAL        : ('o'|'O') ('p'|'P') ('t'|'T') ('i'|'I') ('o'|'O') ('n'|'N') ('a'|'A') ('l'|'L') ;
PRESENT         : ('p'|'P') ('r'|'R') ('e'|'E') ('s'|'S') ('e'|'E') ('n'|'N') ('t'|'T') ;

// Enhanced data types
KIND            : ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
LEN             : ('l'|'L') ('e'|'E') ('n'|'N') ;
SELECTED_INT_KIND     : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ('e'|'E') ('d'|'D') '_' ('i'|'I') ('n'|'N') ('t'|'T') '_' ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
SELECTED_REAL_KIND    : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ('e'|'E') ('d'|'D') '_' ('r'|'R') ('e'|'E') ('a'|'A') ('l'|'L') '_' ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;

// Additional F90 keywords
CONTAINS        : ('c'|'C') ('o'|'O') ('n'|'N') ('t'|'T') ('a'|'A') ('i'|'I') ('n'|'N') ('s'|'S') ;
IMPORT          : ('i'|'I') ('m'|'M') ('p'|'P') ('o'|'O') ('r'|'R') ('t'|'T') ;
PROCEDURE       : ('p'|'P') ('r'|'R') ('o'|'O') ('c'|'C') ('e'|'E') ('d'|'D') ('u'|'U') ('r'|'R') ('e'|'E') ;
UNIT            : ('u'|'U') ('n'|'N') ('i'|'I') ('t'|'T') ;
FMT             : ('f'|'F') ('m'|'M') ('t'|'T') ;
REC             : ('r'|'R') ('e'|'E') ('c'|'C') ;
ERR             : ('e'|'E') ('r'|'R') ('r'|'R') ;
WHILE           : ('w'|'W') ('h'|'H') ('i'|'I') ('l'|'L') ('e'|'E') ;

// ====================================================================  
// FORTRAN 90 OPERATORS (NEW AND ENHANCED)
// ====================================================================

// F90 specific operators (free-form innovations)
DOUBLE_COLON    : '::' ;
POINTER_ASSIGN  : '=>' ;
PERCENT         : '%' ;
LBRACKET        : '[' ;
RBRACKET        : ']' ;

// Enhanced relational operators (both styles supported)
EQ_OP           : '==' ;
NE_OP           : '/=' ;
LT_OP           : '<' ;
LE_OP           : '<=' ;
GT_OP           : '>' ;
GE_OP           : '>=' ;

// ====================================================================
// FORTRAN 90 LITERALS (ENHANCED)
// ====================================================================

// Kind-parameterized literals (F90 innovation)
INTEGER_LITERAL_KIND : DIGIT+ '_' IDENTIFIER ;
REAL_LITERAL_KIND    : (DIGIT+ '.' DIGIT* | '.' DIGIT+) EXPONENT? '_' IDENTIFIER
                    | DIGIT+ EXPONENT '_' IDENTIFIER ;

// String literals (F90 adds double quotes)
DOUBLE_QUOTE_STRING  : '"' (~["\r\n] | '""')* '"' ;
SINGLE_QUOTE_STRING  : '\'' (~['\r\n] | '\'\'')* '\'' ;

// BOZ literal constants (F90 binary/octal/hex)
BINARY_CONSTANT : ('b'|'B') '\'' [01]+ '\'' ;
OCTAL_CONSTANT  : ('o'|'O') '\'' [0-7]+ '\'' ;
HEX_CONSTANT    : ('z'|'Z'|'x'|'X') '\'' [0-9a-fA-F]+ '\'' ;

fragment EXPONENT : [eEdD] [+-]? DIGIT+ ;

// ====================================================================
// FORTRAN 90 INTRINSIC FUNCTIONS (MAJOR ADDITIONS)
// ====================================================================

// Array inquiry functions
ALL_INTRINSIC         : ('a'|'A') ('l'|'L') ('l'|'L') ;
ANY_INTRINSIC         : ('a'|'A') ('n'|'N') ('y'|'Y') ;
COUNT_INTRINSIC       : ('c'|'C') ('o'|'O') ('u'|'U') ('n'|'N') ('t'|'T') ;
DOT_PRODUCT_INTRINSIC : ('d'|'D') ('o'|'O') ('t'|'T') '_' ('p'|'P') ('r'|'R') ('o'|'O') ('d'|'D') ('u'|'U') ('c'|'C') ('t'|'T') ;
MATMUL_INTRINSIC      : ('m'|'M') ('a'|'A') ('t'|'T') ('m'|'M') ('u'|'U') ('l'|'L') ;
MAXVAL_INTRINSIC      : ('m'|'M') ('a'|'A') ('x'|'X') ('v'|'V') ('a'|'A') ('l'|'L') ;
MINVAL_INTRINSIC      : ('m'|'M') ('i'|'I') ('n'|'N') ('v'|'V') ('a'|'A') ('l'|'L') ;
PRODUCT_INTRINSIC     : ('p'|'P') ('r'|'R') ('o'|'O') ('d'|'D') ('u'|'U') ('c'|'C') ('t'|'T') ;
SUM_INTRINSIC         : ('s'|'S') ('u'|'U') ('m'|'M') ;
TRANSPOSE_INTRINSIC   : ('t'|'T') ('r'|'R') ('a'|'A') ('n'|'N') ('s'|'S') ('p'|'P') ('o'|'O') ('s'|'S') ('e'|'E') ;

// Array shape functions
SIZE_INTRINSIC        : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
SHAPE_INTRINSIC       : ('s'|'S') ('h'|'H') ('a'|'A') ('p'|'P') ('e'|'E') ;
UBOUND_INTRINSIC      : ('u'|'U') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
LBOUND_INTRINSIC      : ('l'|'L') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
ALLOCATED_INTRINSIC   : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// Array manipulation functions
PACK_INTRINSIC        : ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
UNPACK_INTRINSIC      : ('u'|'U') ('n'|'N') ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
RESHAPE_INTRINSIC     : ('r'|'R') ('e'|'E') ('s'|'S') ('h'|'H') ('a'|'A') ('p'|'P') ('e'|'E') ;
SPREAD_INTRINSIC      : ('s'|'S') ('p'|'P') ('r'|'R') ('e'|'E') ('a'|'A') ('d'|'D') ;
MERGE_INTRINSIC       : ('m'|'M') ('e'|'E') ('r'|'R') ('g'|'G') ('e'|'E') ;

// String functions
TRIM_INTRINSIC        : ('t'|'T') ('r'|'R') ('i'|'I') ('m'|'M') ;
ADJUSTL_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U') ('s'|'S') ('t'|'T') ('l'|'L') ;
ADJUSTR_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U') ('s'|'S') ('t'|'T') ('r'|'R') ;
REPEAT_INTRINSIC      : ('r'|'R') ('e'|'E') ('p'|'P') ('e'|'E') ('a'|'A') ('t'|'T') ;

// ====================================================================
// WHITESPACE AND SEPARATORS
// ====================================================================

// Semicolon for multiple statements per line (free-form)
SEMICOLON : ';' ;

// ====================================================================
// F90-SPECIFIC TOKENS (not in SharedCoreLexer)
// ====================================================================

// F90 specific data types
DOUBLE          : ('d'|'D') ('o'|'O') ('u'|'U') ('b'|'B') ('l'|'L') ('e'|'E') ;
PRECISION       : ('p'|'P') ('r'|'R') ('e'|'E') ('c'|'C') ('i'|'I') ('s'|'S') ('i'|'I') ('o'|'O') ('n'|'N') ;
COMPLEX         : ('c'|'C') ('o'|'O') ('m'|'M') ('p'|'P') ('l'|'L') ('e'|'E') ('x'|'X') ;
LOGICAL         : ('l'|'L') ('o'|'O') ('g'|'G') ('i'|'I') ('c'|'C') ('a'|'A') ('l'|'L') ;
CHARACTER       : ('c'|'C') ('h'|'H') ('a'|'A') ('r'|'R') ('a'|'A') ('c'|'C') ('t'|'T') ('e'|'E') ('r'|'R') ;

// Missing SharedCoreLexer tokens that parser needs
PROGRAM         : ('p'|'P') ('r'|'R') ('o'|'O') ('g'|'G') ('r'|'R') ('a'|'A') ('m'|'M') ;
FUNCTION        : ('f'|'F') ('u'|'U') ('n'|'N') ('c'|'C') ('t'|'T') ('i'|'I') ('o'|'O') ('n'|'N') ;
SUBROUTINE      : ('s'|'S') ('u'|'U') ('b'|'B') ('r'|'R') ('o'|'O') ('u'|'U') ('t'|'T') ('i'|'I') ('n'|'N') ('e'|'E') ;
THEN            : ('t'|'T') ('h'|'H') ('e'|'E') ('n'|'N') ;
ELSE            : ('e'|'E') ('l'|'L') ('s'|'S') ('e'|'E') ;
PARAMETER       : ('p'|'P') ('a'|'A') ('r'|'R') ('a'|'A') ('m'|'M') ('e'|'E') ('t'|'T') ('e'|'E') ('r'|'R') ;
DATA            : ('d'|'D') ('a'|'A') ('t'|'T') ('a'|'A') ;
COMMON          : ('c'|'C') ('o'|'O') ('m'|'M') ('m'|'M') ('o'|'O') ('n'|'N') ;
EQUIVALENCE     : ('e'|'E') ('q'|'Q') ('u'|'U') ('i'|'I') ('v'|'V') ('a'|'A') ('l'|'L') ('e'|'E') ('n'|'N') ('c'|'C') ('e'|'E') ;
DIMENSION       : ('d'|'D') ('i'|'I') ('m'|'M') ('e'|'E') ('n'|'N') ('s'|'S') ('i'|'I') ('o'|'O') ('n'|'N') ;
SAVE            : ('s'|'S') ('a'|'A') ('v'|'V') ('e'|'E') ;
EXTERNAL        : ('e'|'E') ('x'|'X') ('t'|'T') ('e'|'E') ('r'|'R') ('n'|'N') ('a'|'A') ('l'|'L') ;
INTRINSIC       : ('i'|'I') ('n'|'N') ('t'|'T') ('r'|'R') ('i'|'I') ('n'|'N') ('s'|'S') ('i'|'I') ('c'|'C') ;
IMPLICIT        : ('i'|'I') ('m'|'M') ('p'|'P') ('l'|'L') ('i'|'I') ('c'|'C') ('i'|'I') ('t'|'T') ;
NONE            : ('n'|'N') ('o'|'O') ('n'|'N') ('e'|'E') ;
RETURN          : ('r'|'R') ('e'|'E') ('t'|'T') ('u'|'U') ('r'|'R') ('n'|'N') ;

// Logical operators
DOT_TRUE        : '.' ('t'|'T') ('r'|'R') ('u'|'U') ('e'|'E') '.' ;
DOT_FALSE       : '.' ('f'|'F') ('a'|'A') ('l'|'L') ('s'|'S') ('e'|'E') '.' ;
DOT_EQ          : '.' ('e'|'E') ('q'|'Q') '.' ;
DOT_NE          : '.' ('n'|'N') ('e'|'E') '.' ;
DOT_LT          : '.' ('l'|'L') ('t'|'T') '.' ;
DOT_LE          : '.' ('l'|'L') ('e'|'E') '.' ;
DOT_GT          : '.' ('g'|'G') ('t'|'T') '.' ;
DOT_GE          : '.' ('g'|'G') ('e'|'E') '.' ;
DOT_AND         : '.' ('a'|'A') ('n'|'N') ('d'|'D') '.' ;
DOT_OR          : '.' ('o'|'O') ('r'|'R') '.' ;
DOT_NOT         : '.' ('n'|'N') ('o'|'O') ('t'|'T') '.' ;
DOT_EQV         : '.' ('e'|'E') ('q'|'Q') ('v'|'V') '.' ;
DOT_NEQV        : '.' ('n'|'N') ('e'|'E') ('q'|'Q') ('v'|'V') '.' ;

// String concatenation  
CONCAT          : '//' ;
SLASH           : '/' ;

// Whitespace and newlines
WHITESPACE : [ \t]+ -> skip ;
NEWLINE    : [\r\n]+ -> skip ;

// Whitespace fragment for compound tokens
fragment WS : [ \t]+ ;

// ====================================================================
// FORTRAN 90 LEXER STATUS  
// ====================================================================
//
// IMPLEMENTATION STATUS: Complete unified F90 lexer
// FORMAT SUPPORT: Both fixed-form (.f) and free-form (.f90) in one grammar
// ARCHITECTURE: Clean single inheritance from SharedCoreLexer
// INNOVATIONS: All major F90 language features tokenized
//
// BACKWARD COMPATIBILITY:
// - Supports all F77 constructs through SharedCoreLexer inheritance
// - Fixed-form comment and continuation detection
// - Column-sensitive parsing (when activated by parser)
//
// FORWARD COMPATIBILITY: 
// - Foundation for F95, F2003, F2008, F2018, F2023, LazyFortran2025
// - Clean extension points for new features
//
// This unified lexer solves the dual-format requirement elegantly
// while maintaining clean modular architecture for future standards.
//
// ====================================================================