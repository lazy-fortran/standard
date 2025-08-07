// Fortran 90 (1990) Free-Form Lexer - Revolutionary Modern Foundation
// The bridge between 1957-1977 fixed-form FORTRAN and modern free-form Fortran
lexer grammar Fortran90FreeLexer;

import FreeFormBaseLexer;  // Revolutionary free-form source format

// ====================================================================
// FORTRAN 90 FREE-FORM LEXER OVERVIEW
// ====================================================================
//
// Fortran 90 (ISO 1539:1991) represents the most significant revolution 
// in Fortran history - the transition from rigid fixed-form punch card 
// format to flexible free-form source layout.
//
// This lexer defines the F90 LANGUAGE features in free-form format.
// Format rules are inherited from FreeFormBaseLexer.
//
// REVOLUTIONARY F90 LANGUAGE CHANGES:
// - Module system with explicit interfaces and data encapsulation
// - Dynamic arrays with runtime allocation (ALLOCATABLE, POINTER)
// - Derived types (user-defined structures)
// - Array operations and constructors
// - Enhanced control flow (SELECT CASE, WHERE)
// - Modern I/O (NAMELIST, non-advancing)
// - 31-character identifiers vs 6-character F77 limit
//
// ====================================================================

// ====================================================================
// FORTRAN 90 LANGUAGE KEYWORDS
// ====================================================================

// Module system (F90 major innovation - explicit interfaces and encapsulation)
MODULE          : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
END_MODULE      : ('e'|'E') ('n'|'N') ('d'|'D') [ \t]+ ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
USE             : ('u'|'U') ('s'|'S') ('e'|'E') ;
ONLY            : ('o'|'O') ('n'|'N') ('l'|'L') ('y'|'Y') ;
PUBLIC          : ('p'|'P') ('u'|'U') ('b'|'B') ('l'|'L') ('i'|'I') ('c'|'C') ;
PRIVATE         : ('p'|'P') ('r'|'R') ('i'|'I') ('v'|'V') ('a'|'A') ('t'|'T') ('e'|'E') ;

// Interface blocks (F90 major innovation - generic procedures and operators)
INTERFACE       : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
END_INTERFACE   : ('e'|'E') ('n'|'N') ('d'|'D') [ \t]+ ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
GENERIC         : ('g'|'G') ('e'|'E') ('n'|'N') ('e'|'E') ('r'|'R') ('i'|'I') ('c'|'C') ;
OPERATOR        : ('o'|'O') ('p'|'P') ('e'|'E') ('r'|'R') ('a'|'A') ('t'|'T') ('o'|'O') ('r'|'R') ;
ASSIGNMENT      : ('a'|'A') ('s'|'S') ('s'|'S') ('i'|'I') ('g'|'G') ('n'|'N') ('m'|'M') ('e'|'E') ('n'|'N') ('t'|'T') ;

// Procedure enhancements (F90 improvements)
RECURSIVE       : ('r'|'R') ('e'|'E') ('c'|'C') ('u'|'U') ('r'|'R') ('s'|'S') ('i'|'I') ('v'|'V') ('e'|'E') ;
PURE            : ('p'|'P') ('u'|'U') ('r'|'R') ('e'|'E') ;
ELEMENTAL       : ('e'|'E') ('l'|'L') ('e'|'E') ('m'|'M') ('e'|'E') ('n'|'N') ('t'|'T') ('a'|'A') ('l'|'L') ;
RESULT          : ('r'|'R') ('e'|'E') ('s'|'S') ('u'|'U') ('l'|'L') ('t'|'T') ;

// Derived types (F90 major innovation - user-defined structures)  
TYPE            : ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
END_TYPE        : ('e'|'E') ('n'|'N') ('d'|'D') [ \t]+ ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
SEQUENCE        : ('s'|'S') ('e'|'E') ('q'|'Q') ('u'|'U') ('e'|'E') ('n'|'N') ('c'|'C') ('e'|'E') ;

// Dynamic arrays and pointers (F90 major innovation - runtime memory management)
ALLOCATABLE     : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('a'|'A') ('b'|'B') ('l'|'L') ('e'|'E') ;
POINTER         : ('p'|'P') ('o'|'O') ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R') ;
TARGET          : ('t'|'T') ('a'|'A') ('r'|'R') ('g'|'G') ('e'|'E') ('t'|'T') ;
ALLOCATE        : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
DEALLOCATE      : ('d'|'D') ('e'|'E') ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
NULLIFY         : ('n'|'N') ('u'|'U') ('l'|'L') ('l'|'L') ('i'|'I') ('f'|'F') ('y'|'Y') ;
ASSOCIATED      : ('a'|'A') ('s'|'S') ('s'|'S') ('o'|'O') ('c'|'C') ('i'|'I') ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// Enhanced control flow (F90 innovations)
SELECT          : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
CASE            : ('c'|'C') ('a'|'A') ('s'|'S') ('e'|'E') ;
DEFAULT         : ('d'|'D') ('e'|'E') ('f'|'F') ('a'|'A') ('u'|'U') ('l'|'L') ('t'|'T') ;
END_SELECT      : ('e'|'E') ('n'|'N') ('d'|'D') [ \t]+ ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
WHERE           : ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;
END_WHERE       : ('e'|'E') ('n'|'N') ('d'|'D') [ \t]+ ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;
ELSEWHERE       : ('e'|'E') ('l'|'L') ('s'|'S') ('e'|'E') ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;

// Enhanced loop control (F90 enhancements)
CYCLE           : ('c'|'C') ('y'|'Y') ('c'|'C') ('l'|'L') ('e'|'E') ;
EXIT            : ('e'|'E') ('x'|'X') ('i'|'I') ('t'|'T') ;

// Enhanced I/O (F90 innovations)
NAMELIST        : ('n'|'N') ('a'|'A') ('m'|'M') ('e'|'E') ('l'|'L') ('i'|'I') ('s'|'S') ('t'|'T') ;
ADVANCE         : ('a'|'A') ('d'|'D') ('v'|'V') ('a'|'A') ('n'|'N') ('c'|'C') ('e'|'E') ;
SIZE            : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
STAT            : ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;
EOR             : ('e'|'E') ('o'|'O') ('r'|'R') ;
IOSTAT          : ('i'|'I') ('o'|'O') ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;

// Intent specifications (F90 procedure interface enhancement)
INTENT          : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('n'|'N') ('t'|'T') ;
IN              : ('i'|'I') ('n'|'N') ;
OUT             : ('o'|'O') ('u'|'U') ('t'|'T') ;
INOUT           : ('i'|'I') ('n'|'N') ('o'|'O') ('u'|'U') ('t'|'T') ;

// Optional and keyword arguments (F90 procedure enhancement)
OPTIONAL        : ('o'|'O') ('p'|'P') ('t'|'T') ('i'|'I') ('o'|'O') ('n'|'N') ('a'|'A') ('l'|'L') ;
PRESENT         : ('p'|'P') ('r'|'R') ('e'|'E') ('s'|'S') ('e'|'E') ('n'|'N') ('t'|'T') ;

// Enhanced data types (F90 improvements)
KIND            : ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
LEN             : ('l'|'L') ('e'|'E') ('n'|'N') ;
SELECTED_INT_KIND     : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ('e'|'E') ('d'|'D') '_' ('i'|'I') ('n'|'N') ('t'|'T') '_' ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
SELECTED_REAL_KIND    : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ('e'|'E') ('d'|'D') '_' ('r'|'R') ('e'|'E') ('a'|'A') ('l'|'L') '_' ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;

// Additional F90-specific keywords
CONTAINS        : ('c'|'C') ('o'|'O') ('n'|'N') ('t'|'T') ('a'|'A') ('i'|'I') ('n'|'N') ('s'|'S') ;
IMPORT          : ('i'|'I') ('m'|'M') ('p'|'P') ('o'|'O') ('r'|'R') ('t'|'T') ;
PROCEDURE       : ('p'|'P') ('r'|'R') ('o'|'O') ('c'|'C') ('e'|'E') ('d'|'D') ('u'|'U') ('r'|'R') ('e'|'E') ;
UNIT            : ('u'|'U') ('n'|'N') ('i'|'I') ('t'|'T') ;
FMT             : ('f'|'F') ('m'|'M') ('t'|'T') ;
REC             : ('r'|'R') ('e'|'E') ('c'|'C') ;
ERR             : ('e'|'E') ('r'|'R') ('r'|'R') ;
WHILE           : ('w'|'W') ('h'|'H') ('i'|'I') ('l'|'L') ('e'|'E') ;

// F90-specific operators (inherits others from FreeFormBase)
DOUBLE_COLON    : '::' ;
POINTER_ASSIGN  : '=>' ;

// F90-specific BOZ constants
BINARY_CONSTANT : ('b'|'B') '\'' [01]+ '\'' ;
OCTAL_CONSTANT  : ('o'|'O') '\'' [0-7]+ '\'' ;
HEX_CONSTANT    : ('z'|'Z'|'x'|'X') '\'' [0-9a-fA-F]+ '\'' ;

// ====================================================================
// FORTRAN 90 INTRINSIC FUNCTIONS (MAJOR ADDITIONS)
// ====================================================================

// Array intrinsic functions (F90 major innovation)
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

// Array inquiry functions (F90 major addition)
SIZE_INTRINSIC        : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
SHAPE_INTRINSIC       : ('s'|'S') ('h'|'H') ('a'|'A') ('p'|'P') ('e'|'E') ;
UBOUND_INTRINSIC      : ('u'|'U') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
LBOUND_INTRINSIC      : ('l'|'L') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
ALLOCATED_INTRINSIC   : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// Array manipulation functions (F90 innovations)
PACK_INTRINSIC        : ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
UNPACK_INTRINSIC      : ('u'|'U') ('n'|'N') ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
RESHAPE_INTRINSIC     : ('r'|'R') ('e'|'E') ('s'|'S') ('h'|'H') ('a'|'A') ('p'|'P') ('e'|'E') ;
SPREAD_INTRINSIC      : ('s'|'S') ('p'|'P') ('r'|'R') ('e'|'E') ('a'|'A') ('d'|'D') ;
MERGE_INTRINSIC       : ('m'|'M') ('e'|'E') ('r'|'R') ('g'|'G') ('e'|'E') ;

// String intrinsic functions (F90 enhancements)
TRIM_INTRINSIC        : ('t'|'T') ('r'|'R') ('i'|'I') ('m'|'M') ;
ADJUSTL_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U') ('s'|'S') ('t'|'T') ('l'|'L') ;
ADJUSTR_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U') ('s'|'S') ('t'|'T') ('r'|'R') ;
REPEAT_INTRINSIC      : ('r'|'R') ('e'|'E') ('p'|'P') ('e'|'E') ('a'|'A') ('t'|'T') ;

// ====================================================================
// FORTRAN 90 FREE-FORM LEXER STATUS
// ====================================================================
//
// IMPLEMENTATION STATUS: Complete F90 language feature coverage
// INHERITANCE: Integrates FreeFormBaseLexer for format rules
// ARCHITECTURE: Clean separation of format (base) and language (this)
// INNOVATIONS: All major F90 language constructs tokenized
//
// This lexer enables complete F90 syntax recognition in free-form format
// while inheriting format handling from FreeFormBaseLexer.
//
// ====================================================================