// Fortran 2003 (2004) Lexer - Object-Oriented Programming Revolution
// Building on F95 with OOP, PDTs, C interoperability, and enhanced I/O
// Reference: ISO/IEC 1539-1:2004 (Fortran 2003)
//            J3/03-007 (Fortran 2003 draft/final text)
lexer grammar Fortran2003Lexer;

import Fortran95Lexer;

// ====================================================================
// FORTRAN 2003 LEXER OVERVIEW
// ====================================================================
//
// This lexer implements tokens for Fortran 2003 as defined in:
//   ISO/IEC 1539-1:2004 (Fortran 2003 International Standard)
//   J3/03-007 (Fortran 2003 draft/final text)
//
// F2003 introduces major new keywords and token categories:
// - Object-oriented programming (Section 4.5)
// - Parameterized derived types (Section 4.5.3)
// - C interoperability (Section 15)
// - IEEE arithmetic modules (Section 14)
// - Enhanced I/O (Section 9)
// - New attributes and constructs (Section 5, 8)
//
// ====================================================================

// ====================================================================
// COMMENTS - MUST BE FIRST TO HAVE PRECEDENCE OVER KEYWORDS
// ====================================================================

// Fixed-form comments (ISO/IEC 1539-1:2004 Section 3.3.2)
// In fixed source form, a line is a comment if column 1 contains C, c, or *.
// The newline followed immediately by C/c ensures column 1 position.
//
// For files that start with a comment (no preceding newline), a leading
// newline should be added to the source during preprocessing.

// Fixed-form comment: newline followed immediately by C/c in column 1
// ISO/IEC 1539-1:2004 Section 3.3.2 - column-1 C/c is a comment line
// Match C/c followed by whitespace or non-alphanumeric (avoids keywords).
FIXED_FORM_COMMENT
    : [\r\n]+ [cC] ( [ \t] ~[\r\n]* | [*\-=!+#$%^&/<>:;,~`@0-9] ~[\r\n]* )
      -> channel(HIDDEN)
    ;

// Star comment at column 1 (after newline)
// ISO/IEC 1539-1:2004 Section 3.3.2
STAR_COMMENT
    : [\r\n]+ '*' ~[\r\n]* -> channel(HIDDEN)
    ;

// ====================================================================
// OBJECT-ORIENTED PROGRAMMING KEYWORDS (ISO/IEC 1539-1:2004 Section 4.5)
// ====================================================================
//
// F2003 introduces full OOP support:
// - Type extension via EXTENDS (Section 4.5.6)
// - Abstract types (Section 4.5.6)
// - Type-bound procedures (Section 4.5.4)
// - CLASS declarations for polymorphism (Section 4.5.6)
// - Finalization via FINAL (Section 4.5.5)

// C token for BIND(C) - ISO/IEC 1539-1:2004 Section 15.3
C                : [cC] ;

// Abstract types and interfaces (Section 4.5.6, 12.3.2.3)
ABSTRACT_INTERFACE : A B S T R A C T WS+ I N T E R F A C E ;
ABSTRACT         : A B S T R A C T ;

// Type extension (Section 4.5.6)
EXTENDS          : E X T E N D S ;

// Finalization (Section 4.5.5)
FINAL            : F I N A L ;

// Polymorphism (Section 4.5.6)
CLASS            : [cC] L A S S ;

// Type-bound procedure attributes (Section 4.5.4)
NOPASS           : N O P A S S ;
PASS             : P A S S ;
DEFERRED         : D E F E R R E D ;

// ====================================================================
// PARAMETERIZED DERIVED TYPES (ISO/IEC 1539-1:2004 Section 4.5.3)
// ====================================================================
//
// F2003 introduces KIND and LEN type parameters for derived types.

KIND             : K I N D ;
LEN              : L E N ;

// ====================================================================
// ENHANCED ALLOCATABLE (ISO/IEC 1539-1:2004 Section 6.3.1)
// ====================================================================
//
// F2003 adds SOURCE and MOLD specifiers to ALLOCATE.
// F2003 introduces MOVE_ALLOC intrinsic subroutine (Section 13.7.80).

SOURCE           : S O U R C E ;
MOLD             : M O L D ;
MOVE_ALLOC       : M O V E '_' A L L O C ;

// ====================================================================
// PROCEDURE POINTERS (ISO/IEC 1539-1:2004 Section 12.3.2.3)
// ====================================================================

PROCEDURE        : P R O C E D U R E ;

// ====================================================================
// C INTEROPERABILITY (ISO/IEC 1539-1:2004 Section 15)
// ====================================================================
//
// F2003 introduces C interoperability:
// - BIND(C) attribute (Section 15.3)
// - VALUE attribute for pass-by-value (Section 15.3.5)
// - ISO_C_BINDING module (Section 15.2)

BIND             : B I N D ;
VALUE            : V A L U E ;
NAME             : N A M E ;

// ====================================================================
// ENHANCED I/O (ISO/IEC 1539-1:2004 Section 9)
// ====================================================================
//
// F2003 significantly enhances I/O:
// - Asynchronous I/O (Section 9.5.1)
// - Stream I/O (Section 9.2.2.4)
// - WAIT statement (Section 9.6)
// - FLUSH statement (Section 9.7)
// - IOMSG specifier (Section 9.10)

ASYNCHRONOUS     : A S Y N C H R O N O U S ;
STREAM           : S T R E A M ;
PENDING          : P E N D I N G ;
WAIT             : W A I T ;
FLUSH            : F L U S H ;

// I/O specifier keywords (Section 9.4.1)
FILE             : F I L E ;
ACCESS           : A C C E S S ;
FORM             : F O R M ;
STATUS           : S T A T U S ;
BLANK            : B L A N K ;
POSITION         : P O S I T I O N ;
ACTION           : A C T I O N ;
DELIM            : D E L I M ;
PAD              : P A D ;
RECL             : R E C L ;
IOMSG            : I O M S G ;   // F2003 (Section 9.10)

// ====================================================================
// ASSOCIATE CONSTRUCT (ISO/IEC 1539-1:2004 Section 8.1.3)
// ====================================================================
//
// F2003 introduces ASSOCIATE for creating local aliases.

ASSOCIATE        : A S S O C I A T E ;

// ====================================================================
// BLOCK CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.4)
// ====================================================================
//
// NOTE: The standalone BLOCK construct was introduced in Fortran 2008
// (ISO/IEC 1539-1:2010), NOT Fortran 2003. The BLOCK token is inherited
// for compatibility, but the ENDBLOCK token (which was previously here)
// has been removed as it is not used (parsers use "END BLOCK" instead).
// See issue #469 for details.

BLOCK            : B L O C K ;

// Enhanced WHERE (Section 7.4.3)
MASKED           : M A S K E D ;

// ====================================================================
// IMPORT STATEMENT (ISO/IEC 1539-1:2004 Section 12.3.2.1)
// ====================================================================
//
// IMPORT allows host-associated entities in interface bodies.

IMPORT           : I M P O R T ;

// ====================================================================
// VOLATILE AND PROTECTED (ISO/IEC 1539-1:2004 Section 5.1.2)
// ====================================================================
//
// F2003 introduces VOLATILE (Section 5.1.2.16) and PROTECTED (Section 5.1.2.10)

VOLATILE         : V O L A T I L E ;
PROTECTED        : P R O T E C T E D ;

// ====================================================================
// GENERIC TYPE-BOUND PROCEDURES (ISO/IEC 1539-1:2004 Section 4.5.4)
// ====================================================================

GENERIC          : G E N E R I C ;
NON_OVERRIDABLE  : N O N '_' O V E R R I D A B L E ;

// ====================================================================
// C INTEROPERABILITY TYPES (ISO/IEC 1539-1:2004 Section 15.2.2)
// ====================================================================
//
// Named constants from ISO_C_BINDING module.
// Table 15.2 in ISO/IEC 1539-1:2004 lists these interoperable types.

// Integer types (Table 15.2)
C_INT            : [cC] '_' I N T ;
C_SHORT          : [cC] '_' S H O R T ;
C_LONG           : [cC] '_' L O N G ;
C_LONG_LONG      : [cC] '_' L O N G '_' L O N G ;
C_SIGNED_CHAR    : [cC] '_' S I G N E D '_' C H A R ;
C_SIZE_T         : [cC] '_' S I Z E '_' T ;
C_INT8_T         : [cC] '_' I N T '8' '_' T ;
C_INT16_T        : [cC] '_' I N T '1' '6' '_' T ;
C_INT32_T        : [cC] '_' I N T '3' '2' '_' T ;
C_INT64_T        : [cC] '_' I N T '6' '4' '_' T ;
C_INT_LEAST8_T   : [cC] '_' I N T '_' L E A S T '8' '_' T ;
C_INT_LEAST16_T  : [cC] '_' I N T '_' L E A S T '1' '6' '_' T ;
C_INT_LEAST32_T  : [cC] '_' I N T '_' L E A S T '3' '2' '_' T ;
C_INT_LEAST64_T  : [cC] '_' I N T '_' L E A S T '6' '4' '_' T ;
C_INT_FAST8_T    : [cC] '_' I N T '_' F A S T '8' '_' T ;
C_INT_FAST16_T   : [cC] '_' I N T '_' F A S T '1' '6' '_' T ;
C_INT_FAST32_T   : [cC] '_' I N T '_' F A S T '3' '2' '_' T ;
C_INT_FAST64_T   : [cC] '_' I N T '_' F A S T '6' '4' '_' T ;
C_INTMAX_T       : [cC] '_' I N T M A X '_' T ;
C_INTPTR_T       : [cC] '_' I N T P T R '_' T ;

// Floating-point types (Table 15.2)
C_FLOAT          : [cC] '_' F L O A T ;
C_DOUBLE         : [cC] '_' D O U B L E ;
C_LONG_DOUBLE    : [cC] '_' L O N G '_' D O U B L E ;
C_FLOAT_COMPLEX  : [cC] '_' F L O A T '_' C O M P L E X ;
C_DOUBLE_COMPLEX : [cC] '_' D O U B L E '_' C O M P L E X ;
C_LONG_DOUBLE_COMPLEX : [cC] '_' L O N G '_' D O U B L E '_' C O M P L E X ;

// Other C types (Table 15.2)
C_BOOL           : [cC] '_' B O O L ;
C_CHAR           : [cC] '_' C H A R ;

// C pointer types (Section 15.2.3)
C_PTR            : [cC] '_' P T R ;
C_FUNPTR         : [cC] '_' F U N P T R ;
C_NULL_PTR       : [cC] '_' N U L L '_' P T R ;
C_NULL_FUNPTR    : [cC] '_' N U L L '_' F U N P T R ;

// ====================================================================
// ADDITIONAL F2003 TOKENS
// ====================================================================
//
// SELECT TYPE / TYPE IS / CLASS IS / CLASS DEFAULT (Section 8.1.5):
// These constructs use existing SELECT, TYPE, CLASS and DEFAULT tokens.
// The IS keyword is parsed as IDENTIFIER (see Issue #184 and parser docs).

ERRMSG           : E R R M S G ;    // I/O error message (Section 9.10)
ID               : I D ;            // Async I/O identifier (Section 9.5.1)

// ====================================================================
// IEEE INTRINSIC MODULES (ISO/IEC 1539-1:2004 Section 14)
// ====================================================================
//
// F2003 introduces three IEEE intrinsic modules:
// - IEEE_EXCEPTIONS (Section 14.2): Exception handling
// - IEEE_ARITHMETIC (Section 14.3): Arithmetic functions
// - IEEE_FEATURES (Section 14.4): Feature inquiry

// IEEE module names (Section 14)
IEEE_EXCEPTIONS  : I E E E '_' E X C E P T I O N S ;
IEEE_ARITHMETIC  : I E E E '_' A R I T H M E T I C ;
IEEE_FEATURES    : I E E E '_' F E A T U R E S ;

// IEEE exception types (Section 14.2)
// Derived type IEEE_FLAG_TYPE values
IEEE_OVERFLOW    : I E E E '_' O V E R F L O W ;
IEEE_UNDERFLOW   : I E E E '_' U N D E R F L O W ;
IEEE_DIVIDE_BY_ZERO : I E E E '_' D I V I D E '_' B Y '_' Z E R O ;
IEEE_INVALID     : I E E E '_' I N V A L I D ;
IEEE_INEXACT     : I E E E '_' I N E X A C T ;

// IEEE special values (Section 14.3)
// Derived type IEEE_CLASS_TYPE values
IEEE_POSITIVE_INF : I E E E '_' P O S I T I V E '_' I N F ;
IEEE_NEGATIVE_INF : I E E E '_' N E G A T I V E '_' I N F ;
IEEE_QUIET_NAN   : I E E E '_' Q U I E T '_' N A N ;
IEEE_SIGNALING_NAN : I E E E '_' S I G N A L I N G '_' N A N ;

// IEEE rounding modes (Section 14.3)
// Derived type IEEE_ROUND_TYPE values
IEEE_NEAREST     : I E E E '_' N E A R E S T ;
IEEE_TO_ZERO     : I E E E '_' T O '_' Z E R O ;
IEEE_UP          : I E E E '_' U P ;
IEEE_DOWN        : I E E E '_' D O W N ;

// IEEE features (Section 14.4)
// Derived type IEEE_FEATURES_TYPE values
IEEE_DATATYPE    : I E E E '_' D A T A T Y P E ;
IEEE_DENORMAL    : I E E E '_' D E N O R M A L ;
IEEE_DIVIDE      : I E E E '_' D I V I D E ;
IEEE_HALTING     : I E E E '_' H A L T I N G ;
IEEE_INEXACT_FLAG : I E E E '_' I N E X A C T '_' F L A G ;
IEEE_INF         : I E E E '_' I N F ;
IEEE_INVALID_FLAG : I E E E '_' I N V A L I D '_' F L A G ;
IEEE_NAN         : I E E E '_' N A N ;
IEEE_ROUNDING    : I E E E '_' R O U N D I N G ;
IEEE_SQRT        : I E E E '_' S Q R T ;
IEEE_UNDERFLOW_FLAG : I E E E '_' U N D E R F L O W '_' F L A G ;

// Override F90 keywords to ensure proper precedence
CONTAINS         : C O N T A I N S ;


// ============================================================================
// CASE-INSENSITIVE FRAGMENTS
// ============================================================================

fragment A : [aA] ;
fragment B : [bB] ;
// fragment C removed - now using C as token for BIND(C)
fragment D : [dD] ;
fragment E : [eE] ;
fragment F : [fF] ;
fragment G : [gG] ;
fragment H : [hH] ;
fragment I : [iI] ;
fragment J : [jJ] ;
fragment K : [kK] ;
fragment L : [lL] ;
fragment M : [mM] ;
fragment N : [nN] ;
fragment O : [oO] ;
fragment P : [pP] ;
fragment Q : [qQ] ;
fragment R : [rR] ;
fragment S : [sS] ;
fragment T : [tT] ;
fragment U : [uU] ;
fragment V : [vV] ;
fragment W : [wW] ;
fragment X : [xX] ;
fragment Y : [yY] ;
fragment Z : [zZ] ;

// ============================================================================
// FREE-FORM LINE CONTINUATION
// ============================================================================

// Enhanced continuation handling - hide both & and following newline+whitespace
CONTINUATION
    : '&' [ \t]* ('\r'? '\n') [ \t]* -> channel(HIDDEN)
    ;

// Override inherited NEWLINE to work properly with continuations
NEWLINE : [\r\n]+ ;

// Array constructor brackets (Fortran 2003 feature - ISO/IEC 1539-1:2004)
// Square bracket array constructor syntax [ ... ] was introduced in F2003.
// F90/F95 only support the (/ ... /) syntax.
LSQUARE : '[' ;
RSQUARE : ']' ;

// Enumeration tokens (Fortran 2003 feature - ISO/IEC 1539-1:2004 Section 4.6)
// ENUM construct for C interoperability with BIND(C)
ENUM             : E N U M ;
ENUMERATOR       : E N U M E R A T O R ;

// Whitespace handling - MUST skip spaces and tabs
WHITESPACE : [ \t]+ -> skip ;
