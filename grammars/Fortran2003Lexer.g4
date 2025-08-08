/*
 * Fortran2003Lexer.g4
 * 
 * Fortran 2003 - Object-Oriented Programming Revolution
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

lexer grammar Fortran2003Lexer;

import Fortran95Lexer;

// ============================================================================
// FORTRAN 2003 NEW FEATURES - Object-Oriented Programming
// ============================================================================

// C token for BIND(C) - simple approach like external grammar
C                : [cC] ;

// Object-Oriented Programming (NEW in F2003)
ABSTRACT         : A B S T R A C T ;
EXTENDS          : E X T E N D S ;
FINAL            : F I N A L ;
CLASS            : [cC] L A S S ;
NOPASS           : N O P A S S ;
PASS             : P A S S ;
DEFERRED         : D E F E R R E D ;

// Parameterized Derived Types (NEW in F2003)
KIND             : K I N D ;
LEN              : L E N ;

// Enhanced Allocatable Features (NEW in F2003)
SOURCE           : S O U R C E ;
MOLD             : M O L D ;

// Procedure Pointers (NEW in F2003)
PROCEDURE        : P R O C E D U R E ;

// C Interoperability (NEW in F2003)
BIND             : B I N D ;
VALUE            : V A L U E ;
NAME             : N A M E ;

// Enhanced I/O (NEW in F2003)
ASYNCHRONOUS     : A S Y N C H R O N O U S ;
STREAM           : S T R E A M ;
PENDING          : P E N D I N G ;
WAIT             : W A I T ;
FLUSH            : F L U S H ;

// ASSOCIATE construct (NEW in F2003)
ASSOCIATE        : A S S O C I A T E ;
ENDASSOCIATE     : E N D A S S O C I A T E ;

// BLOCK construct (NEW in F2003)
BLOCK            : B L O C K ;
ENDBLOCK         : E N D B L O C K ;

// Enhanced WHERE (NEW in F2003)
MASKED           : M A S K E D ;

// Import statement (NEW in F2003)
IMPORT           : I M P O R T ;

// Volatile (NEW in F2003)
VOLATILE         : V O L A T I L E ;

// Protected (NEW in F2003)
PROTECTED        : P R O T E C T E D ;

// Generic type-bound procedures (NEW in F2003)
GENERIC          : G E N E R I C ;

// C Interoperability types (NEW in F2003) - use explicit [cC]
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
C_FLOAT          : [cC] '_' F L O A T ;
C_DOUBLE         : [cC] '_' D O U B L E ;
C_LONG_DOUBLE    : [cC] '_' L O N G '_' D O U B L E ;
C_FLOAT_COMPLEX  : [cC] '_' F L O A T '_' C O M P L E X ;
C_DOUBLE_COMPLEX : [cC] '_' D O U B L E '_' C O M P L E X ;
C_LONG_DOUBLE_COMPLEX : [cC] '_' L O N G '_' D O U B L E '_' C O M P L E X ;
C_BOOL           : [cC] '_' B O O L ;
C_CHAR           : [cC] '_' C H A R ;
C_PTR            : [cC] '_' P T R ;
C_FUNPTR         : [cC] '_' F U N P T R ;
C_NULL_PTR       : [cC] '_' N U L L '_' P T R ;
C_NULL_FUNPTR    : [cC] '_' N U L L '_' F U N P T R ;

// Additional F2003 tokens
SELECT_TYPE      : S E L E C T '_' T Y P E ;
TYPE_IS          : T Y P E '_' I S ;
CLASS_IS         : C L A S S '_' I S ;
CLASS_DEFAULT    : C L A S S '_' D E F A U L T ;
ERRMSG           : E R R M S G ;
ID               : I D ;

// Override F90 keywords that conflict with FIXED_FORM_COMMENT
CONTAINS         : [cC] O N T A I N S ;

// Override FIXED_FORM_COMMENT to be much more restrictive for BIND(C) compatibility
FIXED_FORM_COMMENT  
    : [cC] ~[\r\n),=]+ ~[\r\n]*  -> channel(HIDDEN)  // 'c' followed by content but not ), or =
    ;

STAR_COMMENT
    : '*' ~[\r\n]* -> channel(HIDDEN)              // Star comments
    ;

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