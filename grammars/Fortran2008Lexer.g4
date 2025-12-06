/*
 * Fortran2008Lexer.g4
 * 
 * Fortran 2008 - Enhanced Parallel Programming Revolution
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

lexer grammar Fortran2008Lexer;

import Fortran2003Lexer;

// ============================================================================
// FORTRAN 2008 NEW FEATURES - Enhanced Parallel Programming
// ============================================================================

// Coarray Support (NEW in F2008)
LBRACKET         : '[' ;
RBRACKET         : ']' ;

// Image intrinsics and SYNC keyword
THIS_IMAGE       : T H I S '_' I M A G E ;
NUM_IMAGES       : N U M '_' I M A G E S ;
SYNC             : S Y N C ;

// SYNC image control statements use SYNC followed by a keyword-like specifier.
// We introduce dedicated tokens for the specifiers to keep the parser simple.
ALL              : A L L ;
IMAGES           : I M A G E S ;
MEMORY           : M E M O R Y ;

// Submodules (NEW in F2008)
SUBMODULE        : S U B M O D U L E ;
END_SUBMODULE    : E N D WS+ S U B M O D U L E ;

// Enhanced DO Constructs (NEW in F2008)
DO_CONCURRENT    : D O '_' C O N C U R R E N T ;
CONCURRENT       : C O N C U R R E N T ;

// Enhanced Allocatable Features (NEW in F2008)
CONTIGUOUS       : C O N T I G U O U S ;

// Enhanced Error Handling (NEW in F2008)
ERROR_STOP       : E R R O R '_' S T O P ;

// Additional Intrinsic Procedures (NEW in F2008)
BESSEL_J0        : B E S S E L '_' J '0' ;
BESSEL_J1        : B E S S E L '_' J '1' ;
BESSEL_JN        : B E S S E L '_' J N ;
BESSEL_Y0        : B E S S E L '_' Y '0' ;
BESSEL_Y1        : B E S S E L '_' Y '1' ;
BESSEL_YN        : B E S S E L '_' Y N ;
ERF              : E R F ;
ERFC             : E R F C ;
GAMMA            : G A M M A ;
LOG_GAMMA        : L O G '_' G A M M A ;
NORM2            : N O R M '2' ;
PARITY           : P A R I T Y ;
FINDLOC          : F I N D L O C ;
STORAGE_SIZE     : S T O R A G E '_' S I Z E ;

// Enhanced Intrinsic Types (NEW in F2008)
INT8             : I N T '8' ;
INT16            : I N T '1' '6' ;
INT32            : I N T '3' '2' ;
INT64            : I N T '6' '4' ;
REAL32           : R E A L '3' '2' ;
REAL64           : R E A L '6' '4' ;
REAL128          : R E A L '1' '2' '8' ;

// ============================================================================
// CASE-INSENSITIVE FRAGMENTS (inherited from F2003)
// ============================================================================
// All fragments inherited from Fortran2003Lexer
