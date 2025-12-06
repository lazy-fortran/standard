/*
 * Fortran2018Lexer.g4
 * 
 * Fortran 2018 - Modern Fortran Revolution
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 */

lexer grammar Fortran2018Lexer;

import Fortran2008Lexer;

// ============================================================================
// FORTRAN 2018 NEW TOKENS
// ============================================================================

// Collective Coarray Operations (NEW in F2018)
CO_SUM           : C O '_' S U M ;
CO_MIN           : C O '_' M I N ;
CO_MAX           : C O '_' M A X ;
CO_REDUCE        : C O '_' R E D U C E ;
CO_BROADCAST     : C O '_' B R O A D C A S T ;

// Image Status Functions (NEW in F2018)
IMAGE_STATUS     : I M A G E '_' S T A T U S ;
FAILED_IMAGES    : F A I L E D '_' I M A G E S ;
STOPPED_IMAGES   : S T O P P E D '_' I M A G E S ;
IMAGE_STATUS_FAILED    : I M A G E '_' S T A T U S '_' F A I L E D ;
IMAGE_STATUS_STOPPED   : I M A G E '_' S T A T U S '_' S T O P P E D ;

// SELECT RANK Construct (NEW in F2018)
SELECT_RANK      : S E L E C T '_' R A N K ;
RANK_STAR        : R A N K '_' S T A R ;
RANK_DEFAULT     : R A N K '_' D E F A U L T ;

// Random Initialization (NEW in F2018)
RANDOM_INIT      : R A N D O M '_' I N I T ;
REPEATABLE       : R E P E A T A B L E ;
IMAGE_DISTINCT   : I M A G E '_' D I S T I N C T ;

// Enhanced Intrinsics (NEW in F2018)
REDUCE           : R E D U C E ;
OUT_OF_RANGE     : O U T '_' O F '_' R A N G E ;
COSHAPE          : C O S H A P E ;
TEAM_NUMBER      : T E A M '_' N U M B E R ;

// Team Support (NEW in F2018)
FORM_TEAM        : F O R M '_' T E A M ;
CHANGE_TEAM      : C H A N G E '_' T E A M ;
END_TEAM         : E N D '_' T E A M ;
TEAM_TYPE        : T E A M '_' T Y P E ;
GET_TEAM         : G E T '_' T E A M ;

// Events (NEW in F2018)
EVENT_TYPE       : E V E N T '_' T Y P E ;
EVENT_POST       : E V E N T '_' P O S T ;
EVENT_WAIT       : E V E N T '_' W A I T ;
EVENT_QUERY      : E V E N T '_' Q U E R Y ;

// Assumed Rank Support (NEW in F2018)
ASSUMED_RANK     : A S S U M E D '_' R A N K ;
RANK_KEYWORD     : R A N K ;

// Default Accessibility (NEW in F2018)  
DEFAULT_ACCESS   : D E F A U L T '_' A C C E S S ;

// C Descriptor Support (NEW in F2018)
C_F_POINTER_RANK : C '_' F '_' P O I N T E R '_' R A N K ;
C_SIZEOF         : C '_' S I Z E O F ;
CFI_ESTABLISH    : C F I '_' E S T A B L I S H ;
CFI_SETPOINTER   : C F I '_' S E T P O I N T E R ;

// Enhanced Stop (NEW in F2018)
QUIET            : Q U I E T ;

// Locality Specifier (NEW in F2018)
LOCALITY         : L O C A L I T Y ;
LOCAL_INIT       : L O C A L '_' I N I T ;
LOCAL            : L O C A L ;
SHARED           : S H A R E D ;

// Missing tokens referenced in parser (NEW in F2018)
RESULT_IMAGE     : R E S U L T '_' I M A G E ;
SOURCE_IMAGE     : S O U R C E '_' I M A G E ;
ARROW            : '->' ;
NEW_INDEX        : N E W '_' I N D E X ;
COUNT            : C O U N T ;
UNTIL_COUNT      : U N T I L '_' C O U N T ;
DOT_DOT          : '..' ;
TEAM             : T E A M ;
DIM              : D I M ;
MASK             : M A S K ;
NONE             : N O N E ;
KIND             : K I N D ;

// ============================================================================
// F2018 Fragment Helpers (inherit all from F2008)
// ============================================================================
// All fragment rules are inherited from the earlier lexers in this
// repository (FORTRAN, FORTRANII, FORTRAN66, FORTRAN77, Fortran90,
// Fortran95, Fortran2003 and Fortran2008).
