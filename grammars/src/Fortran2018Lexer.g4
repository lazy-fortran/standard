/*
 * Fortran2018Lexer.g4
 *
 * Fortran 2018 (ISO/IEC 1539-1:2018) Lexer
 * Reference: J3/18-007r1 "Fortran 2018" (Working Draft)
 *
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * ISO/IEC 1539-1:2018 References:
 * - Section 3: Lexical tokens and source form
 * - Section 6: Terms and concepts for data
 * - Section 11: Program units and procedures
 * - Section 16: Intrinsic procedures and modules
 */

lexer grammar Fortran2018Lexer;

import Fortran2008Lexer;

// ============================================================================
// FORTRAN 2018 NEW TOKENS (ISO/IEC 1539-1:2018)
// ============================================================================

// ----------------------------------------------------------------------------
// Collective Coarray Subroutines (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 16.9.46-16.9.50: CO_BROADCAST, CO_MAX, CO_MIN,
// CO_REDUCE, CO_SUM
// ----------------------------------------------------------------------------
CO_SUM           : C O '_' S U M ;
CO_MIN           : C O '_' M I N ;
CO_MAX           : C O '_' M A X ;
CO_REDUCE        : C O '_' R E D U C E ;
CO_BROADCAST     : C O '_' B R O A D C A S T ;

// ----------------------------------------------------------------------------
// Image Status Functions (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 16.9.73: FAILED_IMAGES
// ISO/IEC 1539-1:2018 Section 16.9.81: IMAGE_STATUS
// ISO/IEC 1539-1:2018 Section 16.9.182: STOPPED_IMAGES
// Note: IMAGE_STATUS_FAILED and IMAGE_STATUS_STOPPED are NOT in the standard
// and have been removed (see issue #386).
// ----------------------------------------------------------------------------
IMAGE_STATUS     : I M A G E '_' S T A T U S ;
FAILED_IMAGES    : F A I L E D '_' I M A G E S ;
STOPPED_IMAGES   : S T O P P E D '_' I M A G E S ;

// ----------------------------------------------------------------------------
// SELECT RANK Construct (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 11.1.10: SELECT RANK construct
// ISO/IEC 1539-1:2018 R1148-R1151: select-rank-construct, select-rank-stmt,
// select-rank-case-stmt, end-select-rank-stmt
// Note: SELECT RANK uses separate SELECT and RANK_KEYWORD tokens, not compound token
// Parser correctly implements: SELECT RANK_KEYWORD LPAREN ... RPAREN
// See Fortran2018Parser.g4 select_rank_stmt rule (line 227)
// Compound token SELECT_RANK was removed - it was never used by parser
// --------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Random Initialization (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 16.9.152: RANDOM_INIT
// ----------------------------------------------------------------------------
RANDOM_INIT      : R A N D O M '_' I N I T ;
REPEATABLE       : R E P E A T A B L E ;
IMAGE_DISTINCT   : I M A G E '_' D I S T I N C T ;

// ----------------------------------------------------------------------------
// Enhanced Intrinsics (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 16.9.161: REDUCE
// ISO/IEC 1539-1:2018 Section 16.9.140: OUT_OF_RANGE
// ISO/IEC 1539-1:2018 Section 16.9.52: COSHAPE
// ISO/IEC 1539-1:2018 Section 16.9.187: TEAM_NUMBER
// ----------------------------------------------------------------------------
REDUCE           : R E D U C E ;
OUT_OF_RANGE     : O U T '_' O F '_' R A N G E ;
COSHAPE          : C O S H A P E ;
TEAM_NUMBER      : T E A M '_' N U M B E R ;

// ----------------------------------------------------------------------------
// Team Support (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 11.6: Image execution control
// ISO/IEC 1539-1:2018 Section 11.6.7: CHANGE TEAM construct (R1111-R1115)
// ISO/IEC 1539-1:2018 Section 11.6.9: FORM TEAM statement (R1175-R1178)
// ISO/IEC 1539-1:2018 Section 16.9.78: GET_TEAM
// ISO/IEC 1539-1:2018 Section 16.5.6: TEAM_TYPE
// ----------------------------------------------------------------------------
FORM_TEAM        : F O R M WS+ T E A M ;
CHANGE_TEAM      : C H A N G E WS+ T E A M ;
END_TEAM         : E N D WS+ T E A M ;
TEAM_TYPE        : T E A M '_' T Y P E ;
GET_TEAM         : G E T '_' T E A M ;

// ----------------------------------------------------------------------------
// Events (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 11.6.8: EVENT POST and EVENT WAIT statements
// ISO/IEC 1539-1:2018 R1170-R1173: event-post-stmt, event-variable,
// event-wait-stmt, event-wait-spec (with sync-stat R1165)
// ISO/IEC 1539-1:2018 Section 16.9.72: EVENT_QUERY
// ISO/IEC 1539-1:2018 Section 16.5.5: EVENT_TYPE
// ----------------------------------------------------------------------------
EVENT_TYPE       : E V E N T '_' T Y P E ;
EVENT_POST       : E V E N T WS+ P O S T ;
EVENT_WAIT       : E V E N T WS+ W A I T ;
EVENT_QUERY      : E V E N T WS+ Q U E R Y ;

// ----------------------------------------------------------------------------
// Assumed Rank Support (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 8.5.8.7: Assumed-rank entity
// ISO/IEC 1539-1:2018 R825: assumed-rank-spec is ..
// Note: Parser uses DOT_DOT (..) syntax, not ASSUMED_RANK keyword
// See Fortran2018Parser.g4 assumed_rank_declaration rule
// ----------------------------------------------------------------------------
RANK_KEYWORD     : R A N K ;

// ----------------------------------------------------------------------------
// NOTE: DEFAULT_ACCESS was previously defined but NOT in ISO standard
// F2018 uses DEFAULT keyword with PUBLIC/PRIVATE, not DEFAULT_ACCESS token
// Token removed during dead code cleanup (issue #434)

// ----------------------------------------------------------------------------
// C Descriptor Support (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 18: Interoperability with C
// ISO/IEC 1539-1:2018 Section 18.2.3.6: C_F_POINTER
// ISO/IEC 1539-1:2018 Section 16.9.55: C_SIZEOF
// ----------------------------------------------------------------------------
C_F_POINTER_RANK : C '_' F '_' P O I N T E R '_' R A N K ;
C_SIZEOF         : C '_' S I Z E O F ;
CFI_ESTABLISH    : C F I '_' E S T A B L I S H ;
CFI_SETPOINTER   : C F I '_' S E T P O I N T E R ;

// ----------------------------------------------------------------------------
// Enhanced Stop (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 11.4: STOP and ERROR STOP statements
// ISO/IEC 1539-1:2018 R1160/R1161: STOP/ERROR STOP with optional QUIET= specifier
// ----------------------------------------------------------------------------
QUIET            : Q U I E T ;

// ----------------------------------------------------------------------------
// Locality Specifier (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 11.1.7.5: Additional semantics for DO CONCURRENT
// ISO/IEC 1539-1:2018 R1129: concurrent-locality, R1130 locality-spec
// Note: LOCALITY is a grammar rule name, not a keyword (use LOCAL, LOCAL_INIT)
// See Fortran2018Parser.g4 concurrent_locality rule
// ----------------------------------------------------------------------------
LOCAL_INIT       : L O C A L '_' I N I T ;
LOCAL            : L O C A L ;
SHARED           : S H A R E D ;

// ----------------------------------------------------------------------------
// Additional tokens referenced in parser (NEW in F2018)
// ISO/IEC 1539-1:2018 Section 16.9.46-50: RESULT_IMAGE for collective calls
// ISO/IEC 1539-1:2018 Section 16.9.47: SOURCE_IMAGE for CO_BROADCAST
// ISO/IEC 1539-1:2018 R1113: coarray-association uses =>
// ISO/IEC 1539-1:2018 R1178: NEW_INDEX in form-team-spec
// ISO/IEC 1539-1:2018 Section 16.9.72: COUNT for EVENT_QUERY
// ISO/IEC 1539-1:2018 R1173: UNTIL_COUNT for event-wait-spec
// ISO/IEC 1539-1:2018 R825: assumed-rank-spec is ..
// ----------------------------------------------------------------------------
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
AT_SIGN          : '@' ;
DOLLAR_SIGN      : '$' ;
BACKSLASH_SIGN   : '\\\\' ;

// ============================================================================
// F2018 Fragment Helpers (inherit all from F2008)
// ============================================================================
// All fragment rules are inherited from the earlier lexers in this
// repository (FORTRAN, FORTRANII, FORTRAN66, FORTRAN77, Fortran90,
// Fortran95, Fortran2003 and Fortran2008).
