/*
 * Fortran2008Lexer.g4
 *
 * Fortran 2008 - Enhanced Parallel Programming Revolution
 * Unified lexer supporting both fixed-form (.f, .for) and free-form (.f90+)
 *
 * Reference: ISO/IEC 1539-1:2010 (Fortran 2008 International Standard)
 *            J3/10-007 (Fortran 2008 final text)
 *
 * This lexer defines tokens for Fortran 2008 features that extend F2003:
 * - Coarrays and image control (Section 2.4.7, 8.5)
 * - Submodules (Section 11.2.3)
 * - DO CONCURRENT (Section 8.1.6.6)
 * - CONTIGUOUS attribute (Section 5.3.7)
 * - ERROR STOP (Section 8.4)
 * - New intrinsic procedures (Section 13)
 * - Enhanced integer/real kinds (Section 13.8.2)
 */

lexer grammar Fortran2008Lexer;

import Fortran2003Lexer;

// ============================================================================
// FORTRAN 2008 NEW FEATURES - Enhanced Parallel Programming
// ============================================================================
// ISO/IEC 1539-1:2010 introduces major parallel programming features including
// coarrays for SPMD (Single Program Multiple Data) parallelism and the DO
// CONCURRENT construct for explicit parallelization.

// ============================================================================
// COARRAY SUPPORT (ISO/IEC 1539-1:2010 Section 2.4.7, 5.3.6, 6.6)
// ============================================================================
// Coarrays extend Fortran with a partitioned global address space (PGAS) model.
// - Section 2.4.7: Coarray model and image concepts
// - Section 5.3.6: CODIMENSION attribute
// - Section 6.6: Image selectors using square brackets

// Square brackets for coarray image selectors (Section 6.6)
// R624: image-selector -> [ cosubscript-list ]
LBRACKET         : '[' ;
RBRACKET         : ']' ;

// ============================================================================
// IMAGE CONTROL STATEMENTS (ISO/IEC 1539-1:2010 Section 8.5)
// ============================================================================
// Section 8.5 defines image control statements for coarray synchronization:
// - SYNC ALL (Section 8.5.3): Synchronize all images
// - SYNC IMAGES (Section 8.5.4): Synchronize specified images
// - SYNC MEMORY (Section 8.5.5): Memory synchronization
// - LOCK/UNLOCK (Section 8.5.6): Lock synchronization
// - CRITICAL (Section 8.1.5): Critical construct

// ============================================================================
// CRITICAL CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.5)
// ============================================================================
// CRITICAL construct ensures that the enclosed code is executed by only one
// image at a time across all images in the current team:
// - R818: critical-construct -> critical-stmt block end-critical-stmt
// - R819: critical-stmt -> [critical-construct-name :] CRITICAL [(sync-stat-list)]
// - R820: end-critical-stmt -> END CRITICAL [critical-construct-name]
CRITICAL         : C R I T I C A L ;
END_CRITICAL     : E N D WS+ C R I T I C A L ;

// ============================================================================
// LOCK/UNLOCK STATEMENTS (ISO/IEC 1539-1:2010 Section 8.5.6)
// ============================================================================
// LOCK and UNLOCK statements provide mutex-style synchronization for coarrays:
// - R859: lock-stmt -> LOCK ( lock-variable [, lock-stat-list] )
// - R860: unlock-stmt -> UNLOCK ( lock-variable [, sync-stat-list] )
// - R866: lock-variable -> scalar-variable
LOCK             : L O C K ;
UNLOCK           : U N L O C K ;
ACQUIRED_LOCK    : A C Q U I R E D '_' L O C K ;

// Image inquiry intrinsics (Section 13.7)
// THIS_IMAGE(): Returns image index (Section 13.7.165)
// NUM_IMAGES(): Returns number of images (Section 13.7.121)
THIS_IMAGE       : T H I S '_' I M A G E ;
NUM_IMAGES       : N U M '_' I M A G E S ;

// SYNC keyword for image control (Section 8.5.3-8.5.5)
// R858: sync-all-stmt -> SYNC ALL [(sync-stat-list)]
// R859: sync-images-stmt -> SYNC IMAGES (image-set [, sync-stat-list])
// R862: sync-memory-stmt -> SYNC MEMORY [(sync-stat-list)]
SYNC             : S Y N C ;

// SYNC statement specifiers (Section 8.5)
ALL              : A L L ;
IMAGES           : I M A G E S ;
MEMORY           : M E M O R Y ;

// ============================================================================
// SUBMODULES (ISO/IEC 1539-1:2010 Section 11.2.3)
// ============================================================================
// Submodules provide separate compilation units that extend modules:
// - R1116: submodule -> submodule-stmt [specification-part]
//                       [module-subprogram-part] end-submodule-stmt
// - R1117: submodule-stmt -> SUBMODULE (parent-identifier) submodule-name
// - R1119: end-submodule-stmt -> END [SUBMODULE [submodule-name]]
SUBMODULE        : S U B M O D U L E ;
END_SUBMODULE    : E N D WS+ S U B M O D U L E ;

// ============================================================================
// DO CONCURRENT CONSTRUCT (ISO/IEC 1539-1:2010 Section 8.1.6.6)
// ============================================================================
// DO CONCURRENT enables explicit loop parallelization:
// - R818: loop-control -> ... | CONCURRENT concurrent-header
// - R819: concurrent-header -> (concurrent-spec [, scalar-mask-expr])
// DO CONCURRENT indicates iterations may execute in any order or concurrently.
DO_CONCURRENT    : D O '_' C O N C U R R E N T ;
CONCURRENT       : C O N C U R R E N T ;

// ============================================================================
// CONTIGUOUS ATTRIBUTE (ISO/IEC 1539-1:2010 Section 5.3.7)
// ============================================================================
// CONTIGUOUS specifies that an array pointer/assumed-shape dummy argument
// occupies contiguous storage:
// - R523: attr-spec -> ... | CONTIGUOUS
// - R544: contiguous-stmt -> CONTIGUOUS [::] object-name-list
CONTIGUOUS       : C O N T I G U O U S ;

// ============================================================================
// ERROR STOP STATEMENT (ISO/IEC 1539-1:2010 Section 8.4)
// ============================================================================
// ERROR STOP initiates error termination of execution:
// - R856: error-stop-stmt -> ERROR STOP [stop-code]
// Unlike STOP, ERROR STOP indicates abnormal termination.
ERROR_STOP       : E R R O R WS+ S T O P ;

// ============================================================================
// NEW INTRINSIC PROCEDURES (ISO/IEC 1539-1:2010 Section 13.7)
// ============================================================================
// Fortran 2008 adds several new intrinsic procedures:

// Bessel functions (Section 13.7.22-13.7.27)
// Mathematical functions for cylindrical wave problems:
// - BESSEL_J0(X): Bessel function of first kind, order 0 (Section 13.7.22)
// - BESSEL_J1(X): Bessel function of first kind, order 1 (Section 13.7.23)
// - BESSEL_JN(N,X): Bessel function of first kind, order N (Section 13.7.24)
// - BESSEL_Y0(X): Bessel function of second kind, order 0 (Section 13.7.25)
// - BESSEL_Y1(X): Bessel function of second kind, order 1 (Section 13.7.26)
// - BESSEL_YN(N,X): Bessel function of second kind, order N (Section 13.7.27)
BESSEL_J0        : B E S S E L '_' J '0' ;
BESSEL_J1        : B E S S E L '_' J '1' ;
BESSEL_JN        : B E S S E L '_' J N ;
BESSEL_Y0        : B E S S E L '_' Y '0' ;
BESSEL_Y1        : B E S S E L '_' Y '1' ;
BESSEL_YN        : B E S S E L '_' Y N ;

// Error and gamma functions (Section 13.7.52-13.7.53, 13.7.61, 13.7.108)
// - ERF(X): Error function (Section 13.7.52)
// - ERFC(X): Complementary error function (Section 13.7.53)
// - GAMMA(X): Gamma function (Section 13.7.61)
// - LOG_GAMMA(X): Logarithm of absolute value of gamma (Section 13.7.108)
ERF              : E R F ;
ERFC             : E R F C ;
GAMMA            : G A M M A ;
LOG_GAMMA        : L O G '_' G A M M A ;

// Array reduction and inquiry functions (Section 13.7.119, 13.7.127, 13.7.58)
// - NORM2(X[,DIM]): L2 norm of an array (Section 13.7.119)
// - PARITY(MASK[,DIM]): Reduction via .NEQV. (Section 13.7.127)
// - FINDLOC(ARRAY,VALUE,...): Location of value in array (Section 13.7.58)
NORM2            : N O R M '2' ;
PARITY           : P A R I T Y ;
FINDLOC          : F I N D L O C ;

// Storage inquiry function (Section 13.7.163)
// - STORAGE_SIZE(A[,KIND]): Storage size in bits (Section 13.7.163)
STORAGE_SIZE     : S T O R A G E '_' S I Z E ;

// ============================================================================
// ENHANCED INTEGER/REAL KINDS (ISO/IEC 1539-1:2010 Section 13.8.2)
// ============================================================================
// Named constants from ISO_FORTRAN_ENV for specific bit sizes:
// - INT8, INT16, INT32, INT64: Integer kinds for 8/16/32/64-bit integers
// - REAL32, REAL64, REAL128: Real kinds for IEEE single/double/quad precision
// These are named constants, not keywords, but tokenized for grammar clarity.
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
// All letter fragments inherited from Fortran2003Lexer for case-insensitive
// keyword matching per ISO/IEC 1539-1:2010 Section 3.1.1.
