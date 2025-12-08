// Fortran 95 (1995) Lexer - Enhanced Modern Foundation
// Building on F90 with FORALL, WHERE enhancements, and Pure Procedures
lexer grammar Fortran95Lexer;

import Fortran90Lexer;  // F90 unified format support

// ====================================================================
// FORTRAN 95 LEXER OVERVIEW
// ====================================================================
//
// Fortran 95 (ISO 1539-1:1997) builds incrementally on Fortran 90,
// adding several important enhancements while maintaining complete
// backward compatibility.
//
// This lexer inherits unified format support (fixed/free) from F90
// and adds F95-specific language features.
//
// MAJOR F95 ENHANCEMENTS:
// - FORALL construct and statements (advanced array operations)
// - Enhanced WHERE constructs with ELSEWHERE
// - PURE and ELEMENTAL procedure prefixes (NEW in F95)
// - Derived type default initialization
// - Pointer improvements
// - Minor I/O enhancements
//
// INHERITANCE ARCHITECTURE (IN THIS REPO):
// FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
//   → Fortran90Lexer
//   → Fortran95Lexer
//   → F2003+ standards
//
// ====================================================================

// ====================================================================
// FORTRAN 95 KEYWORDS (INCREMENTAL ENHANCEMENTS)
// ====================================================================

// FORALL construct (F95 major addition - advanced array operations)
FORALL          : ('f'|'F') ('o'|'O') ('r'|'R') ('a'|'A') ('l'|'L') ('l'|'L') ;
END_FORALL      : ('e'|'E') ('n'|'N') ('d'|'D') WS+ 
                  ('f'|'F') ('o'|'O') ('r'|'R') ('a'|'A') ('l'|'L') ('l'|'L') ;

// Enhanced WHERE support (F95 improvements)
// WHERE and END_WHERE are inherited from F90
// ELSEWHERE is inherited from F90 but enhanced in F95

// Procedure enhancements (F95 - ISO/IEC 1539-1:1997 Section 12.6)
// PURE and ELEMENTAL are NEW in Fortran 95, NOT inherited from F90.
// RECURSIVE is inherited from F90 (ISO/IEC 1539:1991).
PURE            : ('p'|'P') ('u'|'U') ('r'|'R') ('e'|'E') ;
ELEMENTAL       : ('e'|'E') ('l'|'L') ('e'|'E') ('m'|'M') ('e'|'E')
                  ('n'|'N') ('t'|'T') ('a'|'A') ('l'|'L') ;

// Default initialization keyword (F95 derived type enhancement)
// Uses existing ASSIGN token - no new token needed

// ====================================================================
// FORTRAN 90/95 INTRINSIC FUNCTIONS (MODERN INTRINSICS)
// ====================================================================
//
// NOTE ON HISTORY:
// - CEILING, FLOOR, MODULO and the bit intrinsics below were
//   introduced as intrinsic procedures in Fortran 90 and remain
//   available in Fortran 95 and later.
// - Fortran 95 extends some of these (for example CEILING/FLOOR
//   with a KIND keyword argument) and adds new procedures such
//   as CPU_TIME and NULL().
//
// In this lexer we group these together as the “modern intrinsic”
// set that becomes part of the common core for F90/F95 and later
// standards.
//
// Additional array-related functions (Fortran 90, extended in F95)
CEILING_INTRINSIC     : ('c'|'C') ('e'|'E') ('i'|'I') ('l'|'L') 
                        ('i'|'I') ('n'|'N') ('g'|'G') ;
FLOOR_INTRINSIC       : ('f'|'F') ('l'|'L') ('o'|'O') ('o'|'O') ('r'|'R') ;
MODULO_INTRINSIC      : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('o'|'O') ;

// Bit manipulation functions (introduced in Fortran 90, used in F95)
BIT_SIZE_INTRINSIC    : ('b'|'B') ('i'|'I') ('t'|'T') '_' 
                        ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
BTEST_INTRINSIC       : ('b'|'B') ('t'|'T') ('e'|'E') ('s'|'S') ('t'|'T') ;
IAND_INTRINSIC        : ('i'|'I') ('a'|'A') ('n'|'N') ('d'|'D') ;
IBCLR_INTRINSIC       : ('i'|'I') ('b'|'B') ('c'|'C') ('l'|'L') ('r'|'R') ;
IBITS_INTRINSIC       : ('i'|'I') ('b'|'B') ('i'|'I') ('t'|'T') ('s'|'S') ;
IBSET_INTRINSIC       : ('i'|'I') ('b'|'B') ('s'|'S') ('e'|'E') ('t'|'T') ;
IEOR_INTRINSIC        : ('i'|'I') ('e'|'E') ('o'|'O') ('r'|'R') ;
IOR_INTRINSIC         : ('i'|'I') ('o'|'O') ('r'|'R') ;
ISHFT_INTRINSIC       : ('i'|'I') ('s'|'S') ('h'|'H') ('f'|'F') ('t'|'T') ;
ISHFTC_INTRINSIC      : ('i'|'I') ('s'|'S') ('h'|'H') ('f'|'F') ('t'|'T') ('c'|'C') ;
NOT_INTRINSIC         : ('n'|'N') ('o'|'O') ('t'|'T') ;

// Transfer function (introduced in Fortran 90)
TRANSFER_INTRINSIC    : ('t'|'T') ('r'|'R') ('a'|'A') ('n'|'N') 
                        ('s'|'S') ('f'|'F') ('e'|'E') ('r'|'R') ;

// CPU timing function (new in Fortran 95)
CPU_TIME_INTRINSIC    : ('c'|'C') ('p'|'P') ('u'|'U') '_' 
                        ('t'|'T') ('i'|'I') ('m'|'M') ('e'|'E') ;
// System clock (intrinsic available since Fortran 90, kept here as
// part of the shared F90/F95+ “modern” intrinsic set)
SYSTEM_CLOCK_INTRINSIC : ('s'|'S') ('y'|'Y') ('s'|'S') ('t'|'T') ('e'|'E') ('m'|'M') 
                         '_' ('c'|'C') ('l'|'L') ('o'|'O') ('c'|'C') ('k'|'K') ;

// ====================================================================
// FORTRAN 95 LEXER NOTES
// ====================================================================
//
// This lexer inherits the unified fixed/free-format support from
// Fortran90Lexer and adds tokens for the F95 features that are
// explicitly modelled and tested in this repository.
//
// The list below describes the intended coverage; it is not a formal
// statement of complete ISO/IEC 1539-1:1997 conformance.
//
// MAJOR F95 FEATURES TARGETED (IN THIS LEXER):
// ✅ FORALL constructs and statements (added in F95)
// ✅ Enhanced WHERE/ELSEWHERE constructs and nesting (F95 refinements)
// ✅ PURE/ELEMENTAL procedure prefixes (NEW in F95)
// ✅ Modern intrinsic functions from the F90/F95 core
//    (CEILING, FLOOR, MODULO, bit intrinsics, TRANSFER, etc.)
// ✅ CPU_TIME intrinsic (new in F95) and related timing intrinsics
//
// BACKWARD COMPATIBILITY:
// ✅ Complete F90 compatibility through inheritance
// ✅ All F90 features available unchanged
// ✅ F77 compatibility through inheritance chain
//
// FORWARD COMPATIBILITY:
// ✅ Foundation for F2003 object-oriented features
// ✅ Clean extension points for parameterized types
// ✅ Ready for C interoperability enhancements
//
// This lexer provides practical F95 language support for the features
// above while maintaining the unified format architecture from F90.
//
// ====================================================================
