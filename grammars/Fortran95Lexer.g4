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
// - PURE and ELEMENTAL procedure enhancements
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

// Procedure enhancements (F95 clarifications)
// PURE and ELEMENTAL are inherited from F90 but enhanced in F95
// RECURSIVE is inherited from F90

// Default initialization keyword (F95 derived type enhancement)
// Uses existing ASSIGN token - no new token needed

// ====================================================================
// FORTRAN 95 INTRINSIC FUNCTIONS (ADDITIONS)
// ====================================================================

// Additional array functions (F95 enhancements)
CEILING_INTRINSIC     : ('c'|'C') ('e'|'E') ('i'|'I') ('l'|'L') 
                        ('i'|'I') ('n'|'N') ('g'|'G') ;
FLOOR_INTRINSIC       : ('f'|'F') ('l'|'L') ('o'|'O') ('o'|'O') ('r'|'R') ;
MODULO_INTRINSIC      : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('o'|'O') ;

// Bit manipulation functions (F95 additions)
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

// Transfer function (F95 addition)
TRANSFER_INTRINSIC    : ('t'|'T') ('r'|'R') ('a'|'A') ('n'|'N') 
                        ('s'|'S') ('f'|'F') ('e'|'E') ('r'|'R') ;

// CPU timing function (F95 addition)
CPU_TIME_INTRINSIC    : ('c'|'C') ('p'|'P') ('u'|'U') '_' 
                        ('t'|'T') ('i'|'I') ('m'|'M') ('e'|'E') ;
SYSTEM_CLOCK_INTRINSIC : ('s'|'S') ('y'|'Y') ('s'|'S') ('t'|'T') ('e'|'E') ('m'|'M') 
                         '_' ('c'|'C') ('l'|'L') ('o'|'O') ('c'|'C') ('k'|'K') ;

// ====================================================================
// FORTRAN 95 LEXER NOTES
// ====================================================================
//
// This lexer inherits the unified fixed/free-format support from
// Fortran90Lexer and adds tokens for the F95 features that are
// explicitly modelled in this repository.
//
// The comments below describe the intended coverage; they should not be
// interpreted as a formal claim of complete ISO/IEC 1539-1:1997
// conformance.
//
// MAJOR F95 FEATURES TARGETED:
// ✅ FORALL constructs and statements
// ✅ Enhanced WHERE/ELSEWHERE constructs
// ✅ Enhanced PURE/ELEMENTAL procedures
// ✅ New intrinsic functions (CEILING, FLOOR, MODULO, etc.)
// ✅ Bit manipulation functions
// ✅ System timing functions
// ✅ Transfer function for type conversion
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
// This lexer provides complete F95 language support while maintaining
// the unified format architecture from F90.
//
// ====================================================================
