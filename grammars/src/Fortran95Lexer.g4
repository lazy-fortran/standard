// Fortran 95 (1995) Lexer - Enhanced Modern Foundation
// Building on F90 with FORALL, WHERE enhancements, and Pure Procedures
// Reference: ISO/IEC 1539-1:1997 (Fortran 95)
lexer grammar Fortran95Lexer;

import Fortran90Lexer;  // F90 unified format support

// ====================================================================
// FORTRAN 95 LEXER OVERVIEW
// ====================================================================
//
// This lexer implements tokens for Fortran 95 as defined in:
//   ISO/IEC 1539-1:1997 (Fortran 95 International Standard)
//   J3/98-114 (Fortran 95 Request for Interpretation)
//
// Fortran 95 (ISO/IEC 1539-1:1997) builds incrementally on Fortran 90,
// adding several important enhancements while maintaining complete
// backward compatibility.
//
// This lexer inherits unified format support (fixed/free) from F90
// and adds F95-specific language features.
//
// MAJOR F95 ENHANCEMENTS (ISO/IEC 1539-1:1997):
// - FORALL construct and statements (Section 7.5.4)
// - Enhanced WHERE constructs with ELSEWHERE (Section 7.5.3)
// - PURE and ELEMENTAL procedure prefixes (Section 12.6)
// - Derived type default initialization (Section 4.4.1)
// - Pointer improvements (Section 6.3.1)
// - Minor I/O enhancements (Section 9)
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

// --------------------------------------------------------------------
// FORALL construct (ISO/IEC 1539-1:1997 Section 7.5.4)
// --------------------------------------------------------------------
// The FORALL statement and construct provide a concise syntax for
// array assignments with index control. Unlike DO loops, FORALL
// semantics allow parallel execution of assignments.
//
// Syntax (Section 7.5.4.1):
//   FORALL (forall-triplet-spec-list [, scalar-mask-expr]) forall-assignment-stmt
//   FORALL (forall-triplet-spec-list [, scalar-mask-expr])
//     forall-body-construct
//     ...
//   END FORALL
//
FORALL          : ('f'|'F') ('o'|'O') ('r'|'R') ('a'|'A') ('l'|'L') ('l'|'L') ;
END_FORALL      : ('e'|'E') ('n'|'N') ('d'|'D') WS+
                  ('f'|'F') ('o'|'O') ('r'|'R') ('a'|'A') ('l'|'L') ('l'|'L') ;

// --------------------------------------------------------------------
// Enhanced WHERE support (ISO/IEC 1539-1:1997 Section 7.5.3)
// --------------------------------------------------------------------
// WHERE and END_WHERE tokens are inherited from F90.
// ELSEWHERE is inherited from F90 but F95 clarifies nesting rules
// and allows multiple ELSEWHERE clauses with optional masks.

// --------------------------------------------------------------------
// Procedure enhancements (ISO/IEC 1539-1:1997 Section 12.6)
// --------------------------------------------------------------------
// PURE procedures (Section 12.6): Functions and subroutines declared
// PURE have restricted side effects, enabling use in specification
// expressions and FORALL constructs.
//
// ELEMENTAL procedures (Section 12.6): Scalar procedures that can be
// applied element-by-element to array arguments. ELEMENTAL implies PURE.
//
// PURE and ELEMENTAL are NEW in Fortran 95, NOT inherited from F90.
// RECURSIVE is inherited from F90 (ISO/IEC 1539:1991 Section 12.5.2.1).
//
PURE            : ('p'|'P') ('u'|'U') ('r'|'R') ('e'|'E') ;
ELEMENTAL       : ('e'|'E') ('l'|'L') ('e'|'E') ('m'|'M') ('e'|'E')
                  ('n'|'N') ('t'|'T') ('a'|'A') ('l'|'L') ;

// --------------------------------------------------------------------
// Default initialization (ISO/IEC 1539-1:1997 Section 4.4.1)
// --------------------------------------------------------------------
// F95 allows default initialization of derived type components.
// Uses existing ASSIGN token - no new token needed.

// ====================================================================
// FORTRAN 90/95 INTRINSIC FUNCTIONS (ISO/IEC 1539-1:1997 Section 13)
// ====================================================================
//
// Intrinsic procedures are defined in Section 13 of ISO/IEC 1539-1:1997.
//
// NOTE ON HISTORY:
// - CEILING, FLOOR, MODULO and the bit intrinsics below were
//   introduced as intrinsic procedures in Fortran 90 (ISO/IEC 1539:1991
//   Section 13) and remain available in Fortran 95 and later.
// - Fortran 95 extends some of these (for example CEILING/FLOOR
//   with a KIND keyword argument per Section 13.10.19/13.10.39)
//   and adds new procedures such as CPU_TIME (Section 13.11.2)
//   and NULL() (Section 13.10.79).
//
// In this lexer we group these together as the "modern intrinsic"
// set that becomes part of the common core for F90/F95 and later
// standards.
//
// --------------------------------------------------------------------
// Numeric intrinsics (ISO/IEC 1539-1:1997 Section 13.10)
// --------------------------------------------------------------------
// CEILING: Section 13.10.19 - smallest integer >= argument
// FLOOR: Section 13.10.39 - largest integer <= argument
// MODULO: Section 13.10.75 - modulo function (differs from MOD for negative args)
//
CEILING_INTRINSIC     : ('c'|'C') ('e'|'E') ('i'|'I') ('l'|'L')
                        ('i'|'I') ('n'|'N') ('g'|'G') ;
FLOOR_INTRINSIC       : ('f'|'F') ('l'|'L') ('o'|'O') ('o'|'O') ('r'|'R') ;
MODULO_INTRINSIC      : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('o'|'O') ;

// --------------------------------------------------------------------
// Bit manipulation intrinsics (ISO/IEC 1539-1:1997 Section 13.10)
// --------------------------------------------------------------------
// These were introduced in Fortran 90 and refined in Fortran 95.
// BIT_SIZE: Section 13.10.11 - number of bits in integer model
// BTEST: Section 13.10.17 - test bit value
// IAND: Section 13.10.46 - bitwise AND
// IBCLR: Section 13.10.47 - clear bit
// IBITS: Section 13.10.48 - extract bit field
// IBSET: Section 13.10.49 - set bit
// IEOR: Section 13.10.50 - bitwise exclusive OR
// IOR: Section 13.10.54 - bitwise inclusive OR
// ISHFT: Section 13.10.57 - logical shift
// ISHFTC: Section 13.10.58 - circular shift
// NOT: Section 13.10.78 - bitwise complement
//
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

// --------------------------------------------------------------------
// Data transfer intrinsic (ISO/IEC 1539-1:1997 Section 13.10.112)
// --------------------------------------------------------------------
// TRANSFER: Reinterprets bit pattern of SOURCE as type of MOLD.
// Introduced in Fortran 90; unchanged in Fortran 95.
//
TRANSFER_INTRINSIC    : ('t'|'T') ('r'|'R') ('a'|'A') ('n'|'N')
                        ('s'|'S') ('f'|'F') ('e'|'E') ('r'|'R') ;

// --------------------------------------------------------------------
// Timing intrinsics (ISO/IEC 1539-1:1997 Section 13.11)
// --------------------------------------------------------------------
// CPU_TIME: Section 13.11.2 - NEW in Fortran 95, returns processor time
// SYSTEM_CLOCK: Section 13.11.5 - returns system clock data
//   (introduced in F90, unchanged in F95)
//
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
// explicitly modelled and tested in this repository.
//
// The list below describes the intended coverage; it is not a formal
// statement of complete ISO/IEC 1539-1:1997 conformance.
//
// MAJOR F95 FEATURES TARGETED (IN THIS LEXER):
// ✅ FORALL constructs and statements (Section 7.5.4)
// ✅ Enhanced WHERE/ELSEWHERE constructs and nesting (Section 7.5.3)
// ✅ PURE/ELEMENTAL procedure prefixes (Section 12.6)
// ✅ Modern intrinsic functions from the F90/F95 core (Section 13)
//    (CEILING, FLOOR, MODULO, bit intrinsics, TRANSFER, etc.)
// ✅ CPU_TIME intrinsic (Section 13.11.2) and related timing intrinsics
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
// ISO/IEC 1539-1:1997 SPEC-GRAMMAR MAPPING (LEXER):
// Section 3.2 (Low-level syntax)  -> Inherited from Fortran90Lexer
// Section 7.5.3 (WHERE)           -> WHERE, ELSEWHERE, END_WHERE (F90)
// Section 7.5.4 (FORALL)          -> FORALL, END_FORALL tokens
// Section 12.6 (PURE/ELEMENTAL)   -> PURE, ELEMENTAL tokens
// Section 13 (Intrinsic procs)    -> *_INTRINSIC tokens
//
// This lexer provides practical F95 language support for the features
// above while maintaining the unified format architecture from F90.
//
// ====================================================================
