// Fortran 90 (1990) Lexer - Unified Fixed/Free Form Support
// Reference: ISO/IEC 1539:1991 (Fortran 90)
//            WG5 N692 draft (validation/pdfs/Fortran90_WG5_N692.txt)
lexer grammar Fortran90Lexer;

import FORTRAN77Lexer;  // Inherit F77 (1977) constructs

// ====================================================================
// FORTRAN 90 UNIFIED LEXER OVERVIEW
// ====================================================================
//
// This lexer implements token rules for Fortran 90 as defined in:
//   ISO/IEC 1539:1991 (Fortran 90 International Standard)
//   WG5 N692 (Fortran 90 draft, equivalent to final standard)
//
// ISO/IEC 1539:1991 Standard Structure Overview:
//   Section 1: Overview
//   Section 2: Fortran Terms and Concepts
//   Section 3: Characters and Low Level Syntax (3.1-3.4)
//   Section 4: Data Types (4.1-4.5)
//   Section 5: Data Object Declarations (5.1-5.5)
//   Section 6: Use of Data Objects (6.1-6.5)
//   Section 7: Expressions and Assignment (7.1-7.5)
//   Section 8: Execution Control (8.1-8.3)
//   Section 9: Input/Output Statements (9.1-9.9)
//   Section 10: Input/Output Editing (10.1-10.9)
//   Section 11: Program Units (11.1-11.3)
//   Section 12: Procedures (12.1-12.5)
//   Section 13: Intrinsic Procedures (13.1-13.14)
//   Section 14: Scope, Association, and Definition (14.1-14.7)
//   Annex A: Glossary
//   Annex B: Syntax Rules (BNF summary)
//
// This lexer supports BOTH fixed-form and free-form source formats:
// - Fixed-form: .f, .for files (ISO/IEC 1539:1991 Section 3.3)
// - Free-form: .f90+ files (ISO/IEC 1539:1991 Section 3.4)
//
// Format detection is handled by the parser/driver based on file extension.
//
// MAJOR F90 FEATURES (with ISO section references):
// - Free-form source (Section 3.4)
// - Module system with explicit interfaces (Section 11.3)
// - Dynamic arrays - ALLOCATABLE, POINTER (Section 5.1.2.4, 6.3)
// - Derived types - user-defined structures (Section 4.4)
// - Array operations and constructors (Section 4.5, 7.5.4)
// - Enhanced control flow - SELECT CASE, WHERE (Section 8.1.3, 7.5.3)
// - Modern I/O - NAMELIST, non-advancing (Section 5.4, 9.4.1)
// - KIND type parameters (Section 4.3.1)
// - Recursive procedures (Section 12.5.2)
//
// INHERITANCE ARCHITECTURE (IN THIS REPO):
// FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
//   -> Fortran90Lexer
//   -> Fortran95Lexer
//   -> F2003+ standards
//
// ====================================================================

// ====================================================================
// SOURCE FORM - ISO/IEC 1539:1991 Section 3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 3 defines the source form:
// - Section 3.1: Processor character set (alphanumeric and special chars)
// - Section 3.2: Low-level syntax (lexical tokens)
// - Section 3.3: Fixed source form (columns 1-72, labels in 1-5, cont in 6)
// - Section 3.4: Free source form (line-oriented, ! comments, & continuation)
//
// Default mode is free-form (F90+ primary format)
// Fixed-form mode can be activated by parser based on file extension

// ====================================================================
// COMMENTS - ISO/IEC 1539:1991 Section 3.3.2 and 3.4.4
// ====================================================================

// Free-form comment - ISO/IEC 1539:1991 Section 3.4.4
// Syntax: ! followed by any characters to end of line
// The ! initiates a comment that extends to the end of the line
FREE_FORM_COMMENT
    : '!' ~[\r\n]* -> channel(HIDDEN)
    ;

// Fixed-form comment - ISO/IEC 1539:1991 Section 3.3.2
// Syntax: C, c, or * in column 1 makes entire line a comment
// Must follow a newline to ensure column 1 position
FIXED_FORM_COMMENT
    : [\r\n]+ [Cc] [ \t] ~[\r\n]* -> channel(HIDDEN)
    ;

// Star comment at column 1 (fixed-form only)
// ISO/IEC 1539:1991 Section 3.3.2
STAR_COMMENT
    : [\r\n]+ '*' ~[\r\n]* -> channel(HIDDEN)
    ;

// ====================================================================
// CONTINUATION - ISO/IEC 1539:1991 Section 3.3.1.3 and 3.4.3
// ====================================================================

// Free-form continuation - ISO/IEC 1539:1991 Section 3.4.3
// Syntax: & at end of line continues statement on next line
// An optional & at start of next line indicates where continuation resumes
CONTINUATION
    : '&' [ \t]* FREE_FORM_COMMENT? -> channel(HIDDEN)
    ;

// ====================================================================
// MODULE SYSTEM KEYWORDS - ISO/IEC 1539:1991 Section 11.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 11.3 defines the module system:
// - R1104 (module) -> MODULE module-name [specification-part]
//                     [module-subprogram-part] END [MODULE [module-name]]
// - R1107 (use-stmt) -> USE module-name [, rename-list] |
//                       USE module-name, ONLY: [only-list]
// - Section 11.3.2 defines PUBLIC and PRIVATE accessibility

// MODULE keyword - ISO/IEC 1539:1991 Section 11.3, R1104
MODULE          : ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
// END MODULE - ISO/IEC 1539:1991 Section 11.3, R1104
END_MODULE      : ('e'|'E') ('n'|'N') ('d'|'D') WS+
                  ('m'|'M') ('o'|'O') ('d'|'D') ('u'|'U') ('l'|'L') ('e'|'E') ;
// USE keyword - ISO/IEC 1539:1991 Section 11.3.2, R1107
USE             : ('u'|'U') ('s'|'S') ('e'|'E') ;
// ONLY keyword - ISO/IEC 1539:1991 Section 11.3.2, R1108
ONLY            : ('o'|'O') ('n'|'N') ('l'|'L') ('y'|'Y') ;
// PUBLIC attribute - ISO/IEC 1539:1991 Section 5.1.2.1, R522
PUBLIC          : ('p'|'P') ('u'|'U') ('b'|'B') ('l'|'L') ('i'|'I') ('c'|'C') ;
// PRIVATE attribute - ISO/IEC 1539:1991 Section 5.1.2.1, R522
PRIVATE         : ('p'|'P') ('r'|'R') ('i'|'I') ('v'|'V')
                  ('a'|'A') ('t'|'T') ('e'|'E') ;

// ====================================================================
// INTERFACE BLOCKS - ISO/IEC 1539:1991 Section 12.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 12.3 defines interface blocks:
// - R1201 (interface-block) -> interface-stmt [interface-specification]...
//                              end-interface-stmt
// - R1202 (interface-stmt) -> INTERFACE [generic-spec]
// - R1203 (generic-spec) -> generic-name | OPERATOR(defined-operator) |
//                           ASSIGNMENT(=)

// INTERFACE keyword - ISO/IEC 1539:1991 Section 12.3, R1202
INTERFACE       : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('r'|'R')
                  ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
// END INTERFACE - ISO/IEC 1539:1991 Section 12.3, R1201
END_INTERFACE   : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('i'|'I') ('n'|'N') ('t'|'T')
                  ('e'|'E') ('r'|'R') ('f'|'F') ('a'|'A') ('c'|'C') ('e'|'E') ;
// GENERIC keyword - ISO/IEC 1539:1991 Section 12.3.2.1
GENERIC         : ('g'|'G') ('e'|'E') ('n'|'N') ('e'|'E')
                  ('r'|'R') ('i'|'I') ('c'|'C') ;
// OPERATOR keyword - ISO/IEC 1539:1991 Section 12.3, R1203
OPERATOR        : ('o'|'O') ('p'|'P') ('e'|'E') ('r'|'R')
                  ('a'|'A') ('t'|'T') ('o'|'O') ('r'|'R') ;
// ASSIGNMENT keyword - ISO/IEC 1539:1991 Section 12.3, R1203
ASSIGNMENT      : ('a'|'A') ('s'|'S') ('s'|'S') ('i'|'I') ('g'|'G')
                  ('n'|'N') ('m'|'M') ('e'|'E') ('n'|'N') ('t'|'T') ;

// ====================================================================
// PROCEDURE ENHANCEMENTS - ISO/IEC 1539:1991 Section 12.5.2
// ====================================================================
//
// ISO/IEC 1539:1991 Section 12.5.2 defines procedure prefixes:
// - R1214 (prefix) -> prefix-spec [prefix-spec]...
// - R1215 (prefix-spec) -> type-spec | RECURSIVE
// NOTE: PURE and ELEMENTAL are Fortran 95 features (ISO/IEC 1539-1:1997),
// defined in Fortran95Lexer.g4, NOT here.

// RECURSIVE prefix - ISO/IEC 1539:1991 Section 12.5.2, R1215
RECURSIVE       : ('r'|'R') ('e'|'E') ('c'|'C') ('u'|'U') ('r'|'R')
                  ('s'|'S') ('i'|'I') ('v'|'V') ('e'|'E') ;
// RESULT clause - ISO/IEC 1539:1991 Section 12.5.2.2, R1216
RESULT          : ('r'|'R') ('e'|'E') ('s'|'S') ('u'|'U') ('l'|'L') ('t'|'T') ;

// ====================================================================
// DERIVED TYPES - ISO/IEC 1539:1991 Section 4.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.4 defines derived types:
// - R422 (derived-type-def) -> derived-type-stmt [private-sequence-stmt]...
//                              component-def-stmt... end-type-stmt
// - R423 (derived-type-stmt) -> TYPE [[, access-spec] ::] type-name
// - R429 (end-type-stmt) -> END TYPE [type-name]
// - R430 (sequence-stmt) -> SEQUENCE

// TYPE keyword - ISO/IEC 1539:1991 Section 4.4, R423
TYPE            : ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
// END TYPE - ISO/IEC 1539:1991 Section 4.4, R429
END_TYPE        : ('e'|'E') ('n'|'N') ('d'|'D') WS+
                  ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E') ;
// SEQUENCE statement - ISO/IEC 1539:1991 Section 4.4.1, R430
SEQUENCE        : ('s'|'S') ('e'|'E') ('q'|'Q') ('u'|'U')
                  ('e'|'E') ('n'|'N') ('c'|'C') ('e'|'E') ;

// ====================================================================
// DYNAMIC MEMORY MANAGEMENT - ISO/IEC 1539:1991 Section 5.1.2.4 and 6.3
// ====================================================================
//
// ISO/IEC 1539:1991 defines dynamic memory constructs:
// - Section 5.1.2.4.3: ALLOCATABLE attribute (R515)
// - Section 5.1.2.4.4: POINTER attribute (R516)
// - Section 5.1.2.4.5: TARGET attribute (R517)
// - Section 6.3.1: ALLOCATE statement (R620)
// - Section 6.3.3: DEALLOCATE statement (R626)
// - Section 6.3.2: NULLIFY statement (R624)

// ALLOCATABLE attribute - ISO/IEC 1539:1991 Section 5.1.2.4.3, R515
ALLOCATABLE     : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C') ('a'|'A')
                  ('t'|'T') ('a'|'A') ('b'|'B') ('l'|'L') ('e'|'E') ;
// POINTER attribute - ISO/IEC 1539:1991 Section 5.1.2.4.4, R516
POINTER         : ('p'|'P') ('o'|'O') ('i'|'I') ('n'|'N')
                  ('t'|'T') ('e'|'E') ('r'|'R') ;
// TARGET attribute - ISO/IEC 1539:1991 Section 5.1.2.4.5, R517
TARGET          : ('t'|'T') ('a'|'A') ('r'|'R') ('g'|'G') ('e'|'E') ('t'|'T') ;
// ALLOCATE statement - ISO/IEC 1539:1991 Section 6.3.1, R620
ALLOCATE        : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O')
                  ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
// DEALLOCATE statement - ISO/IEC 1539:1991 Section 6.3.3, R626
DEALLOCATE      : ('d'|'D') ('e'|'E') ('a'|'A') ('l'|'L') ('l'|'L')
                  ('o'|'O') ('c'|'C') ('a'|'A') ('t'|'T') ('e'|'E') ;
// NULLIFY statement - ISO/IEC 1539:1991 Section 6.3.2, R624
NULLIFY         : ('n'|'N') ('u'|'U') ('l'|'L') ('l'|'L')
                  ('i'|'I') ('f'|'F') ('y'|'Y') ;
// ASSOCIATED intrinsic - ISO/IEC 1539:1991 Section 13.8.6
ASSOCIATED      : ('a'|'A') ('s'|'S') ('s'|'S') ('o'|'O') ('c'|'C')
                  ('i'|'I') ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// ====================================================================
// CONTROL FLOW - ISO/IEC 1539:1991 Section 8.1
// ====================================================================
//
// ISO/IEC 1539:1991 Section 8.1 defines execution control constructs:
// - Section 8.1.3: CASE construct (R808-R813)
// - Section 7.5.3: WHERE construct (R738-R743)
// - Section 8.1.4.4: CYCLE and EXIT statements (R834, R835)

// SELECT CASE keywords - ISO/IEC 1539:1991 Section 8.1.3, R808
SELECT          : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
// CASE keyword - ISO/IEC 1539:1991 Section 8.1.3.2, R811
CASE            : ('c'|'C') ('a'|'A') ('s'|'S') ('e'|'E') ;
// DEFAULT case selector - ISO/IEC 1539:1991 Section 8.1.3.2, R812
DEFAULT         : ('d'|'D') ('e'|'E') ('f'|'F') ('a'|'A')
                  ('u'|'U') ('l'|'L') ('t'|'T') ;
// END SELECT - ISO/IEC 1539:1991 Section 8.1.3, R813
END_SELECT      : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('s'|'S') ('e'|'E')
                  ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T') ;
// WHERE keyword - ISO/IEC 1539:1991 Section 7.5.3, R738
WHERE           : ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;
// END WHERE - ISO/IEC 1539:1991 Section 7.5.3, R743
END_WHERE       : ('e'|'E') ('n'|'N') ('d'|'D') WS+ ('w'|'W') ('h'|'H')
                  ('e'|'E') ('r'|'R') ('e'|'E') ;
// ELSEWHERE - ISO/IEC 1539:1991 Section 7.5.3, R741
ELSEWHERE       : ('e'|'E') ('l'|'L') ('s'|'S') ('e'|'E') ('w'|'W')
                  ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E') ;

// ====================================================================
// LOOP CONTROL - ISO/IEC 1539:1991 Section 8.1.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 8.1.4 defines DO construct control:
// - Section 8.1.4.4.1: CYCLE statement (R834)
// - Section 8.1.4.4.2: EXIT statement (R835)

// CYCLE statement - ISO/IEC 1539:1991 Section 8.1.4.4.1, R834
CYCLE           : ('c'|'C') ('y'|'Y') ('c'|'C') ('l'|'L') ('e'|'E') ;
// EXIT statement - ISO/IEC 1539:1991 Section 8.1.4.4.2, R835
EXIT            : ('e'|'E') ('x'|'X') ('i'|'I') ('t'|'T') ;

// ====================================================================
// INPUT/OUTPUT - ISO/IEC 1539:1991 Section 9
// ====================================================================
//
// ISO/IEC 1539:1991 Section 9 defines I/O statements and specifiers:
// - Section 5.4: NAMELIST statement (R527)
// - Section 9.4.1: I/O control specifiers (R911-R921)
// - Section 9.4.1.5: ADVANCE= specifier (non-advancing I/O)
// - Section 9.4.1.6: SIZE= specifier (characters transferred)
// - Section 9.4.1.2: IOSTAT= specifier
// - Section 9.4.1.4: EOR= specifier (end-of-record)

// NAMELIST statement - ISO/IEC 1539:1991 Section 5.4, R527
NAMELIST        : ('n'|'N') ('a'|'A') ('m'|'M') ('e'|'E')
                  ('l'|'L') ('i'|'I') ('s'|'S') ('t'|'T') ;
// ADVANCE= specifier - ISO/IEC 1539:1991 Section 9.4.1.5
ADVANCE         : ('a'|'A') ('d'|'D') ('v'|'V') ('a'|'A')
                  ('n'|'N') ('c'|'C') ('e'|'E') ;
// SIZE= specifier - ISO/IEC 1539:1991 Section 9.4.1.6
SIZE            : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
// STAT= specifier (allocation status) - ISO/IEC 1539:1991 Section 6.3.1, R623
STAT            : ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;
// EOR= specifier - ISO/IEC 1539:1991 Section 9.4.1.4
EOR             : ('e'|'E') ('o'|'O') ('r'|'R') ;
// IOSTAT= specifier - ISO/IEC 1539:1991 Section 9.4.1.2, R921
IOSTAT          : ('i'|'I') ('o'|'O') ('s'|'S') ('t'|'T') ('a'|'A') ('t'|'T') ;

// ====================================================================
// PROCEDURE ARGUMENTS - ISO/IEC 1539:1991 Section 5.1.2.3 and 12.4.1.2
// ====================================================================
//
// ISO/IEC 1539:1991 defines procedure argument attributes:
// - Section 5.1.2.3: INTENT attribute (R518)
// - Section 5.1.2.6: OPTIONAL attribute (R521)
// - Section 13.8.64: PRESENT intrinsic

// INTENT attribute - ISO/IEC 1539:1991 Section 5.1.2.3, R518
INTENT          : ('i'|'I') ('n'|'N') ('t'|'T') ('e'|'E') ('n'|'N') ('t'|'T') ;
// INTENT(IN) - ISO/IEC 1539:1991 Section 5.1.2.3, R519
IN              : ('i'|'I') ('n'|'N') ;
// INTENT(OUT) - ISO/IEC 1539:1991 Section 5.1.2.3, R519
OUT             : ('o'|'O') ('u'|'U') ('t'|'T') ;
// INTENT(INOUT) - ISO/IEC 1539:1991 Section 5.1.2.3, R519
INOUT           : ('i'|'I') ('n'|'N') ('o'|'O') ('u'|'U') ('t'|'T') ;

// OPTIONAL attribute - ISO/IEC 1539:1991 Section 5.1.2.6, R521
OPTIONAL        : ('o'|'O') ('p'|'P') ('t'|'T') ('i'|'I')
                  ('o'|'O') ('n'|'N') ('a'|'A') ('l'|'L') ;
// PRESENT intrinsic - ISO/IEC 1539:1991 Section 13.8.64
PRESENT         : ('p'|'P') ('r'|'R') ('e'|'E') ('s'|'S')
                  ('e'|'E') ('n'|'N') ('t'|'T') ;

// ====================================================================
// KIND TYPE PARAMETERS - ISO/IEC 1539:1991 Section 4.3.1
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.3.1 defines kind type parameters:
// - R401 (type-param-value) -> scalar-int-expr | * | :
// - R404 (kind-selector) -> (KIND = scalar-int-initialization-expr) |
//                           (scalar-int-initialization-expr)
// - Section 13.8.73: SELECTED_INT_KIND intrinsic
// - Section 13.8.74: SELECTED_REAL_KIND intrinsic

// KIND keyword - ISO/IEC 1539:1991 Section 4.3.1, R404
KIND            : ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
// LEN keyword (character length) - ISO/IEC 1539:1991 Section 4.3.2.1, R406
LEN             : ('l'|'L') ('e'|'E') ('n'|'N') ;
// SELECTED_INT_KIND intrinsic - ISO/IEC 1539:1991 Section 13.8.73
SELECTED_INT_KIND     : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C')
                        ('t'|'T') ('e'|'E') ('d'|'D') '_' ('i'|'I') ('n'|'N')
                        ('t'|'T') '_' ('k'|'K') ('i'|'I') ('n'|'N') ('d'|'D') ;
// SELECTED_REAL_KIND intrinsic - ISO/IEC 1539:1991 Section 13.8.74
SELECTED_REAL_KIND    : ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C')
                        ('t'|'T') ('e'|'E') ('d'|'D') '_' ('r'|'R') ('e'|'E')
                        ('a'|'A') ('l'|'L') '_' ('k'|'K') ('i'|'I')
                        ('n'|'N') ('d'|'D') ;

// ====================================================================
// PROGRAM STRUCTURE KEYWORDS - ISO/IEC 1539:1991 Section 11-12
// ====================================================================
//
// ISO/IEC 1539:1991 defines program structure keywords:
// - Section 12.5.1: CONTAINS statement (R1217)
// - Section 12.3.2.2: IMPORT statement (host association)
// - Section 12.3: PROCEDURE keyword
// - Section 9.4.1.1: UNIT=, FMT=, REC=, ERR= specifiers
// - Section 8.1.4: DO WHILE construct

// CONTAINS statement - ISO/IEC 1539:1991 Section 12.5.1, R1217
CONTAINS        : ('c'|'C') ('o'|'O') ('n'|'N') ('t'|'T')
                  ('a'|'A') ('i'|'I') ('n'|'N') ('s'|'S') ;
// IMPORT statement - ISO/IEC 1539:1991 Section 12.3.2.2 (host association)
// NOTE: Full IMPORT is Fortran 2003; F90 has implicit host association
IMPORT          : ('i'|'I') ('m'|'M') ('p'|'P') ('o'|'O') ('r'|'R') ('t'|'T') ;
// PROCEDURE keyword - ISO/IEC 1539:1991 Section 12.3
PROCEDURE       : ('p'|'P') ('r'|'R') ('o'|'O') ('c'|'C') ('e'|'E')
                  ('d'|'D') ('u'|'U') ('r'|'R') ('e'|'E') ;
// UNIT= specifier - ISO/IEC 1539:1991 Section 9.4.1.1, R911
UNIT            : ('u'|'U') ('n'|'N') ('i'|'I') ('t'|'T') ;
// FMT= specifier - ISO/IEC 1539:1991 Section 9.4.1.1, R912
FMT             : ('f'|'F') ('m'|'M') ('t'|'T') ;
// REC= specifier - ISO/IEC 1539:1991 Section 9.4.1.3
REC             : ('r'|'R') ('e'|'E') ('c'|'C') ;
// ERR= specifier - ISO/IEC 1539:1991 Section 9.4.1.2
ERR             : ('e'|'E') ('r'|'R') ('r'|'R') ;
// WHILE keyword (DO WHILE) - ISO/IEC 1539:1991 Section 8.1.4.1.1, R820
WHILE           : ('w'|'W') ('h'|'H') ('i'|'I') ('l'|'L') ('e'|'E') ;

// ====================================================================
// FORTRAN 90 OPERATORS - ISO/IEC 1539:1991 Section 3.2 and 7.2
// ====================================================================
//
// ISO/IEC 1539:1991 defines new operators and special characters:
// - Section 3.2.3: Special characters
// - Section 7.2.2: Relational operators (new symbolic forms)
// - Section 6.1.2: Component selector (%)
// - Section 7.5.2: Pointer assignment (=>)
// - Section 4.5: Array constructor delimiter (/ /)

// Double colon separator - ISO/IEC 1539:1991 Section 5.1, R501
DOUBLE_COLON    : '::' ;
// Pointer assignment operator - ISO/IEC 1539:1991 Section 7.5.2, R735
POINTER_ASSIGN  : '=>' ;
// Component selector - ISO/IEC 1539:1991 Section 6.1.2, R614
PERCENT         : '%' ;
// Slash (array constructor delimiter) - ISO/IEC 1539:1991 Section 4.5, R432
SLASH           : '/' ;
// NOTE: Square brackets for array constructors [ ... ] are a Fortran 2003
// feature (ISO/IEC 1539-1:2004), defined in Fortran2003Lexer.g4, NOT here.

// ====================================================================
// ARRAY INQUIRY KEYWORDS - ISO/IEC 1539:1991 Section 13.8
// ====================================================================

// LBOUND intrinsic - ISO/IEC 1539:1991 Section 13.8.43
LBOUND          : L B O U N D ;
// UBOUND intrinsic - ISO/IEC 1539:1991 Section 13.8.93
UBOUND          : U B O U N D ;
// ALLOCATED intrinsic - ISO/IEC 1539:1991 Section 13.8.4
ALLOCATED       : A L L O C A T E D ;

// ====================================================================
// RELATIONAL OPERATORS - ISO/IEC 1539:1991 Section 7.2.2
// ====================================================================
//
// ISO/IEC 1539:1991 Section 7.2.2.4 defines new symbolic relational operators:
// - .EQ. and == (equal)
// - .NE. and /= (not equal)
// - .LT. and < (less than)
// - .LE. and <= (less than or equal)
// - .GT. and > (greater than)
// - .GE. and >= (greater than or equal)
// The dotted forms are inherited from FORTRAN 77.

// == operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
EQ_OP           : '==' ;
// /= operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
NE_OP           : '/=' ;
// < operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
LT_OP           : '<' ;
// <= operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
LE_OP           : '<=' ;
// > operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
GT_OP           : '>' ;
// >= operator - ISO/IEC 1539:1991 Section 7.2.2.4, R713
GE_OP           : '>=' ;

// ====================================================================
// FORTRAN 90 LITERALS - ISO/IEC 1539:1991 Section 4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4 defines literal constants:
// - Section 4.3.1.1: Integer literal constants (R405)
// - Section 4.3.1.2: Real literal constants (R412)
// - Section 4.3.2: Character literal constants (R420)
// - Section 4.3.3: BOZ literal constants (R407-R409)
// - Section 4.3.1: Kind type parameters (_kind suffix)

// Kind-parameterized integer literal - ISO/IEC 1539:1991 Section 4.3.1.1, R405
INTEGER_LITERAL_KIND : DIGIT+ '_' IDENTIFIER ;

// Integer literal constant - ISO/IEC 1539:1991 Section 4.3.1.1, R405
// Override to take precedence over LABEL from earlier standards
INTEGER_LITERAL : DIGIT+ ;

// Real literal constant - ISO/IEC 1539:1991 Section 4.3.1.2, R412
// Override to ensure consistency
REAL_LITERAL    : DIGIT+ '.' DIGIT* EXPONENT?
                | '.' DIGIT+ EXPONENT?
                | DIGIT+ EXPONENT ;

// Kind-parameterized real literal - ISO/IEC 1539:1991 Section 4.3.1.2, R413
REAL_LITERAL_KIND    : (DIGIT+ '.' DIGIT* | '.' DIGIT+) EXPONENT? '_' IDENTIFIER
                    | DIGIT+ EXPONENT '_' IDENTIFIER ;

// Character literal constants - ISO/IEC 1539:1991 Section 4.3.2.1, R420
// F90 adds double quotes as alternative to single quotes
DOUBLE_QUOTE_STRING  : '"' (~["\r\n] | '""')* '"' ;
SINGLE_QUOTE_STRING  : '\'' (~['\r\n] | '\'\'')* '\'' ;

// ====================================================================
// BOZ LITERAL CONSTANTS - ISO/IEC 1539:1991 Section 4.3.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 4.3.3 defines BOZ literal constants:
// - R407 (boz-literal-constant) -> binary-constant | octal-constant | hex-constant
// - R408 (binary-constant) -> B quote digit [digit]... quote
// - R409 (octal-constant) -> O quote digit [digit]... quote
// - R410 (hex-constant) -> Z quote hex-digit [hex-digit]... quote

// Binary constant - ISO/IEC 1539:1991 Section 4.3.3, R408
BINARY_CONSTANT : ('b'|'B') '\'' [01]+ '\'' ;
// Octal constant - ISO/IEC 1539:1991 Section 4.3.3, R409
OCTAL_CONSTANT  : ('o'|'O') '\'' [0-7]+ '\'' ;
// Hex constant - ISO/IEC 1539:1991 Section 4.3.3, R410
HEX_CONSTANT    : ('z'|'Z'|'x'|'X') '\'' [0-9a-fA-F]+ '\'' ;

// Exponent fragment - ISO/IEC 1539:1991 Section 4.3.1.2
fragment EXPONENT : [eEdD] [+-]? DIGIT+ ;

// ====================================================================
// FORTRAN 90 INTRINSIC PROCEDURES - ISO/IEC 1539:1991 Section 13
// ====================================================================
//
// ISO/IEC 1539:1991 Section 13 defines intrinsic procedures.
// F90 introduces many new array and transformational intrinsics.
// Section numbers reference the individual intrinsic descriptions.

// ====================================================================
// ARRAY REDUCTION INTRINSICS - ISO/IEC 1539:1991 Section 13.8
// ====================================================================

// ALL intrinsic - ISO/IEC 1539:1991 Section 13.8.3
ALL_INTRINSIC         : ('a'|'A') ('l'|'L') ('l'|'L') ;
// ANY intrinsic - ISO/IEC 1539:1991 Section 13.8.5
ANY_INTRINSIC         : ('a'|'A') ('n'|'N') ('y'|'Y') ;
// COUNT intrinsic - ISO/IEC 1539:1991 Section 13.8.17
COUNT_INTRINSIC       : ('c'|'C') ('o'|'O') ('u'|'U') ('n'|'N') ('t'|'T') ;
// DOT_PRODUCT intrinsic - ISO/IEC 1539:1991 Section 13.8.24
DOT_PRODUCT_INTRINSIC : ('d'|'D') ('o'|'O') ('t'|'T') '_' ('p'|'P') ('r'|'R')
                        ('o'|'O') ('d'|'D') ('u'|'U') ('c'|'C') ('t'|'T') ;
// MATMUL intrinsic - ISO/IEC 1539:1991 Section 13.8.50
MATMUL_INTRINSIC      : ('m'|'M') ('a'|'A') ('t'|'T') ('m'|'M') ('u'|'U') ('l'|'L') ;
// MAXVAL intrinsic - ISO/IEC 1539:1991 Section 13.8.52
MAXVAL_INTRINSIC      : ('m'|'M') ('a'|'A') ('x'|'X') ('v'|'V') ('a'|'A') ('l'|'L') ;
// MINVAL intrinsic - ISO/IEC 1539:1991 Section 13.8.55
MINVAL_INTRINSIC      : ('m'|'M') ('i'|'I') ('n'|'N') ('v'|'V') ('a'|'A') ('l'|'L') ;
// PRODUCT intrinsic - ISO/IEC 1539:1991 Section 13.8.65
PRODUCT_INTRINSIC     : ('p'|'P') ('r'|'R') ('o'|'O') ('d'|'D')
                        ('u'|'U') ('c'|'C') ('t'|'T') ;
// SUM intrinsic - ISO/IEC 1539:1991 Section 13.8.88
SUM_INTRINSIC         : ('s'|'S') ('u'|'U') ('m'|'M') ;
// TRANSPOSE intrinsic - ISO/IEC 1539:1991 Section 13.8.89
TRANSPOSE_INTRINSIC   : ('t'|'T') ('r'|'R') ('a'|'A') ('n'|'N') ('s'|'S')
                        ('p'|'P') ('o'|'O') ('s'|'S') ('e'|'E') ;

// ====================================================================
// ARRAY INQUIRY INTRINSICS - ISO/IEC 1539:1991 Section 13.8
// ====================================================================

// SIZE intrinsic - ISO/IEC 1539:1991 Section 13.8.83
SIZE_INTRINSIC        : ('s'|'S') ('i'|'I') ('z'|'Z') ('e'|'E') ;
// SHAPE intrinsic - ISO/IEC 1539:1991 Section 13.8.81
SHAPE_INTRINSIC       : ('s'|'S') ('h'|'H') ('a'|'A') ('p'|'P') ('e'|'E') ;
// UBOUND intrinsic - ISO/IEC 1539:1991 Section 13.8.93
UBOUND_INTRINSIC      : ('u'|'U') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
// LBOUND intrinsic - ISO/IEC 1539:1991 Section 13.8.43
LBOUND_INTRINSIC      : ('l'|'L') ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D') ;
// ALLOCATED intrinsic - ISO/IEC 1539:1991 Section 13.8.4
ALLOCATED_INTRINSIC   : ('a'|'A') ('l'|'L') ('l'|'L') ('o'|'O') ('c'|'C')
                        ('a'|'A') ('t'|'T') ('e'|'E') ('d'|'D') ;

// ====================================================================
// ARRAY MANIPULATION INTRINSICS - ISO/IEC 1539:1991 Section 13.8
// ====================================================================

// PACK intrinsic - ISO/IEC 1539:1991 Section 13.8.60
PACK_INTRINSIC        : ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
// UNPACK intrinsic - ISO/IEC 1539:1991 Section 13.8.94
UNPACK_INTRINSIC      : ('u'|'U') ('n'|'N') ('p'|'P') ('a'|'A') ('c'|'C') ('k'|'K') ;
// RESHAPE intrinsic - ISO/IEC 1539:1991 Section 13.8.69
RESHAPE_INTRINSIC     : ('r'|'R') ('e'|'E') ('s'|'S') ('h'|'H')
                        ('a'|'A') ('p'|'P') ('e'|'E') ;
// SPREAD intrinsic - ISO/IEC 1539:1991 Section 13.8.85
SPREAD_INTRINSIC      : ('s'|'S') ('p'|'P') ('r'|'R') ('e'|'E') ('a'|'A') ('d'|'D') ;
// MERGE intrinsic - ISO/IEC 1539:1991 Section 13.8.53
MERGE_INTRINSIC       : ('m'|'M') ('e'|'E') ('r'|'R') ('g'|'G') ('e'|'E') ;

// ====================================================================
// CHARACTER INTRINSICS - ISO/IEC 1539:1991 Section 13.8
// ====================================================================

// TRIM intrinsic - ISO/IEC 1539:1991 Section 13.8.91
TRIM_INTRINSIC        : ('t'|'T') ('r'|'R') ('i'|'I') ('m'|'M') ;
// ADJUSTL intrinsic - ISO/IEC 1539:1991 Section 13.8.1
ADJUSTL_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U')
                        ('s'|'S') ('t'|'T') ('l'|'L') ;
// ADJUSTR intrinsic - ISO/IEC 1539:1991 Section 13.8.2
ADJUSTR_INTRINSIC     : ('a'|'A') ('d'|'D') ('j'|'J') ('u'|'U')
                        ('s'|'S') ('t'|'T') ('r'|'R') ;
// REPEAT intrinsic - ISO/IEC 1539:1991 Section 13.8.68
REPEAT_INTRINSIC      : ('r'|'R') ('e'|'E') ('p'|'P') ('e'|'E') ('a'|'A') ('t'|'T') ;

// ====================================================================
// STATEMENT SEPARATORS - ISO/IEC 1539:1991 Section 3.4.2
// ====================================================================

// Semicolon for multiple statements per line (free-form)
// ISO/IEC 1539:1991 Section 3.4.2: A semicolon separates statements
SEMICOLON : ';' ;

// ====================================================================
// IMPLICIT STATEMENT - ISO/IEC 1539:1991 Section 5.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 5.3 defines the IMPLICIT statement:
// - R540 (implicit-stmt) -> IMPLICIT implicit-spec-list |
//                           IMPLICIT NONE
// - R541 (implicit-spec) -> type-spec (letter-spec-list)

// IMPLICIT keyword - ISO/IEC 1539:1991 Section 5.3, R540
IMPLICIT        : ('i'|'I') ('m'|'M') ('p'|'P') ('l'|'L')
                  ('i'|'I') ('c'|'C') ('i'|'I') ('t'|'T') ;
// NONE keyword (IMPLICIT NONE) - ISO/IEC 1539:1991 Section 5.3, R540
NONE            : ('n'|'N') ('o'|'O') ('n'|'N') ('e'|'E') ;

// ====================================================================
// WHITESPACE HANDLING - ISO/IEC 1539:1991 Section 3.2.4
// ====================================================================
//
// ISO/IEC 1539:1991 Section 3.2.4 defines blank interpretation:
// - In free form, blanks are significant (separate lexical tokens)
// - In fixed form, blanks in columns 7-72 are generally insignificant

// Whitespace handling - MUST skip spaces and tabs
WHITESPACE : [ \t]+ -> skip ;

// Override inherited NEWLINE to work properly with continuations
NEWLINE    : [\r\n]+ ;

// Whitespace fragment for compound tokens
fragment WS : [ \t]+ ;

// Digit fragment for numeric literals - ISO/IEC 1539:1991 Section 3.1.1
fragment DIGIT : [0-9] ;

// ====================================================================
// FORTRAN 90 LEXER - ISO/IEC 1539:1991 SPEC-GRAMMAR MAPPING
// ====================================================================
//
// This lexer implements token rules for Fortran 90 as defined in
// ISO/IEC 1539:1991 (WG5 N692). It is not a formally validated,
// complete implementation of every detail of the standard, but
// provides a practical basis for parsing F90+ source files.
//
// ISO/IEC 1539:1991 SPEC-GRAMMAR MAPPING (LEXER):
// Section 3 (Source form)     -> FREE_FORM_COMMENT, FIXED_FORM_COMMENT,
//                                CONTINUATION, NEWLINE, SEMICOLON
// Section 4 (Data types)      -> INTEGER_LITERAL*, REAL_LITERAL*, *_CONSTANT,
//                                *_QUOTE_STRING, KIND, LEN
// Section 5 (Declarations)    -> PUBLIC, PRIVATE, ALLOCATABLE, POINTER,
//                                TARGET, INTENT, OPTIONAL, IMPLICIT, NONE
// Section 6 (Use of data)     -> ALLOCATE, DEALLOCATE, NULLIFY, PERCENT
// Section 7 (Expressions)     -> EQ_OP, NE_OP, LT_OP, LE_OP, GT_OP, GE_OP,
//                                POINTER_ASSIGN, DOUBLE_COLON
// Section 8 (Control)         -> SELECT, CASE, DEFAULT, WHERE, ELSEWHERE,
//                                CYCLE, EXIT, WHILE, END_*
// Section 9 (I/O)             -> NAMELIST, ADVANCE, SIZE, STAT, EOR, IOSTAT,
//                                UNIT, FMT, REC, ERR
// Section 11 (Program units)  -> MODULE, END_MODULE, USE, ONLY, CONTAINS
// Section 12 (Procedures)     -> INTERFACE, END_INTERFACE, GENERIC, OPERATOR,
//                                ASSIGNMENT, RECURSIVE, RESULT, PROCEDURE
// Section 13 (Intrinsics)     -> *_INTRINSIC tokens, SELECTED_*_KIND,
//                                ASSOCIATED, ALLOCATED, PRESENT
//
// ====================================================================
