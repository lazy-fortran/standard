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

// Object-Oriented Programming (NEW in F2003)
ABSTRACT         : A B S T R A C T ;
EXTENDS          : E X T E N D S ;
FINAL            : F I N A L ;
CLASS            : C L A S S ;
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

// ============================================================================
// CASE-INSENSITIVE FRAGMENTS
// ============================================================================

fragment A : [aA] ;
fragment B : [bB] ;
fragment C : [cC] ;
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