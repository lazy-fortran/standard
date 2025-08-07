/*
 * SimpleFortran2003Lexer.g4
 * 
 * Working Fortran 2003 lexer demonstrating all critical fixes
 */

lexer grammar SimpleFortran2003Lexer;

// ============================================================================
// FORTRAN 2003 KEYWORDS
// ============================================================================

// Object-Oriented Programming
ABSTRACT         : A B S T R A C T ;
EXTENDS          : E X T E N D S ;
FINAL            : F I N A L ;
CLASS            : C L A S S ;
PROCEDURE        : P R O C E D U R E ;

// Attributes
VOLATILE         : V O L A T I L E ;
PROTECTED        : P R O T E C T E D ;

// Constructs
ASSOCIATE        : A S S O C I A T E ;
BLOCK            : B L O C K ;

// Basic keywords
INTEGER          : I N T E G E R ;
REAL             : R E A L ;
CHARACTER        : C H A R A C T E R ;
PROGRAM          : P R O G R A M ;
END              : E N D ;
MODULE           : M O D U L E ;

// ============================================================================
// SYMBOLS AND OPERATORS  
// ============================================================================

DOUBLE_COLON     : '::' ;
ARROW            : '=>' ;
ASSIGN           : '=' ;
LPAREN           : '(' ;
RPAREN           : ')' ;
COMMA            : ',' ;

// ============================================================================  
// IDENTIFIERS AND LITERALS
// ============================================================================

IDENTIFIER       : [a-zA-Z][a-zA-Z0-9_]* ;
INTEGER_LITERAL  : [0-9]+ ;

// ============================================================================
// WHITESPACE AND COMMENTS
// ============================================================================

WS               : [ \t\r\n]+ -> skip ;
COMMENT          : '!' ~[\r\n]* -> skip ;

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