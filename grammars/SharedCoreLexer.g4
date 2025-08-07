/*
 * SharedCoreLexer.g4
 * 
 * Lexer grammar for shared FORTRAN language constructs
 * present across ALL standards from FORTRAN 1957 to LazyFortran 2023+
 * 
 * This foundational lexer eliminates duplication and provides a single
 * source of truth for universal language elements.
 */

lexer grammar SharedCoreLexer;

// ============================================================================
// KEYWORDS: Present in ALL FORTRAN/Fortran versions (1957-2023)
// ============================================================================
// These tokens have remained consistent across the entire evolution
// and form the core of every FORTRAN variant ever created

// Control Flow (Actually present in FORTRAN 1957)
IF           : I F ;
GOTO         : G O T O ; 
DO           : D O ;
END          : E N D ;
CONTINUE     : C O N T I N U E ;
STOP         : S T O P ;

// I/O Operations (Present in FORTRAN 1957)
READ         : R E A D ;
WRITE        : W R I T E ;

// NOTE: CALL was added in FORTRAN II (1958), not 1957
// It should be added in FORTRAN grammar, not SharedCore

// ============================================================================
// DATA TYPES: Present in FORTRAN 1957
// ============================================================================
INTEGER      : I N T E G E R ;
REAL         : R E A L ;

// NOTE: FORTRAN 1957 was very basic. Most "common" keywords were added later:
// - PROGRAM: Added in FORTRAN 77 (1977) 
// - SUBROUTINE: Added in FORTRAN II (1958)
// - FUNCTION: Added in FORTRAN II (1958)
// - CHARACTER: Added in FORTRAN 77 (1977)
// - LOGICAL: Added in FORTRAN IV (1962)
// - PARAMETER: Added in FORTRAN 77 (1977)
// - THEN/ELSE: Added in FORTRAN 77 (1977)
//
// SharedCore should only contain features present across ALL standards.
// Later features will be added in appropriate standard-specific grammars.

// ============================================================================
// OPERATORS: Arithmetic (Universal since 1957)
// ============================================================================
ASSIGN       : '=' ;
PLUS         : '+' ;
MINUS        : '-' ;
MULTIPLY     : '*' ;
DIVIDE       : '/' ;
POWER        : '**' ;

// ============================================================================
// OPERATORS: Relational (1957 dotted style)
// ============================================================================
// Note: F90+ also allows ==, /=, <, <=, >, >= but these dotted forms
// are the universal subset that works across ALL standards
EQ           : '.' E Q '.' ;
NE           : '.' N E '.' ;
LT           : '.' L T '.' ;
LE           : '.' L E '.' ;
GT           : '.' G T '.' ;
GE           : '.' G E '.' ;

// ============================================================================
// DELIMITERS: Universal punctuation
// ============================================================================
LPAREN       : '(' ;
RPAREN       : ')' ;
COMMA        : ',' ;
COLON        : ':' ;

// ============================================================================
// LITERALS: Numbers and identifiers (Universal patterns)
// ============================================================================
INTEGER_LITERAL : DIGIT+ ;
REAL_LITERAL    : DIGIT+ '.' DIGIT* (EXPONENT)?  // 123.456, 123.456E10
                | DIGIT+ EXPONENT               // 123E10
                ;

IDENTIFIER      : LETTER (LETTER | DIGIT | '_')* ;

// ============================================================================
// WHITESPACE AND COMMENTS: Basic handling
// ============================================================================
// Skip whitespace (tabs, spaces, newlines)
WS : [ \t\r\n]+ -> skip ;

// Comments: Only handle ! style to avoid conflicts with C identifiers
// Column 1 'C' comments will be handled in format-specific lexers
COMMENT : '!' ~[\r\n]* -> skip ;

// ============================================================================
// CHARACTER FRAGMENTS: Basic building blocks
// ============================================================================
fragment LETTER : [A-Za-z] ;  
fragment DIGIT  : [0-9] ;
fragment EXPONENT : [eE] [+-]? DIGIT+ ;

// ============================================================================
// CASE-INSENSITIVE FRAGMENTS: For keyword definitions
// ============================================================================
// These fragments enable case-insensitive keyword matching across
// all FORTRAN standards (historically uppercase-only, modern case-insensitive)
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