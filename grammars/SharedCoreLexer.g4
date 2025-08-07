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
// FORTRAN I (1957) ORIGINAL FEATURES - IBM 704
// ============================================================================
// Based on historical research: FORTRAN I was monolithic - NO user-written
// subroutines, functions, or modular programming. Only built-in functions.

// Control Flow (FORTRAN I, 1957) 
IF           : I F ;        // Arithmetic IF (three-way branch)
GOTO         : G O T O ;    // Unconditional branch  
DO           : D O ;        // DO loops with statement labels
END          : E N D ;      // End of program
CONTINUE     : C O N T I N U E ; // DO loop continuation
STOP         : S T O P ;    // Program termination

// I/O Operations (FORTRAN I, 1957)
READ         : R E A D ;    // Input from cards/tape
WRITE        : W R I T E ;  // Output (same as PRINT)
PRINT        : P R I N T ;  // Line printer output
PUNCH        : P U N C H ;  // Card punch output

// Array and Memory (FORTRAN I, 1957)
DIMENSION    : D I M E N S I O N ;    // Array declarations
EQUIVALENCE  : E Q U I V A L E N C E ; // Memory sharing
FORMAT       : F O R M A T ;         // I/O formatting

// Program Control (FORTRAN I, 1957)  
PAUSE        : P A U S E ;           // Operator intervention
FREQUENCY    : F R E Q U E N C Y ;   // Optimization hint (1957 only)

// Data Types (FORTRAN I, 1957)
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