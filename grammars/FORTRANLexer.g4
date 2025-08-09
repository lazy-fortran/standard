/*
 * FORTRANLexer.g4
 * 
 * FORTRAN I (1957) - The Original IBM 704 FORTRAN
 * The world's first high-level programming language
 * 
 * This lexer defines the complete FORTRAN I language as released in 1957,
 * serving as the foundation for all subsequent FORTRAN/Fortran standards.
 */

lexer grammar FORTRANLexer;

// ============================================================================  
// FORTRAN I (1957) ORIGINAL FEATURES - IBM 704
// ============================================================================
// Based on historical research: FORTRAN I had user-defined subroutines and
// functions, but required monolithic compilation (no separate compilation).
// FORTRAN II (1958) added independent compilation of subroutines.

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
COMMON       : C O M M O N ;         // Shared variable storage

// Statement Functions (FORTRAN I, 1957)
// Note: FORTRAN I only had statement functions (names ending in F)
// Full subroutines with CALL/SUBROUTINE/FUNCTION/RETURN came in FORTRAN II (1958)

// Program Control (FORTRAN I, 1957)  
PAUSE        : P A U S E ;           // Operator intervention
FREQUENCY    : F R E Q U E N C Y ;   // Optimization hint (1957 only)
ASSIGN       : A S S I G N ;         // Assign label to variable (assigned GOTO)

// Data Types (FORTRAN I, 1957)
INTEGER      : I N T E G E R ;
REAL         : R E A L ;
IMPLICIT     : I M P L I C I T ;    // Implicit typing rules (I-N integer, else real)

// NOTE: FORTRAN I (1957) included user-defined subroutines/functions:
// - SUBROUTINE: Present in FORTRAN I (1957) - monolithic compilation
// - FUNCTION: Present in FORTRAN I (1957) - monolithic compilation  
// - CALL: Present in FORTRAN I (1957) - subroutine calls
// - RETURN: Present in FORTRAN I (1957) - return from subprograms
//
// Keywords added later:
// - PROGRAM: Added in FORTRAN 77 (1977) 
// - CHARACTER: Added in FORTRAN 77 (1977)
// - LOGICAL: Added in FORTRAN IV (1962)
// - PARAMETER: Added in FORTRAN 77 (1977)
// - THEN/ELSE: Added in FORTRAN 77 (1977)
//
// This grammar defines FORTRAN I (1957) as the foundation for all 
// subsequent FORTRAN/Fortran standards through inheritance.

// ============================================================================
// OPERATORS: Arithmetic (Universal since 1957)
// ============================================================================
EQUALS       : '=' ;  // Assignment operator
PLUS         : '+' ;
MINUS        : '-' ;
MULTIPLY     : '*' ;
SLASH        : '/' ;    // Division AND DATA delimiters (context-dependent)
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
// Skip whitespace (tabs, spaces only - newlines handled separately)
WS : [ \t]+ -> skip ;

// Newlines are significant in Fortran (statement terminators)
NEWLINE : [\r\n]+ ;

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