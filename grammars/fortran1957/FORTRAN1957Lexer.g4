// FORTRAN 1957 Lexer - Original IBM 704 Language
// The first high-level programming language for scientific computation
lexer grammar FORTRAN1957Lexer;

// ====================================================================
// FORTRAN 1957 HISTORICAL DOCUMENTATION
// ====================================================================
// This grammar represents FORTRAN as it was in 1957 for the IBM 704
// Based on the Preliminary Operator's Manual from April 1957
// 
// Key characteristics:
// - Fixed-form source (columns 1-72 of punch cards)
// - Column 1-5: Statement labels
// - Column 6: Continuation character
// - Column 7-72: Statement text
// - Only uppercase letters
// - Limited keywords (very small language)
// - Arithmetic expressions with +, -, *, /, ** (yes, ** was in 1957!)
// ====================================================================

// Keywords (1957 - very limited set)
IF       : I F ;
GOTO     : G O T O ;
DO       : D O ;
END      : E N D ;
CONTINUE : C O N T I N U E ;
STOP     : S T O P ;
PAUSE    : P A U S E ;        // PAUSE was in 1957
RETURN   : R E T U R N ;

// I/O statements (1957)
READ     : R E A D ;
PRINT    : P R I N T ;
PUNCH    : P U N C H ;        // Output to punch cards
FORMAT   : F O R M A T ;

// Declaration keywords (1957)
DIMENSION : D I M E N S I O N ; // Array dimensions
EQUIVALENCE : E Q U I V A L E N C E ; // Memory sharing
FREQUENCY : F R E Q U E N C Y ; // Optimization hint (1957!)
COMMON   : C O M M O N ;       // Shared storage

// Type keywords (implicit typing in 1957)
// Variables starting with I-N were INTEGER by default
// Variables starting with A-H, O-Z were REAL by default
// No explicit type declarations in 1957!

// Subprogram support (1957 - very basic)
// Note: FUNCTION was not a keyword in 1957 - functions were implicit

// Arithmetic operators
ASSIGN   : '=' ;
PLUS     : '+' ;
MINUS    : '-' ;
MULTIPLY : '*' ;
DIVIDE   : '/' ;
POWER    : '**' ;  // Yes, ** was in FORTRAN 1957!

// Relational operators (1957 style - no dots!)
// In 1957, relational operators were written without dots in arithmetic IF

// Delimiters
LPAREN   : '(' ;
RPAREN   : ')' ;
COMMA    : ',' ;

// Literals
fragment DIGIT : [0-9] ;
fragment LETTER : [A-Z] ;  // Only uppercase in 1957

INTEGER_LITERAL : DIGIT+ ;

// Fixed-point constants with optional scale factor
REAL_LITERAL 
    : DIGIT+ '.' DIGIT*
    | DIGIT+ ('E' [+-]? DIGIT+)  // E notation was in 1957
    | DIGIT+ '.' DIGIT* ('E' [+-]? DIGIT+)
    ;

// Identifiers (1957 rules)
// - Up to 6 characters
// - Start with letter
// - Letters and digits only (no underscore in 1957!)
IDENTIFIER : LETTER (LETTER | DIGIT)* {getText().length() <= 6}? ;

// Statement labels (1957)
LABEL : DIGIT DIGIT? DIGIT? DIGIT? DIGIT? ; // 1-5 digits

// Hollerith constants (string literals in 1957)
// Format: nHtext where n is the number of characters
HOLLERITH : DIGIT+ H .+? {
    // Complex validation would go here
    // The number must match the character count
};

fragment A : [Aa] ;
fragment B : [Bb] ;
fragment C : [Cc] ;
fragment D : [Dd] ;
fragment E : [Ee] ;
fragment F : [Ff] ;
fragment G : [Gg] ;
fragment H : [Hh] ;
fragment I : [Ii] ;
fragment J : [Jj] ;
fragment K : [Kk] ;
fragment L : [Ll] ;
fragment M : [Mm] ;
fragment N : [Nn] ;
fragment O : [Oo] ;
fragment P : [Pp] ;
fragment Q : [Qq] ;
fragment R : [Rr] ;
fragment S : [Ss] ;
fragment T : [Tt] ;
fragment U : [Uu] ;
fragment V : [Vv] ;
fragment W : [Ww] ;
fragment X : [Xx] ;
fragment Y : [Yy] ;
fragment Z : [Zz] ;

// Whitespace (spaces were significant in fixed-form!)
WS : [ \t]+ -> skip ;
NEWLINE : '\r'? '\n' ;

// Comments in 1957 were indicated by 'C' in column 1
// This needs special handling in fixed-form parsing