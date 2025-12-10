/*
 * FORTRANLexer.g4
 *
 * FORTRAN I (1957) - The Original IBM 704 FORTRAN
 * One of the world's first high-level programming languages.
 *
 * This lexer defines a shared core and a substantial subset of the original
 * 1957 FORTRAN language, and serves as the lexical foundation for the later
 * standards implemented in this repository.
 *
 * Reference: IBM Reference Manual, FORTRAN Automatic Coding System for the
 *            IBM 704 Data Processing System (Form C28-6003, October 1958)
 *            - Chapter I: Introduction
 *            - Chapter II: The FORTRAN Language
 *            - Chapter III: Input-Output
 *            - Appendix B: Table of FORTRAN Statements (32 statement types)
 *
 * Manual available at: Computer History Museum archive
 *   https://archive.computerhistory.org/resources/text/Fortran/102649787.05.01.acc.pdf
 */

lexer grammar FORTRANLexer;

// ============================================================================
// FORTRAN I (1957) ORIGINAL FEATURES - IBM 704
// ============================================================================
// Reference: C28-6003 Chapter II (The FORTRAN Language) and Appendix B
//
// Historically, the original FORTRAN system for the IBM 704 provided arithmetic
// expressions, assignment, DO loops, arithmetic IF, GOTO, formatted I/O and
// related facilities. User-written subroutines and functions (SUBROUTINE,
// FUNCTION, CALL, RETURN) and the COMMON statement were introduced later in
// FORTRAN II (1958); they are not modelled as keywords in this lexer.

// Control Flow Keywords
// C28-6003 Appendix B: GO TO, IF, DO, CONTINUE, STOP, END statements
IF           : I F ;        // Arithmetic IF (three-way branch) - Appendix B row 3
GOTO         : G O T O ;    // Unconditional/computed GO TO - Appendix B rows 1-2
DO           : D O ;        // DO loops with statement labels - Appendix B row 18
END          : E N D ;      // End of program - Appendix B row 32
CONTINUE     : C O N T I N U E ; // DO loop continuation - Appendix B row 19
STOP         : S T O P ;    // Program termination - Appendix B row 30

// I/O Keywords
// C28-6003 Chapter III (Input-Output) and Appendix B rows 20-29
READ         : R E A D ;    // Input - Appendix B rows 20-24
WRITE        : W R I T E ;  // Output - Appendix B rows 25-27
PRINT        : P R I N T ;  // Line printer output - Appendix B row 28
PUNCH        : P U N C H ;  // Card punch output - Appendix B row 29

// File-control Keywords (IBM 704 specific)
// C28-6003 Chapter III.F (File-control) - not in Appendix B but documented
REWIND       : R E W I N D ;       // REWIND i - rewind tape unit i
BACKSPACE    : B A C K S P A C E ; // BACKSPACE i - backspace tape unit i

// Note: TAPE, DRUM, INPUT, OUTPUT, FILE are NOT defined as separate keywords
// to avoid conflicts with later Fortran standards where these are used as
// identifiers. The parser uses contextual matching with IDENTIFIER for these.
// See FORTRANParser.g4 for tape/drum I/O statement rules.

// Storage/Declaration Keywords
// C28-6003 Chapter II.B (Variables and Subscripts) and Appendix B
DIMENSION    : D I M E N S I O N ;    // Array declarations - Appendix B row 14
EQUIVALENCE  : E Q U I V A L E N C E ; // Memory overlay - Appendix B row 15
FORMAT       : F O R M A T ;          // I/O formatting - Appendix B row 16

// Statement Functions
// C28-6003 Chapter II.E (Function Statements) and Appendix B row 17
// Note: FORTRAN I only had statement functions (names ending in F)
// Full subroutines with CALL/SUBROUTINE/FUNCTION/RETURN came in FORTRAN II (1958)

// Program Control Keywords
// C28-6003 Chapter II.G and Appendix B
PAUSE        : P A U S E ;           // Operator intervention - Appendix B row 31
FREQUENCY    : F R E Q U E N C Y ;   // Optimization hint - Appendix B row 13
ASSIGN       : A S S I G N ;         // ASSIGN i TO n - Appendix B row 12
TO           : T O ;                 // Used in ASSIGN i TO n - Appendix B row 12

// Hardware-specific IF statements (IBM 704, 1957)
// C28-6003 Appendix B rows 4-11: IF statements for hardware indicators
// These test IBM 704 console switches, sense lights, and overflow indicators
SENSE        : S E N S E ;           // SENSE LIGHT - Appendix B row 11
LIGHT        : L I G H T ;           // SENSE LIGHT i - Appendix B row 11
SWITCH       : S W I T C H ;         // IF (SENSE SWITCH i) - Appendix B row 4
ACCUMULATOR  : A C C U M U L A T O R ; // IF ACCUMULATOR OVERFLOW - Appendix B row 5
QUOTIENT     : Q U O T I E N T ;     // IF QUOTIENT OVERFLOW - Appendix B row 6
DIVIDE       : D I V I D E ;         // IF DIVIDE CHECK - Appendix B row 7
CHECK        : C H E C K ;           // IF DIVIDE CHECK - Appendix B row 7
OVERFLOW     : O V E R F L O W ;     // Used in overflow IF statements

// Data Type Keywords
// C28-6003 Chapter II.A (Constants) and Chapter II.B (Variables)
// Note: In 1957 FORTRAN, types were determined by naming convention (I-N = integer)
// Type declaration statements (INTEGER, REAL, IMPLICIT) were introduced in FORTRAN 66,
// not FORTRAN 1957. See FORTRAN 66 grammar (FORTRAN66Lexer.g4) for these.

// ============================================================================
// OPERATORS: Arithmetic
// C28-6003 Chapter II.C (Expressions) - arithmetic operations
// ============================================================================
EQUALS       : '=' ;    // Assignment - C28-6003 Chapter II.C
PLUS         : '+' ;    // Addition - C28-6003 Chapter II.C
MINUS        : '-' ;    // Subtraction/negation - C28-6003 Chapter II.C
MULTIPLY     : '*' ;    // Multiplication - C28-6003 Chapter II.C
SLASH        : '/' ;    // Division - C28-6003 Chapter II.C
POWER        : '**' ;   // Exponentiation - C28-6003 Chapter II.C

// ============================================================================
// OPERATORS: Relational
// C28-6003 Chapter II.C (Expressions) - relational operators introduced
// in original FORTRAN for use in IF statement expressions
// ============================================================================
// Note: F90+ also allows ==, /=, <, <=, >, >= but these dotted forms
// are the universal subset that works across ALL standards
EQ           : '.' E Q '.' ;    // Equal - C28-6003 Chapter II.C
NE           : '.' N E '.' ;    // Not equal - C28-6003 Chapter II.C
LT           : '.' L T '.' ;    // Less than - C28-6003 Chapter II.C
LE           : '.' L E '.' ;    // Less or equal - C28-6003 Chapter II.C
GT           : '.' G T '.' ;    // Greater than - C28-6003 Chapter II.C
GE           : '.' G E '.' ;    // Greater or equal - C28-6003 Chapter II.C

// ============================================================================
// DELIMITERS: Punctuation
// C28-6003 Chapter II - used throughout for subscripts, lists, ranges
// ============================================================================
LPAREN       : '(' ;    // Subscripts, function args, lists - C28-6003 Chapter II.B
RPAREN       : ')' ;    // Subscripts, function args, lists - C28-6003 Chapter II.B
COMMA        : ',' ;    // List separator - C28-6003 throughout

// ============================================================================
// HOLLERITH CONSTANTS
// C28-6003 Chapter III (Input-Output) and Appendix B row 16: FORMAT
// ============================================================================
// Format: nHcharacters where n = number of characters following H
// Hollerith constants (nHtext) were the ONLY string-literal mechanism in 1957.
// Used in FORMAT statements for literal text output.
// Example: 5HHELLO, 10HRESULT IS:
// Note: This pattern greedily matches nH followed by any characters up to
// a comma, right paren, or end of line - the typical delimiters in FORMAT
// specifications. Strict length-count semantics would require a semantic check.
HOLLERITH : [1-9] [0-9]* H ~[,)\r\n]* ;

// ============================================================================
// LITERALS: Numbers and identifiers
// C28-6003 Chapter II.A (Constants) and Chapter II.B (Variables)
// ============================================================================
// Fixed-point (integer) constants: C28-6003 Chapter II.A.1
INTEGER_LITERAL : DIGIT+ ;
// Floating-point (real) constants: C28-6003 Chapter II.A.2
REAL_LITERAL    : DIGIT+ '.' DIGIT+ (EXPONENT)?  // 123.456, 123.456E10
                | '.' DIGIT+ (EXPONENT)?         // .456, .456E10
                | DIGIT+ EXPONENT                // 123E10
                ;
// Variable names: C28-6003 Chapter II.B (1-6 alphanumeric, start with letter)
// Note: 1957 FORTRAN limited names to 6 characters; this lexer is more permissive
IDENTIFIER      : LETTER (LETTER | DIGIT | '_')* ;

// ============================================================================
// WHITESPACE AND COMMENTS
// C28-6003 Chapter I.B (Coding for FORTRAN) - source format rules
// ============================================================================
// Skip whitespace (tabs, spaces only - newlines handled separately)
// Note: 1957 FORTRAN ignored blanks entirely within statements
WS : [ \t]+ -> skip ;

// Newlines are significant in Fortran (statement terminators)
// C28-6003: each card = one statement (unless continued)
NEWLINE : [\r\n]+ ;

// Comments: Only handle ! style to avoid conflicts with C identifiers
// Note: 1957 used column-1 C for comments; this lexer uses modern ! style
// Column 1 C/* comments are handled in format-specific lexers
COMMENT : '!' ~[\r\n]* -> skip ;

// ============================================================================
// CHARACTER FRAGMENTS
// C28-6003 Appendix A (Table of FORTRAN Characters) - character set
// ============================================================================
fragment LETTER : [A-Za-z] ;    // Alphabetic (1957: uppercase only on 704)
fragment DIGIT  : [0-9] ;       // Numeric digits 0-9
fragment EXPONENT : [eE] [+-]? DIGIT+ ;  // E exponent notation

// ============================================================================
// CASE-INSENSITIVE FRAGMENTS
// Note: 1957 FORTRAN was uppercase-only; modern systems are case-insensitive
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
