// FORTRAN 77 (1977) - Structured Programming Revolution  
// Added CHARACTER data type, IF-THEN-ELSE, and PROGRAM statement
lexer grammar FORTRAN77Lexer;

import FORTRAN66Lexer;  // Import FORTRAN 66 (1966) constructs

// ====================================================================
// FORTRAN 77 (1977) HISTORICAL OVERVIEW  
// ====================================================================
//
// FORTRAN 77 (officially X3.9-1978) was a revolutionary update that added:
// - CHARACTER data type with string processing
// - Block IF-THEN-ELSE-ENDIF structured programming
// - PARAMETER statement for compile-time constants
// - PROGRAM statement for main program units
// - SAVE statement for persistent local variables
// - LIST-directed I/O improvements
// - Comments could use * in column 1 (in addition to C)
//
// Historical Context:
// - Final drafts circulated in 1977
// - Formally approved in April 1978
// - Last version with uppercase-only character set
// - Dominated scientific computing for decades
//

// ====================================================================
// FORTRAN 77 (1977) MAJOR NEW FEATURES
// ====================================================================

// Program unit identification (added in FORTRAN 77, 1977)
PROGRAM         : P R O G R A M ;

// CHARACTER data type (added in FORTRAN 77, 1977)
CHARACTER       : C H A R A C T E R ;

// Structured programming keywords (added in FORTRAN 77, 1977)
THEN            : T H E N ;
ELSE            : E L S E ;
ELSEIF          : E L S E I F ;
ENDIF           : E N D I F ;

// Compile-time constants (added in FORTRAN 77, 1977)
PARAMETER       : P A R A M E T E R ;

// Variable persistence (added in FORTRAN 77, 1977)
SAVE            : S A V E ;

// Additional statements enhanced in FORTRAN 77
DATA            : D A T A ;
EXTERNAL        : E X T E R N A L ;
INTRINSIC       : I N T R I N S I C ;

// Enhanced loop control (FORTRAN 77 improvements)
// DO loops with modern END DO (though labels still supported)
ENDDO           : E N D D O ;

// I/O enhancements
OPEN            : O P E N ;
CLOSE           : C L O S E ;
INQUIRE         : I N Q U I R E ;
BACKSPACE       : B A C K S P A C E ;
REWIND          : R E W I N D ;

// ====================================================================
// FORTRAN 77 STRING PROCESSING
// ====================================================================

// String concatenation operator (FORTRAN 77 innovation)
CONCAT          : '//' ;

// String literals (FORTRAN 77 innovation)
STRING_LITERAL  : '\'' (~'\'' | '\'\'')* '\'' ;  // 'text' with '' for embedded quotes

// ====================================================================  
// FORTRAN 77 (1977) HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN 77 was transformational because:
// 1. Introduced structured programming with IF-THEN-ELSE
// 2. Added CHARACTER data type for text processing
// 3. Provided PARAMETER for named constants
// 4. Established modern program structure with PROGRAM statement
// 5. Enhanced I/O with file handling capabilities
// 6. Became the stable foundation for scientific computing
//
// This version remained dominant until Fortran 90 (1990) - a 13-year reign
// that established FORTRAN as the primary scientific programming language.
//
// Notable: Last version requiring uppercase; Fortran 90 added lowercase support
//
// ====================================================================