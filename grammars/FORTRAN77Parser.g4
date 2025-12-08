// FORTRAN 77 (1977) - Structured Programming Revolution
// Added CHARACTER data type and IF-THEN-ELSE constructs
parser grammar FORTRAN77Parser;

import FORTRAN66Parser;  // Import FORTRAN 66 (1966) constructs

options {
    tokenVocab = FORTRAN77Lexer;
}

// ====================================================================
// FORTRAN 77 (1977) NEW PARSER RULES
// ====================================================================
//
// FORTRAN 77 (1977) MAJOR INNOVATIONS:
// - CHARACTER data type for text/string handling
// - IF-THEN-ELSE-END IF structured programming constructs
// - List-directed I/O capabilities
// - Floating point variables as DO loop indices
// - Generic and specific intrinsic function names
// - Improved portability and standardization
//
// Historical Context (1977):
// - Published as ANSI X3.9-1978 standard
// - Addressed need for structured programming
// - Eliminated need for many FORTRAN preprocessors
// - Became foundation for modern FORTRAN programming
//
// ====================================================================

// ====================================================================
// FORTRAN 77 (1977) - PROGRAM STRUCTURE
// ====================================================================

// Override main_program from FORTRAN 66 to support PROGRAM statement
// FORTRAN 77 introduced the PROGRAM statement (per ANSI X3.9-1978)
main_program
    : program_stmt? statement* end_stmt NEWLINE?
    ;

// PROGRAM statement (NEW in FORTRAN 77)
program_stmt
    : PROGRAM IDENTIFIER NEWLINE
    ;

// ====================================================================
// FORTRAN 77 (1977) - ENHANCED TYPE SYSTEM
// ====================================================================

// FORTRAN 77 type specification with CHARACTER addition
// Unfortunately ANTLR4 doesn't support true rule extension,
// so we must redefine with all inherited types
type_spec
    : INTEGER
    | REAL
    | LOGICAL                    // From FORTRAN IV (1962)
    | DOUBLE PRECISION           // From FORTRAN IV (1962)
    | COMPLEX                    // From FORTRAN IV (1962)
    | CHARACTER character_length? // NEW in FORTRAN 77 (1977)
    ;

// Character length specification
character_length
    : MULTIPLY integer_expr      // CHARACTER*10
    | MULTIPLY LPAREN MULTIPLY RPAREN  // CHARACTER*(*)
    | MULTIPLY LPAREN integer_expr RPAREN  // CHARACTER*(LEN)
    ;

// ====================================================================
// FORTRAN 77 (1977) - STRUCTURED PROGRAMMING
// ====================================================================

// Override statement body to include structured constructs
statement_body
    : assignment_stmt      // Variable = Expression
    | goto_stmt           // Unconditional jump to labeled statement
    | computed_goto_stmt  // Multi-way branch based on integer expression
    | arithmetic_if_stmt  // Three-way branch based on expression sign
    | logical_if_stmt     // Single statement logical IF
    | block_if_construct  // NEW: IF-THEN-ELSE-END IF block
    | do_stmt            // Counted loop with mandatory label
    | continue_stmt      // Loop termination and jump target
    | stop_stmt          // Program termination
    | pause_stmt         // Operator intervention
    | read_stmt          // Input from cards/tape
    | write_stmt         // NEW: Enhanced WRITE statement
    | print_stmt         // Output to line printer
    | punch_stmt         // Output to card punch
    | format_stmt        // I/O formatting specification
    | dimension_stmt     // Array dimension declarations
    | equivalence_stmt   // Variable memory overlay
    | common_stmt        // Global variable declarations
    | data_stmt         // Variable initialization
    | type_declaration  // Variable type declarations
    | end_stmt          // End of program
    | return_stmt       // Return from subprogram
    | call_stmt         // Call subroutine
    | save_stmt         // NEW: SAVE statement
    | intrinsic_stmt    // NEW: INTRINSIC statement
    | external_stmt     // NEW: EXTERNAL statement
    | data_stmt         // NEW: DATA statement
    ;

// Block IF construct (NEW in FORTRAN 77)
block_if_construct
    : if_then_stmt
      execution_part_construct*
      else_if_part*
      else_part?
      end_if_stmt
    ;

if_then_stmt
    : IF LPAREN logical_expr RPAREN THEN
    ;

else_if_part
    : else_if_stmt
      execution_part_construct*
    ;

else_if_stmt
    : ELSE IF LPAREN logical_expr RPAREN THEN
    ;

else_part
    : else_stmt
      execution_part_construct*
    ;

else_stmt
    : ELSE
    ;

end_if_stmt
    : END IF
    | ENDIF
    ;

execution_part_construct
    : label? executable_construct NEWLINE?
    | NEWLINE
    ;

executable_construct
    : assignment_stmt
    | goto_stmt
    | computed_goto_stmt
    | arithmetic_if_stmt
    | logical_if_stmt
    | call_stmt
    | return_stmt
    | stop_stmt
    | pause_stmt
    | continue_stmt
    | read_stmt
    | write_stmt
    | print_stmt
    | punch_stmt
    | do_stmt
    | block_if_construct
    ;

// ====================================================================
// FORTRAN 77 (1977) - ENHANCED I/O
// ====================================================================

// Enhanced READ statement with list-directed I/O (NEW in FORTRAN 77)
// FORTRAN 77 added READ *, list form for list-directed input
read_stmt
    : READ format_identifier (COMMA input_item_list_f77)?
    | READ LPAREN control_info_list RPAREN input_item_list_f77?
    ;

input_item_list_f77
    : input_item_f77 (COMMA input_item_f77)*
    ;

input_item_f77
    : variable
    | implied_do_input
    ;

implied_do_input
    : LPAREN input_item_list_f77 COMMA integer_variable EQUALS integer_expr COMMA
      integer_expr (COMMA integer_expr)? RPAREN
    ;

// Enhanced PRINT statement with list-directed I/O (NEW in FORTRAN 77)
// FORTRAN 77 added PRINT *, list form for list-directed output
print_stmt
    : PRINT format_identifier (COMMA output_item_list)?
    ;

// Enhanced WRITE statement (NEW in FORTRAN 77)
write_stmt
    : WRITE LPAREN control_info_list RPAREN output_item_list?
    ;

control_info_list
    : control_info_item (COMMA control_info_item)*
    ;

control_info_item
    : integer_expr          // Unit number
    | format_identifier     // Format
    ;

format_identifier
    : label              // Format statement label
    | character_expr     // Format string
    | MULTIPLY           // List-directed
    ;

output_item_list
    : output_item (COMMA output_item)*
    ;

output_item
    : expr
    | character_expr  // String literals in output (NEW in FORTRAN 77)
    | implied_do
    ;

// Implied DO (enhanced in FORTRAN 77)
implied_do
    : LPAREN dlist COMMA integer_variable EQUALS integer_expr COMMA integer_expr 
      (COMMA integer_expr)? RPAREN
    ;

dlist
    : output_item (COMMA output_item)*
    ;

// ====================================================================
// FORTRAN 77 (1977) - ENHANCED DO LOOPS
// ====================================================================

// Enhanced DO statement with floating point indices (NEW in FORTRAN 77)
// Uses EQUALS (=) like FORTRAN 66, not ASSIGN keyword
do_stmt
    : DO label do_variable EQUALS initial_expr COMMA final_expr (COMMA increment_expr)?
    ;

do_variable
    : integer_variable
    | real_variable      // NEW in FORTRAN 77: floating point DO variables
    ;

initial_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

final_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

increment_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

// ====================================================================
// FORTRAN 77 (1977) - NEW DECLARATION STATEMENTS
// ====================================================================

// SAVE statement (NEW in FORTRAN 77)
save_stmt
    : SAVE (save_list)?
    ;

save_list
    : save_item (COMMA save_item)*
    ;

save_item
    : variable
    | SLASH common_block_name SLASH
    ;

common_block_name
    : IDENTIFIER
    ;

// INTRINSIC statement (NEW in FORTRAN 77)
intrinsic_stmt
    : INTRINSIC intrinsic_procedure_list
    ;

intrinsic_procedure_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// EXTERNAL statement (NEW in FORTRAN 77)
external_stmt
    : EXTERNAL external_name_list
    ;

external_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// FORTRAN 77 (1977) - DATA STATEMENT
// ====================================================================

// DATA statement (NEW in FORTRAN 77)
data_stmt
    : DATA data_stmt_set (COMMA data_stmt_set)*
    ;

data_stmt_set
    : variable_list SLASH data_constant_list SLASH
    ;

data_constant_list
    : data_constant (COMMA data_constant)*
    ;

data_constant
    : literal
    | PLUS literal
    | MINUS literal
    ;

variable_list
    : variable (COMMA variable)*
    ;

// ====================================================================
// FORTRAN 77 (1977) - ENHANCED EXPRESSIONS
// ====================================================================

// Override literal to accept LABEL tokens as integers due to lexer
// token precedence: LABEL is defined before INTEGER_LITERAL in
// FORTRANIILexer and matches first for 1-5 digit numbers
literal
    : INTEGER_LITERAL
    | LABEL                     // Accept LABEL as integer (token precedence)
    | REAL_LITERAL
    | STRING_LITERAL            // NEW in FORTRAN 77
    | logical_literal           // From FORTRAN IV via FORTRAN 66
    ;

// Logical literals from FORTRAN IV (imported via FORTRAN 66)
logical_literal
    : DOT_TRUE
    | DOT_FALSE
    ;

// Character expressions (NEW in FORTRAN 77)
character_expr
    : character_operand (CONCAT character_operand)*  // Concatenation
    ;

character_operand
    : character_primary
    ;

character_primary
    : character_literal_constant
    | character_variable
    | character_function_reference
    | LPAREN character_expr RPAREN
    ;

character_literal_constant
    : STRING_LITERAL
    | HOLLERITH  // Hollerith constants still supported in F77
    ;

character_variable
    : IDENTIFIER substring_range?
    ;

substring_range
    : LPAREN substr_start_expr COLON substr_end_expr? RPAREN
    ;

substr_start_expr
    : integer_expr
    ;

substr_end_expr
    : integer_expr
    ;

character_function_reference
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    ;

actual_arg_spec_list
    : actual_arg_spec (COMMA actual_arg_spec)*
    ;

actual_arg_spec
    : expr
    ;

// Variable types for enhanced DO loops
integer_variable
    : IDENTIFIER
    ;

real_variable
    : IDENTIFIER
    ;

real_expr
    : expr  // Expression that evaluates to real (semantic constraint)
    ;

// ====================================================================
// FORTRAN 77 (1977) - HISTORICAL SIGNIFICANCE
// ====================================================================
//
// FORTRAN 77 (1977) was revolutionary because:
//
// 1. **Structured Programming**: IF-THEN-ELSE eliminated GOTO spaghetti
//    - Block structured conditionals
//    - Nested IF constructs possible
//    - Made programs more readable and maintainable
//
// 2. **Text Processing**: CHARACTER data type
//    - Native string handling capability
//    - String concatenation with //
//    - Substring operations
//
// 3. **Enhanced I/O**: List-directed and formatted I/O
//    - WRITE statement improvements
//    - Error handling with IOSTAT, ERR, END
//    - More flexible format specifications
//
// 4. **Program Organization**: Declaration statements
//    - SAVE for variable persistence
//    - INTRINSIC and EXTERNAL for procedure declarations
//    - Better scoping control
//
// 5. **Numerical Enhancement**: Floating point DO loops
//    - More flexible iteration control
//    - Better support for scientific computing
//
// This parser captures FORTRAN 77's structured programming revolution
// while maintaining full compatibility with FORTRAN 66 standardization
// and FORTRAN IV data type innovations.
//
// ====================================================================