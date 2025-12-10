// FORTRAN 77 Parser - ISO 1539:1980 / ANSI X3.9-1978
// Reference: ISO 1539:1980 (also published as ANSI X3.9-1978)
//
// This parser implements syntax rules for FORTRAN 77 as defined in the
// ISO/ANSI standard. Section references use the ISO 1539:1980 numbering.
//
// Standard Structure Overview:
//   Section 1: Scope
//   Section 2: Processor Characteristics
//   Section 3: Program Form and Source Format (3.1-3.5)
//   Section 4: Data Types and Constants (4.1-4.8)
//   Section 5: Arrays and Substrings (5.1-5.7)
//   Section 6: Expressions (6.1-6.7)
//   Section 7: Executable and Nonexecutable Statements (7.1-7.2)
//   Section 8: Specification Statements (8.1-8.9)
//   Section 9: DATA Statement (9.1-9.3)
//   Section 10: Assignment Statements (10.1-10.4)
//   Section 11: Control Statements (11.1-11.12)
//   Section 12: Input/Output Statements (12.1-12.10)
//   Section 13: Format Specification (13.1-13.5)
//   Section 14: Main Program (14.1)
//   Section 15: Functions and Subroutines (15.1-15.10)
//   Section 16: Block Data Subprogram (16.1-16.3)
//   Section 17: Association (17.1-17.4)
//   Section 18: Scope and Definition (18.1-18.3)
parser grammar FORTRAN77Parser;

import FORTRAN66Parser;  // Import FORTRAN 66 (1966) constructs

options {
    tokenVocab = FORTRAN77Lexer;
}

// ====================================================================
// FORTRAN 77 (ISO 1539:1980) PARSER OVERVIEW
// ====================================================================
//
// MAJOR INNOVATIONS (with ISO section references):
// - CHARACTER data type for text/string handling (Section 4.8)
// - IF-THEN-ELSE-END IF structured programming (Sections 11.6-11.9)
// - List-directed I/O capabilities (Section 12.4)
// - REAL/DOUBLE PRECISION DO loop indices (Section 11.10)
// - Generic and specific intrinsic function names (Section 15.10)
// - Enhanced file handling: OPEN, CLOSE, INQUIRE (Section 12.10)
//
// Historical Context:
// - ISO 1539:1980 (also ANSI X3.9-1978)
// - Addressed need for structured programming
// - Became foundation for modern Fortran programming
//
// ====================================================================

// ====================================================================
// PROGRAM STRUCTURE - ISO 1539:1980 Section 14
// ====================================================================
//
// ISO 1539:1980 Section 14.1 defines the main program structure:
// - A main program begins with an optional PROGRAM statement
// - Followed by specification statements, DATA statements, and
//   statement function definitions (in that order)
// - Followed by executable statements
// - Terminated by an END statement

// Main program - ISO 1539:1980 Section 14.1
// Override from FORTRAN 66 to add optional PROGRAM statement
main_program
    : program_stmt? statement* end_stmt NEWLINE?
    ;

// PROGRAM statement - ISO 1539:1980 Section 14.1
// Syntax: PROGRAM program-name
// The program-name is optional in FORTRAN 77 (this grammar requires it)
program_stmt
    : PROGRAM IDENTIFIER NEWLINE
    ;

// ====================================================================
// ENTRY STATEMENT - ISO 1539:1980 Section 15.7
// ====================================================================
//
// ISO 1539:1980 Section 15.7 defines the ENTRY statement:
// Syntax: ENTRY entry-name [ ( [ dummy-arg-list ] ) ]
//
// Constraints (Section 15.7):
// - ENTRY may only appear in a FUNCTION or SUBROUTINE subprogram
// - ENTRY must not appear within a DO loop, block IF, or ELSE block
// - The entry-name identifies an alternate entry point
// - Dummy arguments may differ from main subprogram arguments
// - Alternate return specifiers (*) allowed per Section 15.8.3

// ENTRY statement - ISO 1539:1980 Section 15.7
entry_stmt
    : ENTRY IDENTIFIER entry_dummy_arg_list?
    ;

// Dummy argument list for ENTRY - ISO 1539:1980 Section 15.7
entry_dummy_arg_list
    : LPAREN entry_dummy_arg? (COMMA entry_dummy_arg)* RPAREN
    ;

// Dummy argument - ISO 1539:1980 Section 15.4
// Includes alternate return specifier (*) per Section 15.8.3
entry_dummy_arg
    : IDENTIFIER
    | MULTIPLY                // Alternate return specifier (*)
    ;

// ====================================================================
// TYPE SYSTEM - ISO 1539:1980 Section 4 and Section 8.4
// ====================================================================
//
// ISO 1539:1980 Section 4 defines six data types:
// - INTEGER (Section 4.2)
// - REAL (Section 4.3)
// - DOUBLE PRECISION (Section 4.4)
// - COMPLEX (Section 4.5)
// - LOGICAL (Section 4.6)
// - CHARACTER (Section 4.8) - NEW in FORTRAN 77
//
// NOTE: ANTLR4 does not support true rule extension, so we must
// redefine type_spec with all inherited types plus CHARACTER.

// Type specification - ISO 1539:1980 Section 8.4
// Redefines FORTRAN 66 type_spec to add CHARACTER type
type_spec
    : INTEGER                    // ISO 1539:1980 Section 4.2
    | REAL                       // ISO 1539:1980 Section 4.3
    | LOGICAL                    // ISO 1539:1980 Section 4.6
    | DOUBLE PRECISION           // ISO 1539:1980 Section 4.4
    | COMPLEX                    // ISO 1539:1980 Section 4.5
    | CHARACTER character_length? // ISO 1539:1980 Section 4.8
    ;

// Character length specification - ISO 1539:1980 Section 8.4.2
// Forms: CHARACTER*len, CHARACTER*(*), CHARACTER*(len-expr)
character_length
    : MULTIPLY integer_expr      // CHARACTER*10
    | MULTIPLY LPAREN MULTIPLY RPAREN  // CHARACTER*(*) - assumed length
    | MULTIPLY LPAREN integer_expr RPAREN  // CHARACTER*(len-expr)
    ;

// ====================================================================
// TYPE DECLARATION WITH ENTITY DECLARATIONS
// ISO 1539:1980 Section 8.4
// ====================================================================
//
// Override FORTRAN 66 type_declaration to support per-variable
// CHARACTER length specifications in FORTRAN 77.
//
// Forms supported:
//  1. Type-level length: CHARACTER*10 A, B, C (all get length 10)
//  2. Per-variable length: CHARACTER A*10, B*20, C (C has default length)
//  3. Mixed: CHARACTER*5 A, B*10, C (A,C get 5, B gets 10)
//
// The type_spec may have an optional character_length (for type-level).
// Each entity_decl may have an optional character_length (for per-variable).
// Per-variable lengths override type-level lengths.

// Type declaration - ISO 1539:1980 Section 8.4
// Override from FORTRAN 66 to support entity declarations with per-variable
// CHARACTER length specifications
type_declaration
    : type_spec entity_decl_list
    ;

// Entity declaration list - ISO 1539:1980 Section 8.4
// Comma-separated list of entity declarations
entity_decl_list
    : entity_decl (COMMA entity_decl)*
    ;

// Entity declaration - ISO 1539:1980 Section 8.4
// A variable or array with optional per-variable CHARACTER length
// For CHARACTER type: variable_name or array_declarator, each optionally
// followed by a character_length (e.g., NAME*30, CITY*20)
entity_decl
    : variable character_length?      // Simple variable with optional length
    | array_declarator character_length?  // Array with optional length
    ;

// ====================================================================
// STATEMENT CLASSIFICATION - ISO 1539:1980 Section 7
// ====================================================================
//
// ISO 1539:1980 Section 7 classifies statements as:
// - Section 7.1: Executable statements
// - Section 7.2: Nonexecutable statements

// Statement body - ISO 1539:1980 Section 7
// Override from FORTRAN 66 to include structured constructs
statement_body
    // Nonexecutable statements - ISO 1539:1980 Section 7.2
    : statement_function_stmt  // Statement function (Section 15.6)
    | dimension_stmt     // Section 8.1
    | equivalence_stmt   // Section 8.2
    | common_stmt        // Section 8.3
    | type_declaration   // Section 8.4
    | implicit_stmt      // Section 8.5 - NEW in FORTRAN 77
    | parameter_stmt     // Section 8.6 - NEW in FORTRAN 77
    | save_stmt          // Section 8.7 - NEW in FORTRAN 77
    | external_stmt      // Section 8.8
    | intrinsic_stmt     // Section 8.9 - NEW in FORTRAN 77
    | data_stmt          // Section 9
    | format_stmt        // Section 13
    // Executable statements - ISO 1539:1980 Section 7.1
    | assignment_stmt    // Section 10.1-10.3
    | goto_stmt          // Section 11.1 (unconditional GO TO)
    | computed_goto_stmt // Section 11.3 (computed GO TO)
    | arithmetic_if_stmt // Section 11.4
    | logical_if_stmt    // Section 11.5
    | block_if_construct // Sections 11.6-11.9 - NEW in FORTRAN 77
    | do_stmt            // Section 11.10
    | continue_stmt      // Section 11.11
    | stop_stmt          // Section 11.12
    | pause_stmt         // Section 11.12
    | call_stmt          // Section 15.8
    | return_stmt        // Section 15.8
    | entry_stmt         // Section 15.7 - NEW in FORTRAN 77
    | end_stmt           // Section 11.12
    // I/O statements - ISO 1539:1980 Section 12
    | read_stmt          // Section 12.6 (formatted) / 12.4 (list-directed)
    | write_stmt         // Section 12.6 - Enhanced in FORTRAN 77
    | print_stmt         // Section 12.6
    | punch_stmt         // Legacy I/O, inherited from FORTRAN 66
    | open_stmt          // Section 12.10.1 - NEW in FORTRAN 77
    | close_stmt         // Section 12.10.2 - NEW in FORTRAN 77
    | inquire_stmt       // Section 12.10.3 - NEW in FORTRAN 77
    | rewind_stmt        // Section 12.9
    | backspace_stmt     // Section 12.9
    | endfile_stmt       // Section 12.9
    ;

// ====================================================================
// BLOCK IF CONSTRUCT - ISO 1539:1980 Sections 11.6-11.9
// ====================================================================
//
// ISO 1539:1980 Sections 11.6-11.9 define the block IF construct:
// - Section 11.6: IF statement (IF (logical-expr) THEN)
// - Section 11.7: ELSE IF statement (ELSE IF (logical-expr) THEN)
// - Section 11.8: ELSE statement (ELSE)
// - Section 11.9: END IF statement (END IF)

// Block IF construct - ISO 1539:1980 Sections 11.6-11.9
block_if_construct
    : if_then_stmt
      execution_part_construct*
      else_if_part*
      else_part?
      end_if_stmt
    ;

// IF statement - ISO 1539:1980 Section 11.6
// Syntax: IF (logical-expr) THEN
if_then_stmt
    : IF LPAREN logical_expr RPAREN THEN
    ;

// ELSE IF part - ISO 1539:1980 Section 11.7
else_if_part
    : else_if_stmt
      execution_part_construct*
    ;

// ELSE IF statement - ISO 1539:1980 Section 11.7
// Syntax: ELSE IF (logical-expr) THEN
else_if_stmt
    : ELSE IF LPAREN logical_expr RPAREN THEN
    ;

// ELSE part - ISO 1539:1980 Section 11.8
else_part
    : else_stmt
      execution_part_construct*
    ;

// ELSE statement - ISO 1539:1980 Section 11.8
// Syntax: ELSE
else_stmt
    : ELSE
    ;

// END IF statement - ISO 1539:1980 Section 11.9
// Syntax: END IF (or ENDIF as common extension)
end_if_stmt
    : END IF
    | ENDIF
    ;

// Execution part construct - statements within block IF
// Per ISO 1539:1980 Section 11.6, any executable statement
// except DO, block IF, ELSE IF, ELSE, END IF, or END
execution_part_construct
    : label? executable_construct NEWLINE?
    | NEWLINE
    ;

// Executable construct - ISO 1539:1980 Section 7.1
// Executable statements allowed within block IF constructs
executable_construct
    : assignment_stmt    // Section 10
    | goto_stmt          // Section 11.1
    | computed_goto_stmt // Section 11.3
    | arithmetic_if_stmt // Section 11.4
    | logical_if_stmt    // Section 11.5
    | call_stmt          // Section 15.8
    | return_stmt        // Section 15.8
    | stop_stmt          // Section 11.12
    | pause_stmt         // Section 11.12
    | continue_stmt      // Section 11.11
    | read_stmt          // Section 12.6
    | write_stmt         // Section 12.6
    | print_stmt         // Section 12.6
    | punch_stmt         // Legacy I/O
    | do_stmt            // Section 11.10
    | block_if_construct // Sections 11.6-11.9 (nested)
    | open_stmt          // Section 12.10.1
    | close_stmt         // Section 12.10.2
    | inquire_stmt       // Section 12.10.3
    | rewind_stmt        // Section 12.9
    | backspace_stmt     // Section 12.9
    | endfile_stmt       // Section 12.9
    ;

// ====================================================================
// INPUT/OUTPUT STATEMENTS - ISO 1539:1980 Section 12
// ====================================================================
//
// ISO 1539:1980 Section 12 defines I/O statements:
// - Section 12.4: List-directed formatting (NEW in FORTRAN 77)
// - Section 12.6: Formatted READ, WRITE, PRINT statements
// - Section 12.9: File positioning statements (REWIND, BACKSPACE, ENDFILE)
// - Section 12.10: File connection statements (OPEN, CLOSE, INQUIRE)

// READ statement - ISO 1539:1980 Sections 12.4 and 12.6
// Forms: READ fmt, iolist  OR  READ (cilist) iolist
read_stmt
    : READ format_identifier (COMMA input_item_list_f77)?
    | READ LPAREN control_info_list RPAREN input_item_list_f77?
    ;

// Input item list - ISO 1539:1980 Section 12.6.1
input_item_list_f77
    : input_item_f77 (COMMA input_item_f77)*
    ;

// Input item - ISO 1539:1980 Section 12.6.1
input_item_f77
    : variable
    | implied_do_input
    ;

// Implied DO for input - ISO 1539:1980 Section 12.6.1
implied_do_input
    : LPAREN input_item_list_f77 COMMA integer_variable EQUALS integer_expr COMMA
      integer_expr (COMMA integer_expr)? RPAREN
    ;

// PRINT statement - ISO 1539:1980 Section 12.6
// Form: PRINT fmt [, iolist]
// List-directed form: PRINT *, iolist (Section 12.4)
print_stmt
    : PRINT format_identifier (COMMA output_item_list)?
    ;

// WRITE statement - ISO 1539:1980 Section 12.6
// Form: WRITE (cilist) [iolist]
write_stmt
    : WRITE LPAREN control_info_list RPAREN output_item_list?
    ;

// Control information list - ISO 1539:1980 Section 12.6
control_info_list
    : control_info_item (COMMA control_info_item)*
    ;

// Control information item - ISO 1539:1980 Section 12.6
control_info_item
    : integer_expr          // Unit number
    | format_identifier     // Format specifier
    ;

// Format identifier - ISO 1539:1980 Section 12.6
// Identifies the format specification for I/O
format_identifier
    : label              // Format statement label (Section 13)
    | character_expr     // Character format (Section 12.4.1)
    | MULTIPLY           // List-directed (Section 12.4) - NEW in F77
    ;

// Output item list - ISO 1539:1980 Section 12.6.2
output_item_list
    : output_item (COMMA output_item)*
    ;

// Output item - ISO 1539:1980 Section 12.6.2
output_item
    : expr
    | character_expr  // Character expression (Section 6.2)
    | implied_do
    ;

// Implied DO for output - ISO 1539:1980 Section 12.6.2
implied_do
    : LPAREN dlist COMMA integer_variable EQUALS integer_expr COMMA integer_expr
      (COMMA integer_expr)? RPAREN
    ;

// Output list within implied DO - ISO 1539:1980 Section 12.6.2
dlist
    : output_item (COMMA output_item)*
    ;

// ====================================================================
// DO STATEMENT - ISO 1539:1980 Section 11.10
// ====================================================================
//
// ISO 1539:1980 Section 11.10 defines the DO statement:
// Syntax: DO s [,] i = e1, e2 [, e3]
//   s  = label of terminal statement
//   i  = DO variable (integer, real, or double precision)
//   e1 = initial value
//   e2 = terminal value
//   e3 = increment (default 1)
//
// NEW in FORTRAN 77: Real and double precision DO variables allowed

// DO statement - ISO 1539:1980 Section 11.10
do_stmt
    : DO label do_variable EQUALS initial_expr COMMA final_expr (COMMA increment_expr)?
    ;

// DO variable - ISO 1539:1980 Section 11.10
// Integer, real, or double precision scalar variable
do_variable
    : integer_variable
    | real_variable      // NEW in FORTRAN 77: real DO variables
    ;

// Initial value - ISO 1539:1980 Section 11.10
initial_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

// Terminal value - ISO 1539:1980 Section 11.10
final_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

// Increment value - ISO 1539:1980 Section 11.10
increment_expr
    : integer_expr
    | real_expr         // NEW in FORTRAN 77
    ;

// ====================================================================
// SPECIFICATION STATEMENTS - ISO 1539:1980 Section 8
// ====================================================================
//
// ISO 1539:1980 Section 8 defines specification statements:
// - Section 8.1: DIMENSION statement
// - Section 8.2: EQUIVALENCE statement
// - Section 8.3: COMMON statement
// - Section 8.4: Type statements
// - Section 8.5: IMPLICIT statement (NEW in FORTRAN 77)
// - Section 8.6: PARAMETER statement (NEW in FORTRAN 77)
// - Section 8.7: SAVE statement (NEW in FORTRAN 77)
// - Section 8.8: EXTERNAL statement
// - Section 8.9: INTRINSIC statement (NEW in FORTRAN 77)

// SAVE statement - ISO 1539:1980 Section 8.7
// Syntax: SAVE [ save-list ]
// Causes variables to retain values between procedure invocations
save_stmt
    : SAVE (save_list)?
    ;

// Save list - ISO 1539:1980 Section 8.7
save_list
    : save_item (COMMA save_item)*
    ;

// Save item - ISO 1539:1980 Section 8.7
// Variable name or common block name in slashes
save_item
    : variable
    | SLASH common_block_name SLASH
    ;

// Common block name - ISO 1539:1980 Section 8.3
common_block_name
    : IDENTIFIER
    ;

// INTRINSIC statement - ISO 1539:1980 Section 8.9
// Syntax: INTRINSIC name-list
// Declares names to be specific intrinsic functions
intrinsic_stmt
    : INTRINSIC intrinsic_procedure_list
    ;

// Intrinsic procedure list - ISO 1539:1980 Section 8.9
intrinsic_procedure_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// EXTERNAL statement - ISO 1539:1980 Section 8.8
// Syntax: EXTERNAL name-list
// Declares names to be external procedure names
external_stmt
    : EXTERNAL external_name_list
    ;

// External name list - ISO 1539:1980 Section 8.8
external_name_list
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

// ====================================================================
// IMPLICIT STATEMENT - ISO 1539:1980 Section 8.5
// ====================================================================
//
// ISO 1539:1980 Section 8.5 defines the IMPLICIT statement:
// Syntax: IMPLICIT type (letter-spec-list) [, type (letter-spec-list)]...
// Example: IMPLICIT INTEGER (I-N), REAL (A-H, O-Z)
//
// Specifies the type for all variables, arrays, and functions whose
// names begin with specified letters, overriding default typing.
//
// Note: IMPLICIT NONE is NOT part of FORTRAN 77 (added in Fortran 90)

// IMPLICIT statement - ISO 1539:1980 Section 8.5
implicit_stmt
    : IMPLICIT implicit_spec_list
    ;

// Implicit specification list - ISO 1539:1980 Section 8.5
implicit_spec_list
    : implicit_spec (COMMA implicit_spec)*
    ;

// Implicit specification - ISO 1539:1980 Section 8.5
// Associates a type with a letter range
implicit_spec
    : type_spec LPAREN letter_spec_list RPAREN
    ;

// Letter specification list - ISO 1539:1980 Section 8.5
letter_spec_list
    : letter_spec (COMMA letter_spec)*
    ;

// Letter specification - ISO 1539:1980 Section 8.5
// Single letter or letter range (e.g., A or A-H)
letter_spec
    : IDENTIFIER                         // Single letter (e.g., A)
    | IDENTIFIER MINUS IDENTIFIER        // Letter range (e.g., A-H)
    ;

// ====================================================================
// PARAMETER STATEMENT - ISO 1539:1980 Section 8.6
// ====================================================================
//
// ISO 1539:1980 Section 8.6 defines the PARAMETER statement:
// Syntax: PARAMETER (name = const-expr [, name = const-expr]...)
// Example: PARAMETER (PI = 3.14159, TWO_PI = 6.28318)
//
// Creates named constants that may be used wherever constants are allowed.
// Constraints:
// - Named constant must be defined before it is referenced
// - Expression must be a constant expression

// PARAMETER statement - ISO 1539:1980 Section 8.6
parameter_stmt
    : PARAMETER LPAREN parameter_assignment_list RPAREN
    ;

// Parameter assignment list - ISO 1539:1980 Section 8.6
parameter_assignment_list
    : parameter_assignment (COMMA parameter_assignment)*
    ;

// Parameter assignment - ISO 1539:1980 Section 8.6
// Associates a name with a constant expression
parameter_assignment
    : IDENTIFIER EQUALS constant_expr
    ;

// Constant expression - ISO 1539:1980 Section 8.6
// Per the standard, must consist of:
// - Literal constants
// - Named constants previously defined
// - Intrinsic functions with constant arguments
// Semantic validation deferred; syntactically accepts any expression
constant_expr
    : expr
    ;

// ====================================================================
// DATA STATEMENT - ISO 1539:1980 Section 9
// ====================================================================
//
// ISO 1539:1980 Section 9 defines the DATA statement:
// Syntax: DATA nlist /clist/ [[,] nlist /clist/]...
//
// Provides initial values for variables, arrays, and implied-DO lists.
// - Section 9.1: Form of DATA statement
// - Section 9.2: Variables in DATA statements
// - Section 9.3: Initial values

// DATA statement - ISO 1539:1980 Section 9.1
data_stmt
    : DATA data_stmt_set (COMMA data_stmt_set)*
    ;

// Data statement set - ISO 1539:1980 Section 9.1
// Associates a name list with a constant list
// Note: variable_list here is used as data_variable_list (supports implied-DO)
data_stmt_set
    : data_variable_list SLASH data_constant_list SLASH
    ;

// Data variable list - ISO 1539:1980 Section 9.2
// Supports variables and implied-DO lists in DATA statements
data_variable_list
    : data_variable (COMMA data_variable)*
    ;

// Data variable - ISO 1539:1980 Section 9.2
// A variable in a DATA statement may be a simple variable or an implied-DO list
data_variable
    : variable                      // Simple variable or array element
    | implied_do_data              // Implied-DO list for DATA
    ;

// Implied-DO list for DATA statements - ISO 1539:1980 Section 9.2
// Syntax: (data_variable_list, integer_var = expr, expr [, expr])
// Used to initialize ranges of array elements
implied_do_data
    : LPAREN data_variable_list COMMA integer_variable EQUALS integer_expr COMMA
      integer_expr (COMMA integer_expr)? RPAREN
    ;

// Data constant list - ISO 1539:1980 Section 9.3
data_constant_list
    : data_constant (COMMA data_constant)*
    ;

// Data constant - ISO 1539:1980 Section 9.3
// Optional repetition factor followed by optionally signed constant
data_constant
    : data_repetition? signed_constant
    ;

// Data repetition - ISO 1539:1980 Section 9.3
// Unsigned integer followed by asterisk (e.g., 10* in 10*0)
data_repetition
    : unsigned_int MULTIPLY
    ;

// Unsigned integer for repetition factors
// Used in DATA statement repetition (e.g., 10*0.0)
unsigned_int
    : INTEGER_LITERAL
    | LABEL               // LABEL tokens match 1-5 digit integers
    ;

// Signed constant - ISO 1539:1980 Section 9.3
// Used by data_constant rule for repetition factor support
signed_constant
    : literal
    | PLUS literal
    | MINUS literal
    ;

// Variable list - ISO 1539:1980 Section 9.2
variable_list
    : variable (COMMA variable)*
    ;

// ====================================================================
// LITERALS AND CONSTANTS - ISO 1539:1980 Section 4
// ====================================================================
//
// ISO 1539:1980 Section 4 defines data types and constants:
// - Section 4.2: Integer type
// - Section 4.3: Real type
// - Section 4.4: Double precision type
// - Section 4.5: Complex type
// - Section 4.6: Logical type
// - Section 4.8: CHARACTER type and character constants

// Literal constant - ISO 1539:1980 Section 4
// Override to add CHARACTER constants (STRING_LITERAL)
literal
    : INTEGER_LITERAL           // Section 4.2.2
    | LABEL                     // Accept LABEL as integer (lexer precedence)
    | REAL_LITERAL              // Section 4.3.2
    | STRING_LITERAL            // Section 4.8.2 - NEW in FORTRAN 77
    | logical_literal           // Section 4.6.2
    ;

// Logical literal - ISO 1539:1980 Section 4.6.2
logical_literal
    : DOT_TRUE
    | DOT_FALSE
    ;

// ====================================================================
// CHARACTER EXPRESSIONS - ISO 1539:1980 Section 6.2
// ====================================================================
//
// ISO 1539:1980 Section 6.2 defines character expressions:
// - Section 6.2.1: Character primaries
// - Section 6.2.2: Character operators (concatenation //)

// Character expression - ISO 1539:1980 Section 6.2
// Concatenation of character operands using //
character_expr
    : character_operand (CONCAT character_operand)*
    ;

// Character operand - ISO 1539:1980 Section 6.2.1
character_operand
    : character_primary
    ;

// Character primary - ISO 1539:1980 Section 6.2.1
character_primary
    : character_literal_constant
    | character_variable
    | character_function_reference
    | LPAREN character_expr RPAREN
    ;

// Character literal constant - ISO 1539:1980 Section 4.8.2
character_literal_constant
    : STRING_LITERAL
    | HOLLERITH  // Hollerith constants (legacy, Section 13.1.2)
    ;

// Character variable - ISO 1539:1980 Section 5.7
// Supports scalar and array element character variables with substring reference
// - Scalar: NAME(1:5) or NAME(:10)
// - Array element: NAMES(I)(1:5) or MATRIX(I,J)(start:end)
character_variable
    : IDENTIFIER (LPAREN expr_list RPAREN)? substring_range?
    ;

// ====================================================================
// ASSIGNMENT TARGET - ISO 1539:1980 Section 10.3
// ====================================================================
//
// ISO 1539:1980 Section 10.3 defines assignment targets that can be:
// - Numeric/logical variables: X = expr
// - Array elements: A(I) = expr or A(I,J) = expr
// - CHARACTER with substring: NAME(1:5) = expr
// - CHARACTER array element with substring: NAMES(I)(1:5) = expr

// Assignment target - allows substring on character variables
// Per ISO 1539:1980 Section 10.3
assignment_target
    : IDENTIFIER (LPAREN expr_list RPAREN)? substring_range?
    ;

// Override assignment_stmt to support substring assignment
// ISO 1539:1980 Section 10.3 allows substring on LHS for CHARACTER assignment
// Section 10.1-10.3: CHARACTER assignment can use character_expr (with substrings)
// or numeric expr depending on the variable type
assignment_stmt
    : assignment_target EQUALS assignment_rhs
    ;

// Assignment RHS - accepts both numeric and character expressions
// Per ISO 1539:1980 Sections 10.1-10.3
assignment_rhs
    : character_expr    // CHARACTER expressions with optional substrings
    | expr              // Numeric and logical expressions
    ;

// ====================================================================
// SUBSTRINGS - ISO 1539:1980 Section 5.7
// ====================================================================
//
// ISO 1539:1980 Section 5.7 defines substring references:
// Syntax: character-variable (start : end)

// Substring range - ISO 1539:1980 Section 5.7
// Syntax: (start:end) where start and/or end can be omitted
// Examples: (1:5), (1:), (:5), (:)
substring_range
    : LPAREN substr_start_expr? COLON substr_end_expr? RPAREN
    ;

// Substring start - ISO 1539:1980 Section 5.7
// Optional: if omitted, defaults to 1
substr_start_expr
    : integer_expr
    ;

// Substring end - ISO 1539:1980 Section 5.7
// Optional: if omitted, defaults to string length
substr_end_expr
    : integer_expr
    ;

// Character function reference - ISO 1539:1980 Section 15.2
character_function_reference
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    ;

// Actual argument specification list - ISO 1539:1980 Section 15.8.1
actual_arg_spec_list
    : actual_arg_spec (COMMA actual_arg_spec)*
    ;

// Actual argument specification - ISO 1539:1980 Section 15.8.1
actual_arg_spec
    : expr
    ;

// Integer variable - ISO 1539:1980 Section 4.2
integer_variable
    : IDENTIFIER
    ;

// Real variable - ISO 1539:1980 Section 4.3
real_variable
    : IDENTIFIER
    ;

// Real expression - ISO 1539:1980 Section 6.1
// Expression that evaluates to real (semantic constraint)
real_expr
    : expr
    ;

// ====================================================================
// FILE CONNECTION STATEMENTS - ISO 1539:1980 Section 12.10
// ====================================================================
//
// ISO 1539:1980 Section 12.10 defines file connection statements:
// - Section 12.10.1: OPEN statement
// - Section 12.10.2: CLOSE statement
// - Section 12.10.3: INQUIRE statement
//
// These statements are NEW in FORTRAN 77.
// File positioning statements (REWIND, BACKSPACE, ENDFILE) are in
// Section 12.9 and inherited from FORTRAN 66.

// OPEN statement - ISO 1539:1980 Section 12.10.1
// Syntax: OPEN ( olist )
// Connects an external file to a unit for I/O operations.
// Specifiers: UNIT, FILE, STATUS, ACCESS, FORM, RECL, BLANK, IOSTAT, ERR
open_stmt
    : OPEN LPAREN connect_spec_list RPAREN
    ;

// CLOSE statement - ISO 1539:1980 Section 12.10.2
// Syntax: CLOSE ( clist )
// Terminates the connection of an external file to a unit.
// Specifiers: UNIT, STATUS, IOSTAT, ERR
close_stmt
    : CLOSE LPAREN close_spec_list RPAREN
    ;

// INQUIRE statement - ISO 1539:1980 Section 12.10.3
// Syntax: INQUIRE ( ilist )
// Queries properties of a file or unit connection.
// Supports inquiry by unit or by file name.
inquire_stmt
    : INQUIRE LPAREN inquire_spec_list RPAREN
    ;

// Connect specifier list - ISO 1539:1980 Section 12.10.1
// Specifiers may appear in any order but each at most once
connect_spec_list
    : connect_spec (COMMA connect_spec)*
    ;

// Connect specifier - ISO 1539:1980 Section 12.10.1
connect_spec
    : unit_spec                                     // [UNIT=]u
    | file_spec                                     // FILE=fn
    | status_spec                                   // STATUS=sta
    | access_spec                                   // ACCESS=acc
    | form_spec                                     // FORM=fm
    | recl_spec                                     // RECL=rl
    | blank_spec                                    // BLANK=blnk
    | iostat_spec                                   // IOSTAT=ios
    | err_spec                                      // ERR=s
    ;

// Close specifier list - ISO 1539:1980 Section 12.10.2
close_spec_list
    : close_spec (COMMA close_spec)*
    ;

// Close specifier - ISO 1539:1980 Section 12.10.2
close_spec
    : unit_spec                                     // [UNIT=]u
    | status_spec                                   // STATUS=sta
    | iostat_spec                                   // IOSTAT=ios
    | err_spec                                      // ERR=s
    ;

// Inquire specifier list - ISO 1539:1980 Section 12.10.3
inquire_spec_list
    : inquire_spec (COMMA inquire_spec)*
    ;

// Inquire specifier - ISO 1539:1980 Section 12.10.3
inquire_spec
    : unit_spec                                     // [UNIT=]u
    | file_spec                                     // FILE=fn
    | iostat_spec                                   // IOSTAT=ios
    | err_spec                                      // ERR=s
    | exist_spec                                    // EXIST=ex
    | opened_spec                                   // OPENED=od
    | number_spec                                   // NUMBER=num
    | named_spec                                    // NAMED=nmd
    | name_spec                                     // NAME=fn
    | access_spec                                   // ACCESS=acc
    | sequential_spec                               // SEQUENTIAL=seq
    | direct_spec                                   // DIRECT=dir
    | form_spec                                     // FORM=fm
    | formatted_spec                                // FORMATTED=fmt
    | unformatted_spec                              // UNFORMATTED=unf
    | recl_spec                                     // RECL=rl
    | nextrec_spec                                  // NEXTREC=nr
    | blank_spec                                    // BLANK=blnk
    ;

// ====================================================================
// I/O SPECIFIER RULES - ISO 1539:1980 Section 12.10
// ====================================================================

// Unit specifier - ISO 1539:1980 Section 12.10
// [UNIT=]u where u is an integer expression (unit number)
unit_spec
    : IDENTIFIER EQUALS integer_expr                // UNIT=u (keyword form)
    | integer_expr                                  // u (positional form)
    ;

// FILE specifier - ISO 1539:1980 Section 12.10.1
// FILE=fn where fn is a character expression (file name)
file_spec
    : IDENTIFIER EQUALS character_expr
    ;

// STATUS specifier - ISO 1539:1980 Sections 12.10.1, 12.10.2
// STATUS=sta where sta is OLD, NEW, SCRATCH, or UNKNOWN
status_spec
    : IDENTIFIER EQUALS character_expr
    ;

// ACCESS specifier - ISO 1539:1980 Section 12.10.1
// ACCESS=acc where acc is SEQUENTIAL or DIRECT
access_spec
    : IDENTIFIER EQUALS character_expr
    ;

// FORM specifier - ISO 1539:1980 Section 12.10.1
// FORM=fm where fm is FORMATTED or UNFORMATTED
form_spec
    : IDENTIFIER EQUALS character_expr
    ;

// RECL specifier - ISO 1539:1980 Section 12.10.1
// RECL=rl where rl is a positive integer expression (record length)
recl_spec
    : IDENTIFIER EQUALS integer_expr
    ;

// BLANK specifier - ISO 1539:1980 Section 12.10.1
// BLANK=blnk where blnk is NULL or ZERO
blank_spec
    : IDENTIFIER EQUALS character_expr
    ;

// IOSTAT specifier - ISO 1539:1980 Sections 12.10.1-12.10.3
// IOSTAT=ios where ios is an integer variable for I/O status
iostat_spec
    : IDENTIFIER EQUALS variable
    ;

// ERR specifier - ISO 1539:1980 Sections 12.10.1-12.10.3
// ERR=s where s is a statement label for error branching
err_spec
    : IDENTIFIER EQUALS label
    ;

// EXIST specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// EXIST=ex where ex is a logical variable
exist_spec
    : IDENTIFIER EQUALS variable
    ;

// OPENED specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// OPENED=od where od is a logical variable
opened_spec
    : IDENTIFIER EQUALS variable
    ;

// NUMBER specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// NUMBER=num where num is an integer variable
number_spec
    : IDENTIFIER EQUALS variable
    ;

// NAMED specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// NAMED=nmd where nmd is a logical variable
named_spec
    : IDENTIFIER EQUALS variable
    ;

// NAME specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// NAME=fn where fn is a character variable
name_spec
    : IDENTIFIER EQUALS variable
    ;

// SEQUENTIAL specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// SEQUENTIAL=seq where seq is a character variable
sequential_spec
    : IDENTIFIER EQUALS variable
    ;

// DIRECT specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// DIRECT=dir where dir is a character variable
direct_spec
    : IDENTIFIER EQUALS variable
    ;

// FORMATTED specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// FORMATTED=fmt where fmt is a character variable
formatted_spec
    : IDENTIFIER EQUALS variable
    ;

// UNFORMATTED specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// UNFORMATTED=unf where unf is a character variable
unformatted_spec
    : IDENTIFIER EQUALS variable
    ;

// NEXTREC specifier - ISO 1539:1980 Section 12.10.3 (INQUIRE only)
// NEXTREC=nr where nr is an integer variable
nextrec_spec
    : IDENTIFIER EQUALS variable
    ;

// ====================================================================
// ISO 1539:1980 SPEC-GRAMMAR MAPPING (PARSER)
// ====================================================================
//
// This section summarizes the mapping from ISO 1539:1980 sections to
// parser rules defined in this file.
//
// Section 4 (Data Types):
//   - Section 4.2-4.6: Inherited types (INTEGER, REAL, etc.)
//   - Section 4.8: CHARACTER type -> type_spec, character_length
//
// Section 5 (Arrays and Substrings):
//   - Section 5.7: Substrings -> substring_range, character_variable
//
// Section 6 (Expressions):
//   - Section 6.2: Character expressions -> character_expr
//
// Section 7 (Statement Classification):
//   - Section 7.1: Executable statements -> statement_body
//   - Section 7.2: Nonexecutable statements -> statement_body
//
// Section 8 (Specification Statements):
//   - Section 8.4: Type statements -> type_declaration
//   - Section 8.5: IMPLICIT -> implicit_stmt
//   - Section 8.6: PARAMETER -> parameter_stmt
//   - Section 8.7: SAVE -> save_stmt
//   - Section 8.8: EXTERNAL -> external_stmt
//   - Section 8.9: INTRINSIC -> intrinsic_stmt
//
// Section 9 (DATA Statement):
//   - Section 9.1-9.3: DATA -> data_stmt
//
// Section 11 (Control Statements):
//   - Section 11.6-11.9: Block IF -> block_if_construct
//   - Section 11.10: DO -> do_stmt
//
// Section 12 (I/O Statements):
//   - Section 12.4: List-directed I/O -> format_identifier (*)
//   - Section 12.6: Formatted I/O -> read_stmt, write_stmt, print_stmt
//   - Section 12.10.1: OPEN -> open_stmt
//   - Section 12.10.2: CLOSE -> close_stmt
//   - Section 12.10.3: INQUIRE -> inquire_stmt
//
// Section 14 (Main Program):
//   - Section 14.1: Main program -> main_program, program_stmt
//
// Section 15 (Subprograms):
//   - Section 15.7: ENTRY -> entry_stmt
//
// ====================================================================
