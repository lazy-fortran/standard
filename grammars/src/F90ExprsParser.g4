// Fortran 90 Expressions and Array Operations
// Reference: ISO/IEC 1539:1991 Section 4.5 (Arrays) and Section 7 (Expressions)
// Delegate grammar for expressions, primaries, and array constructors
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90ExprsParser;

// ====================================================================
// EXPRESSIONS - ISO/IEC 1539:1991 Section 7
// ====================================================================
//
// ISO/IEC 1539:1991 Section 7 defines expressions:
// - R701 (primary) -> constant | designator | array-constructor |
//                     structure-constructor | function-reference | (expr)
// - R702 (level-1-expr) -> [defined-unary-op] primary
// - R703-R713: Operator precedence and expression formation
// - R714 (logical-expr) -> level-5-expr
//
// Expressions with new operators and array operations.

// F90 expressions (enhanced with new operators and array operations)
expr_f90
    : expr_f90 DOT_EQV expr_f90                          # EquivalenceExprF90
    | expr_f90 DOT_NEQV expr_f90                         # NotEquivalenceExprF90
    | expr_f90 DOT_OR expr_f90                           # LogicalOrExprF90
    | expr_f90 DOT_AND expr_f90                          # LogicalAndExprF90
    | DOT_NOT expr_f90                                   # LogicalNotExprF90
    | expr_f90 (DOT_EQ | EQ_OP) expr_f90                 # EqualExprF90
    | expr_f90 (DOT_NE | NE_OP) expr_f90                 # NotEqualExprF90
    | expr_f90 (DOT_LT | LT_OP) expr_f90                 # LessExprF90
    | expr_f90 (DOT_LE | LE_OP) expr_f90                 # LessEqualExprF90
    | expr_f90 (DOT_GT | GT_OP) expr_f90                 # GreaterExprF90
    | expr_f90 (DOT_GE | GE_OP) expr_f90                 # GreaterEqualExprF90
    | expr_f90 CONCAT expr_f90                           # ConcatExprF90
    | expr_f90 POWER expr_f90                            # PowerExprF90
    | expr_f90 (MULTIPLY | SLASH) expr_f90              # MultDivExprF90
    | expr_f90 (PLUS | MINUS) expr_f90                   # AddSubExprF90
    | (PLUS | MINUS) expr_f90                            # UnaryExprF90
    | primary_f90                                        # PrimaryExprF90
    ;

// F90 primary expressions (enhanced with new constructs)
primary_f90
    : literal_f90
    | variable_f90
    | function_reference_f90
    | intrinsic_function_f90        // F90 array intrinsics
    | array_constructor_f90         // F90 innovation
    | structure_constructor         // F90 innovation
    | LPAREN expr_f90 RPAREN
    ;

// ====================================================================
// F90 INTRINSIC FUNCTIONS
// ====================================================================
// ISO/IEC 1539:1991 Section 13
// Array inquiry and manipulation intrinsics.

// F90 intrinsic functions (SIZE, SHAPE, RESHAPE, etc.)
intrinsic_function_f90
    : SIZE LPAREN actual_arg_spec_list RPAREN         // SIZE array intrinsic
    | SHAPE_INTRINSIC LPAREN actual_arg_spec_list RPAREN     // SHAPE array intrinsic
    | RESHAPE_INTRINSIC LPAREN actual_arg_spec_list RPAREN   // RESHAPE array intrinsic
    | LBOUND LPAREN actual_arg_spec_list RPAREN       // LBOUND array intrinsic
    | UBOUND LPAREN actual_arg_spec_list RPAREN       // UBOUND array intrinsic
    | ALLOCATED LPAREN variable_f90 RPAREN            // ALLOCATED status
    | PRESENT LPAREN IDENTIFIER RPAREN                // PRESENT argument check
    | SELECTED_REAL_KIND LPAREN actual_arg_spec_list RPAREN
    | SELECTED_INT_KIND LPAREN actual_arg_spec_list RPAREN
    | KIND LPAREN expr_f90 RPAREN
    | LEN LPAREN expr_f90 RPAREN
    ;

// ====================================================================
// F90 VARIABLES
// ====================================================================
// ISO/IEC 1539:1991 Section 6.1
// Variables with derived type components and array sections.

// F90 variables (enhanced with derived type components and array sections)
variable_f90
    : identifier_or_keyword (substring_range)?                   // Simple variable
    | identifier_or_keyword LPAREN section_subscript_list RPAREN (substring_range)?
        // Array element/section
    // Derived type component
    | variable_f90 PERCENT identifier_or_keyword (substring_range)?
    | variable_f90 LPAREN section_subscript_list RPAREN (substring_range)?
        // Component array element
    ;

// Array section subscripting (F90 enhancement)
section_subscript_list
    : section_subscript (COMMA section_subscript)*
    ;

section_subscript
    : expr_f90                      // Single subscript
    | subscript_triplet             // Array section triplet
    ;

subscript_triplet
    : expr_f90? COLON expr_f90? (COLON expr_f90)?    // start:end:stride
    ;

// Substring range
substring_range
    : LPAREN expr_f90? COLON expr_f90? RPAREN
    ;

// ====================================================================
// ARRAY OPERATIONS (F90 MAJOR INNOVATION)
// ====================================================================
// ISO/IEC 1539:1991 Section 4.5
// Array constructors using (/ ... /) syntax.

// Array constructor (F90 revolutionary feature)
// NOTE: Square bracket syntax [ ... ] is Fortran 2003 (ISO/IEC 1539-1:2004),
// defined in Fortran2003Parser.g4, NOT here.
array_constructor_f90
    : LPAREN SLASH ac_spec SLASH RPAREN     // F90 syntax: (/ ... /)
    ;

// Array constructor specification
ac_spec
    : ac_value_list?
    ;

ac_value_list
    : ac_value (COMMA ac_value)*
    ;

ac_value
    : expr_f90
    | ac_implied_do                 // Implied DO in array constructor
    ;

// Implied DO in array constructor (F90 feature)
ac_implied_do
    : LPAREN ac_value_list COMMA do_variable EQUALS expr_f90 COMMA expr_f90
      (COMMA expr_f90)? RPAREN
    ;

// ====================================================================
// ENHANCED LITERALS (F90 IMPROVEMENTS)
// ====================================================================
// ISO/IEC 1539:1991 Section 4.3

// F90 literals (enhanced with kind specifiers)
literal_f90
    : INTEGER_LITERAL_KIND          // Integer with kind (123_int32)
    | INTEGER_LITERAL               // Traditional integer
    | REAL_LITERAL_KIND             // Real with kind (3.14_real64)
    | REAL_LITERAL                  // Traditional real
    | DOUBLE_QUOTE_STRING           // Double-quoted string (F90)
    | SINGLE_QUOTE_STRING           // Single-quoted string
    | logical_literal_f90           // Enhanced logical literals
    | boz_literal_constant          // Binary/octal/hex literals (F90)
    ;

// Logical literals (F90 enhancements)
logical_literal_f90
    : DOT_TRUE                      // .TRUE.
    | DOT_FALSE                     // .FALSE.
    ;

// BOZ literal constants (F90 binary/octal/hex)
boz_literal_constant
    : BINARY_CONSTANT               // Binary BOZ literal (for example B 10101)
    | OCTAL_CONSTANT                // Octal BOZ literal (for example O 777)
    | HEX_CONSTANT                  // Hex BOZ literal (for example Z FF or X FF)
    ;

// ====================================================================
// ASSIGNMENT STATEMENTS (F90)
// ====================================================================
// ISO/IEC 1539:1991 Section 7.5

// F90 assignment statements
assignment_stmt_f90
    : variable_f90 EQUALS expr_f90 NEWLINE?
    ;

// F90 pointer assignment (major innovation)
pointer_assignment_stmt
    : variable_f90 POINTER_ASSIGN expr_f90
    ;

// Function reference (enhanced for F90)
function_reference_f90
    : IDENTIFIER LPAREN actual_arg_spec_list? RPAREN
    ;

// Actual argument specification (F90 keyword arguments)
actual_arg_spec_list
    : actual_arg_spec (COMMA actual_arg_spec)*
    ;

actual_arg_spec
    : IDENTIFIER EQUALS expr_f90    // Keyword argument
    | expr_f90                      // Positional argument
    | MULTIPLY IDENTIFIER           // Alternate return (F77 compatibility)
    ;
