/*
 * LazyFortran2025Parser_Enhanced.g4
 * 
 * LazyFortran2025 - Modern syntactic relaxations using dual entry points
 * This implementation works around ANTLR4's inability to disable inherited rules
 * 
 * KEY INNOVATION: Dual entry points for traditional vs lazy syntax
 * 
 * RELAXATIONS:
 * 1. Optional program/module blocks - use lazy_fortran entry point
 * 2. Optional CONTAINS keyword - procedures follow directly
 * 3. Type inference - variables used without declarations
 * 4. Implicit none default - compiler-enforced
 */

parser grammar LazyFortran2025Parser_Enhanced;

options {
    tokenVocab = LazyFortran2025Lexer;
}

import Fortran2023Parser;

// ============================================================================
// DUAL ENTRY POINTS - The key to making ANTLR4 support lazy syntax!
// ============================================================================

// Entry Point 1: Traditional Fortran (fully compatible)
traditional_fortran
    : program_unit_f2023+ EOF
    ;

// Entry Point 2: LAZY FORTRAN - Direct code without wrappers!
lazy_fortran
    : lazy_program EOF
    ;

// ============================================================================
// LAZY PROGRAM STRUCTURE - No required PROGRAM/MODULE blocks!
// ============================================================================

lazy_program
    : lazy_element*
    ;

lazy_element
    : use_stmt NEWLINE?                        // USE statements at top level
    | implicit_stmt NEWLINE?                   // IMPLICIT (though NONE is default)
    | declaration_lazy NEWLINE?                // Flexible declarations
    | executable_lazy NEWLINE?                 // Direct executable statements
    | procedure_lazy                           // Procedures without CONTAINS!
    | NEWLINE                                  // Allow blank lines
    ;

// ============================================================================
// RELAXATION #1: Optional Program/Module Blocks
// ============================================================================

// No PROGRAM statement required - just write code!
// The compiler will wrap in implicit PROGRAM LAZY_MAIN if needed

// ============================================================================
// RELAXATION #2: Implicit None Default
// ============================================================================

// IMPLICIT NONE is the default - no need to specify
// Parser accepts both forms, compiler enforces typing

// ============================================================================
// RELAXATION #3: Optional CONTAINS
// ============================================================================

// Procedures can appear directly without CONTAINS keyword
procedure_lazy
    : function_lazy
    | subroutine_lazy
    ;

function_lazy
    : function_prefix_lazy function_name 
      '(' parameter_list_lazy? ')' result_clause_lazy? NEWLINE
      specification_part_lazy?
      execution_part_lazy?
      end_function_lazy
    ;

subroutine_lazy
    : subroutine_prefix_lazy subroutine_name 
      '(' parameter_list_lazy? ')' NEWLINE
      specification_part_lazy?
      execution_part_lazy?
      end_subroutine_lazy
    ;

function_prefix_lazy
    : type_spec_lazy? FUNCTION
    | FUNCTION
    ;

subroutine_prefix_lazy
    : SUBROUTINE
    ;

result_clause_lazy
    : RESULT '(' IDENTIFIER ')'
    ;

end_function_lazy
    : END FUNCTION function_name? NEWLINE
    | END NEWLINE
    ;

end_subroutine_lazy
    : END SUBROUTINE subroutine_name? NEWLINE
    | END NEWLINE
    ;

// ============================================================================
// RELAXATION #4: Type Inference
// ============================================================================

// Variables can be used without prior declaration
declaration_lazy
    : type_spec_lazy '::' variable_list_lazy     // Explicit declaration
    | variable_assignment_lazy                    // Implicit through assignment
    ;

// Assignment creates variable if not declared
variable_assignment_lazy
    : IDENTIFIER '=' expr_lazy
    | IDENTIFIER '(' array_indices_lazy ')' '=' expr_lazy
    ;

// Any identifier can be a variable (type inferred)
variable_lazy
    : IDENTIFIER
    | IDENTIFIER '(' array_indices_lazy ')'
    ;

array_indices_lazy
    : expr_lazy (',' expr_lazy)*
    ;

// ============================================================================
// LAZY EXECUTABLE STATEMENTS
// ============================================================================

executable_lazy
    : assignment_lazy
    | call_lazy
    | print_lazy
    | if_construct_lazy
    | do_construct_lazy
    | select_case_lazy
    | where_construct_lazy
    | forall_construct_lazy
    | stop_lazy
    | return_lazy
    ;

assignment_lazy
    : variable_lazy '=' expr_lazy
    ;

call_lazy
    : CALL procedure_name_lazy '(' argument_list_lazy? ')'
    ;

print_lazy
    : PRINT print_format_lazy (',' output_item_lazy)*
    ;

print_format_lazy
    : '*'                                    // List-directed
    | STRING_LITERAL                         // Format string
    | '(' format_items_lazy ')'             // Format specification
    ;

// ============================================================================
// LAZY CONTROL CONSTRUCTS
// ============================================================================

if_construct_lazy
    : IF '(' logical_expr_lazy ')' THEN NEWLINE
      (executable_lazy NEWLINE)*
      (ELSE IF '(' logical_expr_lazy ')' THEN NEWLINE
       (executable_lazy NEWLINE)*)*
      (ELSE NEWLINE
       (executable_lazy NEWLINE)*)?
      END IF NEWLINE
    | IF '(' logical_expr_lazy ')' executable_lazy  // Single-line IF
    ;

do_construct_lazy
    : DO do_control_lazy? NEWLINE
      (executable_lazy NEWLINE)*
      END DO NEWLINE
    ;

do_control_lazy
    : IDENTIFIER '=' expr_lazy ',' expr_lazy (',' expr_lazy)?
    | WHILE '(' logical_expr_lazy ')'
    | CONCURRENT '(' concurrent_header_lazy ')'
    ;

concurrent_header_lazy
    : index_spec_lazy (',' index_spec_lazy)* (',' logical_expr_lazy)?
    ;

index_spec_lazy
    : IDENTIFIER '=' expr_lazy ':' expr_lazy (':' expr_lazy)?
    ;

// ============================================================================
// LAZY EXPRESSIONS - Including F2023 conditional expressions!
// ============================================================================

expr_lazy
    : conditional_expr_lazy
    ;

// F2023 conditional expression (ternary operator)
conditional_expr_lazy
    : logical_expr_lazy '?' expr_lazy ':' expr_lazy
    | logical_expr_lazy
    ;

logical_expr_lazy
    : logical_expr_lazy '.OR.' logical_term_lazy
    | logical_term_lazy
    ;

logical_term_lazy
    : logical_term_lazy '.AND.' logical_factor_lazy
    | logical_factor_lazy
    ;

logical_factor_lazy
    : '.NOT.' logical_factor_lazy
    | relational_expr_lazy
    ;

relational_expr_lazy
    : arithmetic_expr_lazy relop_lazy arithmetic_expr_lazy
    | arithmetic_expr_lazy
    ;

relop_lazy
    : '.EQ.' | '.NE.' | '.LT.' | '.LE.' | '.GT.' | '.GE.'
    | '==' | '/=' | '<' | '<=' | '>' | '>='
    ;

arithmetic_expr_lazy
    : arithmetic_expr_lazy '+' term_lazy
    | arithmetic_expr_lazy '-' term_lazy
    | term_lazy
    ;

term_lazy
    : term_lazy '*' factor_lazy
    | term_lazy '/' factor_lazy
    | factor_lazy
    ;

factor_lazy
    : factor_lazy '**' primary_lazy
    | primary_lazy
    ;

primary_lazy
    : variable_lazy                        // Can be undeclared!
    | literal_lazy
    | '(' expr_lazy ')'
    | function_call_lazy
    | array_constructor_lazy
    ;

function_call_lazy
    : function_name_lazy '(' argument_list_lazy? ')'
    ;

array_constructor_lazy
    : '[' array_element_list_lazy ']'      // Modern bracket syntax
    | '(/' array_element_list_lazy '/)'    // Traditional syntax
    ;

array_element_list_lazy
    : expr_lazy (',' expr_lazy)*
    ;

// ============================================================================
// LAZY TYPE SYSTEM
// ============================================================================

type_spec_lazy
    : INTEGER
    | REAL
    | COMPLEX
    | LOGICAL
    | CHARACTER
    | DOUBLE PRECISION
    | TYPE '(' type_name_lazy ')'
    | CLASS '(' class_name_lazy ')'
    ;

// ============================================================================
// SPECIFICATION PART (more flexible)
// ============================================================================

specification_part_lazy
    : specification_item_lazy*
    ;

specification_item_lazy
    : declaration_lazy NEWLINE
    | use_stmt NEWLINE
    | implicit_stmt NEWLINE
    | parameter_stmt_lazy NEWLINE
    | NEWLINE
    ;

parameter_stmt_lazy
    : PARAMETER '(' parameter_def_lazy (',' parameter_def_lazy)* ')'
    ;

parameter_def_lazy
    : IDENTIFIER '=' expr_lazy
    ;

// ============================================================================
// EXECUTION PART (more flexible)
// ============================================================================

execution_part_lazy
    : execution_item_lazy*
    ;

execution_item_lazy
    : executable_lazy NEWLINE
    | declaration_lazy NEWLINE              // Declarations can appear here too!
    | NEWLINE
    ;

// ============================================================================
// IDENTIFIERS AND LITERALS
// ============================================================================

variable_list_lazy
    : variable_decl_lazy (',' variable_decl_lazy)*
    ;

variable_decl_lazy
    : IDENTIFIER ('=' expr_lazy)?           // Optional initialization
    ;

argument_list_lazy
    : argument_lazy (',' argument_lazy)*
    ;

argument_lazy
    : expr_lazy
    | IDENTIFIER '=' expr_lazy              // Keyword argument
    ;

output_item_lazy
    : expr_lazy
    ;

format_items_lazy
    : format_item_lazy (',' format_item_lazy)*
    ;

format_item_lazy
    : STRING_LITERAL
    | INTEGER_LITERAL IDENTIFIER
    ;

literal_lazy
    : INTEGER_LITERAL
    | REAL_LITERAL
    | COMPLEX_LITERAL
    | LOGICAL_LITERAL
    | STRING_LITERAL
    ;

// Names (can be undeclared for type inference)
function_name_lazy      : IDENTIFIER ;
subroutine_name_lazy    : IDENTIFIER ;
procedure_name_lazy     : IDENTIFIER ;
type_name_lazy          : IDENTIFIER ;
class_name_lazy         : IDENTIFIER ;
function_name           : IDENTIFIER ;
subroutine_name         : IDENTIFIER ;

// ============================================================================
// COMMENTS AND WHITESPACE (handled by lexer)
// ============================================================================

// Note: Both ! comments and C/c/* comments are supported
// Whitespace and newlines are handled appropriately