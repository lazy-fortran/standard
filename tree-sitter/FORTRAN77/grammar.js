/**
 * FORTRAN 77 (1977) - The Structured Programming Revolution
 * Inheriting from FORTRAN 66 (1966) with Tree-sitter composition
 * 
 * REVOLUTIONARY additions in FORTRAN 77:
 * - CHARACTER data type and string handling
 * - IF-THEN-ELSE-ENDIF structured programming (goodbye arithmetic IF!)
 * - DO-WHILE loops (structured loops)
 * - IMPLICIT NONE statement
 * - Enhanced I/O with file handling
 * - PARAMETER statement for named constants
 * - INTRINSIC and EXTERNAL statements
 * - Generic and specific intrinsic functions
 */

const fortran66 = require('../FORTRAN66/grammar.js');

module.exports = grammar(fortran66, {
  name: 'FORTRAN77',

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.simple_variable, $.subroutine_name],
    [$.variable, $.logical_variable],
    [$.constant, $.logical_primary],
    [$.expression, $.if_then_statement],
    [$.subscripted_variable, $.character_variable],
    [$.variable, $.logical_variable, $.character_variable],
    [$.constant, $.character_expression]
  ],

  rules: {
    // ============================================================================
    // ENHANCED: Type system with CHARACTER (FORTRAN 77 revolution!)
    // ============================================================================

    // Enhanced type specification with CHARACTER
    type_spec: $ => choice(
      'INTEGER',
      'REAL', 
      'LOGICAL',
      seq('DOUBLE', 'PRECISION'),       // DOUBLE PRECISION as sequence
      'COMPLEX',
      'CHARACTER'                       // NEW in FORTRAN 77!
    ),

    // ============================================================================
    // NEW: CHARACTER literals and constants
    // ============================================================================

    // Extend constant to include character literals
    constant: $ => choice(
      $.integer_constant,
      $.real_constant,
      $.logical_constant,
      $.character_constant    // NEW in FORTRAN 77!
    ),

    // NEW: Character constants (strings)
    character_constant: $ => choice(
      seq("'", /[^']*/, "'"),           // Single quotes
      seq('"', /[^"]*/, '"')            // Double quotes (FORTRAN 77 standard)
    ),

    // ============================================================================
    // REVOLUTIONARY: IF-THEN-ELSE-ENDIF constructs (goodbye arithmetic IF!)
    // ============================================================================

    // Override control_statement to include new structured constructs
    control_statement: $ => choice(
      // Old FORTRAN constructs (still supported)
      $.goto_statement,
      $.if_statement,           // Arithmetic IF (kept for compatibility)
      $.do_statement,
      $.continue_statement,
      $.stop_statement,
      $.pause_statement,
      $.call_statement,
      $.return_statement,
      // NEW structured programming constructs
      $.if_construct,           // NEW: IF-THEN-ELSE-ENDIF
      $.do_while_construct      // NEW: DO WHILE loops
    ),

    // NEW: IF-THEN-ELSE-ENDIF construct (structured programming!)
    if_construct: $ => seq(
      $.if_then_statement,
      repeat($.statement_without_end),
      optional(seq(
        $.else_statement,
        repeat($.statement_without_end)
      )),
      $.endif_statement
    ),

    if_then_statement: $ => seq(
      'IF',
      '(',
      $.logical_expression,
      ')',
      'THEN'
    ),

    else_statement: $ => 'ELSE',

    endif_statement: $ => choice('ENDIF', seq('END', 'IF')),

    // NEW: DO WHILE construct (structured loops)
    do_while_construct: $ => seq(
      $.do_while_statement,
      repeat($.statement_without_end),
      $.enddo_statement
    ),

    do_while_statement: $ => seq(
      'DO',
      'WHILE',
      '(',
      $.logical_expression,
      ')'
    ),

    enddo_statement: $ => choice('ENDDO', seq('END', 'DO')),

    // ============================================================================
    // NEW: PARAMETER statement (named constants)
    // ============================================================================

    // Extend specification_statement to include new FORTRAN 77 features
    specification_statement: $ => choice(
      // All FORTRAN 66 specification statements
      $.dimension_statement,
      $.equivalence_statement,
      $.common_statement,
      $.external_statement,
      // NEW in FORTRAN 77
      $.parameter_statement,
      $.implicit_statement,
      $.character_declaration
    ),

    // NEW: PARAMETER statement for named constants
    parameter_statement: $ => seq(
      'PARAMETER',
      '(',
      sep1($.parameter_assignment, ','),
      ')'
    ),

    parameter_assignment: $ => seq(
      $.simple_variable,
      '=',
      $.expression
    ),

    // NEW: IMPLICIT statement
    implicit_statement: $ => choice(
      seq('IMPLICIT', 'NONE'),          // Turn off implicit typing
      seq('IMPLICIT', $.implicit_specification_list)
    ),

    implicit_specification_list: $ => sep1($.implicit_specification, ','),

    implicit_specification: $ => seq(
      $.type_spec,
      '(',
      sep1($.letter_range, ','),
      ')'
    ),

    letter_range: $ => choice(
      /[A-Z]/,                          // Single letter
      seq(/[A-Z]/, '-', /[A-Z]/)        // Letter range
    ),

    // NEW: CHARACTER type declarations
    character_declaration: $ => seq(
      'CHARACTER',
      optional(seq('*', $.character_length)),
      sep1($.variable_name, ',')
    ),

    character_length: $ => choice(
      $.integer_constant,
      '(*)'                             // Assumed length
    ),

    variable_name: $ => $.simple_variable,

    // ============================================================================
    // ENHANCED: I/O with file handling
    // ============================================================================

    // Enhanced OPEN statement for file handling
    open_statement: $ => seq(
      'OPEN',
      '(',
      sep1($.open_specifier, ','),
      ')'
    ),

    open_specifier: $ => choice(
      seq('UNIT', '=', $.integer_expression),
      seq('FILE', '=', $.character_expression), 
      seq('STATUS', '=', $.character_expression),
      seq('ACCESS', '=', $.character_expression),
      seq('FORM', '=', $.character_expression),
      seq('RECL', '=', $.integer_expression)
    ),

    // Enhanced CLOSE statement
    close_statement: $ => seq(
      'CLOSE',
      '(',
      sep1($.close_specifier, ','),
      ')'
    ),

    close_specifier: $ => choice(
      seq('UNIT', '=', $.integer_expression),
      seq('STATUS', '=', $.character_expression)
    ),

    // Override io_statement to include new I/O
    io_statement: $ => choice(
      $.read_statement,
      $.print_statement,
      $.punch_statement,
      $.read_tape_statement,
      $.write_tape_statement,
      $.write_statement,
      $.open_statement,         // NEW
      $.close_statement         // NEW
    ),

    // ============================================================================
    // ENHANCED: Expressions with CHARACTER support
    // ============================================================================

    // Character expressions
    character_expression: $ => choice(
      $.character_constant,
      $.character_variable,
      $.character_substring,
      $.character_concatenation
    ),

    character_variable: $ => alias($.simple_variable, 'character_variable'),

    // NEW: Character substring operations
    character_substring: $ => seq(
      $.character_variable,
      '(',
      $.substring_range,
      ')'
    ),

    substring_range: $ => choice(
      seq($.integer_expression, ':', $.integer_expression),
      seq($.integer_expression, ':'),
      seq(':', $.integer_expression)
    ),

    // NEW: Character concatenation
    character_concatenation: $ => prec.left(1, seq(
      $.character_expression,
      '//',
      $.character_expression
    )),

    integer_expression: $ => alias($.arithmetic_expression, 'integer_expression'),

    // ============================================================================
    // ENHANCED: Assignment to handle CHARACTER expressions
    // ============================================================================

    // Override assignment to handle character expressions
    assignment_statement: $ => seq(
      $.variable,
      '=',
      $.expression
    ),

    // Enhanced expression to include character expressions
    expression: $ => choice(
      $.arithmetic_expression,
      $.logical_expression,
      $.character_expression      // NEW in FORTRAN 77!
    ),

    // All other rules inherited from FORTRAN 66!
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}