/**
 * FORTRAN 66 (1966) - First ANSI Standard with Logical Operations
 * Inheriting from FORTRAN II (1958) and combining FORTRAN IV features
 * 
 * Key additions from FORTRAN IV (1962) + FORTRAN 66 (1966):
 * - LOGICAL data type with .TRUE. and .FALSE. constants  
 * - Boolean operators: .AND., .OR., .NOT.
 * - Enhanced logical expressions
 * - Machine independence and standardization
 * - Standard intrinsic functions
 */

const fortranii = require('../FORTRANII/grammar.js');

module.exports = grammar(fortranii, {
  name: 'FORTRAN66',

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.simple_variable, $.subroutine_name],
    [$.variable, $.logical_variable],
    [$.constant, $.logical_primary]
  ],

  rules: {
    // ============================================================================
    // NEW: LOGICAL data type (from FORTRAN IV)
    // ============================================================================

    // Extend constant to include logical constants
    constant: $ => choice(
      $.integer_constant,
      $.real_constant,
      $.logical_constant    // NEW!
    ),

    // NEW: Logical constants
    logical_constant: $ => choice('.TRUE.', '.FALSE.'),

    // ============================================================================
    // NEW: Enhanced logical expressions (from FORTRAN IV)
    // ============================================================================

    // Override expression to handle logical expressions
    expression: $ => choice(
      $.arithmetic_expression,
      $.logical_expression
    ),

    // Enhanced logical expressions with full Boolean algebra
    logical_expression: $ => choice(
      $.logical_primary,
      $.logical_not_expression,
      $.logical_and_expression,
      $.logical_or_expression,
      $.relational_expression
    ),

    logical_primary: $ => choice(
      $.logical_constant,
      $.logical_variable,
      seq('(', $.logical_expression, ')')
    ),

    logical_variable: $ => alias($.simple_variable, 'logical_variable'),

    // NEW: Logical NOT operation
    logical_not_expression: $ => prec(4, seq(
      '.NOT.',
      $.logical_primary
    )),

    // NEW: Logical AND operation
    logical_and_expression: $ => prec.left(3, seq(
      $.logical_expression,
      '.AND.',
      $.logical_expression
    )),

    // NEW: Logical OR operation  
    logical_or_expression: $ => prec.left(2, seq(
      $.logical_expression,
      '.OR.',
      $.logical_expression
    )),

    // Enhanced relational expressions
    relational_expression: $ => prec.left(1, seq(
      $.arithmetic_expression,
      $.relational_operator,
      $.arithmetic_expression
    )),

    relational_operator: $ => choice(
      '.GT.',  '.GE.',  '.LT.',  '.LE.',  '.EQ.',  '.NE.'
    ),

    // ============================================================================
    // ENHANCE: Primary expressions to include logical
    // ============================================================================

    // Override primary to include logical constants
    primary: $ => choice(
      $.constant,
      $.variable,
      $.function_call,
      seq('(', $.arithmetic_expression, ')')
    ),

    // ============================================================================
    // STANDARDIZE: Standard intrinsic function names (FORTRAN 66)
    // ============================================================================

    // Override function_call to include standard FORTRAN 66 intrinsics
    function_call: $ => choice(
      // User-defined functions (from FORTRAN II)
      seq(
        $.function_name,
        '(',
        $.argument_list,
        ')'
      ),
      // Standard intrinsic functions (FORTRAN 66 standardized these)
      seq(
        $.intrinsic_function_name,
        '(',
        $.argument_list,
        ')'
      )
    ),

    // Standard FORTRAN 66 intrinsic function names
    intrinsic_function_name: $ => choice(
      // Arithmetic functions
      'ABS', 'IABS', 'DABS',
      'SQRT', 'DSQRT',
      'EXP', 'DEXP',
      'ALOG', 'DLOG',
      'SIN', 'DSIN', 'COS', 'DCOS',
      'ATAN', 'DATAN', 'ATAN2', 'DATAN2',
      // Type conversion functions
      'FLOAT', 'IFIX', 'INT',
      'AINT', 'DINT',
      // Mathematical functions
      'MOD', 'AMOD', 'DMOD',
      'SIGN', 'ISIGN', 'DSIGN',
      'DIM', 'IDIM', 'DDIM',
      'MAX0', 'MAX1', 'AMAX0', 'AMAX1', 'DMAX1',
      'MIN0', 'MIN1', 'AMIN0', 'AMIN1', 'DMIN1'
    ),

    // ============================================================================
    // STANDARDIZE: Enhanced I/O statements
    // ============================================================================

    // More standardized WRITE statement (replaces some PRINT usage)
    write_statement: $ => choice(
      seq('WRITE', '(', $.integer_constant, ',', $.integer_constant, ')', $.io_list),
      seq('WRITE', $.integer_constant, ',', $.io_list)
    ),

    // Override io_statement to include WRITE
    io_statement: $ => choice(
      $.read_statement,
      $.print_statement,
      $.punch_statement,
      $.read_tape_statement,
      $.write_tape_statement,
      $.write_statement    // NEW standardized WRITE
    ),

    // All other rules inherited from FORTRAN II (1958)!
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}