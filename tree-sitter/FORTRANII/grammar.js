/**
 * FORTRAN II (1958) - Inherits from FORTRAN (1957) and adds FUNCTION and SUBROUTINE
 */

const FORTRAN = require('../FORTRAN/grammar.js');

module.exports = grammar(FORTRAN, {
  name: 'FORTRANII',

  rules: {
    // Extend program to support program units
    program: $ => repeat(choice($.program_unit, $.statement)),

    // NEW: Program units (functions and subroutines)
    program_unit: $ => choice($.function_subprogram, $.subroutine_subprogram),

    function_subprogram: $ => prec(1, seq(
      $.function_statement,
      repeat($.statement),
      $.end_stmt
    )),

    subroutine_subprogram: $ => prec(1, seq(
      $.subroutine_statement, 
      repeat($.statement),
      $.end_stmt
    )),

    function_statement: $ => seq('FUNCTION', $.function_name, '(', optional($.parameter_list), ')'),
    subroutine_statement: $ => seq('SUBROUTINE', $.subroutine_name, '(', optional($.parameter_list), ')'),
    
    function_name: $ => /[A-Z][A-Z0-9_]*/,
    subroutine_name: $ => /[A-Z][A-Z0-9_]*/,
    parameter_list: $ => prec.left(seq($.variable, repeat(seq(',', $.variable)))),

    // Extend statement_body to include new statements
    statement_body: $ => choice(
      $.assignment,
      $.goto,
      $.if_stmt,
      $.do_stmt,
      $.continue_stmt,
      $.end_stmt,
      $.dimension_stmt,
      $.format_stmt,
      // NEW in FORTRAN II
      $.return_stmt,
      $.call_stmt,
      $.print_stmt
    ),

    // Extend expression to include function calls
    expression: $ => choice(
      $.number,
      $.variable,
      $.arithmetic_expr,
      // NEW in FORTRAN II
      $.function_call
    ),
    
    // NEW: Function calls and related statements
    function_call: $ => seq($.function_name, '(', optional($.argument_list), ')'),
    argument_list: $ => prec.left(seq($.expression, repeat(seq(',', $.expression)))),
    call_stmt: $ => seq('CALL', $.subroutine_name, '(', optional($.argument_list), ')'),
    return_stmt: $ => 'RETURN',
    print_stmt: $ => seq('PRINT', '*', ',', $.expression),

    // Update variable pattern to support underscores (improvement over FORTRAN 1957)
    variable: $ => /[A-Z][A-Z0-9_]*/,

    // Extend arithmetic expressions for function calls
    arithmetic_expr: $ => prec.left(seq(
      choice($.variable, $.number, $.function_call),
      choice('+', '-', '*', '/'),
      choice($.variable, $.number, $.function_call)
    )),
  }
});