/**
 * FORTRAN II (1958) - Adds FUNCTION and SUBROUTINE
 */

module.exports = grammar({
  name: 'FORTRANII',

  extras: $ => [/\s+/, $.comment],

  rules: {
    program: $ => repeat(choice($.program_unit, $.statement)),

    program_unit: $ => choice($.function_subprogram, $.subroutine_subprogram),

    function_subprogram: $ => prec(1, seq(
      $.function_statement,
      repeat($.statement),
      $.end_statement
    )),

    subroutine_subprogram: $ => prec(1, seq(
      $.subroutine_statement, 
      repeat($.statement),
      $.end_statement
    )),

    function_statement: $ => seq('FUNCTION', $.function_name, '(', optional($.parameter_list), ')'),
    subroutine_statement: $ => seq('SUBROUTINE', $.subroutine_name, '(', optional($.parameter_list), ')'),
    
    function_name: $ => /[A-Z][A-Z0-9_]*/,
    subroutine_name: $ => /[A-Z][A-Z0-9_]*/,
    parameter_list: $ => seq($.variable, repeat(seq(',', $.variable))),

    statement: $ => choice(
      seq($.label, $.statement_body),
      $.statement_body
    ),

    statement_body: $ => choice(
      $.assignment,
      $.goto,
      $.if_stmt,
      $.do_stmt,
      $.continue_stmt,
      $.return_stmt,
      $.call_stmt,
      $.end_statement,
      $.dimension_stmt,
      $.format_stmt,
      $.print_stmt
    ),

    assignment: $ => seq($.variable, '=', $.expression),
    variable: $ => /[A-Z][A-Z0-9_]*/,
    
    expression: $ => choice(
      $.arithmetic_expr,
      $.number, 
      $.variable, 
      $.function_call
    ),
    
    arithmetic_expr: $ => prec.left(seq(
      choice($.variable, $.number, $.function_call),
      choice('+', '-', '*', '/'),
      choice($.variable, $.number, $.function_call)
    )),
    
    number: $ => /[0-9]+(\.[0-9]+)?/,

    function_call: $ => seq($.function_name, '(', optional($.argument_list), ')'),
    argument_list: $ => seq($.expression, repeat(seq(',', $.expression))),

    call_stmt: $ => seq('CALL', $.subroutine_name, '(', optional($.argument_list), ')'),
    print_stmt: $ => seq('PRINT', '*', ',', $.expression),

    goto: $ => seq('GO', 'TO', $.label),
    if_stmt: $ => seq('IF', '(', $.expression, ')', $.label, ',', $.label, ',', $.label),
    do_stmt: $ => seq('DO', $.label, $.variable, '=', $.expression, ',', $.expression),
    continue_stmt: $ => 'CONTINUE',
    return_stmt: $ => 'RETURN',
    end_statement: $ => 'END',

    dimension_stmt: $ => seq('DIMENSION', /[A-Z][A-Z0-9]*/, '(', /[0-9]+/, ')'),
    format_stmt: $ => seq('FORMAT', '(', /[^)]*/, ')'),

    label: $ => /[0-9]+/,
    comment: $ => seq(/[Cc*]/, /.*/),
  }
});