/**
 * FORTRAN (1957) - Simple working grammar
 */

module.exports = grammar({
  name: 'FORTRAN66',

  extras: $ => [/\s+/, $.comment],

  rules: {
    program: $ => repeat($.statement),

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
      $.end_stmt,
      $.dimension_stmt,
      $.format_stmt,
      $.type_declaration
    ),

    assignment: $ => seq($.variable, '=', $.expression),
    
    variable: $ => choice(
      $.array_element,
      /[A-Z][A-Z0-9]*/
    ),
    
    array_element: $ => seq(
      /[A-Z][A-Z0-9]*/,
      '(',
      $.subscript_list,
      ')'
    ),
    
    subscript_list: $ => seq($.expression, repeat(seq(',', $.expression))),
    expression: $ => choice($.number, $.variable, $.logical_expr, $.arithmetic_expr),
    
    arithmetic_expr: $ => prec.left(seq(
      choice($.variable, $.number),
      choice('+', '-', '*', '/'),
      choice($.variable, $.number)
    )),
    
    logical_expr: $ => choice(
      $.logical_constant,
      $.logical_operation
    ),
    
    logical_constant: $ => choice('.TRUE.', '.FALSE.'),
    
    logical_operation: $ => choice(
      prec.left(1, seq($.expression, '.AND.', $.expression)),
      prec.left(1, seq($.expression, '.OR.', $.expression)), 
      prec(2, seq('.NOT.', $.expression)),
      prec.left(1, seq($.expression, '.GT.', $.expression)),
      prec.left(1, seq($.expression, '.LT.', $.expression)),
      prec.left(1, seq($.expression, '.EQ.', $.expression)),
      prec.left(1, seq($.expression, '.NE.', $.expression))
    ),
    number: $ => /[0-9]+(\.[0-9]+)?/,

    goto: $ => seq('GO', 'TO', $.label),
    if_stmt: $ => seq('IF', '(', $.expression, ')', $.label, ',', $.label, ',', $.label),
    do_stmt: $ => seq('DO', $.label, $.variable, '=', $.expression, ',', $.expression),
    continue_stmt: $ => 'CONTINUE',
    end_stmt: $ => 'END',

    dimension_stmt: $ => seq('DIMENSION', $.array_declarator_list),
    
    array_declarator_list: $ => seq($.array_declarator, repeat(seq(',', $.array_declarator))),
    
    array_declarator: $ => seq(
      /[A-Z][A-Z0-9]*/,
      '(',
      $.dimension_list,
      ')'
    ),
    
    dimension_list: $ => seq($.dimension_bound, repeat(seq(',', $.dimension_bound))),
    dimension_bound: $ => $.number,
    format_stmt: $ => seq('FORMAT', '(', /[^)]*/, ')'),

    type_declaration: $ => seq(
      choice('LOGICAL', 'INTEGER', 'REAL'),
      $.variable_list
    ),
    
    variable_list: $ => seq($.variable, repeat(seq(',', $.variable))),

    label: $ => /[0-9]+/,
    comment: $ => seq(/[Cc*]/, /.*/),
  }
});