/**
 * FORTRAN (1957) - Simple working grammar
 */

module.exports = grammar({
  name: 'FORTRAN77',

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
      $.type_declaration,
      $.character_declaration,
      $.if_then_construct
    ),

    assignment: $ => seq($.variable, '=', $.expression),
    variable: $ => /[A-Z][A-Z0-9]*/,
    expression: $ => choice($.number, $.variable, $.string_literal, $.logical_expr, $.arithmetic_expr),
    
    string_literal: $ => /'[^']*'/,
    
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

    dimension_stmt: $ => seq('DIMENSION', /[A-Z][A-Z0-9]*/, '(', /[0-9]+/, ')'),
    format_stmt: $ => seq('FORMAT', '(', /[^)]*/, ')'),

    type_declaration: $ => seq(
      choice('LOGICAL', 'INTEGER', 'REAL'),
      $.variable_list
    ),
    
    character_declaration: $ => seq(
      'CHARACTER',
      optional(seq('*', $.number)),
      $.variable_list
    ),
    
    variable_list: $ => seq($.variable, repeat(seq(',', $.variable))),
    
    if_then_construct: $ => seq(
      'IF', '(', $.expression, ')', 'THEN',
      repeat($.statement),
      optional(seq('ELSE', repeat($.statement))),
      'ENDIF'
    ),

    label: $ => /[0-9]+/,
    comment: $ => seq(/[Cc*]/, /.*/),
  }
});