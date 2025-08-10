/**
 * FORTRAN (1957) - Simple working grammar
 */

module.exports = grammar({
  name: 'Fortran2003',

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
      $.format_stmt
    ),

    assignment: $ => seq($.variable, '=', $.expression),
    variable: $ => /[A-Z][A-Z0-9]*/,
    expression: $ => choice($.number, $.variable),
    number: $ => /[0-9]+(\.[0-9]+)?/,

    goto: $ => seq('GO', 'TO', $.label),
    if_stmt: $ => seq('IF', '(', $.expression, ')', $.label, ',', $.label, ',', $.label),
    do_stmt: $ => seq('DO', $.label, $.variable, '=', $.expression, ',', $.expression),
    continue_stmt: $ => 'CONTINUE',
    end_stmt: $ => 'END',

    dimension_stmt: $ => seq('DIMENSION', /[A-Z][A-Z0-9]*/, '(', /[0-9]+/, ')'),
    format_stmt: $ => seq('FORMAT', '(', /[^)]*/, ')'),

    label: $ => /[0-9]+/,
    comment: $ => seq(/[Cc*]/, /.*/),
  }
});