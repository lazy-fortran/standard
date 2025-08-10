/**
 * FORTRAN IV (1962) - Inherits from FORTRAN II (1958) and adds LOGICAL data type
 */

const FORTRANII = require('../FORTRANII/grammar.js');

module.exports = grammar(FORTRANII, {
  name: 'FORTRANIV',

  rules: {
    // Extend statement_body to include logical declarations  
    statement_body: $ => choice(
      $.assignment,
      $.goto,
      $.if_stmt,
      $.do_stmt,
      $.continue_stmt,
      $.end_stmt,
      $.dimension_stmt,
      $.format_stmt,
      $.return_stmt,
      $.call_stmt,
      $.print_stmt,
      // NEW in FORTRAN IV
      $.logical_declaration
    ),

    // Extend expression to include logical expressions
    expression: $ => choice(
      $.number,
      $.variable,
      $.arithmetic_expr,
      $.function_call,
      // NEW in FORTRAN IV
      $.logical_expr
    ),

    // NEW: LOGICAL data type and operations
    logical_declaration: $ => seq('LOGICAL', $.variable_list),
    variable_list: $ => prec.left(seq($.variable, repeat(seq(',', $.variable)))),
    
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
  }
});