/**
 * FORTRAN 66 (1966) - Inherits from FORTRAN II and adds LOGICAL type + ANSI standardization
 * Note: FORTRAN IV (1962) and FORTRAN 66 (1966) are essentially the same - 
 * FORTRAN 66 was just the ANSI standardization of FORTRAN IV features
 */

const FORTRANII = require('../FORTRANII/grammar.js');

module.exports = grammar(FORTRANII, {
  name: 'FORTRAN66',

  rules: {
    // Extend statement_body to include FORTRAN IV/66 features
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
      // NEW in FORTRAN IV/66
      $.logical_declaration,
      $.type_declaration
    ),

    // Extend expression to include logical expressions
    expression: $ => choice(
      $.number,
      $.variable,
      $.arithmetic_expr,
      $.function_call,
      // NEW in FORTRAN IV/66
      $.logical_expr
    ),

    // NEW: LOGICAL data type and operations (from FORTRAN IV)
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

    // NEW: Standard type declarations
    type_declaration: $ => seq(
      choice('INTEGER', 'REAL'),
      $.variable_list
    ),
    
    // Improve array support
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
    
    subscript_list: $ => prec.left(seq($.expression, repeat(seq(',', $.expression)))),
    
    // Enhanced dimension statement with array declarators
    dimension_stmt: $ => seq('DIMENSION', $.array_declarator_list),
    
    array_declarator_list: $ => prec.left(seq($.array_declarator, repeat(seq(',', $.array_declarator)))),
    
    array_declarator: $ => seq(
      /[A-Z][A-Z0-9]*/,
      '(',
      $.dimension_list,
      ')'
    ),
    
    dimension_list: $ => prec.left(seq($.dimension_bound, repeat(seq(',', $.dimension_bound)))),
    dimension_bound: $ => $.number,
  }
});