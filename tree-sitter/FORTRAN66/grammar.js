/**
 * FORTRAN 66 (1966) - Inherits from FORTRAN IV (1962) and standardizes language
 */

const FORTRANIV = require('../FORTRANIV/grammar.js');

module.exports = grammar(FORTRANIV, {
  name: 'FORTRAN66',

  rules: {
    // Extend statement_body to include type declarations
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
      $.logical_declaration,
      // NEW in FORTRAN 66
      $.type_declaration
    ),

    // NEW: Standard type declarations (LOGICAL handled by logical_declaration)
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