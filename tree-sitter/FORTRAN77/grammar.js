/**
 * FORTRAN 77 (1977) - Inherits from FORTRAN 66 and adds CHARACTER type, block IF constructs
 */

const FORTRAN66 = require('../FORTRAN66/grammar.js');

module.exports = grammar(FORTRAN66, {
  name: 'FORTRAN77',

  rules: {
    // Extend statement_body to include new FORTRAN 77 features
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
      $.type_declaration,
      // NEW in FORTRAN 77
      $.character_declaration,
      $.if_then_construct
    ),

    // Extend expression to include string literals
    expression: $ => choice(
      $.number,
      $.variable,
      $.arithmetic_expr,
      $.function_call,
      $.logical_expr,
      // NEW in FORTRAN 77
      $.string_literal
    ),

    // NEW: CHARACTER type support
    character_declaration: $ => seq(
      'CHARACTER',
      optional(seq('*', $.number)),
      $.variable_list
    ),
    
    string_literal: $ => /'[^']*'/,

    // NEW: Block IF constructs (structured programming!)
    if_then_construct: $ => seq(
      'IF', '(', $.expression, ')', 'THEN',
      repeat($.statement),
      repeat(seq('ELSEIF', '(', $.expression, ')', 'THEN', repeat($.statement))),
      optional(seq('ELSE', repeat($.statement))),
      'ENDIF'
    ),

    // Enhanced type declaration (LOGICAL handled by inherited logical_declaration)
    type_declaration: $ => seq(
      choice('INTEGER', 'REAL'),
      $.variable_list
    ),

    // Enhanced print statement
    print_stmt: $ => seq('PRINT', '*', ',', $.expression),

    // Enhanced arithmetic expressions for string operations
    arithmetic_expr: $ => prec.left(seq(
      choice($.variable, $.number, $.string_literal, $.function_call),
      choice('+', '-', '*', '/'),
      choice($.variable, $.number, $.string_literal, $.function_call)
    )),
  }
});