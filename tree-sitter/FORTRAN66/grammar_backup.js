/**
 * FORTRAN 66 (1966) - Inherits from FORTRAN II and adds LOGICAL type + ANSI standardization
 * Note: FORTRAN IV (1962) and FORTRAN 66 (1966) are essentially the same - 
 * FORTRAN 66 was just the ANSI standardization of FORTRAN IV features
 */

const FORTRANII = require('../FORTRANII/grammar.js');

module.exports = grammar(FORTRANII, {
  name: 'FORTRAN66',

  rules: {
    // Add program structure for ANSI test
    program: $ => choice(
      repeat1($.statement),
      $.program_unit
    ),
    
    program_unit: $ => $.main_program,
    
    main_program: $ => seq(
      optional($.program_statement),
      repeat($.statement),
      $.end_statement
    ),
    
    program_statement: $ => seq(
      'PROGRAM',
      $.program_name
    ),
    
    program_name: $ => /[A-Z][A-Z0-9]*/,
    
    end_statement: $ => prec(2, 'END'),
    
    // Add implicit statement
    implicit_statement: $ => seq(
      'IMPLICIT',
      choice(
        'NONE',
        $.implicit_spec_list
      )
    ),
    
    implicit_spec_list: $ => seq(
      $.type_spec,
      '(',
      $.letter_range_list,
      ')'
    ),
    
    letter_range_list: $ => seq(
      $.letter_range,
      repeat(seq(',', $.letter_range))
    ),
    
    letter_range: $ => choice(
      /[A-Z]/,
      seq(/[A-Z]/, '-', /[A-Z]/)
    ),
    // Statements
    statement: $ => choice(
      seq(optional($.label), $.statement_body),
      $.labeled_statement
    ),
    
    labeled_statement: $ => seq(
      $.label,
      $.statement_body
    ),
    
    label: $ => /[0-9]+/,
    
    // Extend statement_body to include FORTRAN IV/66 features
    statement_body: $ => choice(
      $.specification_statement,
      $.arithmetic_statement,
      $.control_statement,
      $.io_statement,
      $.assignment,
      $.goto,
      $.continue_stmt,
      $.end_stmt,
      $.dimension_stmt,
      $.format_stmt,
      $.format_statement,
      $.return_stmt,
      $.call_stmt,
      // NEW in FORTRAN IV/66
      $.logical_declaration
    ),
    
    specification_statement: $ => choice(
      $.type_declaration,
      $.implicit_statement,
      $.dimension_stmt
    ),
    
    arithmetic_statement: $ => $.assignment_statement,
    
    assignment_statement: $ => seq(
      $.variable,
      '=',
      $.expression
    ),
    
    control_statement: $ => choice(
      $.do_statement,
      $.if_stmt
    ),
    
    io_statement: $ => choice(
      $.print_statement,
      $.print_stmt
    ),
    
    print_statement: $ => seq(
      'PRINT',
      $.format_label,
      optional(seq(',', $.io_list))
    ),
    
    format_label: $ => /[0-9]+/,
    
    io_list: $ => seq(
      $.variable,
      repeat(seq(',', $.variable))
    ),
    
    format_statement: $ => seq(
      'FORMAT',
      '(',
      $.format_spec,
      ')'
    ),
    
    format_spec: $ => repeat1($.format_item),
    
    format_item: $ => choice(
      $.character_string_edit_descriptor,
      $.format_descriptor
    ),
    
    character_string_edit_descriptor: $ => /'[^']*'/,
    format_descriptor: $ => /[A-Z][0-9]+(\.[0-9]+)?/,

    // Extend expression to include logical expressions
    expression: $ => choice(
      $.number,
      $.variable,
      $.arithmetic_expr,
      $.function_call,
      // NEW in FORTRAN IV/66
      $.logical_expr
    ),

    // DO statement with proper structure
    do_statement: $ => seq(
      'DO',
      $.label,
      $.do_variable,
      '=',
      $.initial_value,
      ',',
      $.final_value,
      optional(seq(',', $.increment))
    ),
    
    do_variable: $ => $.variable,
    initial_value: $ => $.arithmetic_expression,
    final_value: $ => $.arithmetic_expression,
    increment: $ => $.arithmetic_expression,
    
    // Array and entity support
    entity_list: $ => seq(
      $.entity,
      repeat(seq(',', $.entity))
    ),
    
    entity: $ => seq(
      $.variable,
      optional($.array_spec)
    ),
    
    array_spec: $ => seq(
      '(',
      $.explicit_shape_spec,
      repeat(seq(',', $.explicit_shape_spec)),
      ')'
    ),
    
    explicit_shape_spec: $ => $.upper_bound,
    
    upper_bound: $ => $.arithmetic_expression,
    
    arithmetic_expression: $ => choice(
      $.term,
      seq($.arithmetic_expression, choice('+', '-'), $.term)
    ),
    
    term: $ => choice(
      $.factor,
      seq($.term, choice('*', '/'), $.factor)
    ),
    
    factor: $ => choice(
      $.primary,
      seq(choice('+', '-'), $.primary)
    ),
    
    primary: $ => choice(
      $.constant,
      $.variable,
      $.function_reference,
      seq('(', $.arithmetic_expression, ')')
    ),
    
    constant: $ => choice(
      $.integer_constant,
      $.real_constant
    ),
    
    integer_constant: $ => /[0-9]+/,
    real_constant: $ => /[0-9]+\.[0-9]*/,
    
    function_reference: $ => seq(
      $.function_name,
      '(',
      optional($.actual_argument_list),
      ')'
    ),
    
    function_name: $ => choice(
      'FLOAT',
      'INT',
      'ABS',
      'SQRT',
      /[A-Z][A-Z0-9]*/
    ),
    
    actual_argument_list: $ => seq(
      $.actual_argument,
      repeat(seq(',', $.actual_argument))
    ),
    
    actual_argument: $ => $.arithmetic_expression,
    
    // Array element with proper structure
    array_element: $ => seq(
      $.variable_name,
      '(',
      $.subscript_list,
      ')'
    ),
    
    variable_name: $ => /[A-Z][A-Z0-9]*/,
    
    subscript_list: $ => seq(
      $.subscript,
      repeat(seq(',', $.subscript))
    ),
    
    subscript: $ => $.arithmetic_expression,
    
    // Variable can be simple or array element
    variable: $ => choice(
      $.simple_variable,
      $.array_element
    ),
    
    simple_variable: $ => /[A-Z][A-Z0-9]*/,
    
    // Type spec
    type_spec: $ => choice(
      'INTEGER',
      'REAL',
      'LOGICAL',
      'DOUBLE PRECISION',
      'COMPLEX'
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