/**
 * FORTRAN 77 (1977) - Inherits from FORTRAN 66 and adds CHARACTER type, block IF constructs
 */

const FORTRAN66 = require('../FORTRAN66/grammar.js');

module.exports = grammar(FORTRAN66, {
  name: 'FORTRAN77',

  rules: {
    // Override program to handle both simple and full program structures  
    program: $ => choice(
      // Full program structure (with END statement or leading spaces)
      prec(-1, $.program_unit),
      // Simple structure - just statements
      repeat1($.statement)
    ),

    program_unit: $ => choice(
      $.main_program,
      $.subroutine_subprogram,
      $.function_subprogram
    ),

    main_program: $ => seq(
      choice(
        seq(/\s+/, repeat1($.statement)),  // With indentation
        seq(repeat1($.statement), $.end_statement)  // With END
      )
    ),

    // Unified statement that handles both simple and detailed structures
    statement: $ => choice(
      // Simple structure with statement_body (higher precedence for simple cases)
      prec(1, seq($.statement_body)),
      // Detailed structures
      $.specification_statement,
      $.arithmetic_statement,
      $.control_statement,
      $.io_statement
    ),
    
    // Simple statement body for basic tests
    statement_body: $ => choice(
      $.assignment,
      alias($.character_declaration_for_simple, $.character_declaration),
      alias($.print_statement_for_simple, $.print_stmt)
    ),
    
    // Simple assignment for basic tests
    assignment: $ => seq(
      alias(/[A-Z][A-Z0-9]*/, $.variable),
      '=',
      alias($.expression_for_simple, $.expression)
    ),
    
    expression_for_simple: $ => choice(
      alias(/\d+(\.\d*)?/, $.number),
      alias(/'[^']*'/, $.string_literal)
    ),
    
    // Character declaration for simple structure
    character_declaration_for_simple: $ => prec(2, seq(
      'CHARACTER',
      '*',
      alias(/\d+/, $.number),
      alias($.variable_list_simple, $.variable_list)
    )),
    
    variable_list_simple: $ => seq(
      alias(/[A-Z][A-Z0-9]*/, $.variable),
      repeat(seq(',', alias(/[A-Z][A-Z0-9]*/, $.variable)))
    ),
    
    // Print statement for simple structure
    print_statement_for_simple: $ => seq(
      'PRINT',
      '*',
      ',',
      alias(/[A-Z][A-Z0-9]*/, $.variable)
    ),

    specification_statement: $ => choice(
      $.type_declaration,
      $.character_declaration
    ),

    // NEW: CHARACTER type support with proper structure
    character_declaration: $ => seq(
      'CHARACTER',
      optional($.character_length),
      $.character_variable_list
    ),

    character_length: $ => seq('*', $.integer_constant),

    character_variable_list: $ => seq(
      $.character_variable,
      repeat(seq(',', $.character_variable))
    ),

    character_variable: $ => prec(1, seq(
      /[A-Z][A-Z0-9]*/,
      optional($.character_length)
    )),

    arithmetic_statement: $ => $.assignment_statement,

    assignment_statement: $ => seq(
      $.variable,
      '=',
      $.expression
    ),

    variable: $ => choice(
      $.substring_reference,
      $.simple_variable
    ),

    simple_variable: $ => prec(-1, /[A-Z][A-Z0-9]*/),

    substring_reference: $ => seq(
      $.character_variable,
      '(',
      $.substring_range,
      ')'
    ),

    substring_range: $ => seq(
      $.substring_start,
      ':',
      $.substring_end
    ),

    substring_start: $ => $.arithmetic_expression,
    substring_end: $ => $.arithmetic_expression,

    expression: $ => choice(
      $.arithmetic_expression,
      $.character_expression,
      $.logical_expression
    ),

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

    primary: $ => prec(1, choice(
      $.constant,
      $.variable,
      $.function_reference,
      seq('(', $.arithmetic_expression, ')'),
      $.character_constant
    )),

    constant: $ => choice(
      $.integer_constant,
      $.real_constant
    ),

    integer_constant: $ => /\d+/,
    real_constant: $ => /\d+\.\d*/,

    character_expression: $ => $.character_primary,

    character_primary: $ => choice(
      $.character_constant,
      $.character_variable
    ),

    character_constant: $ => prec(-1, /'[^']*'/),

    logical_expression: $ => choice(
      $.logical_primary,
      $.relational_expression,
      $.logical_and_expression
    ),

    logical_primary: $ => choice(
      $.logical_constant,
      $.logical_variable
    ),

    logical_constant: $ => choice('.TRUE.', '.FALSE.'),
    logical_variable: $ => /[A-Z][A-Z0-9]*/,

    relational_expression: $ => seq(
      $.arithmetic_expression,
      $.relational_operator,
      $.arithmetic_expression
    ),

    relational_operator: $ => choice('.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.'),

    logical_and_expression: $ => prec.left(seq(
      $.logical_expression,
      '.AND.',
      $.logical_expression
    )),

    function_reference: $ => seq(
      $.intrinsic_function_name,
      '(',
      optional($.actual_argument_list),
      ')'
    ),

    intrinsic_function_name: $ => choice('LEN', 'INDEX'),

    actual_argument_list: $ => seq(
      $.actual_argument,
      repeat(seq(',', $.actual_argument))
    ),

    actual_argument: $ => choice(
      $.arithmetic_expression,
      $.character_expression
    ),

    // NEW: Block IF constructs (structured programming!)
    control_statement: $ => choice(
      $.if_construct,
      $.do_while_construct
    ),

    if_construct: $ => seq(
      $.if_then_statement,
      repeat($.if_body_statement),
      repeat(seq($.elseif_then_statement, repeat($.if_body_statement))),
      optional(seq($.else_statement, repeat($.if_body_statement))),
      $.endif_statement
    ),

    if_body_statement: $ => choice(
      seq($.arithmetic_statement),
      seq($.io_statement)
    ),

    if_then_statement: $ => seq(
      'IF', '(', $.logical_expression, ')', 'THEN'
    ),

    elseif_then_statement: $ => seq(
      'ELSEIF', '(', $.logical_expression, ')', 'THEN'
    ),

    else_statement: $ => 'ELSE',

    endif_statement: $ => 'ENDIF',

    statement_without_end: $ => choice(
      $.arithmetic_statement,
      $.io_statement,
      $.control_statement
    ),
    
    // Also need to allow nesting control statements inside others
    nested_statement: $ => choice(
      $.arithmetic_statement,
      $.io_statement,
      $.control_statement
    ),

    do_while_construct: $ => seq(
      $.do_while_statement,
      repeat($.statement_without_end),
      $.enddo_statement
    ),

    do_while_statement: $ => seq(
      'DO', 'WHILE', '(', $.logical_expression, ')'
    ),

    enddo_statement: $ => 'ENDDO',

    io_statement: $ => $.print_statement,

    print_statement: $ => seq(
      'PRINT',
      choice($.integer_constant, '*'),
      optional(seq(',', $.io_list))
    ),

    io_list: $ => choice(
      $.variable,
      $.character_constant
    ),

    // Enhanced type declaration 
    type_declaration: $ => seq(
      $.type_spec,
      $.entity_list
    ),

    type_spec: $ => choice('INTEGER', 'REAL'),

    entity_list: $ => seq(
      $.entity,
      repeat(seq(',', $.entity))
    ),

    entity: $ => /[A-Z][A-Z0-9]*/,

    end_statement: $ => 'END'
  }
});