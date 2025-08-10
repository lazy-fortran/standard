/**
 * Fortran 90 - Free-form with modules and modern features
 */

module.exports = grammar({
  name: 'Fortran90',

  extras: $ => [/\s+/, $.comment, $.free_comment],
  
  conflicts: $ => [
    [$.statement],
    [$.write_stmt],
    [$.read_stmt]
  ],

  rules: {
    program: $ => repeat($.program_unit),
    
    program_unit: $ => choice(
      $.main_program,
      $.module,
      $.subroutine_subprogram,
      $.function_subprogram
    ),
    
    main_program: $ => seq(
      optional(seq('PROGRAM', $.program_name)),
      repeat($.statement),
      choice(
        prec(3, seq('END', 'PROGRAM', $.program_name)),
        prec(2, seq('END', 'PROGRAM')),
        prec(1, 'END')
      )
    ),
    
    module: $ => seq(
      'MODULE', $.module_name,
      repeat($.statement),
      optional(seq('CONTAINS', repeat(choice($.subroutine_subprogram, $.function_subprogram)))),
      choice(
        prec(3, seq('END', 'MODULE', $.module_name)),
        prec(2, seq('END', 'MODULE')),
        prec(1, 'END')
      )
    ),
    
    subroutine_subprogram: $ => seq(
      'SUBROUTINE', $.subroutine_name, '(', optional($.parameter_list), ')',
      repeat($.statement),
      choice(
        prec(3, seq('END', 'SUBROUTINE', $.subroutine_name)),
        prec(2, seq('END', 'SUBROUTINE')),
        prec(1, 'END')
      )
    ),
    
    function_subprogram: $ => seq(
      optional($.type_spec), 'FUNCTION', $.function_name, '(', optional($.parameter_list), ')', optional(seq('RESULT', '(', $.variable, ')')),
      repeat($.statement),
      choice(
        prec(3, seq('END', 'FUNCTION', $.function_name)),
        prec(2, seq('END', 'FUNCTION')),
        prec(1, 'END')
      )
    ),
    
    program_name: $ => /[A-Za-z][A-Za-z0-9_]*/,
    module_name: $ => /[A-Za-z][A-Za-z0-9_]*/,
    subroutine_name: $ => /[A-Za-z][A-Za-z0-9_]*/,
    function_name: $ => /[A-Za-z][A-Za-z0-9_]*/,
    parameter_list: $ => prec.left(seq($.variable, repeat(seq(',', $.variable)))),

    statement: $ => choice(
      seq($.label, $.statement_body),
      $.statement_body
    ),

    statement_body: $ => choice(
      $.assignment,
      $.allocate_stmt,
      $.deallocate_stmt,
      $.use_stmt,
      $.implicit_stmt,
      $.type_declaration,
      $.if_construct,
      $.do_construct,
      $.select_construct,
      $.where_construct,
      $.goto,
      $.if_stmt,
      $.do_stmt,
      $.continue_stmt,
      $.stop_stmt,
      $.return_stmt,
      $.call_stmt,
      $.print_stmt,
      $.write_stmt,
      $.read_stmt
    ),

    assignment: $ => seq($.variable, '=', $.expression),
    variable: $ => choice(
      prec(2, $.array_element),
      prec(1, /[A-Za-z][A-Za-z0-9_]*/)
    ),
    
    array_element: $ => seq(
      /[A-Za-z][A-Za-z0-9_]*/,
      '(',
      $.subscript_list,
      ')'
    ),
    
    subscript_list: $ => prec.left(seq($.expression, repeat(seq(',', $.expression)))),
    
    expression: $ => choice(
      $.number, 
      $.variable, 
      $.string_literal,
      $.logical_expr,
      $.arithmetic_expr,
      $.array_constructor
    ),
    
    number: $ => /[0-9]+(\.[0-9]+)?([eEdD][+-]?[0-9]+)?/,
    string_literal: $ => choice(/"[^"]*"/, /'[^']*'/),
    
    array_constructor: $ => seq('(/', repeat(seq($.expression, optional(','))), '/)', ),
    
    arithmetic_expr: $ => prec.left(seq(
      choice($.variable, $.number, $.string_literal),
      choice('+', '-', '*', '/', '**'),
      choice($.variable, $.number, $.string_literal)
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
      prec.left(1, seq($.expression, '.NE.', $.expression)),
      prec.left(1, seq($.expression, '.GE.', $.expression)),
      prec.left(1, seq($.expression, '.LE.', $.expression))
    ),

    allocate_stmt: $ => seq('ALLOCATE', '(', $.allocate_list, optional(seq(',', 'STAT', '=', $.variable)), ')'),
    deallocate_stmt: $ => seq('DEALLOCATE', '(', $.allocate_list, optional(seq(',', 'STAT', '=', $.variable)), ')'),
    allocate_list: $ => prec.left(seq($.variable, repeat(seq(',', $.variable)))),
    
    use_stmt: $ => seq('USE', $.module_name, optional(seq(',', 'ONLY', ':', $.use_list))),
    use_list: $ => prec.left(seq($.identifier, repeat(seq(',', $.identifier)))),
    identifier: $ => /[A-Za-z][A-Za-z0-9_]*/,
    
    implicit_stmt: $ => choice(
      'IMPLICIT NONE',
      seq('IMPLICIT', $.implicit_spec_list)
    ),
    implicit_spec_list: $ => seq($.implicit_spec, repeat(seq(',', $.implicit_spec))),
    implicit_spec: $ => seq($.type_spec, '(', $.letter_spec_list, ')'),
    letter_spec_list: $ => seq($.letter_spec, repeat(seq(',', $.letter_spec))),
    letter_spec: $ => choice(/[A-Za-z]/, seq(/[A-Za-z]/, '-', /[A-Za-z]/)),

    type_declaration: $ => seq(
      $.type_spec,
      optional($.attr_spec_list),
      '::',
      $.entity_decl_list
    ),
    
    type_spec: $ => choice(
      prec(2, seq('INTEGER', '(', $.kind_selector, ')')),
      prec(2, seq('REAL', '(', $.kind_selector, ')')),
      prec(2, seq('CHARACTER', '(', $.char_selector, ')')),
      prec(1, 'INTEGER'),
      prec(1, 'REAL'), 
      prec(1, 'DOUBLE PRECISION'), 
      prec(1, 'COMPLEX'), 
      prec(1, 'LOGICAL'), 
      prec(1, 'CHARACTER')
    ),
    
    kind_selector: $ => choice($.expression, seq('KIND', '=', $.expression)),
    char_selector: $ => choice(
      seq('LEN', '=', $.expression),
      seq('KIND', '=', $.expression),
      seq($.expression, ',', $.expression)
    ),
    
    attr_spec_list: $ => seq(',', $.attr_spec, repeat(seq(',', $.attr_spec))),
    attr_spec: $ => choice(
      'PARAMETER', 'ALLOCATABLE', 'POINTER', 'TARGET', 'INTENT(IN)', 'INTENT(OUT)', 'INTENT(INOUT)',
      seq('DIMENSION', '(', $.array_spec, ')'),
    ),
    
    array_spec: $ => seq($.dimension_spec, repeat(seq(',', $.dimension_spec))),
    dimension_spec: $ => choice(':', $.expression, seq($.expression, ':', $.expression)),
    
    entity_decl_list: $ => prec.left(seq($.entity_decl, repeat(seq(',', $.entity_decl)))),
    entity_decl: $ => seq(
      $.variable,
      optional(seq('(', $.array_spec, ')')),
      optional(seq('=', $.expression))
    ),

    if_construct: $ => seq(
      'IF', '(', $.expression, ')', 'THEN',
      repeat($.statement),
      repeat(seq('ELSE IF', '(', $.expression, ')', 'THEN', repeat($.statement))),
      optional(seq('ELSE', repeat($.statement))),
      'END IF'
    ),
    
    do_construct: $ => choice(
      prec(2, seq(
        'DO', optional($.label), optional($.variable), optional(seq('=', $.expression, ',', $.expression, optional(seq(',', $.expression)))),
        repeat($.statement),
        optional($.label), 'END DO'
      )),
      prec(2, seq(
        'DO', 'WHILE', '(', $.expression, ')',
        repeat($.statement),
        'END DO'
      ))
    ),
    
    select_construct: $ => seq(
      'SELECT CASE', '(', $.expression, ')',
      repeat($.case_block),
      optional(seq('CASE DEFAULT', repeat($.statement))),
      'END SELECT'
    ),
    
    case_block: $ => seq(
      'CASE', '(', $.case_value_range, ')',
      repeat($.statement)
    ),
    
    case_value_range: $ => $.expression,
    
    where_construct: $ => seq(
      'WHERE', '(', $.expression, ')',
      repeat($.statement),
      optional(seq('ELSEWHERE', repeat($.statement))),
      'END WHERE'
    ),

    goto: $ => seq('GO', 'TO', $.label),
    if_stmt: $ => seq('IF', '(', $.expression, ')', $.statement_body),
    do_stmt: $ => prec(1, seq('DO', $.label, $.variable, '=', $.expression, ',', $.expression, optional(seq(',', $.expression)))),
    continue_stmt: $ => 'CONTINUE',
    stop_stmt: $ => seq('STOP', optional(choice($.number, $.string_literal))),
    return_stmt: $ => 'RETURN',
    
    call_stmt: $ => seq('CALL', $.subroutine_name, optional(seq('(', optional($.argument_list), ')'))),
    argument_list: $ => prec.left(seq($.expression, repeat(seq(',', $.expression)))),
    
    print_stmt: $ => seq('PRINT', $.format_spec, optional(seq(',', $.output_list))),
    write_stmt: $ => seq('WRITE', '(', $.io_control_spec_list, ')', optional($.output_list)),
    read_stmt: $ => seq('READ', '(', $.io_control_spec_list, ')', optional($.input_list)),
    
    format_spec: $ => choice('*', $.label, $.string_literal),
    output_list: $ => prec.left(seq($.expression, repeat(seq(',', $.expression)))),
    input_list: $ => prec.left(seq($.variable, repeat(seq(',', $.variable)))),
    
    io_control_spec_list: $ => seq($.io_control_spec, repeat(seq(',', $.io_control_spec))),
    io_control_spec: $ => choice(
      $.expression,
      seq('UNIT', '=', $.expression),
      seq('FMT', '=', $.format_spec),
      seq('IOSTAT', '=', $.variable),
      seq('ERR', '=', $.label),
      seq('END', '=', $.label)
    ),

    label: $ => /[0-9]+/,
    comment: $ => seq(/[Cc*]/, /.*/),
    free_comment: $ => seq('!', /.*/),
  }
});