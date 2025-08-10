/**
 * FORTRAN (1957) - The Original IBM 704 Grammar
 * Tree-sitter implementation with better composability than ANTLR4
 * 
 * Historical significance: First high-level programming language
 * Released: October 15, 1957
 * Target: IBM 704 mainframe
 */

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar({
  name: 'FORTRAN',

  extras: $ => [
    /\s+/,
    $.comment
  ],

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.variable, $.constant]
  ],

  precedences: $ => [
    ['power', 'mult', 'add', 'relop']
  ],

  word: $ => $.simple_variable,

  inline: $ => [
    $.label,
    $.subscript
  ],

  rules: {
    // ============================================================================
    // PROGRAM STRUCTURE: IBM 704 FORTRAN
    // ============================================================================
    
    program: $ => repeat($.statement),

    statement: $ => seq(
      optional($.label),
      $.statement_body
    ),

    // Base hooks for extension by descendants
    statement_body: $ => choice(
      $.arithmetic_statement,
      $.control_statement,
      $.io_statement,
      $.specification_statement,
      $.format_statement,
      $.end_statement
    ),

    label: $ => /[1-9][0-9]{0,4}/,

    // Comments (fixed-form style for FORTRAN I)
    comment: $ => seq(
      choice('C', 'c', '*'),
      /.*/
    ),

    // ============================================================================
    // ARITHMETIC STATEMENTS (1957)
    // ============================================================================

    arithmetic_statement: $ => $.assignment_statement,

    assignment_statement: $ => seq(
      $.variable,
      '=',
      $.expression
    ),

    variable: $ => choice(
      $.simple_variable,
      $.subscripted_variable
    ),

    simple_variable: $ => /[A-Z][A-Z0-9]*/,

    subscripted_variable: $ => seq(
      $.simple_variable,
      '(',
      $.subscript_list,
      ')'
    ),

    subscript_list: $ => sep1($.subscript, ','),

    subscript: $ => $.expression,

    // ============================================================================
    // EXPRESSIONS (1957) - Base hooks for extension
    // ============================================================================

    // Base expression hook - descendants can extend this
    expression_base: $ => $.arithmetic_expression,
    
    expression: $ => $.expression_base,

    // Base primary hook - descendants can extend this 
    primary_base: $ => choice(
      $.constant,
      $.variable,
      $.function_call,
      seq('(', $.arithmetic_expression, ')')
    ),

    arithmetic_expression: $ => choice(
      $.term,
      prec.left('add', seq(
        $.arithmetic_expression,
        choice('+', '-'),
        $.term
      ))
    ),

    term: $ => choice(
      $.factor,
      prec.left('mult', seq(
        $.term,
        choice('*', '/'),
        $.factor
      ))
    ),

    factor: $ => choice(
      $.primary,
      prec.right('power', seq(
        $.primary,
        '**',
        $.factor
      ))
    ),

    primary: $ => $.primary_base,

    // Base constant hook - descendants can extend this
    constant_base: $ => choice(
      $.integer_constant,
      $.real_constant
    ),
    
    constant: $ => $.constant_base,

    integer_constant: $ => /[0-9]+/,
    
    real_constant: $ => /[0-9]+\.[0-9]+([E][+-]?[0-9]+)?/,

    // Base function name hook - descendants can override
    function_name_base: $ => /[A-Z]+F/,  // Functions end with F in FORTRAN I
    
    function_name: $ => $.function_name_base,

    function_call: $ => seq(
      $.function_name,
      '(',
      $.argument_list,
      ')'
    ),

    argument_list: $ => sep1($.expression, ','),

    // ============================================================================
    // CONTROL STATEMENTS (1957) - Base hooks for extension
    // ============================================================================

    // Base control statement hook - descendants can extend this
    control_statement_base: $ => choice(
      $.goto_statement,
      $.if_statement,
      $.do_statement,
      $.continue_statement,
      $.stop_statement,
      $.pause_statement
    ),
    
    control_statement: $ => $.control_statement_base,

    goto_statement: $ => seq(
      'GO', 'TO',
      $.label
    ),

    // Arithmetic IF - three-way branch (unique to early FORTRAN)
    if_statement: $ => seq(
      'IF',
      '(',
      field('condition', $.expression),
      ')',
      field('negative_label', $.label),
      ',',
      field('zero_label', $.label),
      ',',
      field('positive_label', $.label)
    ),

    do_statement: $ => seq(
      'DO',
      $.label,
      $.simple_variable,
      '=',
      $.expression,
      ',',
      $.expression,
      optional(seq(',', $.expression))
    ),

    continue_statement: $ => 'CONTINUE',
    
    stop_statement: $ => seq('STOP', optional($.integer_constant)),
    
    pause_statement: $ => seq('PAUSE', optional($.integer_constant)),

    // ============================================================================
    // I/O STATEMENTS (1957) - Base hooks for extension  
    // ============================================================================

    // Base I/O statement hook - descendants can extend this
    io_statement_base: $ => choice(
      $.read_statement,
      $.print_statement,
      $.punch_statement,
      $.read_tape_statement,
      $.write_tape_statement
    ),
    
    io_statement: $ => $.io_statement_base,

    read_statement: $ => seq(
      'READ',
      $.integer_constant,
      ',',
      $.io_list
    ),

    print_statement: $ => seq(
      'PRINT',
      $.integer_constant,
      ',',
      $.io_list
    ),

    punch_statement: $ => seq(
      'PUNCH',
      $.integer_constant,
      ',',
      $.io_list
    ),

    // Order specific before general for proper precedence
    read_tape_statement: $ => prec(1, seq(
      'READ', 'TAPE',
      $.integer_constant,
      ',',
      $.io_list
    )),

    write_tape_statement: $ => prec(1, seq(
      'WRITE', 'TAPE',
      $.integer_constant,
      ',',
      $.io_list
    )),

    io_list: $ => sep1($.variable, ','),

    // ============================================================================
    // SPECIFICATION STATEMENTS (1957) - Base hooks for extension
    // ============================================================================

    // Base specification statement hook - descendants can extend this
    specification_statement_base: $ => choice(
      $.dimension_statement,
      $.equivalence_statement,
      $.frequency_statement
    ),
    
    specification_statement: $ => $.specification_statement_base,

    dimension_statement: $ => seq(
      'DIMENSION',
      sep1($.array_declarator, ',')
    ),

    array_declarator: $ => seq(
      $.simple_variable,
      '(',
      sep1($.integer_constant, ','),
      ')'
    ),

    equivalence_statement: $ => seq(
      'EQUIVALENCE',
      '(',
      sep1($.variable, ','),
      ')'
    ),

    frequency_statement: $ => seq(
      'FREQUENCY',
      sep1(seq($.label, '(', $.integer_constant, ')'), ',')
    ),

    // ============================================================================
    // FORMAT STATEMENT (1957)
    // ============================================================================

    format_statement: $ => seq(
      'FORMAT',
      '(',
      $.format_specification,
      ')'
    ),

    format_specification: $ => /.*/,  // Simplified for now

    // ============================================================================
    // END STATEMENT (1957)
    // ============================================================================

    end_statement: $ => 'END',

    // ============================================================================
    // LOGICAL EXPRESSIONS (limited in 1957) - Base hooks for extension
    // ============================================================================

    // Base logical expression hook - descendants can extend this
    logical_expression_base: $ => $.relational_expression,
    
    logical_expression: $ => $.logical_expression_base,

    // Tokenized relational operators to avoid splitting on '.'
    relop: $ => choice(
      token('.GT.'),
      token('.GE.'),
      token('.LT.'),
      token('.LE.'),
      token('.EQ.'),
      token('.NE.')
    ),

    relational_expression: $ => prec.left('relop', seq(
      field('left', $.arithmetic_expression),
      field('operator', $.relop),
      field('right', $.arithmetic_expression)
    ))
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}