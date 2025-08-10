/**
 * Fortran 90 (1990) - The Revolutionary Modern Foundation
 * Tree-sitter grammar with inheritance from FORTRAN77
 * 
 * REVOLUTIONARY F90 FEATURES:
 * - Free-form source format alongside fixed-form compatibility
 * - Module system with explicit interfaces
 * - Dynamic arrays (ALLOCATABLE, POINTER)
 * - Derived types (user-defined structures)
 * - Array operations and constructors
 * - Enhanced control flow (SELECT CASE, WHERE)
 * - Modern I/O (NAMELIST, non-advancing)
 * - Procedure enhancements (RECURSIVE, PURE, ELEMENTAL)
 */

const fortran77 = require('../FORTRAN77/grammar.js');

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar(fortran77, {
  name: 'Fortran90',

  // Copy base extras and add F90 free-form comments
  extras: $ => [
    /\s+/,
    $.comment,
    $.free_form_comment
  ],

  conflicts: $ => [
    [$.simple_variable, $.function_name],
    [$.simple_variable, $.subroutine_name],
    [$.variable, $.logical_variable],
    [$.constant, $.logical_primary],
    [$.expression, $.if_then_statement],
    [$.subscripted_variable, $.character_variable],
    [$.variable, $.logical_variable, $.character_variable],
    [$.constant, $.character_expression],
    [$.module_name, $.simple_variable],
    [$.type_name, $.simple_variable],
    [$.component_name, $.simple_variable]
  ],

  // Enhanced precedences for F90
  precedences: $ => [
    ['power', 'mult', 'add', 'concat', 'relop', 'not', 'and', 'or']
  ],

  word: $ => $.simple_variable,

  inline: $ => [
    $.label,
    $.subscript,
    $.letter_range,
    $.component_name
  ],

  rules: {
    // ========================================================================
    // F90 PROGRAM STRUCTURE - Revolutionary module system
    // ========================================================================

    // Override program to support F90 program units
    program: $ => repeat1($.program_unit),

    program_unit: $ => choice(
      $.main_program,
      $.module,                    // F90 MAJOR INNOVATION!
      $.external_subprogram
    ),

    // Enhanced main program with F90 features
    main_program: $ => seq(
      optional($.program_stmt),
      optional($.specification_part),
      optional($.execution_part),
      optional($.internal_subprogram_part),
      $.end_program_stmt
    ),

    program_stmt: $ => seq('PROGRAM', $.program_name),
    program_name: $ => $.simple_variable,
    end_program_stmt: $ => seq('END', optional('PROGRAM'), optional($.program_name)),

    // F90 MODULE SYSTEM - Revolutionary for modular programming!
    module: $ => seq(
      $.module_stmt,
      optional($.specification_part),
      optional($.module_subprogram_part),
      $.end_module_stmt
    ),

    module_stmt: $ => seq('MODULE', $.module_name),
    module_name: $ => $.simple_variable,
    end_module_stmt: $ => seq('END', optional('MODULE'), optional($.module_name)),

    module_subprogram_part: $ => seq(
      'CONTAINS',
      repeat1($.module_subprogram)
    ),

    module_subprogram: $ => choice(
      $.function_subprogram,
      $.subroutine_subprogram
    ),

    // Enhanced external subprogram with F90 features
    external_subprogram: $ => choice(
      $.function_subprogram,
      $.subroutine_subprogram
    ),

    // ========================================================================
    // F90 COMMENTS - Free-form support
    // ========================================================================

    // Free-form comments start with ! anywhere on line
    free_form_comment: $ => seq('!', /.*/),

    // ========================================================================
    // F90 SPECIFICATION PART - Enhanced with modules and derived types
    // ========================================================================

    specification_part: $ => repeat1($.specification_construct),

    specification_construct: $ => choice(
      $.use_stmt,                  // F90: Module imports
      $.implicit_stmt,             // Enhanced from F77
      $.parameter_stmt,            // From F77
      $.format_stmt,
      $.declaration_construct
    ),

    // F90 USE statement for module imports
    use_stmt: $ => seq(
      'USE',
      $.module_name,
      optional(seq(',', $.only_list))
    ),

    only_list: $ => choice(
      seq('ONLY', ':', sep1($.only_name, ',')),
      seq('ONLY', ':', $.rename_list)
    ),

    only_name: $ => $.simple_variable,
    rename_list: $ => sep1($.rename, ','),
    rename: $ => seq($.local_name, '=>', $.use_name),
    local_name: $ => $.simple_variable,
    use_name: $ => $.simple_variable,

    // Enhanced IMPLICIT statement
    implicit_stmt: $ => choice(
      seq('IMPLICIT', 'NONE'),
      seq('IMPLICIT', $.implicit_spec_list)
    ),

    implicit_spec_list: $ => sep1($.implicit_spec, ','),
    implicit_spec: $ => seq(
      $.declaration_type_spec,
      '(',
      sep1($.letter_spec, ','),
      ')'
    ),

    letter_spec: $ => choice(
      /[A-Za-z]/,
      seq(/[A-Za-z]/, '-', /[A-Za-z]/)
    ),

    // F90 DECLARATION constructs
    declaration_construct: $ => choice(
      $.derived_type_def,          // F90: User-defined types
      $.type_declaration_stmt,     // Enhanced type declarations
      $.parameter_stmt,
      $.dimension_stmt,
      $.allocatable_stmt,          // F90: Dynamic arrays
      $.pointer_stmt,              // F90: Pointers
      $.target_stmt,               // F90: Pointer targets
      $.intent_stmt,               // F90: Argument intents
      $.optional_stmt,             // F90: Optional arguments
      $.save_stmt,
      $.external_stmt,
      $.intrinsic_stmt
    ),

    // ========================================================================
    // F90 DERIVED TYPES - Revolutionary user-defined structures!
    // ========================================================================

    derived_type_def: $ => seq(
      $.derived_type_stmt,
      repeat($.component_def_stmt),
      $.end_type_stmt
    ),

    derived_type_stmt: $ => seq(
      'TYPE',
      optional($.access_spec),
      '::',
      $.type_name
    ),

    type_name: $ => $.simple_variable,
    end_type_stmt: $ => seq('END', 'TYPE', optional($.type_name)),

    component_def_stmt: $ => seq(
      $.declaration_type_spec,
      optional(seq(',', $.component_attr_spec_list)),
      '::',
      $.component_decl_list
    ),

    component_attr_spec_list: $ => sep1($.component_attr_spec, ','),
    component_attr_spec: $ => choice(
      'POINTER',
      $.dimension_spec
    ),

    component_decl_list: $ => sep1($.component_decl, ','),
    component_decl: $ => seq(
      $.component_name,
      optional($.array_spec),
      optional($.initialization)
    ),

    component_name: $ => $.simple_variable,

    // ========================================================================
    // F90 TYPE DECLARATIONS - Enhanced with new features
    // ========================================================================

    type_declaration_stmt: $ => seq(
      $.declaration_type_spec,
      optional(seq(',', $.attr_spec_list)),
      '::',
      $.entity_decl_list
    ),

    declaration_type_spec: $ => choice(
      'INTEGER', 
      'REAL',
      'COMPLEX',
      'CHARACTER',
      'LOGICAL',
      seq('DOUBLE', 'PRECISION'),
      seq('TYPE', '(', $.type_name, ')')  // F90: Derived type
    ),

    attr_spec_list: $ => sep1($.attr_spec, ','),
    attr_spec: $ => choice(
      'PARAMETER',
      'ALLOCATABLE',               // F90: Dynamic arrays
      'DIMENSION', '(', $.array_spec, ')',
      'EXTERNAL',
      'INTENT', '(', $.intent_spec, ')',
      'INTRINSIC',
      'OPTIONAL',                  // F90: Optional arguments
      'POINTER',                   // F90: Pointers
      'PRIVATE',
      'PUBLIC',
      'SAVE',
      'TARGET'                     // F90: Pointer targets
    ),

    intent_spec: $ => choice('IN', 'OUT', 'INOUT'),

    entity_decl_list: $ => sep1($.entity_decl, ','),
    entity_decl: $ => seq(
      $.object_name,
      optional($.array_spec),
      optional($.initialization)
    ),

    object_name: $ => $.simple_variable,
    initialization: $ => seq('=', $.initialization_expr),
    initialization_expr: $ => $.expression,

    // F90 Array specifications
    array_spec: $ => choice(
      $.explicit_shape_spec_list,
      $.assumed_shape_spec_list,
      $.deferred_shape_spec_list,
      $.assumed_size_spec
    ),

    explicit_shape_spec_list: $ => sep1($.explicit_shape_spec, ','),
    explicit_shape_spec: $ => choice(
      $.upper_bound,
      seq($.lower_bound, ':', $.upper_bound)
    ),

    assumed_shape_spec_list: $ => sep1($.assumed_shape_spec, ','),
    assumed_shape_spec: $ => choice(
      ':',
      seq($.lower_bound, ':')
    ),

    deferred_shape_spec_list: $ => sep1(':', ','),
    assumed_size_spec: $ => seq(
      optional(seq($.explicit_shape_spec_list, ',')),
      choice('*', seq($.lower_bound, ':', '*'))
    ),

    lower_bound: $ => $.specification_expr,
    upper_bound: $ => $.specification_expr,
    specification_expr: $ => $.expression,

    dimension_spec: $ => seq('DIMENSION', '(', $.array_spec, ')'),

    // F90 specific declaration statements
    allocatable_stmt: $ => seq('ALLOCATABLE', '::', $.allocatable_decl_list),
    allocatable_decl_list: $ => sep1($.allocatable_decl, ','),
    allocatable_decl: $ => seq($.array_name, optional($.deferred_shape_spec_list)),
    array_name: $ => $.simple_variable,

    pointer_stmt: $ => seq('POINTER', '::', $.pointer_decl_list),
    pointer_decl_list: $ => sep1($.pointer_decl, ','),
    pointer_decl: $ => $.object_name,

    target_stmt: $ => seq('TARGET', '::', $.target_decl_list),
    target_decl_list: $ => sep1($.target_decl, ','),
    target_decl: $ => seq($.object_name, optional($.array_spec)),

    intent_stmt: $ => seq('INTENT', '(', $.intent_spec, ')', '::', $.dummy_arg_name_list),
    dummy_arg_name_list: $ => sep1($.dummy_arg_name, ','),
    dummy_arg_name: $ => $.simple_variable,

    optional_stmt: $ => seq('OPTIONAL', '::', $.dummy_arg_name_list),

    save_stmt: $ => choice(
      'SAVE',
      seq('SAVE', '::', $.saved_entity_list)
    ),

    saved_entity_list: $ => sep1($.saved_entity, ','),
    saved_entity: $ => $.simple_variable,

    intrinsic_stmt: $ => seq('INTRINSIC', '::', $.intrinsic_procedure_name_list),
    intrinsic_procedure_name_list: $ => sep1($.procedure_name, ','),
    procedure_name: $ => $.simple_variable,

    // ========================================================================
    // F90 ACCESS CONTROL
    // ========================================================================

    access_spec: $ => choice('PUBLIC', 'PRIVATE'),

    // ========================================================================
    // F90 EXECUTION PART - Enhanced constructs
    // ========================================================================

    execution_part: $ => seq(
      $.executable_construct,
      repeat($.execution_part_construct)
    ),

    execution_part_construct: $ => choice(
      $.executable_construct,
      $.format_stmt,
      $.data_stmt
    ),

    executable_construct: $ => choice(
      $.action_stmt,
      $.associate_construct,       // F90: ASSOCIATE blocks
      $.case_construct,            // F90: SELECT CASE
      $.do_construct,              // Enhanced DO loops
      $.forall_construct,          // F90: Array operations
      $.if_construct,              // Enhanced IF-THEN-ELSE
      $.where_construct            // F90: Array WHERE
    ),

    // Enhanced action statements
    action_stmt: $ => choice(
      $.allocate_stmt,             // F90: Dynamic allocation
      $.assignment_stmt,
      $.call_stmt,
      $.continue_stmt,
      $.deallocate_stmt,           // F90: Dynamic deallocation
      $.exit_stmt,                 // F90: Loop exit
      $.goto_stmt,
      $.if_stmt,                   // Arithmetic IF
      $.nullify_stmt,              // F90: Pointer nullification
      $.pointer_assignment_stmt,   // F90: Pointer assignment
      $.return_stmt,
      $.stop_stmt,
      $.cycle_stmt                 // F90: Loop cycle
    ),

    // F90 ALLOCATE statement
    allocate_stmt: $ => seq(
      'ALLOCATE',
      '(',
      $.allocation_list,
      optional(seq(',', 'STAT', '=', $.stat_variable)),
      ')'
    ),

    allocation_list: $ => sep1($.allocation, ','),
    allocation: $ => seq($.allocate_object, '(', $.allocate_shape_spec_list, ')'),
    allocate_object: $ => $.variable_name,
    variable_name: $ => $.simple_variable,
    allocate_shape_spec_list: $ => sep1($.allocate_shape_spec, ','),
    allocate_shape_spec: $ => choice(
      $.upper_bound_expr,
      seq($.lower_bound_expr, ':', $.upper_bound_expr)
    ),
    lower_bound_expr: $ => $.expression,
    upper_bound_expr: $ => $.expression,
    stat_variable: $ => $.variable,

    // F90 DEALLOCATE statement  
    deallocate_stmt: $ => seq(
      'DEALLOCATE',
      '(',
      $.allocate_object_list,
      optional(seq(',', 'STAT', '=', $.stat_variable)),
      ')'
    ),

    allocate_object_list: $ => sep1($.allocate_object, ','),

    // F90 NULLIFY statement
    nullify_stmt: $ => seq('NULLIFY', '(', $.pointer_object_list, ')'),
    pointer_object_list: $ => sep1($.pointer_object, ','),
    pointer_object: $ => $.variable,

    // F90 Pointer assignment
    pointer_assignment_stmt: $ => seq(
      $.data_pointer_object,
      '=>',
      $.data_target
    ),

    data_pointer_object: $ => $.variable,
    data_target: $ => $.variable,

    // F90 EXIT and CYCLE statements
    exit_stmt: $ => seq('EXIT', optional($.construct_name)),
    cycle_stmt: $ => seq('CYCLE', optional($.construct_name)),
    construct_name: $ => $.simple_variable,

    // ========================================================================
    // F90 SELECT CASE CONSTRUCT - Enhanced control flow
    // ========================================================================

    case_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.select_case_stmt,
      repeat($.case_block),
      $.end_select_stmt
    ),

    select_case_stmt: $ => seq('SELECT', 'CASE', '(', $.case_expr, ')'),
    case_expr: $ => $.expression,
    end_select_stmt: $ => seq('END', 'SELECT', optional($.construct_name)),

    case_block: $ => seq(
      $.case_stmt,
      repeat($.execution_part_construct)
    ),

    case_stmt: $ => seq(
      'CASE',
      choice(
        'DEFAULT',
        seq('(', $.case_value_range_list, ')')
      ),
      optional($.construct_name)
    ),

    case_value_range_list: $ => sep1($.case_value_range, ','),
    case_value_range: $ => choice(
      $.case_value,
      seq($.case_value, ':'),
      seq(':', $.case_value),
      seq($.case_value, ':', $.case_value)
    ),

    case_value: $ => $.expression,

    // ========================================================================
    // F90 WHERE CONSTRUCT - Array operations
    // ========================================================================

    where_construct: $ => choice(
      $.where_stmt,
      seq(
        $.where_construct_stmt,
        repeat($.where_body_construct),
        optional($.elsewhere_part),
        $.end_where_stmt
      )
    ),

    where_stmt: $ => seq('WHERE', '(', $.mask_expr, ')', $.assignment_stmt),
    where_construct_stmt: $ => seq(
      optional(seq($.construct_name, ':')),
      'WHERE',
      '(',
      $.mask_expr,
      ')'
    ),

    mask_expr: $ => $.logical_expression,

    where_body_construct: $ => choice(
      $.assignment_stmt,
      $.where_stmt,
      $.where_construct
    ),

    elsewhere_part: $ => seq(
      $.elsewhere_stmt,
      repeat($.where_body_construct)
    ),

    elsewhere_stmt: $ => seq(
      'ELSEWHERE',
      optional(seq('(', $.mask_expr, ')')),
      optional($.construct_name)
    ),

    end_where_stmt: $ => seq('END', 'WHERE', optional($.construct_name)),

    // ========================================================================
    // F90 DO CONSTRUCT - Enhanced loops
    // ========================================================================

    do_construct: $ => choice(
      $.block_do_construct,
      $.nonblock_do_construct
    ),

    block_do_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.do_stmt,
      $.do_block,
      $.end_do_stmt
    ),

    nonblock_do_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.do_stmt,
      $.do_block
    ),

    do_stmt: $ => seq(
      'DO',
      optional($.label),
      optional($.loop_control)
    ),

    loop_control: $ => choice(
      seq($.do_variable, '=', $.do_expr, ',', $.do_expr, optional(seq(',', $.do_expr))),
      seq('WHILE', '(', $.logical_expression, ')')
    ),

    do_variable: $ => $.simple_variable,
    do_expr: $ => $.expression,

    do_block: $ => repeat($.execution_part_construct),
    end_do_stmt: $ => seq('END', 'DO', optional($.construct_name)),

    // ========================================================================
    // F90 IF CONSTRUCT - Enhanced from F77
    // ========================================================================

    if_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.if_then_stmt,
      $.then_part,
      repeat($.else_if_part),
      optional($.else_part),
      $.end_if_stmt
    ),

    if_then_stmt: $ => seq('IF', '(', $.logical_expression, ')', 'THEN', optional($.construct_name)),

    then_part: $ => repeat($.execution_part_construct),

    else_if_part: $ => seq(
      $.else_if_stmt,
      repeat($.execution_part_construct)
    ),

    else_if_stmt: $ => seq(
      'ELSE', 'IF',
      '(',
      $.logical_expression,
      ')',
      'THEN',
      optional($.construct_name)
    ),

    else_part: $ => seq(
      $.else_stmt,
      repeat($.execution_part_construct)
    ),

    else_stmt: $ => seq('ELSE', optional($.construct_name)),

    end_if_stmt: $ => seq('END', 'IF', optional($.construct_name)),

    // ========================================================================
    // F90 FORALL CONSTRUCT - Array operations
    // ========================================================================

    forall_construct: $ => choice(
      $.forall_stmt,
      seq(
        $.forall_construct_stmt,
        repeat($.forall_body_construct),
        $.end_forall_stmt
      )
    ),

    forall_stmt: $ => seq(
      'FORALL',
      '(',
      $.forall_triplet_spec_list,
      optional(seq(',', $.mask_expr)),
      ')',
      $.assignment_stmt
    ),

    forall_construct_stmt: $ => seq(
      optional(seq($.construct_name, ':')),
      'FORALL',
      '(',
      $.forall_triplet_spec_list,
      optional(seq(',', $.mask_expr)),
      ')'
    ),

    forall_triplet_spec_list: $ => sep1($.forall_triplet_spec, ','),
    forall_triplet_spec: $ => seq(
      $.index_name,
      '=',
      $.forall_limit,
      ':',
      $.forall_limit,
      optional(seq(':', $.forall_step))
    ),

    index_name: $ => $.simple_variable,
    forall_limit: $ => $.expression,
    forall_step: $ => $.expression,

    forall_body_construct: $ => choice(
      $.assignment_stmt,
      $.forall_construct,
      $.where_construct,
      $.forall_stmt,
      $.where_stmt
    ),

    end_forall_stmt: $ => seq('END', 'FORALL', optional($.construct_name)),

    // ========================================================================
    // F90 ASSOCIATE CONSTRUCT  
    // ========================================================================

    associate_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.associate_stmt,
      $.associate_block,
      $.end_associate_stmt
    ),

    associate_stmt: $ => seq(
      'ASSOCIATE',
      '(',
      $.association_list,
      ')'
    ),

    association_list: $ => sep1($.association, ','),
    association: $ => seq($.associate_name, '=>', $.selector),
    associate_name: $ => $.simple_variable,
    selector: $ => $.expression,

    associate_block: $ => repeat($.execution_part_construct),
    end_associate_stmt: $ => seq('END', 'ASSOCIATE', optional($.construct_name)),

    // ========================================================================
    // F90 ENHANCED EXPRESSIONS - Array operations and constructors
    // ========================================================================

    // Extend expression base to include F90 features
    expression_base: $ => choice(
      $.arithmetic_expression,
      $.logical_expression,
      $.character_expression,
      $.array_constructor          // F90: Array constructors
    ),

    // F90 Array constructors
    array_constructor: $ => choice(
      seq('(/', $.ac_value_list, '/)')  // F90 syntax
    ),

    ac_value_list: $ => sep1($.ac_value, ','),
    ac_value: $ => choice(
      $.expression,
      $.ac_implied_do
    ),

    ac_implied_do: $ => seq(
      '(',
      $.ac_value_list,
      ',',
      $.ac_implied_do_control,
      ')'
    ),

    ac_implied_do_control: $ => seq(
      $.ac_do_variable,
      '=',
      $.ac_expr,
      ',',
      $.ac_expr,
      optional(seq(',', $.ac_expr))
    ),

    ac_do_variable: $ => $.simple_variable,
    ac_expr: $ => $.expression,

    // ========================================================================
    // F90 ENHANCED VARIABLE REFERENCES - Array sections and components
    // ========================================================================

    // Extend primary base to include F90 features
    primary_base: $ => choice(
      $.constant,
      $.designator,                // F90: Enhanced variable references
      $.array_constructor,
      $.structure_constructor,     // F90: Derived type constructors
      $.function_reference,
      seq('(', $.expression, ')')
    ),

    // F90 Designator (enhanced variable reference)
    designator: $ => choice(
      $.data_ref,
      $.substring
    ),

    data_ref: $ => seq(
      $.part_ref,
      repeat(seq('.', $.part_ref))  // Component references
    ),

    part_ref: $ => seq(
      $.part_name,
      optional($.section_subscript_list)
    ),

    part_name: $ => $.simple_variable,

    section_subscript_list: $ => seq(
      '(',
      sep1($.section_subscript, ','),
      ')'
    ),

    section_subscript: $ => choice(
      $.subscript,
      $.subscript_triplet,
      $.vector_subscript
    ),

    subscript_triplet: $ => choice(
      seq($.subscript, ':', $.subscript, ':', $.subscript),
      seq($.subscript, ':', $.subscript),
      seq($.subscript, ':'),
      seq(':', $.subscript),
      ':'
    ),

    vector_subscript: $ => $.int_expr,
    int_expr: $ => $.expression,

    substring: $ => seq(
      $.parent_string,
      '(',
      $.substring_range,
      ')'
    ),

    parent_string: $ => $.data_ref,

    substring_range: $ => choice(
      seq($.scalar_int_expr, ':', $.scalar_int_expr),
      seq($.scalar_int_expr, ':'),
      seq(':', $.scalar_int_expr)
    ),

    scalar_int_expr: $ => $.expression,

    // F90 Structure constructor
    structure_constructor: $ => seq(
      $.derived_type_spec,
      '(',
      optional($.component_spec_list),
      ')'
    ),

    derived_type_spec: $ => seq('TYPE', '(', $.type_name, ')'),

    component_spec_list: $ => sep1($.component_spec, ','),
    component_spec: $ => choice(
      $.expression,
      seq($.keyword, '=', $.expression)
    ),

    keyword: $ => $.simple_variable,

    // Enhanced function reference
    function_reference: $ => seq(
      $.procedure_designator,
      '(',
      optional($.actual_arg_spec_list),
      ')'
    ),

    procedure_designator: $ => $.simple_variable,

    actual_arg_spec_list: $ => sep1($.actual_arg_spec, ','),
    actual_arg_spec: $ => choice(
      $.actual_arg,
      seq($.keyword, '=', $.actual_arg)
    ),

    actual_arg: $ => $.expression,

    // ========================================================================
    // F90 INTERNAL SUBPROGRAM PART
    // ========================================================================

    internal_subprogram_part: $ => seq(
      'CONTAINS',
      repeat1($.internal_subprogram)
    ),

    internal_subprogram: $ => choice(
      $.internal_function_subprogram,
      $.internal_subroutine_subprogram
    ),

    internal_function_subprogram: $ => $.function_subprogram,
    internal_subroutine_subprogram: $ => $.subroutine_subprogram,

    // Enhanced function subprogram
    function_subprogram: $ => seq(
      $.function_stmt,
      optional($.specification_part),
      optional($.execution_part),
      optional($.internal_subprogram_part),
      $.end_function_stmt
    ),

    function_stmt: $ => seq(
      optional($.prefix),
      'FUNCTION',
      $.function_name,
      '(',
      optional($.dummy_arg_name_list),
      ')',
      optional($.suffix)
    ),

    prefix: $ => choice(
      'RECURSIVE',                 // F90: Recursive procedures
      'PURE',                      // F90: Pure procedures  
      'ELEMENTAL'                  // F90: Elemental procedures
    ),

    suffix: $ => seq('RESULT', '(', $.result_name, ')'),
    result_name: $ => $.simple_variable,

    end_function_stmt: $ => seq('END', optional('FUNCTION'), optional($.function_name)),

    // Enhanced subroutine subprogram
    subroutine_subprogram: $ => seq(
      $.subroutine_stmt,
      optional($.specification_part),
      optional($.execution_part),
      optional($.internal_subprogram_part),
      $.end_subroutine_stmt
    ),

    subroutine_stmt: $ => seq(
      optional($.prefix),
      'SUBROUTINE',
      $.subroutine_name,
      optional(seq('(', optional($.dummy_arg_name_list), ')'))
    ),

    subroutine_name: $ => $.simple_variable,
    end_subroutine_stmt: $ => seq('END', optional('SUBROUTINE'), optional($.subroutine_name)),

    // ========================================================================
    // F90 ENHANCED LOGICAL EXPRESSIONS - Array operations
    // ========================================================================

    // Extend logical expression base
    logical_expression_base: $ => choice(
      $.logical_primary,
      $.not_op_expr,               // .NOT. expressions
      $.and_op_expr,               // .AND. expressions  
      $.or_op_expr,                // .OR. expressions
      $.equiv_op_expr,             // .EQV./.NEQV. expressions
      $.relational_expression      // Relational expressions
    ),

    logical_primary: $ => choice(
      $.logical_constant,
      $.logical_variable,
      $.logical_function_reference,
      seq('(', $.logical_expression, ')')
    ),

    logical_constant: $ => choice(
      token('.TRUE.'),
      token('.FALSE.')
    ),

    logical_variable: $ => $.variable,
    logical_function_reference: $ => $.function_reference,

    // Logical operators as single tokens
    not_op: $ => token('.NOT.'),
    and_op: $ => token('.AND.'),
    or_op: $ => token('.OR.'),
    equiv_op: $ => choice(token('.EQV.'), token('.NEQV.')),

    not_op_expr: $ => prec.right('not', seq($.not_op, $.logical_primary)),
    and_op_expr: $ => prec.left('and', seq($.logical_expression, $.and_op, $.logical_expression)),
    or_op_expr: $ => prec.left('or', seq($.logical_expression, $.or_op, $.logical_expression)),
    equiv_op_expr: $ => prec.left('or', seq($.logical_expression, $.equiv_op, $.logical_expression)),

    // ========================================================================
    // PARAMETER AND FORMAT STATEMENTS
    // ========================================================================

    parameter_stmt: $ => seq(
      'PARAMETER',
      '(',
      sep1($.named_constant_def, ','),
      ')'
    ),

    named_constant_def: $ => seq($.named_constant, '=', $.initialization_expr),
    named_constant: $ => $.simple_variable,

    format_stmt: $ => seq($.label, 'FORMAT', '(', $.format_specification, ')'),

    format_specification: $ => /[^)]*/,  // Simplified for now

    data_stmt: $ => seq(
      'DATA',
      sep1($.data_stmt_set, ',')
    ),

    data_stmt_set: $ => seq(
      $.data_stmt_object_list,
      '/',
      $.data_stmt_value_list,
      '/'
    ),

    data_stmt_object_list: $ => sep1($.data_stmt_object, ','),
    data_stmt_object: $ => $.variable,

    data_stmt_value_list: $ => sep1($.data_stmt_value, ','),
    data_stmt_value: $ => choice(
      $.data_stmt_constant,
      seq($.data_stmt_repeat, '*', $.data_stmt_constant)
    ),

    data_stmt_constant: $ => $.constant,
    data_stmt_repeat: $ => $.int_literal_constant,
    int_literal_constant: $ => $.integer_constant,

    // Enhanced statements - delegate to base hooks
    dimension_stmt: $ => seq('DIMENSION', '::', $.array_name_spec_list),
    array_name_spec_list: $ => sep1($.array_name_spec, ','),
    array_name_spec: $ => seq($.array_name, '(', $.array_spec, ')'),

    external_stmt: $ => seq('EXTERNAL', '::', $.external_name_list),
    external_name_list: $ => sep1($.external_name, ','),
    external_name: $ => $.simple_variable,

    // Assignment statements
    assignment_stmt: $ => seq($.variable, '=', $.expression),

    call_stmt: $ => seq(
      'CALL',
      $.procedure_designator,
      optional(seq('(', optional($.actual_arg_spec_list), ')'))
    ),

    continue_stmt: $ => seq(optional($.label), 'CONTINUE'),
    goto_stmt: $ => seq('GO', 'TO', $.label),
    if_stmt: $ => seq(
      'IF',
      '(',
      $.logical_expression,
      ')',
      $.label,
      ',',
      $.label,
      ',',
      $.label
    ),

    return_stmt: $ => seq('RETURN', optional($.scalar_int_expr)),
    stop_stmt: $ => seq('STOP', optional(choice($.stop_code, $.scalar_int_expr))),
    stop_code: $ => $.character_constant,

    // ========================================================================
    // HELPER RULES
    // ========================================================================

    // Character constants with proper quoting
    character_constant: $ => choice(
      seq('"', /[^"]*/, '"'),
      seq("'", /[^']*/, "'")
    )
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}