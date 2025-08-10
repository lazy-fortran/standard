/**
 * Fortran 2008 (2008) - Coarrays and Parallel Programming
 * Inheriting from Fortran2003 with coarray and parallel features
 * 
 * KEY F2008 ENHANCEMENTS per ISO/IEC 1539-1:2010:
 * - Coarrays for parallel programming (REAL :: x[*])
 * - Submodules for enhanced module organization
 * - DO CONCURRENT construct for parallelizable loops
 * - BLOCK construct for local scoping
 * - CRITICAL construct and synchronization (SYNC ALL, SYNC IMAGES, SYNC MEMORY)
 * - LOCK and UNLOCK statements for mutual exclusion
 * - Enhanced ALLOCATE with MOLD keyword
 * - NEWUNIT for automatic unit number assignment
 * - Enhanced bit intrinsics and mathematical functions
 * - Implied-shape arrays and contiguous attribute
 */

const fortran2003 = require('../Fortran2003/grammar.js');

function grammar(base, config) {
  if (!config) {
    config = base;
    base = undefined;
  }
  return config;
}

module.exports = grammar(fortran2003, {
  name: 'Fortran2008',

  // Copy base configuration
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
    [$.component_name, $.simple_variable],
    [$.class_name, $.simple_variable],
    [$.binding_name, $.simple_variable],
    [$.procedure_name, $.simple_variable],
    [$.submodule_name, $.simple_variable],
    [$.coarray_name, $.simple_variable]
  ],

  precedences: $ => [
    ['power', 'mult', 'add', 'concat', 'relop', 'not', 'and', 'or', 'defined_op']
  ],

  word: $ => $.simple_variable,

  inline: $ => [
    $.label,
    $.subscript,
    $.letter_range,
    $.component_name,
    $.binding_name
  ],

  rules: {
    // ========================================================================
    // F2008 SUBMODULES - Enhanced module organization
    // ========================================================================

    // Enhanced program unit with submodules
    program_unit: $ => choice(
      $.main_program,
      $.module,
      $.submodule,                        // F2008: Submodules!
      $.external_subprogram
    ),

    // F2008: Submodule definition
    submodule: $ => seq(
      $.submodule_stmt,
      optional($.specification_part),
      optional($.module_subprogram_part),
      $.end_submodule_stmt
    ),

    submodule_stmt: $ => seq(
      'SUBMODULE',
      '(',
      $.parent_identifier,
      ')',
      $.submodule_name
    ),

    parent_identifier: $ => choice(
      $.module_name,
      seq($.ancestor_module_name, ':', $.parent_submodule_name)
    ),

    ancestor_module_name: $ => $.module_name,
    parent_submodule_name: $ => $.submodule_name,
    submodule_name: $ => $.simple_variable,

    end_submodule_stmt: $ => seq(
      'END',
      optional('SUBMODULE'),
      optional($.submodule_name)
    ),

    // ========================================================================
    // F2008 COARRAYS - Parallel programming revolution!
    // ========================================================================

    // Enhanced array specification with coarrays
    array_spec: $ => choice(
      $.explicit_shape_spec_list,
      $.assumed_shape_spec_list,
      $.deferred_shape_spec_list,
      $.assumed_size_spec,
      $.implied_shape_spec_list,          // F2008: Implied shape
      $.assumed_rank_spec                 // F2008: Assumed rank
    ),

    implied_shape_spec_list: $ => sep1('*', ','),
    assumed_rank_spec: $ => '..',

    // F2008: Coarray specification
    coarray_spec: $ => choice(
      $.deferred_coshape_spec_list,
      $.explicit_coshape_spec
    ),

    deferred_coshape_spec_list: $ => sep1(':', ','),
    
    explicit_coshape_spec: $ => seq(
      optional(seq($.explicit_coshape_spec_list, ',')),
      choice(
        $.lower_cobound_expr,
        seq($.lower_cobound_expr, ':', $.upper_cobound_expr),
        '*'
      )
    ),

    explicit_coshape_spec_list: $ => sep1($.explicit_coshape_spec_item, ','),
    explicit_coshape_spec_item: $ => choice(
      $.upper_cobound_expr,
      seq($.lower_cobound_expr, ':', $.upper_cobound_expr)
    ),

    lower_cobound_expr: $ => $.specification_expr,
    upper_cobound_expr: $ => $.specification_expr,

    // Enhanced component attributes with coarrays
    component_attr_spec: $ => choice(
      'POINTER',
      'ALLOCATABLE',
      $.dimension_spec,
      $.codimension_spec,                 // F2008: Codimension
      $.access_spec
    ),

    codimension_spec: $ => seq('CODIMENSION', '[', $.coarray_spec, ']'),

    // Enhanced attribute specifications with coarrays
    attr_spec: $ => choice(
      'PARAMETER',
      'ALLOCATABLE',
      seq('DIMENSION', '(', $.array_spec, ')'),
      seq('CODIMENSION', '[', $.coarray_spec, ']'),  // F2008: Coarray attribute
      'EXTERNAL',
      seq('INTENT', '(', $.intent_spec, ')'),
      'INTRINSIC',
      'OPTIONAL',
      'POINTER',
      'PRIVATE',
      'PUBLIC',
      'SAVE',
      'TARGET',
      'VALUE',
      'VOLATILE',
      seq('BIND', '(', 'C', optional(seq(',', 'NAME', '=', $.scalar_char_constant)), ')'),
      'ABSTRACT',
      'ASYNCHRONOUS',
      'CONTIGUOUS'                        // F2008: Contiguous attribute
    ),

    // Enhanced part reference with coarray support
    part_ref: $ => seq(
      $.part_name,
      optional($.section_subscript_list),
      optional($.image_selector)          // F2008: Enhanced coarray support
    ),

    image_selector: $ => seq(
      '[',
      $.cosubscript_list,
      ']'
    ),

    // ========================================================================
    // F2008 DO CONCURRENT - Parallelizable loops
    // ========================================================================

    // Enhanced DO construct with DO CONCURRENT
    do_stmt: $ => seq(
      'DO',
      optional($.label),
      optional($.loop_control)
    ),

    loop_control: $ => choice(
      seq($.do_variable, '=', $.do_expr, ',', $.do_expr, optional(seq(',', $.do_expr))),
      seq('WHILE', '(', $.logical_expression, ')'),
      $.concurrent_control                // F2008: DO CONCURRENT
    ),

    // F2008: Concurrent loop control
    concurrent_control: $ => seq(
      'CONCURRENT',
      '(',
      $.concurrent_header,
      ')'
    ),

    concurrent_header: $ => seq(
      $.index_name_spec_list,
      optional(seq(',', $.scalar_mask_expr))
    ),

    index_name_spec_list: $ => sep1($.index_name_spec, ','),
    index_name_spec: $ => seq(
      $.index_name,
      '=',
      $.concurrent_limit,
      ':',
      $.concurrent_limit,
      optional(seq(':', $.concurrent_step))
    ),

    concurrent_limit: $ => $.scalar_int_expr,
    concurrent_step: $ => $.scalar_int_expr,
    scalar_mask_expr: $ => $.logical_expression,

    // ========================================================================
    // F2008 BLOCK CONSTRUCT - Local scoping
    // ========================================================================

    // Enhanced executable construct with BLOCK
    executable_construct: $ => choice(
      $.action_stmt,
      $.associate_construct,
      $.block_construct,                  // F2008: BLOCK construct
      $.case_construct,
      $.do_construct,
      $.forall_construct,
      $.if_construct,
      $.where_construct,
      $.select_type_construct
    ),

    // F2008: BLOCK construct for local declarations
    block_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.block_stmt,
      optional($.specification_part),
      $.execution_part,
      $.end_block_stmt
    ),

    block_stmt: $ => 'BLOCK',
    end_block_stmt: $ => seq('END', 'BLOCK', optional($.construct_name)),

    // ========================================================================
    // F2008 ENHANCED ALLOCATION
    // ========================================================================

    // Enhanced allocation with coarrays and more options
    allocate_stmt: $ => seq(
      'ALLOCATE',
      '(',
      optional($.type_spec),
      $.allocation_list,
      optional(seq(',', $.alloc_opt_list)),
      ')'
    ),

    allocation: $ => seq(
      $.allocate_object,
      optional(seq('(', $.allocate_shape_spec_list, ')')),
      optional(seq('[', $.allocate_coshape_spec, ']'))  // F2008: Coarray allocation
    ),

    allocate_coshape_spec: $ => $.explicit_coshape_spec,

    alloc_opt: $ => choice(
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable),
      seq('SOURCE', '=', $.source_expr),
      seq('MOLD', '=', $.mold_expr)       // F2008: MOLD allocation
    ),

    mold_expr: $ => $.expression,

    // ========================================================================
    // F2008 ENHANCED SYNCHRONIZATION
    // ========================================================================

    // F2008: Synchronization statements
    action_stmt: $ => choice(
      $.allocate_stmt,
      $.assignment_stmt,
      $.call_stmt,
      $.continue_stmt,
      $.deallocate_stmt,
      $.exit_stmt,
      $.goto_stmt,
      $.if_stmt,
      $.nullify_stmt,
      $.pointer_assignment_stmt,
      $.return_stmt,
      $.stop_stmt,
      $.cycle_stmt,
      $.wait_stmt,
      $.sync_all_stmt,                    // F2008: SYNC ALL
      $.sync_images_stmt,                 // F2008: SYNC IMAGES
      $.sync_memory_stmt,                 // F2008: SYNC MEMORY
      $.lock_stmt,                        // F2008: LOCK
      $.unlock_stmt                       // F2008: UNLOCK
    ),

    // F2008: Synchronization statements
    sync_all_stmt: $ => seq(
      'SYNC', 'ALL',
      optional(seq('(', optional($.sync_stat_list), ')'))
    ),

    sync_images_stmt: $ => seq(
      'SYNC', 'IMAGES',
      '(',
      $.image_set,
      optional(seq(',', $.sync_stat_list)),
      ')'
    ),

    sync_memory_stmt: $ => seq(
      'SYNC', 'MEMORY',
      optional(seq('(', optional($.sync_stat_list), ')'))
    ),

    image_set: $ => choice(
      $.int_expr_list,
      '*'
    ),

    int_expr_list: $ => sep1($.int_expr, ','),

    sync_stat_list: $ => sep1($.sync_stat, ','),
    sync_stat: $ => choice(
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable)
    ),

    // F2008: LOCK/UNLOCK statements
    lock_stmt: $ => seq(
      'LOCK',
      '(',
      $.lock_variable,
      optional(seq(',', $.lock_stat_list)),
      ')'
    ),

    unlock_stmt: $ => seq(
      'UNLOCK',
      '(',
      $.lock_variable,
      optional(seq(',', $.lock_stat_list)),
      ')'
    ),

    lock_variable: $ => $.variable,

    lock_stat_list: $ => sep1($.lock_stat, ','),
    lock_stat: $ => choice(
      seq('ACQUIRED_LOCK', '=', $.logical_variable),
      seq('STAT', '=', $.stat_variable),
      seq('ERRMSG', '=', $.errmsg_variable)
    ),

    logical_variable: $ => $.variable,

    // ========================================================================
    // F2008 ENHANCED INTRINSIC PROCEDURES
    // ========================================================================

    // F2008 intrinsic procedures
    f2008_intrinsic_name: $ => choice(
      // Coarray intrinsics
      'NUM_IMAGES',
      'THIS_IMAGE',
      'IMAGE_INDEX',
      'LCOBOUND',
      'UCOBOUND',
      // Bit intrinsics
      'LEADZ',
      'TRAILZ',
      'POPCNT',
      'POPPAR',
      'BGE',
      'BGT',
      'BLE',
      'BLT',
      'DSHIFTL',
      'DSHIFTR',
      'MASKL',
      'MASKR',
      'MERGE_BITS',
      'SHIFTA',
      'SHIFTL',
      'SHIFTR',
      'STORAGE_SIZE',
      // Other F2008 intrinsics
      'BESSEL_J0',
      'BESSEL_J1',
      'BESSEL_JN',
      'BESSEL_Y0',
      'BESSEL_Y1',
      'BESSEL_YN',
      'ERF',
      'ERFC',
      'ERFC_SCALED',
      'GAMMA',
      'HYPOT',
      'LOG_GAMMA',
      'NORM2',
      'PARITY',
      'FINDLOC',
      'IS_CONTIGUOUS'
    ),

    // ========================================================================
    // F2008 ENHANCED MODULE PROCEDURES
    // ========================================================================

    // Enhanced interface specification with module procedures
    interface_specification: $ => choice(
      $.interface_body,
      $.procedure_stmt,
      $.module_procedure_stmt             // F2008: MODULE PROCEDURE
    ),

    module_procedure_stmt: $ => seq(
      'MODULE', 'PROCEDURE',
      '::',
      $.procedure_name_list
    ),

    // ========================================================================
    // F2008 ENHANCED I/O WITH NEWUNIT
    // ========================================================================

    // Enhanced I/O control specs
    io_control_spec: $ => choice(
      seq('UNIT', '=', $.io_unit),
      seq('FMT', '=', $.format),
      seq('NML', '=', $.namelist_group_name),
      seq('ADVANCE', '=', $.scalar_char_expr),
      seq('ASYNCHRONOUS', '=', $.scalar_char_expr),
      seq('BLANK', '=', $.scalar_char_expr),
      seq('DECIMAL', '=', $.scalar_char_expr),
      seq('DELIM', '=', $.scalar_char_expr),
      seq('ENCODING', '=', $.scalar_char_expr),   // F2008: Encoding
      seq('END', '=', $.label),
      seq('EOR', '=', $.label),
      seq('ERR', '=', $.label),
      seq('ID', '=', $.int_variable),
      seq('IOMSG', '=', $.iomsg_variable),
      seq('IOSTAT', '=', $.scalar_int_variable),
      seq('NEWUNIT', '=', $.scalar_int_variable), // F2008: NEWUNIT
      seq('PAD', '=', $.scalar_char_expr),
      seq('POS', '=', $.scalar_int_expr),
      seq('REC', '=', $.scalar_int_expr),
      seq('ROUND', '=', $.scalar_char_expr),
      seq('SIGN', '=', $.scalar_char_expr),
      seq('SIZE', '=', $.int_variable),
      seq('STREAM', '=', $.scalar_char_expr)
    ),

    // ========================================================================
    // F2008 CRITICAL CONSTRUCT
    // ========================================================================

    // Enhanced executable construct with CRITICAL
    executable_construct: $ => choice(
      $.action_stmt,
      $.associate_construct,
      $.block_construct,
      $.case_construct,
      $.critical_construct,               // F2008: CRITICAL construct
      $.do_construct,
      $.forall_construct,
      $.if_construct,
      $.where_construct,
      $.select_type_construct
    ),

    // F2008: CRITICAL construct for thread safety
    critical_construct: $ => seq(
      optional(seq($.construct_name, ':')),
      $.critical_stmt,
      $.block,
      $.end_critical_stmt
    ),

    critical_stmt: $ => 'CRITICAL',
    end_critical_stmt: $ => seq('END', 'CRITICAL', optional($.construct_name)),
    block: $ => repeat($.execution_part_construct)
  }
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}