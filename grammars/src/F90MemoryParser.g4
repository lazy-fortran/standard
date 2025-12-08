// Fortran 90 Dynamic Memory Management
// Reference: ISO/IEC 1539:1991 Section 6.3 (Dynamic allocation)
// Delegate grammar for ALLOCATE, DEALLOCATE, and NULLIFY
// Extracted from Fortran90Parser.g4 per issue #252
parser grammar F90MemoryParser;

// ====================================================================
// DYNAMIC MEMORY MANAGEMENT - ISO/IEC 1539:1991 Section 6.3
// ====================================================================
//
// ISO/IEC 1539:1991 Section 6.3 defines dynamic memory management:
// - R620 (allocate-stmt) -> ALLOCATE (allocation-list [, STAT=stat-variable])
// - R624 (nullify-stmt) -> NULLIFY (pointer-object-list)
// - R626 (deallocate-stmt) -> DEALLOCATE (allocate-object-list [, STAT=stat-variable])
//
// Dynamic allocation and deallocation of arrays and pointers.

// ALLOCATE statement (F90 dynamic memory allocation)
// ISO/IEC 1539:1991 Section 6.3.1
allocate_stmt
    : ALLOCATE LPAREN allocation_list (COMMA stat_variable)? RPAREN
    ;

allocation_list
    : allocation (COMMA allocation)*
    ;

allocation
    : allocate_object (LPAREN allocate_shape_spec_list RPAREN)?
    ;

allocate_object
    : variable_f90                  // ALLOCATABLE variable or POINTER
    ;

allocate_shape_spec_list
    : allocate_shape_spec (COMMA allocate_shape_spec)*
    ;

allocate_shape_spec
    : expr_f90 (COLON expr_f90)?    // Allocation bounds
    ;

// DEALLOCATE statement (F90 dynamic memory deallocation)
// ISO/IEC 1539:1991 Section 6.3.3
deallocate_stmt
    : DEALLOCATE LPAREN deallocate_list (COMMA stat_variable)? RPAREN
    ;

deallocate_list
    : allocate_object (COMMA allocate_object)*
    ;

// NULLIFY statement (F90 pointer nullification)
// ISO/IEC 1539:1991 Section 6.3.2
nullify_stmt
    : NULLIFY LPAREN pointer_object_list RPAREN
    ;

pointer_object_list
    : pointer_object (COMMA pointer_object)*
    ;

pointer_object
    : variable_f90                  // POINTER variable
    ;

// Status variable for allocation/deallocation
stat_variable
    : STAT EQUALS variable_f90
    ;
