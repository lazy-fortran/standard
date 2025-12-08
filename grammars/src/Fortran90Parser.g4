// Fortran 90 (1990) Parser - Unified Fixed/Free Form Support
// Revolutionary Modern Foundation with Complete Format Compatibility
// Refactored per issue #252 into smaller delegate modules
parser grammar Fortran90Parser;

import FORTRAN77Parser,  // FORTRAN 77 (1977) - structured programming foundation
       F90ModulesParser, // Module system and interfaces
       F90TypesParser,   // Type declarations and derived types
       F90ControlParser, // Control structures (SELECT CASE, WHERE, DO, IF)
       F90IOParser,      // I/O statements and NAMELIST
       F90ExprsParser,   // Expressions and array operations
       F90MemoryParser,  // Dynamic memory management
       F90ProcsParser,   // Procedures and subprograms
       F90InheritedParser;  // Inherited F77 compatibility rules

options {
    tokenVocab = Fortran90Lexer;
}

// ====================================================================
// FORTRAN 90 UNIFIED PARSER OVERVIEW
// ====================================================================
//
// Fortran 90 (ISO 1539:1991) represents the most significant revolution
// in Fortran history, introducing free-form source format while maintaining
// complete backward compatibility with fixed-form.
//
// This parser handles BOTH formats in a single grammar:
// - Fixed-form: .f, .for files (F77 compatibility)
// - Free-form: .f90+ files (modern syntax)
//
// REVOLUTIONARY F90 FEATURES (in delegate grammars):
// - Module system with explicit interfaces (F90ModulesParser)
// - Dynamic memory management (F90MemoryParser)
// - Derived types and enhanced type system (F90TypesParser)
// - Array operations and constructors (F90ExprsParser)
// - Enhanced control flow (F90ControlParser)
// - Enhanced I/O with NAMELIST (F90IOParser)
// - RECURSIVE procedures (F90ProcsParser)
//
// NOTE: PURE and ELEMENTAL are Fortran 95 features (ISO/IEC 1539-1:1997),
// NOT Fortran 90 features. They are properly defined in Fortran95Parser.g4.
// F90 prefix_spec only supports RECURSIVE per ISO/IEC 1539:1991 Section 12.5.2.
//
// INHERITANCE ARCHITECTURE (IN THIS REPO):
// FORTRAN / FORTRANII / FORTRAN66 / FORTRAN77
//   -> Fortran90Parser (+ delegate grammars)
//   -> Fortran95Parser
//   -> F2003+ standards
//
// ====================================================================

// ====================================================================
// FORTRAN 90 PROGRAM STRUCTURE (ENHANCED)
// ====================================================================
// ISO/IEC 1539:1991 Section 2.1
// Program units: main program, external subprogram, module, block data.

// F90 program unit (major enhancement - adds modules)
program_unit_f90
    : NEWLINE* (main_program | module | external_subprogram) NEWLINE*
    ;

// Main program structure (enhanced with F90 features)
// ISO/IEC 1539:1991 Section 11.1
main_program
    : program_stmt specification_part? execution_part?
      internal_subprogram_part? end_program_stmt
    ;

program_stmt
    : PROGRAM IDENTIFIER NEWLINE*
    ;

end_program_stmt
    : END (PROGRAM (IDENTIFIER)?)? NEWLINE*
    ;

// ====================================================================
// SPECIFICATION PART (F90 ENHANCED)
// ====================================================================
// ISO/IEC 1539:1991 Section 5
// Declares variables, types, interfaces, and attributes.

// Specification part (F90 enhancements)
specification_part
    : (NEWLINE* (use_stmt | import_stmt))*
        (NEWLINE* implicit_stmt_f90)?
        (NEWLINE* declaration_construct)* NEWLINE*
    ;

// Declaration construct (F90 extensions)
declaration_construct
    : type_declaration_stmt_f90
    | derived_type_def              // F90 derived types
    | interface_block               // F90 interface blocks
    | parameter_stmt
    | data_stmt
    | namelist_stmt                 // F90 namelist
    | common_stmt
    | equivalence_stmt
    | dimension_stmt
    | allocatable_stmt              // F90 allocatable declaration
    | pointer_stmt                  // F90 pointer declaration
    | target_stmt                   // F90 target declaration
    | optional_stmt                 // F90 optional declaration
    | intent_stmt                   // F90 intent declaration
    | public_stmt                   // F90 visibility control
    | private_stmt                  // F90 visibility control
    | save_stmt
    | external_stmt
    | intrinsic_stmt
    ;

// ====================================================================
// EXECUTION PART (F90 ENHANCED)
// ====================================================================
// ISO/IEC 1539:1991 Section 8
// Contains executable statements and constructs.

// Execution part (enhanced for F90)
execution_part
    : (NEWLINE* executable_construct)* NEWLINE*
    ;

executable_construct
    : executable_stmt
    | construct
    ;

executable_stmt
    : call_stmt_f90                 // Enhanced procedure calls
    | return_stmt
    | stop_stmt
    | cycle_stmt                    // F90 enhanced loop control
    | exit_stmt                     // F90 enhanced loop control
    | goto_stmt
    | arithmetic_if_stmt
    | continue_stmt
    | read_stmt_f90                 // Enhanced I/O
    | write_stmt_f90                // Enhanced I/O
    | print_stmt_f90                // F77-inherited print statement
    | allocate_stmt                 // F90 memory management
    | deallocate_stmt               // F90 memory management
    | nullify_stmt                  // F90 pointer nullification
    | where_stmt                    // F90 array conditional
    | pointer_assignment_stmt       // F90 pointer assignment
    | assignment_stmt_f90           // Last resort to avoid ENDIF conflict
    ;

// Construct (F90 enhanced control structures)
construct
    : if_construct
    | select_case_construct         // F90 innovation
    | do_construct_f90              // F90 enhanced DO
    | where_construct               // F90 array construct
    ;

// ====================================================================
// FORTRAN 90 UNIFIED PARSER STATUS
// ====================================================================
//
// Refactored per issue #252 into smaller delegate grammars:
// - F90ModulesParser.g4: Module system and interface blocks (~100 lines)
// - F90TypesParser.g4: Type declarations and derived types (~180 lines)
// - F90ControlParser.g4: Control structures (~150 lines)
// - F90IOParser.g4: I/O statements and NAMELIST (~100 lines)
// - F90ExprsParser.g4: Expressions and array operations (~170 lines)
// - F90MemoryParser.g4: Dynamic memory management (~60 lines)
// - F90ProcsParser.g4: Procedures and subprograms (~100 lines)
// - F90InheritedParser.g4: Inherited F77 compatibility (~180 lines)
// - Fortran90Parser.g4 (this file): Program structure (~150 lines)
//
// MAJOR F90 FEATURES COVERED BY THIS GRAMMAR INCLUDE:
// - Module system (MODULE, USE, PUBLIC/PRIVATE visibility)
// - Interface blocks (explicit interfaces, generic procedures, operator overloading)
// - Derived types (TYPE definitions, structure constructors, component access)
// - Dynamic arrays (ALLOCATABLE, POINTER, TARGET, memory management)
// - Enhanced control structures (SELECT CASE, WHERE, named constructs)
// - Array operations (constructors, sections, intrinsic functions)
// - Enhanced I/O (NAMELIST, non-advancing I/O, enhanced error handling)
// - Modern expressions (new operators, logical expressions, array expressions)
// - Enhanced procedures (RECURSIVE, OPTIONAL, INTENT, keyword arguments)
// - Unified format support (both fixed and free form in one grammar)
//
// COMPATIBILITY FEATURES:
// - F77 backward compatibility through FORTRAN77Parser inheritance
// - Legacy constructs (arithmetic IF, computed GOTO, etc.)
// - Traditional I/O (FORMAT statements, unit-based I/O)
// - Backward compatibility with all inherited FORTRAN features
//
// EXTENSION FOUNDATION:
// - Architecture ready for F95 inheritance and extension
// - Modular design supports seamless F95+ feature addition
// - Clear extension points for subsequent standards
//
// ====================================================================
