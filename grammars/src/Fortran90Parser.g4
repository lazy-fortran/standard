// Fortran 90 (1990) Parser - Unified Fixed/Free Form Support
// Reference: ISO/IEC 1539:1991 (Fortran 90)
//            WG5 N692 draft (validation/pdfs/Fortran90_WG5_N692.txt)
// Refactored per issue #252 into smaller delegate modules
parser grammar Fortran90Parser;

import FORTRAN77Parser,  // FORTRAN 77 (1977) - structured programming foundation
       F90ModulesParser, // Module system and interfaces (Section 11.3, 12.3)
       F90TypesParser,   // Type declarations and derived types (Section 4, 5)
       F90ControlParser, // Control structures (Section 7.5.3, 8.1)
       F90IOParser,      // I/O statements and NAMELIST (Section 5.4, 9)
       F90ExprsParser,   // Expressions and array operations (Section 4.5, 7)
       F90MemoryParser,  // Dynamic memory management (Section 6.3)
       F90ProcsParser,   // Procedures and subprograms (Section 12)
       F90InheritedParser;  // Inherited F77 compatibility rules

options {
    tokenVocab = Fortran90Lexer;
}

// ====================================================================
// FORTRAN 90 UNIFIED PARSER OVERVIEW
// ====================================================================
//
// This parser implements syntax rules for Fortran 90 as defined in:
//   ISO/IEC 1539:1991 (Fortran 90 International Standard)
//   WG5 N692 (Fortran 90 draft, equivalent to final standard)
//
// ISO/IEC 1539:1991 Standard Structure Overview:
//   Section 1: Overview
//   Section 2: Fortran Terms and Concepts (2.1-2.5)
//   Section 3: Characters and Low Level Syntax (3.1-3.4)
//   Section 4: Data Types (4.1-4.5)
//   Section 5: Data Object Declarations (5.1-5.5)
//   Section 6: Use of Data Objects (6.1-6.5)
//   Section 7: Expressions and Assignment (7.1-7.5)
//   Section 8: Execution Control (8.1-8.3)
//   Section 9: Input/Output Statements (9.1-9.9)
//   Section 10: Input/Output Editing (10.1-10.9)
//   Section 11: Program Units (11.1-11.3)
//   Section 12: Procedures (12.1-12.5)
//   Section 13: Intrinsic Procedures (13.1-13.14)
//   Section 14: Scope, Association, and Definition (14.1-14.7)
//   Annex B: Syntax Rules (BNF summary)
//
// This parser handles BOTH fixed-form and free-form formats:
// - Fixed-form: .f, .for files (ISO/IEC 1539:1991 Section 3.3)
// - Free-form: .f90+ files (ISO/IEC 1539:1991 Section 3.4)
//
// MAJOR F90 FEATURES (with ISO section references and delegate files):
// - Module system (Section 11.3) -> F90ModulesParser
// - Dynamic memory (Section 6.3) -> F90MemoryParser
// - Derived types (Section 4.4) -> F90TypesParser
// - Array operations (Section 4.5, 7.5.4) -> F90ExprsParser
// - Control flow (Section 8.1) -> F90ControlParser
// - Enhanced I/O (Section 9) -> F90IOParser
// - Procedures (Section 12) -> F90ProcsParser
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
// PROGRAM STRUCTURE - ISO/IEC 1539:1991 Section 2 and 11
// ====================================================================
//
// ISO/IEC 1539:1991 Section 2.1 defines the concept of program units:
// - R201 (program) -> program-unit [program-unit]...
// - R202 (program-unit) -> main-program | external-subprogram |
//                          module | block-data
//
// ISO/IEC 1539:1991 Section 11 defines program unit structure in detail.

// F90 program unit (major enhancement - adds modules)
// ISO/IEC 1539:1991 Section 2.1, R202
program_unit_f90
    : NEWLINE* (main_program | module | external_subprogram_f90) NEWLINE*
    ;

// ====================================================================
// MAIN PROGRAM - ISO/IEC 1539:1991 Section 11.1
// ====================================================================
//
// ISO/IEC 1539:1991 Section 11.1 defines the main program:
// - R1101 (main-program) -> [program-stmt] [specification-part]
//                           [execution-part] [internal-subprogram-part]
//                           end-program-stmt
// - R1102 (program-stmt) -> PROGRAM program-name
// - R1103 (end-program-stmt) -> END [PROGRAM [program-name]]

// Main program structure - ISO/IEC 1539:1991 Section 11.1, R1101
main_program
    : program_stmt specification_part? execution_part?
      internal_subprogram_part_f90? end_program_stmt
    ;

// PROGRAM statement - ISO/IEC 1539:1991 Section 11.1, R1102
program_stmt
    : PROGRAM IDENTIFIER NEWLINE*
    ;

// END PROGRAM statement - ISO/IEC 1539:1991 Section 11.1, R1103
end_program_stmt
    : END (PROGRAM (IDENTIFIER)?)? NEWLINE*
    ;

// ====================================================================
// SPECIFICATION PART - ISO/IEC 1539:1991 Section 2.3 and 5
// ====================================================================
//
// ISO/IEC 1539:1991 Section 2.3.1 defines the specification part:
// - R204 (specification-part) -> [use-stmt]... [implicit-part]
//                                [declaration-construct]...
// - R207 (declaration-construct) -> derived-type-def | interface-block |
//                                   type-declaration-stmt | ...
//
// Section 5 provides detailed specification statement rules.

// Specification part - ISO/IEC 1539:1991 Section 2.3.1, R204
specification_part
    : (NEWLINE* (use_stmt | import_stmt))*
        (NEWLINE* implicit_stmt_f90)?
        (NEWLINE* declaration_construct)* NEWLINE*
    ;

// Declaration construct - ISO/IEC 1539:1991 Section 2.3.1, R207
declaration_construct
    : type_declaration_stmt_f90      // Section 5.1
    | derived_type_def               // Section 4.4 - F90 derived types
    | interface_block                // Section 12.3 - F90 interface blocks
    | parameter_stmt                 // Section 5.1.2.11
    | data_stmt                      // Section 5.2.9
    | namelist_stmt                  // Section 5.4 - F90 namelist
    | common_stmt                    // Section 5.5.1
    | equivalence_stmt               // Section 5.5.2
    | dimension_stmt                 // Section 5.1.2.4.1
    | allocatable_stmt               // Section 5.1.2.4.3 - F90 allocatable
    | pointer_stmt                   // Section 5.1.2.4.4 - F90 pointer
    | target_stmt                    // Section 5.1.2.4.5 - F90 target
    | optional_stmt                  // Section 5.1.2.6 - F90 optional
    | intent_stmt                    // Section 5.1.2.3 - F90 intent
    | public_stmt                    // Section 5.1.2.1 - F90 visibility
    | private_stmt                   // Section 5.1.2.1 - F90 visibility
    | save_stmt                      // Section 5.1.2.5
    | external_stmt                  // Section 5.1.2.7
    | intrinsic_stmt                 // Section 5.1.2.8
    ;

// ====================================================================
// EXECUTION PART - ISO/IEC 1539:1991 Section 2.3 and 8
// ====================================================================
//
// ISO/IEC 1539:1991 Section 2.3.3 defines the execution part:
// - R208 (execution-part) -> executable-construct [execution-part-construct]...
// - R214 (executable-construct) -> action-stmt | construct
// - R215 (action-stmt) -> assignment-stmt | call-stmt | ...
// - R216 (construct) -> if-construct | case-construct | do-construct |
//                       where-construct
//
// Section 8 provides detailed execution control rules.

// Execution part - ISO/IEC 1539:1991 Section 2.3.3, R208
execution_part
    : (NEWLINE* executable_construct)* NEWLINE*
    ;

// Executable construct - ISO/IEC 1539:1991 Section 2.3.3, R214
executable_construct
    : executable_stmt
    | construct
    ;

// Action statement - ISO/IEC 1539:1991 Section 2.3.3, R215
executable_stmt
    : call_stmt_f90                  // Section 12.4 - Enhanced procedure calls
    | return_stmt                    // Section 12.4.3
    | stop_stmt                      // Section 8.3
    | cycle_stmt                     // Section 8.1.4.4.1 - F90 loop control
    | exit_stmt                      // Section 8.1.4.4.2 - F90 loop control
    | goto_stmt                      // Section 8.2
    | arithmetic_if_stmt             // Section 8.1.1.2 (obsolescent)
    | continue_stmt                  // Section 8.2
    | read_stmt_f90                  // Section 9.4 - Enhanced I/O
    | write_stmt_f90                 // Section 9.4 - Enhanced I/O
    | print_stmt_f90                 // Section 9.4 - F77-inherited print
    | open_stmt_f90                  // Section 9.7.1 - File connection (R904)
    | close_stmt_f90                 // Section 9.7.2 - File disconnection (R908)
    | inquire_stmt_f90               // Section 9.7.3 - File inquiry (R929)
    | backspace_stmt_f90             // Section 9.6.1 - File positioning (R923)
    | endfile_stmt_f90               // Section 9.6.2 - File positioning (R924)
    | rewind_stmt_f90                // Section 9.6.3 - File positioning (R925)
    | allocate_stmt                  // Section 6.3.1 - F90 memory management
    | deallocate_stmt                // Section 6.3.3 - F90 memory management
    | nullify_stmt                   // Section 6.3.2 - F90 pointer nullification
    | where_stmt                     // Section 7.5.3 - F90 array conditional
    | pointer_assignment_stmt        // Section 7.5.2 - F90 pointer assignment
    | entry_stmt_f90                 // Section 12.5.2.4 - F90 ENTRY statement
    | assignment_stmt_f90            // Section 7.5.1 - Last resort for ENDIF
    ;

// Construct - ISO/IEC 1539:1991 Section 2.3.3, R216
construct
    : if_construct                   // Section 8.1.1
    | select_case_construct          // Section 8.1.3 - F90 innovation
    | do_construct_f90               // Section 8.1.4 - F90 enhanced DO
    | where_construct                // Section 7.5.3 - F90 array construct
    ;

// ====================================================================
// FORTRAN 90 PARSER - ISO/IEC 1539:1991 SPEC-GRAMMAR MAPPING
// ====================================================================
//
// Refactored per issue #252 into smaller delegate grammars.
// Each delegate file maps to specific ISO/IEC 1539:1991 sections:
//
// - F90ModulesParser.g4: Section 11.3 (Modules), Section 12.3 (Interfaces)
// - F90TypesParser.g4: Section 4.4 (Derived types), Section 5.1 (Declarations)
// - F90ControlParser.g4: Section 8.1 (Constructs), Section 7.5.3 (WHERE)
// - F90IOParser.g4: Section 5.4 (NAMELIST), Section 9 (I/O)
// - F90ExprsParser.g4: Section 4.5 (Arrays), Section 7 (Expressions)
// - F90MemoryParser.g4: Section 6.3 (Dynamic allocation)
// - F90ProcsParser.g4: Section 12 (Procedures)
// - F90InheritedParser.g4: F77 compatibility (inherited rules)
// - Fortran90Parser.g4: Section 2, 11 (Program structure)
//
// ISO/IEC 1539:1991 SPEC-GRAMMAR MAPPING (PARSER):
// Section 2 (Concepts)       -> program_unit_f90
// Section 4 (Types)          -> type_spec_f90, derived_type_def, literal_f90
// Section 5 (Declarations)   -> specification_part, declaration_construct,
//                               type_declaration_stmt_f90, *_stmt
// Section 6 (Data objects)   -> variable_f90, allocate_stmt, deallocate_stmt,
//                               nullify_stmt
// Section 7 (Expressions)    -> expr_f90, primary_f90, where_*, assignment_*
// Section 8 (Control)        -> if_construct, select_case_construct,
//                               do_construct_f90, cycle_stmt, exit_stmt
// Section 9 (I/O)            -> read_stmt_f90, write_stmt_f90, io_control_spec
// Section 11 (Programs)      -> main_program, module, end_*_stmt
// Section 12 (Procedures)    -> function_stmt, subroutine_stmt, interface_block,
//                               call_stmt_f90
// Section 13 (Intrinsics)    -> intrinsic_function_f90
//
// MAJOR F90 FEATURES IMPLEMENTED (with ISO section refs):
// - Module system (Section 11.3): MODULE, USE, PUBLIC/PRIVATE
// - Interface blocks (Section 12.3): explicit interfaces, generic procedures
// - Derived types (Section 4.4): TYPE definitions, structure constructors
// - Dynamic arrays (Section 6.3): ALLOCATABLE, POINTER, TARGET
// - Control structures (Section 8.1): SELECT CASE, WHERE, named constructs
// - Array operations (Section 4.5, 7.5.4): constructors, sections, intrinsics
// - Enhanced I/O (Section 9): NAMELIST, non-advancing I/O
// - Procedures (Section 12): RECURSIVE, OPTIONAL, INTENT, keyword arguments
// - Unified format support (Section 3.3, 3.4): fixed and free form
//
// BACKWARD COMPATIBILITY:
// - F77 compatibility through FORTRAN77Parser inheritance
// - Legacy constructs maintained for compatibility
//
// FORWARD COMPATIBILITY:
// - Architecture ready for F95 inheritance
// - Clear extension points for subsequent standards
//
// ====================================================================
