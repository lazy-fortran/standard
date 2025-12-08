#!/usr/bin/env python3
"""Expected-failure (XPASS) fixture definitions for generic fixture parsing tests.

Fixtures listed here are known to produce syntax errors with the current
simplified grammars but are still valuable as examples. Each entry maps a
(standard, relpath) tuple to a reason template that documents the limitation.

This module is imported by test_fixture_parsing.py to keep the generic test
module focused on parsing logic.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, Tuple

XPASS_FIXTURES: Dict[Tuple[str, Path], str] = {
    # Fortran2023 fixtures exercising areas where the F2023 parser is still
    # intentionally minimal and not yet fully error-free. These are forward-
    # looking examples of incremental F2023 features.
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/basic_program.f90"),
    ): (
        "Fortran 2023 fixture {relpath} is a forward-looking basic program "
        "example; the current F2023 parser is intentionally minimal and still "
        "reports {errors} syntax errors (see the Fortran 2023 entry in "
        "README.md)."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/boz_array_constructor.f90"),
    ): (
        "Fortran 2023 BOZ array-constructor fixture {relpath} currently "
        "produces {errors} syntax errors; support for these F2023 enhancements "
        "is still evolving and is documented as intentionally minimal in the "
        "Fortran 2023 README entry."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/conditional_expression.f90"),
    ): (
        "Fortran 2023 conditional expression fixture {relpath} exercises new "
        "F2023 expression forms; the parser still reports {errors} syntax "
        "errors while incremental F2023 coverage is being completed."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/f2018_compat_program.f90"),
    ): (
        "Fortran 2023 F2018-compatibility fixture {relpath} documents how the "
        "F2023 grammar currently handles legacy F2018-style programs; it still "
        "produces {errors} syntax errors and is treated as a status test "
        "rather than a hard requirement."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/enum_program.f90"),
    ): (
        "Fortran 2023 enumeration fixture {relpath} exercises F2023 ENUM "
        "support; the grammar is not yet fully strict here and still reports "
        "{errors} syntax errors."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/ieee_program.f90"),
    ): (
        "Fortran 2023 IEEE fixture {relpath} combines IEEE features with "
        "F2023 syntax; until F2023 coverage is complete this is expected to "
        "report {errors} syntax errors."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/mixed_era_program.f90"),
    ): (
        "Fortran 2023 mixed-era program fixture {relpath} intentionally mixes "
        "features from multiple standards; the unified grammar still reports "
        "{errors} syntax errors here."
    ),
    (
        "Fortran2023",
        Path(
            "Fortran2023/"
            "test_fortran_2023_comprehensive_extra/namelist_enhancements_module.f90"
        ),
    ): (
        "Fortran 2023 NAMELIST enhancements fixture {relpath} exercises "
        "forward-looking F2023 features that are not yet fully implemented; "
        "the current grammar still reports {errors} syntax errors."
    ),
    # Fortran90 comprehensive fixtures currently exercise areas where the F90
    # grammar still has known limitations (e.g. modules as top-level units,
    # array constructors, and enhanced procedures). These are covered by the
    # dedicated Fortran90 comprehensive test suites; there is no dedicated
    # GitHub issue yet, so these XPASS entries document the remaining gaps.
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/basic_program.f90"),
    ): (
        "Fortran 90 basic program fixture {relpath} still triggers {errors} "
        "syntax errors in the unified F90 grammar; the detailed status is "
        "tracked by tests/Fortran90/test_fortran_90_comprehensive.py."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/mathematics_module.f90"),
    ): (
        "Fortran 90 mathematics module fixture {relpath} exposes remaining "
        "limitations in module/program-unit handling and currently produces "
        "{errors} syntax errors."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/derived_types_module.f90"),
    ): (
        "Fortran 90 derived-types module fixture {relpath} still produces "
        "{errors} syntax errors; richer derived-type constructs are exercised "
        "more directly in the dedicated Fortran90 comprehensive tests."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/dynamic_arrays.f90"),
    ): (
        "Fortran 90 dynamic array fixture {relpath} stresses allocatable and "
        "pointer arrays; the current F90 grammar reports {errors} syntax "
        "errors here and this is recorded as an expected limitation."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/select_case_program.f90"),
    ): (
        "Fortran 90 SELECT CASE fixture {relpath} is a complex control-"
        "structure example that still yields {errors} syntax errors with the "
        "present F90 grammar."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/array_constructor_program.f90"),
    ): (
        "Fortran 90 array-constructor fixture {relpath} highlights remaining "
        "array-constructor coverage gaps and currently produces {errors} "
        "syntax errors."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/enhanced_procedures_program.f90"),
    ): (
        "Fortran 90 enhanced-procedures fixture {relpath} still reports "
        "{errors} syntax errors; procedure enhancements are validated more "
        "directly in the dedicated comprehensive tests."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_dual_format/free_form_module.f90"),
    ): (
        "Fortran 90 dual-format free-form module fixture {relpath} exposes "
        "remaining rough edges in unified fixed/free-form handling and "
        "currently yields {errors} syntax errors."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/mathematics_module.f90"),
    ): (
        "Fortran 90 mathematics module fixture {relpath} (integration test) "
        "currently produces {errors} syntax errors; the dedicated F90 "
        "comprehensive suite carries the strict expectations."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/dynamic_arrays_program.f90"),
    ): (
        "Fortran 90 dynamic-arrays integration fixture {relpath} still "
        "reports {errors} syntax errors and therefore remains an expected "
        "failure in the generic discovery test."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/types_module.f90"),
    ): (
        "Fortran 90 types-module integration fixture {relpath} currently "
        "yields {errors} syntax errors; type-related coverage is exercised "
        "more directly in the F90 comprehensive tests."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/free_form_features_program.f90"),
    ): (
        "Fortran 90 free-form features fixture {relpath} currently reports "
        "{errors} syntax errors; this XPASS marks the remaining gaps in the "
        "free-form side of the unified grammar."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/advanced_features_module.f90"),
    ): (
        "Fortran 90 advanced-features module fixture {relpath} still yields "
        "{errors} syntax errors; the more focused F90 tests track progress "
        "toward full coverage."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/fortran95_features_program.f90"),
    ): (
        "Fortran 90/95 features fixture {relpath} exercises F95-style "
        "features via the F90 integration harness; it currently produces "
        "{errors} syntax errors and is treated as an expected failure here."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/mixed_format_program.f90"),
    ): (
        "Fortran 90 mixed-format fixture {relpath} intentionally mixes layout "
        "styles; the unified grammar still reports {errors} syntax errors on "
        "this example."
    ),
    # Historical FORTRAN dialect fixtures that exceed the current
    # simplified/stub grammars but are still valuable examples. These
    # dialects are intentionally limited and do not yet have dedicated
    # GitHub issues for full historical coverage; the XPASS entries
    # simply document the expected rough edges.
    #
    # Note: Several FORTRAN 1957 fixtures (array_program_1957.f, format_stmt.f,
    # format_tests_1957.f, historical_quadratic_program.f, io_operations_1957.f)
    # now parse correctly after the FORMAT E-descriptor grammar fix (issue #141)
    # and are no longer marked as XPASS.
    (
        "FORTRAN",
        Path("FORTRAN/test_comprehensive_validation/complex_program.f"),
    ): (
        "Historical FORTRAN complex-program fixture {relpath} uses CALL "
        "statements which were introduced in FORTRAN II (1958), not in the "
        "1957 grammar. It is expected to report {errors} syntax errors with "
        "the FORTRAN 1957 parser."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/authentic_1957_program.f"),
    ): (
        "Historical FORTRAN authentic-1957 program fixture {relpath} uses "
        "column-1 C comments which are not yet supported in the layout-lenient "
        "1957 lexer (see issue #155 for strict fixed-form mode). It currently "
        "yields {errors} syntax errors."
    ),
    # FORTRAN II fixtures have been updated to parse correctly with the
    # enhanced grammar and fortran_program entry rule per issue #157.
    # They are no longer marked as XPASS.
    #
    # FORTRAN II strict fixed-form fixtures require preprocessing via
    # tools/strict_fixed_form.py before parsing; they use authentic IBM 704
    # card-image format which the layout-lenient grammar does not support
    # directly. These are tested by tests/FORTRANII/test_strict_fixed_form.py.
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/valid_subroutine.f"),
    ): (
        "FORTRAN II strict fixed-form fixture {relpath} uses authentic "
        "80-column IBM 704 card layout with sequence numbers in cols 73-80 and "
        "C-comments in col 1. It requires preprocessing via "
        "tools/strict_fixed_form.py before parsing. It produces {errors} syntax "
        "errors when parsed directly (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/valid_function.f"),
    ): (
        "FORTRAN II strict fixed-form fixture {relpath} uses authentic "
        "80-column IBM 704 card layout. It requires preprocessing via "
        "tools/strict_fixed_form.py. It produces {errors} syntax errors when "
        "parsed directly (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/valid_main.f"),
    ): (
        "FORTRAN II strict fixed-form fixture {relpath} uses authentic "
        "80-column IBM 704 card layout. It requires preprocessing via "
        "tools/strict_fixed_form.py. It produces {errors} syntax errors when "
        "parsed directly (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/valid_continuation.f"),
    ): (
        "FORTRAN II strict fixed-form fixture {relpath} demonstrates "
        "continuation cards with col 6 marks. It requires preprocessing via "
        "tools/strict_fixed_form.py. It produces {errors} syntax errors when "
        "parsed directly (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/star_comment.f"),
    ): (
        "FORTRAN II strict fixed-form fixture {relpath} uses *-comments in "
        "col 1. It requires preprocessing via tools/strict_fixed_form.py. "
        "It produces {errors} syntax errors when parsed directly (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/invalid_label_alpha.f"),
    ): (
        "FORTRAN II strict fixed-form negative fixture {relpath} has "
        "intentionally invalid alphabetic label field. It is expected to fail "
        "both strict validation and direct parsing. It produces {errors} syntax "
        "errors (Issue #143)."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_strict_fixed_form/invalid_continuation_labeled.f"),
    ): (
        "FORTRAN II strict fixed-form negative fixture {relpath} has "
        "intentionally invalid label on continuation card. It is expected to "
        "fail strict validation. It produces {errors} syntax errors when parsed "
        "directly (Issue #143)."
    ),
    # FORTRAN 66 fixtures have been updated to parse correctly with the
    # enhanced grammar per issue #144. They are no longer marked as XPASS.
    # FORTRAN 77 multi-unit fixtures require fortran66_program entry rule
    # (not main_program). These are tested via dedicated test methods.
    (
        "FORTRAN77",
        Path("FORTRAN77/test_fortran77_parser_extra/entry_statements.f"),
    ): (
        "FORTRAN 77 ENTRY statement fixture {relpath} contains multiple "
        "program units (subroutines and functions) that require the "
        "fortran66_program entry rule; with main_program entry rule it "
        "produces {errors} syntax errors. The fixture is tested via "
        "test_entry_statement_in_subroutine_fixture in "
        "tests/FORTRAN77/test_fortran77_parser.py."
    ),
    # Fortran2003 negative / fixed-form fixtures that intentionally exercise
    # error paths or comment-handling edge cases. These are tied to specific
    # Fortran 2003 issues in the audit document.
    (
        "Fortran2003",
        Path("Fortran2003/test_fortran_2003_comprehensive/fixed_form_f2003.f"),
    ): (
        "Fortran 2003 fixed-form fixture {relpath} currently reports {errors} "
        "syntax errors because of leading-comment handling in the unified "
        "lexer; see Issue #72 and docs/fortran_2003_audit.md section 6 "
        "(fixed-form F2003)."
    ),
    (
        "Fortran2003",
        Path("Fortran2003/test_issue68_defined_io/bad_dt_generic_module.f90"),
    ): (
        "Fortran 2003 DT= defined derived-type I/O negative fixture {relpath} "
        "is expected to produce {errors} syntax errors while DT edit "
        "descriptors remain parsed as opaque literals (Issue #68; see "
        "docs/fortran_2003_audit.md section 6 and "
        "tests/Fortran2003/test_issue68_defined_io.py)."
    ),
    (
        "Fortran2003",
        Path("Fortran2003/test_issue69_select_type_semantics/bad_default_program.f90"),
    ): (
        "Fortran 2003 SELECT TYPE semantics negative fixture {relpath} "
        "represents a bad DEFAULT case that currently yields {errors} syntax "
        "errors; see Issue #69 and "
        "tests/Fortran2003/test_issue69_select_type_semantics.py."
    ),
    (
        "Fortran2003",
        Path("Fortran2003/test_issue69_select_type_semantics/bad_guard_program.f90"),
    ): (
        "Fortran 2003 SELECT TYPE semantics negative fixture {relpath} "
        "represents a bad guard case that still produces {errors} syntax "
        "errors; see Issue #69 and "
        "tests/Fortran2003/test_issue69_select_type_semantics.py."
    ),
    (
        "Fortran2003",
        Path("Fortran2003/test_issue70_c_interop_extended/bad_lang_module.f90"),
    ): (
        "Fortran 2003 C interoperability negative fixture {relpath} exercises "
        "LANG= usage that the grammar does not yet accept cleanly and "
        "currently reports {errors} syntax errors (Issue #70; see "
        "docs/fortran_2003_audit.md section 7 and "
        "tests/Fortran2003/test_issue70_c_interop_extended.py)."
    ),
    (
        "Fortran2003",
        Path("Fortran2003/test_issue70_c_interop_extended/bad_name_module.f90"),
    ): (
        "Fortran 2003 C interoperability negative fixture {relpath} exercises "
        "NAME= usage that remains partially unsupported; it is expected to "
        "produce {errors} syntax errors (Issue #70; see "
        "docs/fortran_2003_audit.md section 7 and "
        "tests/Fortran2003/test_issue70_c_interop_extended.py)."
    ),
    # Fortran 95 fragment fixtures from test_fortran_95_features are designed
    # to be parsed via dedicated entry rules (forall_stmt, where_construct_f95,
    # function_stmt_f95, etc.) in tests/Fortran95/test_fortran_95_features.py,
    # not via the program_unit_f95 entry rule used by the generic fixture
    # harness. These fixtures are bare tokens or partial constructs, not
    # complete program units.
    # Issue #148 tracks expanding F95 test coverage with complete program
    # fixtures that exercise the integrated program_unit_f95 entry point.
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/array_constructor.f90"),
    ): (
        "Fortran 95 array-constructor fragment fixture {relpath} is tested via "
        "array_constructor_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/bit_size.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/btest.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/ceiling.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/ceiling_call.f90"),
    ): (
        "Fortran 95 function reference fragment fixture {relpath} is tested via "
        "function_reference_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/ceiling_kind_call.f90"),
    ): (
        "Fortran 95 function reference fragment fixture {relpath} is tested via "
        "function_reference_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/cpu_time.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/cpu_time_call.f90"),
    ): (
        "Fortran 95 call statement fragment fixture {relpath} is tested via "
        "call_stmt_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/elemental_subroutine_stmt.f90"),
    ): (
        "Fortran 95 ELEMENTAL subroutine header fragment fixture {relpath} is "
        "tested via subroutine_stmt_f95 entry rule; produces {errors} "
        "syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/end_forall.f90"),
    ): (
        "Fortran 95 END FORALL fragment fixture {relpath} is tested via "
        "end_forall_stmt entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/floor.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/floor_call.f90"),
    ): (
        "Fortran 95 function reference fragment fixture {relpath} is tested via "
        "function_reference_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/forall_construct.f90"),
    ): (
        "Fortran 95 FORALL construct fragment fixture {relpath} is tested via "
        "forall_construct entry rule; this is a bare construct fragment, not a "
        "complete program unit, so it produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/forall_header.f90"),
    ): (
        "Fortran 95 FORALL header fragment fixture {relpath} is tested via "
        "forall_header entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/forall_stmt.f90"),
    ): (
        "Fortran 95 FORALL statement fragment fixture {relpath} is tested via "
        "forall_stmt entry rule; this is a bare statement fragment, not a "
        "complete program unit, so it produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/iand.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/iand_call.f90"),
    ): (
        "Fortran 95 function reference fragment fixture {relpath} is tested via "
        "function_reference_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/ibits.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/modulo.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/pure_function_stmt.f90"),
    ): (
        "Fortran 95 PURE function header fragment fixture {relpath} is tested via "
        "function_stmt_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/system_clock.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/transfer.f90"),
    ): (
        "Fortran 95 intrinsic token fixture {relpath} is a bare token; produces "
        "{errors} syntax errors with program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/transfer_call.f90"),
    ): (
        "Fortran 95 function reference fragment fixture {relpath} is tested via "
        "function_reference_f95 entry rule; produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
    (
        "Fortran95",
        Path("Fortran95/test_fortran_95_features/where_construct.f90"),
    ): (
        "Fortran 95 WHERE construct fragment fixture {relpath} is tested via "
        "where_construct_f95 entry rule; this is a bare construct fragment, not "
        "a complete program unit, so it produces {errors} syntax errors with "
        "program_unit_f95 entry (Issue #148)."
    ),
}
