#!/usr/bin/env python3
"""Generic fixture-based parsing tests for standard Fortran grammars.

This module auto-discovers Fortran source fixtures under tests/fixtures/<Standard>/
and verifies that each example parses with zero syntax errors using the
appropriate lexer/parser and top-level program_unit rule for that standard.

The goal is that adding a new .f / .f90 / .lf fixture file automatically
adds coverage, without needing to edit per-standard pytest files.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Dict, List, Tuple

import pytest

# Make grammars and fixture utilities importable
ROOT = Path(__file__).resolve().parent
GRAMMARS = ROOT.parent / "grammars"
sys.path.insert(0, str(GRAMMARS))
sys.path.append(str(ROOT))

from fixture_utils import load_fixture  # noqa: E402

FIXTURES_ROOT = ROOT / "fixtures"


class StandardConfig:
    """Configuration for a standard's lexer/parser and entry rule."""

    def __init__(self, lexer_name: str, parser_name: str, entry_rule: str):
        self.lexer_name = lexer_name
        self.parser_name = parser_name
        self.entry_rule = entry_rule
        self.lexer_cls = None
        self.parser_cls = None
        self._import()

    def _import(self) -> None:
        try:
            module = __import__(self.lexer_name, fromlist=[self.lexer_name])
            self.lexer_cls = getattr(module, self.lexer_name)
            parser_module = __import__(self.parser_name, fromlist=[self.parser_name])
            self.parser_cls = getattr(parser_module, self.parser_name)
        except Exception:
            # Leave classes as None; tests will be skipped for this standard.
            self.lexer_cls = None
            self.parser_cls = None

    @property
    def available(self) -> bool:
        return self.lexer_cls is not None and self.parser_cls is not None


STANDARD_CONFIGS: Dict[str, StandardConfig] = {
    # Historical FORTRAN grammars – included in dynamic discovery so that
    # their fixtures are exercised automatically. Known rough edges are
    # explicitly marked in XPASS_FIXTURES.
    "FORTRAN": StandardConfig(
        lexer_name="FORTRANLexer",
        parser_name="FORTRANParser",
        entry_rule="program_unit_core",
    ),
    "FORTRANII": StandardConfig(
        lexer_name="FORTRANIILexer",
        parser_name="FORTRANIIParser",
        entry_rule="program_unit_core",
    ),
    "FORTRAN66": StandardConfig(
        lexer_name="FORTRAN66Lexer",
        parser_name="FORTRAN66Parser",
        entry_rule="fortran66_program",
    ),
    "FORTRAN77": StandardConfig(
        lexer_name="FORTRAN77Lexer",
        parser_name="FORTRAN77Parser",
        entry_rule="main_program",
    ),
    # Modern standardized Fortran grammars.
    "Fortran90": StandardConfig(
        lexer_name="Fortran90Lexer",
        parser_name="Fortran90Parser",
        entry_rule="program_unit_f90",
    ),
    "Fortran2003": StandardConfig(
        lexer_name="Fortran2003Lexer",
        parser_name="Fortran2003Parser",
        entry_rule="program_unit_f2003",
    ),
    "Fortran2008": StandardConfig(
        lexer_name="Fortran2008Lexer",
        parser_name="Fortran2008Parser",
        entry_rule="program_unit_f2008",
    ),
    "Fortran2023": StandardConfig(
        lexer_name="Fortran2023Lexer",
        parser_name="Fortran2023Parser",
        entry_rule="program_unit_f2023",
    ),
}


def _discover_fixtures() -> List[Tuple[str, Path]]:
    """Discover all .f / .f90 / .lf fixtures under tests/fixtures."""
    cases: List[Tuple[str, Path]] = []
    if not FIXTURES_ROOT.exists():
        return cases

    for standard_dir in FIXTURES_ROOT.iterdir():
        if not standard_dir.is_dir():
            continue
        standard = standard_dir.name
        if standard not in STANDARD_CONFIGS:
            continue
        for path in standard_dir.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.lower() not in {".f", ".f90", ".lf"}:
                continue
            rel = path.relative_to(FIXTURES_ROOT)
            cases.append((standard, rel))

    # Deterministic order for stable test ids
    cases.sort(key=lambda item: str(item[1]))
    return cases


FIXTURE_CASES = _discover_fixtures()

# Fixtures that are known to produce syntax errors with the current
# simplified grammars but are still valuable as examples. These are
# covered by more focused tests and should not fail the generic
# auto-discovery suite. Each entry carries a reason string so that the
# xfail emitted by the generic test clearly documents which issue or
# limitation it is tracking.
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
        Path("Fortran90/test_comprehensive_parsing/fixed_form_program.f"),
    ): (
        "Fortran 90 fixed-form integration fixture {relpath} still produces "
        "{errors} syntax errors; unified fixed/free-form handling is "
        "documented as a known limitation of the current F90 grammar."
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
    (
        "FORTRAN",
        Path("FORTRAN/test_comprehensive_validation/complex_program.f"),
    ): (
        "Historical FORTRAN complex-program fixture {relpath} goes beyond the "
        "simplified FORTRAN grammar and currently reports {errors} syntax "
        "errors; this is a known limitation rather than a hard failure."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/arithmetic_if_control_flow.f"),
    ): (
        "Historical FORTRAN arithmetic-IF control-flow fixture {relpath} "
        "still produces {errors} syntax errors with the current stub "
        "grammar; the example is retained for documentation."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/array_program_1957.f"),
    ): (
        "Historical FORTRAN 1957 array-program fixture {relpath} exercises "
        "more of the original language than the simplified grammar accepts; "
        "it is expected to report {errors} syntax errors here."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/authentic_1957_program.f"),
    ): (
        "Historical FORTRAN authentic-1957 program fixture {relpath} exceeds "
        "the current stub grammar and currently yields {errors} syntax errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/format_stmt.f"),
    ): (
        "Historical FORTRAN FORMAT-statement fixture {relpath} remains beyond "
        "the scope of the simplified FORMAT grammar and reports {errors} "
        "syntax errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/format_tests_1957.f"),
    ): (
        "Historical FORTRAN 1957 FORMAT test fixture {relpath} intentionally "
        "stretches the FORMAT grammar and still produces {errors} syntax "
        "errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/historical_quadratic_program.f"),
    ): (
        "Historical FORTRAN quadratic-program fixture {relpath} is more "
        "complex than the stub grammar supports and is expected to yield "
        "{errors} syntax errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/io_operations_1957.f"),
    ): (
        "Historical FORTRAN I/O-operations fixture {relpath} exercises "
        "1957-era I/O patterns beyond the simplified grammar and reports "
        "{errors} syntax errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/io_statements.f"),
    ): (
        "Historical FORTRAN I/O-statements fixture {relpath} goes beyond what "
        "the current stub grammar handles and therefore produces {errors} "
        "syntax errors."
    ),
    (
        "FORTRAN",
        Path("FORTRAN/test_fortran_historical_stub/pause_test_1957.f"),
    ): (
        "Historical FORTRAN PAUSE-test fixture {relpath} remains outside the "
        "strict subset accepted by the stub grammar and is expected to report "
        "{errors} syntax errors."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_fortran_ii_parser/function_text.f"),
    ): (
        "FORTRAN II function-text fixture {relpath} documents constructs that "
        "are not yet fully accepted by the simplified FORTRAN II grammar and "
        "currently produce {errors} syntax errors."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_fortran_ii_parser/subroutine_program.f"),
    ): (
        "FORTRAN II subroutine-program fixture {relpath} exceeds the current "
        "stub grammar and is expected to yield {errors} syntax errors."
    ),
    (
        "FORTRANII",
        Path("FORTRANII/test_fortran_ii_parser/subroutine_text.f"),
    ): (
        "FORTRAN II subroutine-text fixture {relpath} represents richer "
        "historical usage than the simplified grammar accepts and currently "
        "reports {errors} syntax errors."
    ),
    (
        "FORTRAN66",
        Path("FORTRAN66/test_fortran66_parser/first_standard_demo.f"),
    ): (
        "FORTRAN 66 first-standard demo fixture {relpath} is more ambitious "
        "than the simplified grammar and still produces {errors} syntax "
        "errors."
    ),
    (
        "FORTRAN66",
        Path("FORTRAN66/test_fortran66_parser/function_program.f"),
    ): (
        "FORTRAN 66 function-program fixture {relpath} exercises constructs "
        "that are only partially modeled in the current grammar and reports "
        "{errors} syntax errors."
    ),
    (
        "FORTRAN66",
        Path("FORTRAN66/test_fortran66_parser/main_program.f"),
    ): (
        "FORTRAN 66 main-program fixture {relpath} still yields {errors} "
        "syntax errors; the historical grammar is intentionally simplified."
    ),
    (
        "FORTRAN66",
        Path("FORTRAN66/test_fortran66_parser/standard_program.f"),
    ): (
        "FORTRAN 66 standard-program fixture {relpath} is a richer example "
        "than the stub grammar supports and is expected to report {errors} "
        "syntax errors."
    ),
    (
        "FORTRAN66",
        Path("FORTRAN66/test_fortran66_parser/subroutine_program.f"),
    ): (
        "FORTRAN 66 subroutine-program fixture {relpath} currently produces "
        "{errors} syntax errors; full historical coverage is out of scope for "
        "the simplified grammar."
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
}


@pytest.mark.parametrize("standard, relpath", FIXTURE_CASES)
def test_fixture_parses_without_errors(standard: str, relpath: Path) -> None:
    """Ensure each fixture parses with zero syntax errors for its standard."""
    cfg = STANDARD_CONFIGS[standard]
    if not cfg.available:
        pytest.skip(f"{standard} lexer/parser not available")

    # Lazy import to avoid importing antlr4 at module import time if unnecessary
    from antlr4 import InputStream, CommonTokenStream  # type: ignore

    # Load code from fixture and parse
    code = load_fixture(*relpath.parts)
    input_stream = InputStream(code)
    lexer = cfg.lexer_cls(input_stream)
    parser = cfg.parser_cls(CommonTokenStream(lexer))

    entry = getattr(parser, cfg.entry_rule)
    tree = entry()
    errors = parser.getNumberOfSyntaxErrors()

    assert tree is not None, f"{standard} fixture {relpath} did not produce a parse tree"

    key = (standard, relpath)
    if key in XPASS_FIXTURES:
        reason_template = XPASS_FIXTURES[key]
        pytest.xfail(
            reason_template.format(
                standard=standard,
                relpath=relpath,
                errors=errors,
            )
        )

    assert errors == 0, (
        f"{standard} fixture {relpath} expected 0 syntax errors, got {errors}"
    )
