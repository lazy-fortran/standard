#!/usr/bin/env python3
"""Expected-failure (XPASS) fixture definitions for generic fixture parsing tests.

Fixtures listed here are known to produce syntax errors with current grammars
due to identified gaps tracked by GitHub issues. Each entry maps a
(standard, relpath) tuple to a reason template documenting the specific gap.

This module is imported by test_fixture_parsing.py to mark known failures
as xfail rather than hard failures while the issues are being resolved.

NOTE: Fixtures in excluded directories (test_strict_fixed_form,
test_fortran_95_features, test_fortran77_parser_extra) and fixtures with
excluded prefixes (bad_, invalid_) are NOT discovered by the generic test
and do NOT need entries here.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, Tuple

XPASS_FIXTURES: Dict[Tuple[str, Path], str] = {
    # =========================================================================
    # Fortran 2003 - Issue #309
    # Fixed-form source with column-1 C comments
    # =========================================================================
    (
        "Fortran2003",
        Path("Fortran2003/test_fortran_2003_comprehensive/fixed_form_f2003.f"),
    ): (
        "Fortran 2003 fixed-form fixture {relpath} reports {errors} syntax "
        "errors due to column-1 C comment handling. Tracked by Issue #309."
    ),
    # =========================================================================
    # Fortran 2023 - Issue #310
    # Grammar gaps for F2023 features
    # =========================================================================
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/boz_array_constructor.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "missing bracket array constructor support. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/conditional_expression.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "missing conditional expression (? :) support. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/enum_program.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "missing ENUM TYPE support. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/f2018_compat_program.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "coarray/image syntax gaps. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/ieee_program.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "IEEE arithmetic module intrinsic gaps. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path("Fortran2023/test_fortran_2023_comprehensive/mixed_era_program.f90"),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "character*n length syntax gaps. Tracked by Issue #310."
    ),
    (
        "Fortran2023",
        Path(
            "Fortran2023/"
            "test_fortran_2023_comprehensive_extra/namelist_enhancements_module.f90"
        ),
    ): (
        "Fortran 2023 fixture {relpath} reports {errors} syntax errors due to "
        "module PRIVATE attribute syntax gaps. Tracked by Issue #310."
    ),
    # =========================================================================
    # Fortran 90 - Issue #311
    # Grammar gaps for module and control flow constructs
    # =========================================================================
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/advanced_features_module.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "module PRIVATE attribute handling. Tracked by Issue #311."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/fortran95_features_program.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "FORALL construct gaps. Tracked by Issue #311."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/free_form_features_program.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "character*n length syntax gaps. Tracked by Issue #311."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_comprehensive_parsing/types_module.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "TYPE construct in module handling. Tracked by Issue #311."
    ),
    (
        "Fortran90",
        Path("Fortran90/test_fortran_90_comprehensive/array_constructor_program.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "bracket array constructor syntax (F2003 feature). Tracked by Issue #311."
    ),
}
