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
    # Fortran 90 - Issue #311
    # Grammar gaps for module and control flow constructs
    # =========================================================================
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
        Path("Fortran90/test_fortran_90_comprehensive/array_constructor_program.f90"),
    ): (
        "Fortran 90 fixture {relpath} reports {errors} syntax errors due to "
        "bracket array constructor syntax (F2003 feature). Tracked by Issue #311."
    ),
}
