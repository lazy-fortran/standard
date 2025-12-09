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
    # No expected failures currently. All fixtures use standard-conformant syntax.
}
