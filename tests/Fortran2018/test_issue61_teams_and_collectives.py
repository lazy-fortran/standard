#!/usr/bin/env python3
"""
Issue #61 – Fortran 2018 teams/events and collectives (status tests)

These tests document what the current Fortran 2018 grammar actually
accepts for teams/events/collective syntax, without over-claiming
full F2018 coverage.
"""

import os
import sys
from pathlib import Path

import pytest
from antlr4 import CommonTokenStream, InputStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../grammars"))

from Fortran2018Lexer import Fortran2018Lexer  # type: ignore
from Fortran2018Parser import Fortran2018Parser  # type: ignore
from fixture_utils import load_fixture


def parse_f2018(code: str):
    """Parse Fortran 2018 code and return (tree, errors)."""
    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    parser = Fortran2018Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2018()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2018TeamsAndCollectivesStatus:
    """Status tests for F2018 team/event/collective syntax."""

    @pytest.mark.xfail(
        reason=(
            "Teams and collectives are not yet fully implemented "
            "(status tests for Issue #61; see implementation issue #88)"
        )
    )
    def test_co_sum_collective_is_tokenized_and_parsed(self):
        """Document current behavior of CO_SUM collective in F2018."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        tree, errors, _ = parse_f2018(code)
        assert tree is not None
        # Once issue #88 is complete, this should parse with zero errors
        assert errors == 0

    @pytest.mark.xfail(
        reason=(
            "Teams and collectives are not yet fully implemented "
            "(status tests for Issue #61; see implementation issue #88)"
        )
    )
    def test_basic_team_skeleton_parses_reasonably(self):
        """
        Skeleton FORTRAN 2018 team constructs.

        This is intentionally minimal and serves to document how far the
        current grammar gets; we do not require zero errors.
        """
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        tree, errors, _ = parse_f2018(code)
        assert tree is not None
        # Once issue #88 is complete, this should parse with zero errors
        assert errors == 0
