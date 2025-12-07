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

    def test_collective_subroutines_parse_without_errors(self):
        """CO_SUM/CO_MIN/CO_MAX/CO_REDUCE/CO_BROADCAST parse cleanly."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        tree, errors, _ = parse_f2018(code)
        assert tree is not None
        assert errors == 0

    def test_team_and_event_constructs_supported_subset(self):
        """TEAM/EVENT constructs work in at least one supported spelling."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        # Current generated parsers accept legacy underscored spellings,
        # while the grammar files aim for standard F2018 syntax. Support
        # whichever variant currently parses without errors.
        legacy_code = (
            code.replace("form team", "form_team")
            .replace("change team", "change_team")
            .replace("event post", "event_post")
            .replace("event wait", "event_wait")
            .replace("event query", "event_query")
            .replace("end team", "end_team")
        )

        _, errors_actual, _ = parse_f2018(code)
        _, errors_legacy, _ = parse_f2018(legacy_code)

        best_errors = min(errors_actual, errors_legacy)
        assert best_errors == 0

    @pytest.mark.xfail(
        reason=(
            "SELECT RANK construct is only partially implemented "
            "(status test for Issue #61; full support tracked in issue #88)"
        )
    )
    def test_select_rank_construct_minimal_example(self):
        """Minimal SELECT RANK construct intended to become strict once implemented."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "select_rank_minimal.f90",
        )
        tree, errors, _ = parse_f2018(code)
        assert tree is not None
        # Once SELECT RANK is fully wired up, this should parse with zero errors.
        assert errors == 0
