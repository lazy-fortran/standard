#!/usr/bin/env python3
"""Semantic Validation Tests for F2018 Teams, Events, Collectives - Issue #192

Comprehensive test suite for Fortran 2018 teams, events, and collective
subroutine semantic validation per ISO/IEC 1539-1:2018:
- Team constructs and TEAM_TYPE declarations (Section 11.6)
- Event constructs and EVENT_TYPE declarations (Section 11.6.8)
- Collective subroutine calls (Sections 16.9.46-50)

These tests validate the semantic analysis layer on top of the ANTLR grammar,
ensuring that teams/events/collectives code conforms to the standard beyond
syntactic correctness.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2018_teams_events_validator import (
    F2018TeamsEventsValidator,
    F2018TeamsEventsValidationResult,
    DiagnosticSeverity,
    COLLECTIVE_TYPES,
    validate_teams_semantics,
    validate_events_semantics,
    validate_collective_semantics,
)
from fixture_utils import load_fixture


class TestF2018TeamsEventsValidatorBasic:
    """Basic tests for the teams/events/collectives validator infrastructure."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_empty_module_no_errors(self):
        """Empty module should have no semantic errors."""
        code = """
module empty_mod
    implicit none
end module empty_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Unexpected errors: {result.diagnostics}"

    def test_syntax_error_detected(self):
        """Syntax errors should be reported before semantic analysis."""
        code = """
module broken
    this is not valid fortran syntax!!!
end module broken
"""
        result = self.validator.validate_code(code)
        assert result.has_errors
        assert any(d.code == "SYNTAX_E001" for d in result.diagnostics)

    def test_validation_result_properties(self):
        """F2018TeamsEventsValidationResult should correctly report counts."""
        code = """
module test_mod
    implicit none
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert result.error_count == 0
        assert result.warning_count == 0
        assert not result.has_errors


class TestTeamDeclarationSemantics:
    """Semantic validation tests for TEAM_TYPE declarations (Section 11.6)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_team_type_declaration_detected(self):
        """TEAM_TYPE declaration should be detected and tracked."""
        code = """
program team_decl_test
    use, intrinsic :: iso_fortran_env
    implicit none
    type(team_type) :: my_team
end program team_decl_test
"""
        result = self.validator.validate_code(code)
        assert len(result.team_declarations) > 0

    def test_multiple_team_declarations(self):
        """Multiple TEAM_TYPE declarations should all be tracked."""
        code = """
program multi_team_test
    use, intrinsic :: iso_fortran_env
    implicit none
    type(team_type) :: team1
    type(team_type) :: team2
end program multi_team_test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestEventDeclarationSemantics:
    """Semantic validation tests for EVENT_TYPE declarations (Section 11.6.8)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_event_type_declaration_detected(self):
        """EVENT_TYPE declaration should be detected and tracked."""
        code = """
program event_decl_test
    use, intrinsic :: iso_fortran_env
    implicit none
    type(event_type) :: my_event
end program event_decl_test
"""
        result = self.validator.validate_code(code)
        assert len(result.event_declarations) > 0

    def test_multiple_event_declarations(self):
        """Multiple EVENT_TYPE declarations should all be tracked."""
        code = """
program multi_event_test
    use, intrinsic :: iso_fortran_env
    implicit none
    type(event_type) :: event1
    type(event_type) :: event2
end program multi_event_test
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestFormTeamSemantics:
    """Semantic validation tests for FORM TEAM statement (Section 11.6.9)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_form_team_statement_detected(self):
        """FORM TEAM statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.form_team_statements) > 0

    def test_form_team_with_new_index(self):
        """FORM TEAM with NEW_INDEX should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        form_teams_with_new_index = [
            s for s in result.form_team_statements if s.has_new_index
        ]
        assert len(form_teams_with_new_index) > 0

    def test_form_team_with_stat_errmsg(self):
        """FORM TEAM with STAT and ERRMSG should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors


class TestChangeTeamSemantics:
    """Semantic validation tests for CHANGE TEAM construct (Section 11.6.7)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_change_team_construct_detected(self):
        """CHANGE TEAM construct should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.change_team_constructs) > 0

    def test_change_team_with_stat(self):
        """CHANGE TEAM with STAT should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        change_teams_with_stat = [
            c for c in result.change_team_constructs if c.has_stat
        ]
        assert len(change_teams_with_stat) > 0


class TestEventStatementSemantics:
    """Semantic validation tests for event statements (Section 11.6.8)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_event_post_detected(self):
        """EVENT POST statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        event_posts = [
            e for e in result.event_statements if e.stmt_type == "event_post"
        ]
        assert len(event_posts) > 0

    def test_event_wait_detected(self):
        """EVENT WAIT statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        event_waits = [
            e for e in result.event_statements if e.stmt_type == "event_wait"
        ]
        assert len(event_waits) > 0

    def test_event_query_detected(self):
        """EVENT_QUERY statement should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        event_queries = [
            e for e in result.event_statements if e.stmt_type == "event_query"
        ]
        assert len(event_queries) > 0

    def test_event_wait_with_until_count(self):
        """EVENT WAIT with UNTIL_COUNT should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        event_waits_with_until = [
            e for e in result.event_statements
            if e.stmt_type == "event_wait" and e.has_until_count
        ]
        assert len(event_waits_with_until) > 0


class TestCollectiveSubroutineSemantics:
    """Semantic validation tests for collective subroutines (Sections 16.9.46-50)."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_co_sum_detected(self):
        """CO_SUM call should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        co_sums = [c for c in result.collective_calls if c.collective_type == "co_sum"]
        assert len(co_sums) > 0

    def test_co_min_detected(self):
        """CO_MIN call should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        co_mins = [c for c in result.collective_calls if c.collective_type == "co_min"]
        assert len(co_mins) > 0

    def test_co_max_detected(self):
        """CO_MAX call should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        co_maxs = [c for c in result.collective_calls if c.collective_type == "co_max"]
        assert len(co_maxs) > 0

    def test_co_reduce_detected(self):
        """CO_REDUCE call should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        co_reduces = [
            c for c in result.collective_calls if c.collective_type == "co_reduce"
        ]
        assert len(co_reduces) > 0

    def test_co_broadcast_detected(self):
        """CO_BROADCAST call should be detected and tracked."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        co_broadcasts = [
            c for c in result.collective_calls if c.collective_type == "co_broadcast"
        ]
        assert len(co_broadcasts) > 0

    def test_collective_with_stat(self):
        """Collective subroutine with STAT should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        collectives_with_stat = [c for c in result.collective_calls if c.has_stat]
        assert len(collectives_with_stat) > 0

    def test_collective_with_errmsg(self):
        """Collective subroutine with ERRMSG should be validated."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        collectives_with_errmsg = [c for c in result.collective_calls if c.has_errmsg]
        assert len(collectives_with_errmsg) > 0

    def test_collective_types_complete(self):
        """COLLECTIVE_TYPES should include all F2018 collective subroutines."""
        expected = {"co_sum", "co_min", "co_max", "co_reduce", "co_broadcast"}
        assert expected == COLLECTIVE_TYPES


class TestTeamEventCollectiveInteractions:
    """Tests for interactions between teams, events, and collectives."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_full_teams_events_collectives_program(self):
        """Full program with teams, events, and collectives should validate."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

        assert len(result.form_team_statements) > 0
        assert len(result.change_team_constructs) > 0
        assert len(result.event_statements) > 0
        assert len(result.collective_calls) > 0

    def test_collectives_outside_change_team_info(self):
        """Collectives outside CHANGE TEAM should produce INFO diagnostic."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        has_coll_info = any(
            d.code == "COLL_I001" for d in result.diagnostics
        )
        assert has_coll_info, "Expected COLL_I001 info for collectives outside team"


class TestConvenienceFunctions:
    """Tests for convenience validation functions."""

    def test_validate_teams_semantics_function(self):
        """validate_teams_semantics() should work as standalone function."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = validate_teams_semantics(code)
        assert not result.has_errors
        assert len(result.form_team_statements) > 0

    def test_validate_events_semantics_function(self):
        """validate_events_semantics() should work as standalone function."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_event_semantics.f90",
        )
        result = validate_events_semantics(code)
        assert not result.has_errors
        assert len(result.event_statements) > 0

    def test_validate_collective_semantics_function(self):
        """validate_collective_semantics() should work as standalone function."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = validate_collective_semantics(code)
        assert not result.has_errors
        assert len(result.collective_calls) > 0


class TestDiagnosticQuality:
    """Tests for diagnostic message quality and ISO references."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_diagnostic_has_severity(self):
        """All diagnostics should have a severity level."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.severity in DiagnosticSeverity

    def test_diagnostic_has_code(self):
        """All diagnostics should have a unique code."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.code is not None
            assert len(diag.code) > 0

    def test_diagnostic_has_message(self):
        """All diagnostics should have a descriptive message."""
        code = """
module broken
    invalid syntax here
end module broken
"""
        result = self.validator.validate_code(code)
        for diag in result.diagnostics:
            assert diag.message is not None
            assert len(diag.message) > 0


class TestISOComplianceValidation:
    """Tests verifying ISO/IEC 1539-1:2018 compliance checks."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_iso_section_references_in_diagnostics(self):
        """Diagnostics should reference ISO sections per standard."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        iso_refs = [d.iso_section for d in result.diagnostics if d.iso_section]
        assert len(iso_refs) > 0, "Expected ISO section references in diagnostics"

    def test_collective_diagnostics_reference_sections(self):
        """Collective-related diagnostics should cite ISO standard sections."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "teams_collectives_module.f90",
        )
        result = self.validator.validate_code(code)
        info_diags = [
            d for d in result.diagnostics
            if d.severity == DiagnosticSeverity.INFO
        ]
        assert len(info_diags) > 0, "Expected INFO diagnostics for collective code"
        assert any(
            d.iso_section for d in info_diags
        ), "Expected ISO section references in diagnostics"


class TestTeamSkeleton:
    """Tests using the team_skeleton_module.f90 fixture."""

    def setup_method(self):
        self.validator = F2018TeamsEventsValidator()

    def test_team_skeleton_parses_and_validates(self):
        """team_skeleton_module.f90 should parse and validate."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_team_skeleton_detects_form_team(self):
        """team_skeleton_module.f90 should have FORM TEAM detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.form_team_statements) > 0

    def test_team_skeleton_detects_change_team(self):
        """team_skeleton_module.f90 should have CHANGE TEAM detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.change_team_constructs) > 0

    def test_team_skeleton_detects_events(self):
        """team_skeleton_module.f90 should have event statements detected."""
        code = load_fixture(
            "Fortran2018",
            "test_issue61_teams_and_collectives",
            "team_skeleton_module.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.event_statements) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
