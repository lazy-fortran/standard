#!/usr/bin/env python3
"""Fortran 2018 Teams, Events, and Collectives Semantic Validator

Implements semantic validation for Fortran 2018 teams, events, and collective
coarray features per ISO/IEC 1539-1:2018 (Fortran 2018 standard).

This validator checks:
- Team constructs and TEAM_TYPE declarations (Section 11.6)
- Event constructs and EVENT_TYPE declarations (Section 11.6.8)
- Collective subroutine calls (Sections 16.9.46-50)
- Interactions between teams, events, and collectives

Reference: ISO/IEC 1539-1:2018 (Fortran 2018 International Standard)
"""

import re
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "modern")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser
from Fortran2018ParserListener import Fortran2018ParserListener


class DiagnosticSeverity(Enum):
    ERROR = auto()
    WARNING = auto()
    INFO = auto()


@dataclass
class SemanticDiagnostic:
    severity: DiagnosticSeverity
    code: str
    message: str
    line: Optional[int] = None
    column: Optional[int] = None
    iso_section: Optional[str] = None


@dataclass
class TeamDeclaration:
    """Represents a TEAM_TYPE variable declaration."""
    name: str
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class EventDeclaration:
    """Represents an EVENT_TYPE variable declaration."""
    name: str
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class FormTeamStatement:
    """Represents a FORM TEAM statement for analysis."""
    team_number_expr: str
    team_variable: str
    has_new_index: bool = False
    has_stat: bool = False
    has_errmsg: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ChangeTeamConstruct:
    """Represents a CHANGE TEAM construct for analysis."""
    team_value: str
    has_coarray_associations: bool = False
    has_stat: bool = False
    has_errmsg: bool = False
    in_pure_procedure: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class EventStatement:
    """Represents an event statement (POST/WAIT/QUERY) for analysis."""
    stmt_type: str
    event_variable: str
    has_stat: bool = False
    has_errmsg: bool = False
    has_until_count: bool = False
    has_count: bool = False
    in_pure_procedure: bool = False
    in_change_team: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class CollectiveCall:
    """Represents a collective subroutine call for analysis."""
    collective_type: str
    variable: str
    has_stat: bool = False
    has_errmsg: bool = False
    has_result_image: bool = False
    has_source_image: bool = False
    has_operation: bool = False
    in_pure_procedure: bool = False
    in_change_team: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class F2018TeamsEventsValidationResult:
    """Results from F2018 teams/events/collectives semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    team_declarations: Dict[str, TeamDeclaration] = field(default_factory=dict)
    event_declarations: Dict[str, EventDeclaration] = field(default_factory=dict)
    form_team_statements: List[FormTeamStatement] = field(default_factory=list)
    change_team_constructs: List[ChangeTeamConstruct] = field(default_factory=list)
    event_statements: List[EventStatement] = field(default_factory=list)
    collective_calls: List[CollectiveCall] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        return any(d.severity == DiagnosticSeverity.ERROR for d in self.diagnostics)

    @property
    def error_count(self) -> int:
        return sum(
            1 for d in self.diagnostics if d.severity == DiagnosticSeverity.ERROR
        )

    @property
    def warning_count(self) -> int:
        return sum(
            1 for d in self.diagnostics if d.severity == DiagnosticSeverity.WARNING
        )


COLLECTIVE_TYPES: Set[str] = {
    "co_sum", "co_min", "co_max", "co_reduce", "co_broadcast",
}


class F2018TeamsEventsListener(Fortran2018ParserListener):
    """ANTLR listener for F2018 teams/events/collectives semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = F2018TeamsEventsValidationResult()
        self._in_pure_procedure = False
        self._in_change_team = False
        self._change_team_depth = 0
        self._current_team_var: Optional[str] = None
        self._current_event_var: Optional[str] = None

    def _get_token_text(self, ctx) -> str:
        if ctx is None:
            return ""
        return ctx.getText().lower()

    def _get_location(self, ctx) -> Tuple[Optional[int], Optional[int]]:
        if ctx is None:
            return None, None
        if hasattr(ctx, "start") and ctx.start:
            return ctx.start.line, ctx.start.column
        return None, None

    def _add_diagnostic(
        self,
        severity: DiagnosticSeverity,
        code: str,
        message: str,
        ctx=None,
        iso_section: Optional[str] = None,
    ):
        line, column = self._get_location(ctx)
        self.result.diagnostics.append(
            SemanticDiagnostic(
                severity=severity,
                code=code,
                message=message,
                line=line,
                column=column,
                iso_section=iso_section,
            )
        )

    def enterFunction_stmt_f2018(self, ctx):
        """Track PURE function context."""
        text = self._get_token_text(ctx)
        if "pure" in text or "elemental" in text:
            self._in_pure_procedure = True

    def exitEnd_function_stmt(self, ctx):
        """Exit function context."""
        self._in_pure_procedure = False

    def enterSubroutine_stmt_f2018(self, ctx):
        """Track PURE subroutine context."""
        text = self._get_token_text(ctx)
        if "pure" in text or "elemental" in text:
            self._in_pure_procedure = True

    def exitEnd_subroutine_stmt(self, ctx):
        """Exit subroutine context."""
        self._in_pure_procedure = False

    def enterTeam_declaration_stmt(self, ctx):
        """Process TEAM_TYPE declaration."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)
        names = re.findall(r"::\s*(\w+)", text)
        for name in names:
            decl = TeamDeclaration(name=name, line=line, column=col)
            self.result.team_declarations[name] = decl

    def enterEvent_declaration_stmt(self, ctx):
        """Process EVENT_TYPE declaration."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)
        names = re.findall(r"::\s*(\w+)", text)
        for name in names:
            decl = EventDeclaration(name=name, line=line, column=col)
            self.result.event_declarations[name] = decl

    def enterForm_team_stmt(self, ctx):
        """Process FORM TEAM statement per ISO/IEC 1539-1:2018 Section 11.6.9."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        team_match = re.search(
            r"form\s*team\s*\(\s*([^,]+)\s*,\s*(\w+)", text
        )
        team_num = team_match.group(1).strip() if team_match else ""
        team_var = team_match.group(2) if team_match else ""

        stmt = FormTeamStatement(
            team_number_expr=team_num,
            team_variable=team_var,
            has_new_index="new_index" in text,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            line=line,
            column=col,
        )
        self.result.form_team_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "TEAM_E001",
                "FORM TEAM statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2018 Section 11.6.9, FORM TEAM is an image "
                "control statement and shall not appear in a pure subprogram.",
                ctx,
                "11.6.9",
            )

        self._validate_form_team_statement(stmt, ctx)

    def _validate_form_team_statement(self, stmt: FormTeamStatement, ctx):
        """Validate FORM TEAM statement per ISO/IEC 1539-1:2018."""
        if stmt.team_variable:
            if stmt.team_variable not in self.result.team_declarations:
                self._add_diagnostic(
                    DiagnosticSeverity.INFO,
                    "TEAM_I001",
                    f"Team variable '{stmt.team_variable}' in FORM TEAM may "
                    "reference a TEAM_TYPE variable declared in another scope. "
                    "Per ISO/IEC 1539-1:2018 Section 11.6.9, the team-variable "
                    "shall be a scalar variable of type TEAM_TYPE.",
                    ctx,
                    "11.6.9",
                )

    def enterChange_team_construct(self, ctx):
        """Enter CHANGE TEAM construct per ISO/IEC 1539-1:2018 Section 11.6.7."""
        self._in_change_team = True
        self._change_team_depth += 1

    def exitChange_team_construct(self, ctx):
        """Exit CHANGE TEAM construct."""
        if self._change_team_depth > 0:
            self._change_team_depth -= 1
        if self._change_team_depth == 0:
            self._in_change_team = False

    def enterChange_team_stmt(self, ctx):
        """Process CHANGE TEAM statement per ISO/IEC 1539-1:2018 Section 11.6.7."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        team_match = re.search(r"change\s*team\s*\(\s*([^,)]+)", text)
        team_val = team_match.group(1).strip() if team_match else ""

        construct = ChangeTeamConstruct(
            team_value=team_val,
            has_coarray_associations="=>" in text,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            in_pure_procedure=self._in_pure_procedure,
            line=line,
            column=col,
        )
        self.result.change_team_constructs.append(construct)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "TEAM_E002",
                "CHANGE TEAM statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2018 Section 11.6.7, CHANGE TEAM is an "
                "image control statement and shall not appear in a pure "
                "subprogram.",
                ctx,
                "11.6.7",
            )

        if self._change_team_depth > 1:
            self._add_diagnostic(
                DiagnosticSeverity.INFO,
                "TEAM_I002",
                "Nested CHANGE TEAM construct detected. Per ISO/IEC 1539-1:2018 "
                "Section 11.6.7, nested CHANGE TEAM constructs create a team "
                "hierarchy that must be carefully managed.",
                ctx,
                "11.6.7",
            )

    def enterEnd_team_stmt(self, ctx):
        """Process END TEAM statement per ISO/IEC 1539-1:2018 Section 11.6.7."""
        text = self._get_token_text(ctx)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "TEAM_E003",
                "END TEAM statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2018 Section 11.6.7, END TEAM is part of "
                "an image control construct and shall not appear in a pure "
                "subprogram.",
                ctx,
                "11.6.7",
            )

    def enterEvent_post_stmt(self, ctx):
        """Process EVENT POST statement per ISO/IEC 1539-1:2018 Section 11.6.8."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        event_match = re.search(r"event\s*post\s*\(\s*(\w+)", text)
        event_var = event_match.group(1) if event_match else ""

        stmt = EventStatement(
            stmt_type="event_post",
            event_variable=event_var,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.event_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "EVENT_E001",
                "EVENT POST statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2018 Section 11.6.8, EVENT POST is an "
                "image control statement and shall not appear in a pure "
                "subprogram.",
                ctx,
                "11.6.8",
            )

        self._validate_event_statement(stmt, ctx)

    def enterEvent_wait_stmt(self, ctx):
        """Process EVENT WAIT statement per ISO/IEC 1539-1:2018 Section 11.6.8."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        event_match = re.search(r"event\s*wait\s*\(\s*(\w+)", text)
        event_var = event_match.group(1) if event_match else ""

        stmt = EventStatement(
            stmt_type="event_wait",
            event_variable=event_var,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            has_until_count="until_count" in text,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.event_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "EVENT_E002",
                "EVENT WAIT statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2018 Section 11.6.8, EVENT WAIT is an "
                "image control statement and shall not appear in a pure "
                "subprogram.",
                ctx,
                "11.6.8",
            )

        self._validate_event_statement(stmt, ctx)

    def enterEvent_query_stmt(self, ctx):
        """Process EVENT_QUERY per ISO/IEC 1539-1:2018 Section 16.9.72."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        event_match = re.search(r"event\s*query\s*\(\s*(\w+)", text)
        event_var = event_match.group(1) if event_match else ""

        stmt = EventStatement(
            stmt_type="event_query",
            event_variable=event_var,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            has_count="count=" in text or "count =" in text,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.event_statements.append(stmt)

        self._validate_event_statement(stmt, ctx)

    def _validate_event_statement(self, stmt: EventStatement, ctx):
        """Validate event statement per ISO/IEC 1539-1:2018."""
        if stmt.event_variable:
            if stmt.event_variable not in self.result.event_declarations:
                self._add_diagnostic(
                    DiagnosticSeverity.INFO,
                    "EVENT_I001",
                    f"Event variable '{stmt.event_variable}' may reference an "
                    "EVENT_TYPE variable declared in another scope. Per "
                    "ISO/IEC 1539-1:2018 Section 11.6.8, the event-variable "
                    "shall be a scalar variable of type EVENT_TYPE.",
                    ctx,
                    "11.6.8",
                )

    def enterCo_sum_stmt(self, ctx):
        """Process CO_SUM call per ISO/IEC 1539-1:2018 Section 16.9.50."""
        self._process_collective_call(ctx, "co_sum")

    def enterCo_min_stmt(self, ctx):
        """Process CO_MIN call per ISO/IEC 1539-1:2018 Section 16.9.48."""
        self._process_collective_call(ctx, "co_min")

    def enterCo_max_stmt(self, ctx):
        """Process CO_MAX call per ISO/IEC 1539-1:2018 Section 16.9.47."""
        self._process_collective_call(ctx, "co_max")

    def enterCo_reduce_stmt(self, ctx):
        """Process CO_REDUCE call per ISO/IEC 1539-1:2018 Section 16.9.49."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        var_match = re.search(r"co_reduce\s*\(\s*(\w+)", text)
        variable = var_match.group(1) if var_match else ""

        call = CollectiveCall(
            collective_type="co_reduce",
            variable=variable,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            has_result_image="result_image=" in text or "result_image =" in text,
            has_operation=True,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.collective_calls.append(call)

        self._validate_collective_call(call, ctx, "16.9.49")

    def enterCo_broadcast_stmt(self, ctx):
        """Process CO_BROADCAST call per ISO/IEC 1539-1:2018 Section 16.9.46."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        var_match = re.search(r"co_broadcast\s*\(\s*(\w+)", text)
        variable = var_match.group(1) if var_match else ""

        call = CollectiveCall(
            collective_type="co_broadcast",
            variable=variable,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            has_source_image="source_image=" in text or "source_image =" in text,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.collective_calls.append(call)

        if not call.has_source_image:
            self._add_diagnostic(
                DiagnosticSeverity.WARNING,
                "COLL_W001",
                "CO_BROADCAST call without SOURCE_IMAGE argument. Per "
                "ISO/IEC 1539-1:2018 Section 16.9.46, SOURCE_IMAGE is a "
                "required argument that specifies the source image.",
                ctx,
                "16.9.46",
            )

        self._validate_collective_call(call, ctx, "16.9.46")

    def _process_collective_call(self, ctx, collective_type: str):
        """Process a collective subroutine call."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        var_match = re.search(rf"{collective_type}\s*\(\s*(\w+)", text)
        variable = var_match.group(1) if var_match else ""

        section_map = {
            "co_sum": "16.9.50",
            "co_min": "16.9.48",
            "co_max": "16.9.47",
        }

        call = CollectiveCall(
            collective_type=collective_type,
            variable=variable,
            has_stat="stat=" in text or "stat =" in text,
            has_errmsg="errmsg=" in text or "errmsg =" in text,
            has_result_image="result_image=" in text or "result_image =" in text,
            in_pure_procedure=self._in_pure_procedure,
            in_change_team=self._in_change_team,
            line=line,
            column=col,
        )
        self.result.collective_calls.append(call)

        self._validate_collective_call(call, ctx, section_map.get(collective_type, ""))

    def _validate_collective_call(
        self, call: CollectiveCall, ctx, iso_section: str
    ):
        """Validate collective subroutine call per ISO/IEC 1539-1:2018."""
        if call.in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "COLL_E001",
                f"{call.collective_type.upper()} call is not permitted in a "
                "PURE procedure. Per ISO/IEC 1539-1:2018, collective "
                "subroutines perform inter-image communication and shall "
                "not appear in a pure subprogram.",
                ctx,
                iso_section,
            )


class F2018TeamsEventsValidator:
    """Semantic validator for Fortran 2018 teams, events, and collectives."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> F2018TeamsEventsValidationResult:
        """Validate Fortran 2018 code for teams/events/collectives semantics."""
        input_stream = InputStream(code)
        self._lexer = Fortran2018Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2018Parser(token_stream)

        tree = self._parser.program_unit_f2018()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = F2018TeamsEventsValidationResult()

        if syntax_errors > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="SYNTAX_E001",
                    message=f"Code contains {syntax_errors} syntax error(s). "
                    "Fix syntax errors before semantic validation.",
                )
            )
            return result

        listener = F2018TeamsEventsListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_cross_entity_validation(listener.result)

        return listener.result

    def _perform_cross_entity_validation(
        self, result: F2018TeamsEventsValidationResult
    ):
        """Perform validation that requires cross-referencing entities."""
        self._validate_team_event_interactions(result)
        self._validate_collective_team_interactions(result)

    def _validate_team_event_interactions(
        self, result: F2018TeamsEventsValidationResult
    ):
        """Validate interactions between teams and events."""
        team_count = len(result.team_declarations)
        event_count = len(result.event_declarations)
        change_team_count = len(result.change_team_constructs)

        if event_count > 0 and change_team_count > 0:
            events_in_team = [
                e for e in result.event_statements if e.in_change_team
            ]
            if events_in_team:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="TEAM_EVENT_I001",
                        message="Event statements within CHANGE TEAM constructs "
                        "detected. Per ISO/IEC 1539-1:2018 Section 11.6, events "
                        "used within teams operate on the current team context.",
                        iso_section="11.6",
                    )
                )

    def _validate_collective_team_interactions(
        self, result: F2018TeamsEventsValidationResult
    ):
        """Validate interactions between collectives and teams."""
        change_team_count = len(result.change_team_constructs)
        collective_count = len(result.collective_calls)

        if collective_count > 0 and change_team_count > 0:
            collectives_in_team = [
                c for c in result.collective_calls if c.in_change_team
            ]
            if collectives_in_team:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="TEAM_COLL_I001",
                        message="Collective subroutine calls within CHANGE TEAM "
                        "constructs detected. Per ISO/IEC 1539-1:2018 Sections "
                        "11.6 and 16.9.46-50, collectives operate on the "
                        "current team by default.",
                        iso_section="11.6",
                    )
                )

        if collective_count > 0 and change_team_count == 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="COLL_I001",
                    message=f"Code contains {collective_count} collective "
                    "subroutine call(s) outside any CHANGE TEAM construct. "
                    "Per ISO/IEC 1539-1:2018 Section 16.9.46-50, these "
                    "collectives operate on the initial team (all images).",
                    iso_section="16.9.46",
                )
            )

    def validate_file(self, filepath: str) -> F2018TeamsEventsValidationResult:
        """Validate a Fortran 2018 file for teams/events/collectives."""
        path = Path(filepath)
        if not path.exists():
            result = F2018TeamsEventsValidationResult()
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FILE_E001",
                    message=f"File not found: {filepath}",
                )
            )
            return result

        code = path.read_text()
        return self.validate_code(code)


def validate_teams_semantics(code: str) -> F2018TeamsEventsValidationResult:
    """Convenience function to validate team semantics."""
    validator = F2018TeamsEventsValidator()
    return validator.validate_code(code)


def validate_events_semantics(code: str) -> F2018TeamsEventsValidationResult:
    """Convenience function to validate event semantics."""
    validator = F2018TeamsEventsValidator()
    return validator.validate_code(code)


def validate_collective_semantics(code: str) -> F2018TeamsEventsValidationResult:
    """Convenience function to validate collective subroutine semantics."""
    validator = F2018TeamsEventsValidator()
    return validator.validate_code(code)
