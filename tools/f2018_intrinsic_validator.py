#!/usr/bin/env python3
"""Fortran 2018 Intrinsic Procedures Semantic Validator

Implements semantic validation for Fortran 2018 new and extended intrinsic
procedures per ISO/IEC 1539-1:2018 (Fortran 2018 standard).

This validator checks:
- Image status functions (Section 16.9.73, 16.9.81, 16.9.182)
- Collective functions (Section 16.9.52, 16.9.187)
- RANDOM_INIT subroutine (Section 16.9.152)
- REDUCE function (Section 16.9.161)
- OUT_OF_RANGE function (Section 16.9.140)

Reference: ISO/IEC 1539-1:2018 (Fortran 2018 International Standard)
"""

import re
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, List, Optional, Set


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
class IntrinsicCallInfo:
    """Represents an intrinsic procedure call for analysis."""
    name: str
    arguments: List[str] = field(default_factory=list)
    keyword_args: Dict[str, str] = field(default_factory=dict)
    in_pure_procedure: bool = False
    in_elemental_procedure: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ProcedureScope:
    """Represents a procedure scope for tracking PURE/ELEMENTAL context."""
    name: str
    is_pure: bool = False
    is_elemental: bool = False
    start_line: int = 0
    end_line: Optional[int] = None


@dataclass
class F2018IntrinsicValidationResult:
    """Results from F2018 intrinsic procedure semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    intrinsic_calls: List[IntrinsicCallInfo] = field(default_factory=list)
    image_status_calls: List[IntrinsicCallInfo] = field(default_factory=list)
    collective_function_calls: List[IntrinsicCallInfo] = field(default_factory=list)
    random_init_calls: List[IntrinsicCallInfo] = field(default_factory=list)
    reduce_calls: List[IntrinsicCallInfo] = field(default_factory=list)
    out_of_range_calls: List[IntrinsicCallInfo] = field(default_factory=list)

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


class F2018IntrinsicValidator:
    """Semantic validator for Fortran 2018 intrinsic procedures.

    Uses regex-based detection to identify F2018 intrinsic procedure calls
    in Fortran source code and validates them per ISO/IEC 1539-1:2018.
    """

    def __init__(self):
        self._procedure_scopes: List[ProcedureScope] = []

    def validate_code(self, code: str) -> F2018IntrinsicValidationResult:
        """Validate Fortran 2018 code for intrinsic procedure semantics."""
        result = F2018IntrinsicValidationResult()
        lines = code.split("\n")

        self._build_procedure_scopes(lines)

        self._detect_image_status_calls(lines, result)
        self._detect_collective_function_calls(lines, result)
        self._detect_random_init_calls(lines, result)
        self._detect_reduce_calls(lines, result)
        self._detect_out_of_range_calls(lines, result)

        self._add_summary_diagnostics(result)

        return result

    def _build_procedure_scopes(self, lines: List[str]):
        """Build procedure scopes to track PURE/ELEMENTAL context."""
        self._procedure_scopes = []

        pure_subroutine_pattern = re.compile(
            r"^\s*(pure|elemental)\s+(recursive\s+)?(subroutine|function)\s+(\w+)",
            re.IGNORECASE,
        )
        end_pattern = re.compile(
            r"^\s*end\s+(subroutine|function)(\s+\w+)?", re.IGNORECASE
        )

        scope_stack: List[ProcedureScope] = []

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = pure_subroutine_pattern.search(line_lower)
            if match:
                prefix = match.group(1).lower()
                is_pure = prefix in ("pure", "elemental")
                is_elemental = prefix == "elemental"
                name = match.group(4)
                scope = ProcedureScope(
                    name=name,
                    is_pure=is_pure,
                    is_elemental=is_elemental,
                    start_line=i,
                )
                scope_stack.append(scope)
                continue

            end_match = end_pattern.search(line_lower)
            if end_match and scope_stack:
                scope = scope_stack.pop()
                scope.end_line = i
                self._procedure_scopes.append(scope)

    def _is_in_pure_procedure(self, line_num: int) -> bool:
        """Check if line is inside a PURE procedure."""
        for scope in self._procedure_scopes:
            if scope.start_line <= line_num:
                if scope.end_line is None or line_num <= scope.end_line:
                    if scope.is_pure:
                        return True
        return False

    def _extract_keyword_args(self, text: str) -> Dict[str, str]:
        """Extract keyword arguments from call text."""
        kwargs = {}
        patterns = [
            (r"team\s*=\s*(\w+)", "team"),
            (r"kind\s*=\s*(\w+)", "kind"),
            (r"dim\s*=\s*(\w+)", "dim"),
            (r"mask\s*=\s*(\w+)", "mask"),
            (r"repeatable\s*=\s*(\.\w+\.|\w+)", "repeatable"),
            (r"image_distinct\s*=\s*(\.\w+\.|\w+)", "image_distinct"),
            (r"round\s*=\s*(\.\w+\.|\w+)", "round"),
            (r"identity\s*=\s*(\w+)", "identity"),
            (r"ordered\s*=\s*(\.\w+\.|\w+)", "ordered"),
            (r"array\s*=\s*(\w+)", "array"),
            (r"operation\s*=\s*(\w+)", "operation"),
            (r"x\s*=\s*([^\s,\)]+)", "x"),
            (r"mold\s*=\s*(\w+)", "mold"),
        ]
        for pattern, key in patterns:
            match = re.search(pattern, text, re.IGNORECASE)
            if match:
                kwargs[key] = match.group(1)
        return kwargs

    def _count_positional_args(self, text: str, intrinsic_name: str) -> int:
        """Count positional arguments in an intrinsic call.

        Uses lightweight regex matching to extract the argument list and count
        comma-separated items that are not keyword arguments.
        """
        pattern = re.compile(
            rf"\b{intrinsic_name}\s*\(([^)]*)\)", re.IGNORECASE
        )
        match = pattern.search(text)
        if not match:
            return 0

        args_text = match.group(1).strip()
        if not args_text:
            return 0

        args = []
        depth = 0
        current = ""
        for char in args_text:
            if char == "(":
                depth += 1
                current += char
            elif char == ")":
                depth -= 1
                current += char
            elif char == "," and depth == 0:
                args.append(current.strip())
                current = ""
            else:
                current += char
        if current.strip():
            args.append(current.strip())

        positional_count = 0
        for arg in args:
            if "=" not in arg:
                positional_count += 1
        return positional_count

    def _detect_image_status_calls(
        self, lines: List[str], result: F2018IntrinsicValidationResult
    ):
        """Detect IMAGE_STATUS, FAILED_IMAGES, STOPPED_IMAGES calls."""
        image_status_pattern = re.compile(
            r"\bimage_status\s*\(", re.IGNORECASE
        )
        failed_images_pattern = re.compile(
            r"\bfailed_images\s*\(", re.IGNORECASE
        )
        stopped_images_pattern = re.compile(
            r"\bstopped_images\s*\(", re.IGNORECASE
        )

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = image_status_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                call_info = IntrinsicCallInfo(
                    name="image_status",
                    keyword_args=self._extract_keyword_args(line_lower),
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.image_status_calls.append(call_info)
                result.intrinsic_calls.append(call_info)
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="INTR_I001",
                        message="IMAGE_STATUS intrinsic detected. Per "
                        "ISO/IEC 1539-1:2018 Section 16.9.81, "
                        "IMAGE_STATUS(IMAGE [, TEAM]) returns the execution "
                        "status of the specified image.",
                        line=i,
                        column=col,
                        iso_section="16.9.81",
                    )
                )

            match = failed_images_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                call_info = IntrinsicCallInfo(
                    name="failed_images",
                    keyword_args=self._extract_keyword_args(line_lower),
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.image_status_calls.append(call_info)
                result.intrinsic_calls.append(call_info)
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="INTR_I002",
                        message="FAILED_IMAGES intrinsic detected. Per "
                        "ISO/IEC 1539-1:2018 Section 16.9.73, "
                        "FAILED_IMAGES([TEAM, KIND]) returns an array of "
                        "image indices that have failed.",
                        line=i,
                        column=col,
                        iso_section="16.9.73",
                    )
                )

            match = stopped_images_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                call_info = IntrinsicCallInfo(
                    name="stopped_images",
                    keyword_args=self._extract_keyword_args(line_lower),
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.image_status_calls.append(call_info)
                result.intrinsic_calls.append(call_info)
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="INTR_I003",
                        message="STOPPED_IMAGES intrinsic detected. Per "
                        "ISO/IEC 1539-1:2018 Section 16.9.182, "
                        "STOPPED_IMAGES([TEAM, KIND]) returns an array of "
                        "image indices that have stopped.",
                        line=i,
                        column=col,
                        iso_section="16.9.182",
                    )
                )

    def _detect_collective_function_calls(
        self, lines: List[str], result: F2018IntrinsicValidationResult
    ):
        """Detect COSHAPE and TEAM_NUMBER calls."""
        coshape_pattern = re.compile(r"\bcoshape\s*\(", re.IGNORECASE)
        team_number_pattern = re.compile(r"\bteam_number\s*\(", re.IGNORECASE)

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = coshape_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                call_info = IntrinsicCallInfo(
                    name="coshape",
                    keyword_args=self._extract_keyword_args(line_lower),
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.collective_function_calls.append(call_info)
                result.intrinsic_calls.append(call_info)
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="INTR_I004",
                        message="COSHAPE intrinsic detected. Per "
                        "ISO/IEC 1539-1:2018 Section 16.9.52, "
                        "COSHAPE(COARRAY [, KIND]) returns the sizes "
                        "of the codimensions of a coarray.",
                        line=i,
                        column=col,
                        iso_section="16.9.52",
                    )
                )

            match = team_number_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                call_info = IntrinsicCallInfo(
                    name="team_number",
                    keyword_args=self._extract_keyword_args(line_lower),
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.collective_function_calls.append(call_info)
                result.intrinsic_calls.append(call_info)
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="INTR_I005",
                        message="TEAM_NUMBER intrinsic detected. Per "
                        "ISO/IEC 1539-1:2018 Section 16.9.187, "
                        "TEAM_NUMBER([TEAM]) returns the team number "
                        "of the specified team.",
                        line=i,
                        column=col,
                        iso_section="16.9.187",
                    )
                )

    def _detect_random_init_calls(
        self, lines: List[str], result: F2018IntrinsicValidationResult
    ):
        """Detect RANDOM_INIT calls and validate per ISO Section 16.9.152."""
        random_init_pattern = re.compile(
            r"\bcall\s+random_init\s*\(", re.IGNORECASE
        )

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = random_init_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                kwargs = self._extract_keyword_args(line_lower)
                in_pure = self._is_in_pure_procedure(i)

                call_info = IntrinsicCallInfo(
                    name="random_init",
                    keyword_args=kwargs,
                    in_pure_procedure=in_pure,
                    line=i,
                    column=col,
                )
                result.random_init_calls.append(call_info)
                result.intrinsic_calls.append(call_info)

                if in_pure:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="INTR_E001",
                            message="RANDOM_INIT is not permitted in a PURE "
                            "procedure. Per ISO/IEC 1539-1:2018 Section "
                            "16.9.152, RANDOM_INIT is an impure subroutine "
                            "that modifies the random number generator state "
                            "and shall not appear in a pure subprogram.",
                            line=i,
                            column=col,
                            iso_section="16.9.152",
                        )
                    )

                has_repeatable = "repeatable" in kwargs
                has_image_distinct = "image_distinct" in kwargs

                if not has_repeatable:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="INTR_E002",
                            message="RANDOM_INIT call missing required "
                            "REPEATABLE argument. Per ISO/IEC 1539-1:2018 "
                            "Section 16.9.152, REPEATABLE is a mandatory "
                            "argument (INTENT(IN), LOGICAL) that specifies "
                            "whether the random sequence is repeatable. "
                            "NON-COMPLIANT: Missing required argument.",
                            line=i,
                            column=col,
                            iso_section="16.9.152",
                        )
                    )

                if not has_image_distinct:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="INTR_E003",
                            message="RANDOM_INIT call missing required "
                            "IMAGE_DISTINCT argument. Per ISO/IEC 1539-1:2018 "
                            "Section 16.9.152, IMAGE_DISTINCT is a mandatory "
                            "argument (INTENT(IN), LOGICAL) that specifies "
                            "whether different images get different random "
                            "sequences. NON-COMPLIANT: Missing required argument.",
                            line=i,
                            column=col,
                            iso_section="16.9.152",
                        )
                    )

                if has_repeatable and has_image_distinct:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.INFO,
                            code="INTR_I006",
                            message="RANDOM_INIT call with both required "
                            "arguments detected. Per ISO/IEC 1539-1:2018 "
                            "Section 16.9.152, REPEATABLE and IMAGE_DISTINCT "
                            "control random sequence behavior.",
                            line=i,
                            column=col,
                            iso_section="16.9.152",
                        )
                    )

    def _detect_reduce_calls(
        self, lines: List[str], result: F2018IntrinsicValidationResult
    ):
        """Detect REDUCE intrinsic calls and validate per ISO Section 16.9.161."""
        reduce_pattern = re.compile(r"\breduce\s*\(", re.IGNORECASE)

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = reduce_pattern.search(line_lower)
            if not match:
                continue

            col = match.start() + 1
            kwargs = self._extract_keyword_args(line_lower)
            positional_count = self._count_positional_args(line_lower, "reduce")
            call_info = IntrinsicCallInfo(
                name="reduce",
                keyword_args=kwargs,
                in_pure_procedure=self._is_in_pure_procedure(i),
                line=i,
                column=col,
            )
            result.reduce_calls.append(call_info)
            result.intrinsic_calls.append(call_info)

            self._add_reduce_diagnostics(
                result=result,
                line_num=i,
                column=col,
                kwargs=kwargs,
                positional_count=positional_count,
            )

    def _add_reduce_diagnostics(
        self,
        result: F2018IntrinsicValidationResult,
        line_num: int,
        column: int,
        kwargs: Dict[str, str],
        positional_count: int,
    ):
        """Add diagnostics for a single REDUCE intrinsic call."""
        has_array = "array" in kwargs or positional_count >= 1
        has_operation = "operation" in kwargs or positional_count >= 2

        if not has_array:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="INTR_E004",
                    message="REDUCE call missing required ARRAY argument. "
                    "Per ISO/IEC 1539-1:2018 Section 16.9.161, ARRAY is a "
                    "mandatory argument that shall be an array of any type. "
                    "NON-COMPLIANT: Missing required argument.",
                    line=line_num,
                    column=column,
                    iso_section="16.9.161",
                )
            )

        if not has_operation:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="INTR_E005",
                    message="REDUCE call missing required OPERATION argument. "
                    "Per ISO/IEC 1539-1:2018 Section 16.9.161, OPERATION is "
                    "a mandatory argument that shall be a pure function with "
                    "two arguments of the same type as ARRAY elements. "
                    "NON-COMPLIANT: Missing required argument.",
                    line=line_num,
                    column=column,
                    iso_section="16.9.161",
                )
            )

        if has_array and has_operation:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_I008",
                    message="REDUCE intrinsic detected. Per ISO/IEC "
                    "1539-1:2018 Section 16.9.161, REDUCE(ARRAY, OPERATION "
                    "[, DIM] [, MASK] [, IDENTITY] [, ORDERED]) performs a "
                    "user-defined reduction.",
                    line=line_num,
                    column=column,
                    iso_section="16.9.161",
                )
            )

        if "dim" in kwargs:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_I009",
                    message="REDUCE with DIM argument reduces along a "
                    "specific dimension. Per ISO/IEC 1539-1:2018 Section "
                    "16.9.161, when DIM is present, the reduction is "
                    "performed along that dimension.",
                    line=line_num,
                    column=column,
                    iso_section="16.9.161",
                )
            )

        if "mask" in kwargs:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_I010",
                    message="REDUCE with MASK argument applies reduction "
                    "conditionally. Per ISO/IEC 1539-1:2018 Section "
                    "16.9.161, MASK selects which elements participate in "
                    "the reduction.",
                    line=line_num,
                    column=column,
                    iso_section="16.9.161",
                )
            )

    def _detect_out_of_range_calls(
        self, lines: List[str], result: F2018IntrinsicValidationResult
    ):
        """Detect OUT_OF_RANGE intrinsic calls and validate per ISO Section 16.9.140."""
        out_of_range_pattern = re.compile(
            r"\bout_of_range\s*\(", re.IGNORECASE
        )

        for i, line in enumerate(lines, start=1):
            line_lower = line.lower()

            match = out_of_range_pattern.search(line_lower)
            if match:
                col = match.start() + 1
                kwargs = self._extract_keyword_args(line_lower)
                positional_count = self._count_positional_args(
                    line_lower, "out_of_range"
                )
                call_info = IntrinsicCallInfo(
                    name="out_of_range",
                    keyword_args=kwargs,
                    in_pure_procedure=self._is_in_pure_procedure(i),
                    line=i,
                    column=col,
                )
                result.out_of_range_calls.append(call_info)
                result.intrinsic_calls.append(call_info)

                has_x = "x" in kwargs or positional_count >= 1
                has_mold = "mold" in kwargs or positional_count >= 2

                if not has_x:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="INTR_E006",
                            message="OUT_OF_RANGE call missing required X "
                            "argument. Per ISO/IEC 1539-1:2018 Section "
                            "16.9.140, X is a mandatory argument that shall "
                            "be of type integer or real. "
                            "NON-COMPLIANT: Missing required argument.",
                            line=i,
                            column=col,
                            iso_section="16.9.140",
                        )
                    )

                if not has_mold:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="INTR_E007",
                            message="OUT_OF_RANGE call missing required MOLD "
                            "argument. Per ISO/IEC 1539-1:2018 Section "
                            "16.9.140, MOLD is a mandatory argument that "
                            "shall be of type integer or real and specifies "
                            "the target type for the conversion test. "
                            "NON-COMPLIANT: Missing required argument.",
                            line=i,
                            column=col,
                            iso_section="16.9.140",
                        )
                    )

                if has_x and has_mold:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.INFO,
                            code="INTR_I007",
                            message="OUT_OF_RANGE intrinsic detected. Per "
                            "ISO/IEC 1539-1:2018 Section 16.9.140, "
                            "OUT_OF_RANGE(X, MOLD [, ROUND]) tests whether X "
                            "can be converted to the type of MOLD without "
                            "overflow.",
                            line=i,
                            column=col,
                            iso_section="16.9.140",
                        )
                    )

                if "round" in kwargs:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.INFO,
                            code="INTR_I011",
                            message="OUT_OF_RANGE with ROUND argument. Per "
                            "ISO/IEC 1539-1:2018 Section 16.9.140, ROUND is "
                            "an optional LOGICAL argument that specifies "
                            "whether rounding should be considered when "
                            "testing for overflow.",
                            line=i,
                            column=col,
                            iso_section="16.9.140",
                        )
                    )

    def _add_summary_diagnostics(self, result: F2018IntrinsicValidationResult):
        """Add summary diagnostics for intrinsic usage patterns."""
        image_count = len(result.image_status_calls)
        collective_count = len(result.collective_function_calls)
        random_init_count = len(result.random_init_calls)
        reduce_count = len(result.reduce_calls)
        out_of_range_count = len(result.out_of_range_calls)

        if image_count > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_SUMM_I001",
                    message=f"Code contains {image_count} image status "
                    "intrinsic call(s). Per ISO/IEC 1539-1:2018 Sections "
                    "16.9.73, 16.9.81, and 16.9.182, these intrinsics "
                    "provide coarray image execution status information.",
                    iso_section="16.9",
                )
            )

        if collective_count > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_SUMM_I002",
                    message=f"Code contains {collective_count} collective "
                    "function call(s). Per ISO/IEC 1539-1:2018 Sections "
                    "16.9.52 and 16.9.187, these intrinsics provide "
                    "coarray team information.",
                    iso_section="16.9",
                )
            )

        if random_init_count > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_SUMM_I003",
                    message=f"Code contains {random_init_count} RANDOM_INIT "
                    "call(s). Per ISO/IEC 1539-1:2018 Section 16.9.152, "
                    "RANDOM_INIT initializes the random number generator.",
                    iso_section="16.9.152",
                )
            )

        if reduce_count > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_SUMM_I004",
                    message=f"Code contains {reduce_count} REDUCE intrinsic "
                    "call(s). Per ISO/IEC 1539-1:2018 Section 16.9.161, "
                    "REDUCE performs user-defined array reductions.",
                    iso_section="16.9.161",
                )
            )

        if out_of_range_count > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="INTR_SUMM_I005",
                    message=f"Code contains {out_of_range_count} OUT_OF_RANGE "
                    "intrinsic call(s). Per ISO/IEC 1539-1:2018 Section "
                    "16.9.140, OUT_OF_RANGE tests type conversion overflow.",
                    iso_section="16.9.140",
                )
            )

    def validate_file(self, filepath: str) -> F2018IntrinsicValidationResult:
        """Validate a Fortran 2018 file for intrinsic procedure semantics."""
        path = Path(filepath)
        if not path.exists():
            result = F2018IntrinsicValidationResult()
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


def validate_intrinsic_semantics(code: str) -> F2018IntrinsicValidationResult:
    """Convenience function to validate F2018 intrinsic procedure semantics."""
    validator = F2018IntrinsicValidator()
    return validator.validate_code(code)
