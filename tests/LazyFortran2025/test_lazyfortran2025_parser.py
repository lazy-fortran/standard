#!/usr/bin/env python3
"""Tests for LazyFortran2025 lexer and parser.

These tests focus on the dual entry points and the relaxed program structure
for .lf files, while preserving compatibility with strict Fortran 2023 code.
"""

import os
import sys
from pathlib import Path

import pytest


GRAMMARS_DIR = Path(__file__).resolve().parents[2] / "grammars/generated/modern"
FIXTURES_DIR = Path(__file__).resolve().parents[2] / "grammars/fixtures"
sys.path.insert(0, str(GRAMMARS_DIR))

TEST_ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(TEST_ROOT))

from fixture_utils import load_fixture  # noqa: E402


try:
    from LazyFortran2025Lexer import LazyFortran2025Lexer  # type: ignore
    from LazyFortran2025Parser import LazyFortran2025Parser  # type: ignore
    PARSER_AVAILABLE = True
except ImportError as exc:  # pragma: no cover - environment dependent
    PARSER_AVAILABLE = False
    LazyFortran2025Lexer = None  # type: ignore
    LazyFortran2025Parser = None  # type: ignore
    pytest.skip(f"LazyFortran2025 grammar not built: {exc}", allow_module_level=True)


class TestLazyFortran2025Lexer:
    """Basic lexer smoke tests ensuring F2023 compatibility."""

    def create_lexer(self, input_text: str) -> LazyFortran2025Lexer:  # type: ignore[name-defined]
        from antlr4 import InputStream  # type: ignore

        stream = InputStream(input_text)
        return LazyFortran2025Lexer(stream)  # type: ignore[call-arg]

    def test_lexer_recognizes_f2023_tokens(self) -> None:
        """LazyFortran2025 lexer reuses Fortran 2023 token vocabulary."""
        lexer = self.create_lexer("PROGRAM test\nx = 42\n? IEEE_MAX\n")
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)

        assert any(t.type == LazyFortran2025Lexer.PROGRAM for t in tokens)  # type: ignore[attr-defined]
        assert any(t.type == LazyFortran2025Lexer.QUESTION for t in tokens)  # type: ignore[attr-defined]
        assert any(t.type == LazyFortran2025Lexer.IEEE_MAX for t in tokens)  # type: ignore[attr-defined]


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="Parser not available")
class TestLazyFortran2025Parser:
    """Tests for LazyFortran2025 parser entry points."""

    def create_parser(self, input_text: str) -> LazyFortran2025Parser:  # type: ignore[name-defined]
        from antlr4 import CommonTokenStream, InputStream  # type: ignore

        stream = InputStream(input_text)
        lexer = LazyFortran2025Lexer(stream)  # type: ignore[call-arg]
        tokens = CommonTokenStream(lexer)
        parser = LazyFortran2025Parser(tokens)  # type: ignore[call-arg]
        return parser

    def test_has_dual_entry_points(self) -> None:
        """Parser exposes both traditional and lazy entry points."""
        assert hasattr(LazyFortran2025Parser, "traditional_entry")  # type: ignore[attr-defined]
        assert hasattr(LazyFortran2025Parser, "lazy_entry")  # type: ignore[attr-defined]

    def test_traditional_entry_parses_traditional_fixture(self) -> None:
        """traditional_entry preserves strict Fortran 2023 behavior."""
        fixture_path = FIXTURES_DIR / "test_traditional.f90"
        code = fixture_path.read_text()

        parser = self.create_parser(code)
        tree = parser.traditional_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_lazy_entry_parses_lazy_fixture(self) -> None:
        """lazy_entry accepts top-level statements and procedures."""
        fixture_path = FIXTURES_DIR / "test_lazy.lf"
        code = fixture_path.read_text()

        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_lazy_entry_supports_implicit_program(self) -> None:
        """Bare statements without PROGRAM or MODULE parse in lazy mode."""
        code = (
            "x = 3.14\n"
            "y = 2.71\n"
            "print *, x + y\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_lazy_entry_allows_procedures_without_contains(self) -> None:
        """Procedures following executable statements parse without CONTAINS."""
        code = (
            "x = 5.0\n"
            "print *, x\n"
            "function double_value(v) result(r)\n"
            "    real :: v, r\n"
            "    r = v * 2.0\n"
            "end function\n"
            "y = double_value(x)\n"
            "print *, y\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_lazy_entry_uses_f2023_conditional_expressions(self) -> None:
        """Lazy programs can use F2023 conditional expressions."""
        code = (
            "x = 5.0\n"
            "y = 10.0\n"
            "max_val = (x > y ? x : y)\n"
            "print *, max_val\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_traditional_entry_parses_f2023_fixture(self) -> None:
        """traditional_entry delegates to program_unit_f2023 for strict code."""
        # Reuse an existing Fortran2023 basic program fixture.
        fixture = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "basic_program.f90",
        )
        parser = self.create_parser(fixture)
        tree = parser.traditional_entry()  # type: ignore[call-arg]
        assert tree is not None

    def test_lazy_entry_allows_spec_and_exec_constructs(self) -> None:
        """Top-level USE/IMPLICIT and statements are accepted."""
        code = (
            "implicit none\n"
            "use iso_fortran_env, only: int32\n"
            "integer(int32) :: x\n"
            "x = 1_int32\n"
            "print *, x\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_lazy_entry_supports_procedure_only_library_files(self) -> None:
        """Procedure-only .lf files parse as library-style units."""
        code = (
            "function square(x) result(r)\n"
            "  real, intent(in) :: x\n"
            "  real :: r\n"
            "  r = x * x\n"
            "end function square\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    @pytest.mark.skipif(
        not hasattr(LazyFortran2025Lexer, "TRAIT"),  # type: ignore[arg-type]
        reason="Lazy trait tokens not available in generated lexer",
    )
    def test_lazy_entry_parses_trait_definition(self) -> None:
        """Lazy mode accepts top-level trait definitions."""
        code = (
            "trait AdditiveMonoid(T)\n"
            "  procedure :: add\n"
            "  procedure :: zero\n"
            "end trait AdditiveMonoid\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    @pytest.mark.skipif(
        not hasattr(LazyFortran2025Lexer, "TRAIT"),  # type: ignore[arg-type]
        reason="Lazy trait tokens not available in generated lexer",
    )
    def test_lazy_entry_parses_trait_annotation_statement(self) -> None:
        """Trait annotations parse as lightweight specification constructs."""
        code = (
            "trait AdditiveMonoid(T)\n"
            "  procedure :: add\n"
            "end trait AdditiveMonoid\n"
            "@AdditiveMonoid(real(8))\n"
            "function sum_all(x) result(r)\n"
            "  real(8), intent(in) :: x(:)\n"
            "  r = 0.0_real64\n"
            "end function sum_all\n"
        )
        parser = self.create_parser(code)
        tree = parser.lazy_entry()  # type: ignore[call-arg]
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0
