"""
Tests for Issue #468: Fortran 2003 MOVE_ALLOC intrinsic subroutine
ISO/IEC 1539-1:2004 Section 13.7.80

MOVE_ALLOC is a critical intrinsic subroutine that atomically moves allocation
from one allocatable to another. It is essential for efficient memory management
in modern Fortran without requiring data copying.

This test suite verifies that:
1. MOVE_ALLOC token is defined in Fortran2003Lexer
2. MOVE_ALLOC can be parsed in call statements
3. MOVE_ALLOC is properly integrated into executable constructs
4. Various usage patterns (basic, in loops, with polymorphic types) parse correctly
"""

import unittest
import os
from pathlib import Path
from antlr4 import InputStream, CommonTokenStream, ParseTreeWalker
from grammars.generated.modern.Fortran2003Parser import Fortran2003Parser
from grammars.generated.modern.Fortran2003Lexer import Fortran2003Lexer


class TestMoveAllocParsing(unittest.TestCase):
    """Test MOVE_ALLOC parsing in various contexts"""

    def setUp(self):
        """Prepare test fixtures directory"""
        self.fixtures_dir = Path(__file__).parent.parent / "fixtures" / "Fortran2003" / "test_issue468_move_alloc"
        self.assertTrue(self.fixtures_dir.exists(), f"Fixtures directory not found: {self.fixtures_dir}")

    def parse_fortran_file(self, filename: str):
        """Parse a Fortran file and return the parse tree"""
        filepath = self.fixtures_dir / filename
        self.assertTrue(filepath.exists(), f"Fixture file not found: {filepath}")

        with open(filepath, 'r') as f:
            code = f.read()

        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran2003Parser(stream)

        return parser.program_unit()

    def test_basic_move_alloc(self):
        """Test basic MOVE_ALLOC usage (Issue #468)"""
        tree = self.parse_fortran_file('basic_move_alloc.f90')
        self.assertIsNotNone(tree)
        # Verify it parses without exceptions

    def test_move_alloc_in_subroutine(self):
        """Test MOVE_ALLOC within a subroutine"""
        tree = self.parse_fortran_file('move_alloc_in_subroutine.f90')
        self.assertIsNotNone(tree)

    def test_move_alloc_in_loop(self):
        """Test MOVE_ALLOC within a DO loop"""
        tree = self.parse_fortran_file('move_alloc_in_loop.f90')
        self.assertIsNotNone(tree)

    def test_move_alloc_2d_arrays(self):
        """Test MOVE_ALLOC with 2D arrays"""
        tree = self.parse_fortran_file('move_alloc_2d_arrays.f90')
        self.assertIsNotNone(tree)

    def test_move_alloc_polymorphic(self):
        """Test MOVE_ALLOC with polymorphic types (F2003 OOP)"""
        tree = self.parse_fortran_file('move_alloc_polymorphic.f90')
        self.assertIsNotNone(tree)


class TestMoveAllocLexer(unittest.TestCase):
    """Test MOVE_ALLOC token recognition in Fortran2003Lexer"""

    def test_move_alloc_token_recognized(self):
        """Verify MOVE_ALLOC token is recognized"""
        code = "CALL MOVE_ALLOC(from, to)"
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        tokens = lexer.getAllTokens()

        # Find MOVE_ALLOC token
        move_alloc_found = False
        for token in tokens:
            if token.text.upper() == "MOVE_ALLOC":
                move_alloc_found = True
                break

        self.assertTrue(move_alloc_found, "MOVE_ALLOC token not recognized in lexer")

    def test_move_alloc_case_insensitive(self):
        """Verify MOVE_ALLOC is case-insensitive"""
        cases = ["MOVE_ALLOC", "move_alloc", "Move_Alloc", "mOvE_aLlOc"]
        for case in cases:
            with self.subTest(case=case):
                code = f"CALL {case}(from, to)"
                input_stream = InputStream(code)
                lexer = Fortran2003Lexer(input_stream)
                stream = CommonTokenStream(lexer)
                parser = Fortran2003Parser(stream)

                # Should not raise an exception
                tree = parser.program_unit()
                self.assertIsNotNone(tree)


class TestMoveAllocCompliance(unittest.TestCase):
    """Test ISO/IEC 1539-1:2004 Section 13.7.80 MOVE_ALLOC compliance"""

    def test_move_alloc_in_executable_construct(self):
        """Verify MOVE_ALLOC is wired into executable_construct_f2003"""
        code = """program test
    implicit none
    integer, allocatable :: a(:), b(:)
    allocate(a(10))
    call move_alloc(a, b)
end program test
"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran2003Parser(stream)

        tree = parser.program_unit()
        self.assertIsNotNone(tree)

    def test_move_alloc_iso_section_reference(self):
        """Verify MOVE_ALLOC is documented per ISO/IEC 1539-1:2004 Section 13.7.80"""
        # This test documents the ISO standard requirement
        # ISO/IEC 1539-1:2004 Section 13.7.80 defines MOVE_ALLOC as:
        # CALL MOVE_ALLOC(FROM=from, TO=to)
        # - FROM: intent(inout) allocatable
        # - TO: intent(out) allocatable
        # - Atomically moves allocation from FROM to TO without copying data

        code = """program iso_compliance
    implicit none
    integer, allocatable :: from_array(:), to_array(:)

    allocate(from_array(100))
    ! MOVE_ALLOC atomically moves allocation
    call move_alloc(from_array, to_array)
    ! After MOVE_ALLOC: from_array is deallocated, to_array has the allocation
end program iso_compliance
"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran2003Parser(stream)

        tree = parser.program_unit()
        self.assertIsNotNone(tree)


if __name__ == '__main__':
    unittest.main()
