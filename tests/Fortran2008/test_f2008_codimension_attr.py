#!/usr/bin/env python3
"""F2008 CODIMENSION Attribute Tests

Test F2008 CODIMENSION attribute functionality including:
- Deferred codimension declarations [*]
- Explicit codimension bounds [n] and [lower:upper]
- CODIMENSION combined with other attributes
- CODIMENSION in subroutine arguments
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from fixture_utils import load_fixture


class TestF2008CodimensionAttr:
    """Test F2008 CODIMENSION attribute functionality"""

    def parse_code(self, code):
        """Parse Fortran 2008 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        parser = Fortran2008Parser(CommonTokenStream(lexer))

        tree = parser.program_unit_f2008()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors

    def test_codimension_deferred(self):
        """Test CODIMENSION attribute with deferred codimension [*]"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_codimension_attr",
            "codimension_deferred.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, (
            "CODIMENSION deferred declaration failed to produce parse tree"
        )
        assert errors == 0, (
            f"Expected 0 errors for CODIMENSION deferred syntax, got {errors}"
        )

    def test_codimension_explicit(self):
        """Test CODIMENSION attribute with explicit bounds"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_codimension_attr",
            "codimension_explicit.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, (
            "CODIMENSION explicit bounds failed to produce parse tree"
        )
        assert errors == 0, (
            f"Expected 0 errors for CODIMENSION explicit bounds, got {errors}"
        )

    def test_codimension_with_attrs(self):
        """Test CODIMENSION combined with other attributes"""
        code = load_fixture(
            "Fortran2008",
            "test_f2008_codimension_attr",
            "codimension_with_attrs.f90",
        )
        tree, errors = self.parse_code(code)
        assert tree is not None, (
            "CODIMENSION with other attributes failed to produce parse tree"
        )
        assert errors == 0, (
            f"Expected 0 errors for CODIMENSION with attributes, got {errors}"
        )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
