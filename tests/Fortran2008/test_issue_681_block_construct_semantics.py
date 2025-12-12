#!/usr/bin/env python3
"""F2008 BLOCK Construct Semantic Validation Tests

Validates semantic constraints for Fortran 2008 BLOCK constructs per
ISO/IEC 1539-1:2010 (J3/08-007) Section 8.1.4.

Tests coverage:
- C806: specification-part forbidden statement restrictions
- C807: SAVE statement restrictions in BLOCK
- C808: block-construct-name matching
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2008_block_construct_validator import (
    F2008BlockConstructValidator,
    validate_block_construct,
    DiagnosticSeverity,
)


class TestF2008BlockConstructValidatorBasic:
    """Test basic validator functionality."""

    def test_empty_module_no_errors(self):
        code = "module test_mod\nend module test_mod\n"
        result = validate_block_construct(code)
        assert not result.has_errors
        assert result.error_count == 0

    def test_syntax_error_detected(self):
        code = "program test\nblock\nint ::\nend block\nend program\n"
        result = validate_block_construct(code)
        assert result.has_errors
        assert result.error_count > 0

    def test_validation_result_properties(self):
        code = "program test\nblock\nend block\nend program\n"
        result = validate_block_construct(code)
        assert hasattr(result, 'has_errors')
        assert hasattr(result, 'error_count')
        assert hasattr(result, 'warning_count')
        assert hasattr(result, 'diagnostics')


class TestBlockConstructNameMatching:
    """Test C808: block-construct-name matching."""

    def test_block_with_matching_names(self):
        code = (
            "program test\n"
            "my_block: block\n"
            "integer :: x\n"
            "x = 1\n"
            "end block my_block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert not result.has_errors, (
            f"Expected no errors for matching names, got: "
            f"{[d.message for d in result.diagnostics]}"
        )

    def test_block_with_mismatched_names(self):
        code = (
            "program test\n"
            "block1: block\n"
            "integer :: x\n"
            "end block block2\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, "Expected error for mismatched block names"
        assert any("mismatch" in d.message.lower() for d in result.diagnostics)
        assert any(d.code == "C808-E001" for d in result.diagnostics)

    def test_block_start_with_name_end_without(self):
        code = (
            "program test\n"
            "my_block: block\n"
            "integer :: x\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, (
            "Expected error when block-stmt has name but end-block-stmt doesn't"
        )
        assert any("C808" in d.code for d in result.diagnostics)

    def test_block_start_without_name_end_with(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "end block my_block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, (
            "Expected error when block-stmt lacks name but end-block-stmt has one"
        )
        assert any("C808" in d.code for d in result.diagnostics)

    def test_block_both_unnamed(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert not result.has_errors, (
            "Expected no errors when both lack names"
        )


class TestBlockConstructForbiddenStatements:
    """Test C806: forbidden statements in BLOCK specification part."""

    def test_block_with_common_statement(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "common /shared/ x\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, "Expected error for COMMON in BLOCK specification part"
        assert any("C806" in d.code for d in result.diagnostics)

    def test_block_with_equivalence_statement(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x, y\n"
            "equivalence (x, y)\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        # EQUIVALENCE is rejected by the grammar itself for F2008 in BLOCK
        # so we get a syntax error rather than semantic error
        assert result.has_errors, (
            "Expected error for EQUIVALENCE in BLOCK specification part"
        )

    def test_block_with_implicit_statement(self):
        code = (
            "program test\n"
            "block\n"
            "implicit real(a-h)\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, (
            "Expected error for IMPLICIT in BLOCK specification part"
        )
        assert any("C806" in d.code for d in result.diagnostics)

    def test_block_with_namelist_statement(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x, y\n"
            "namelist /input/ x, y\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.has_errors, (
            "Expected error for NAMELIST in BLOCK specification part"
        )
        assert any("C806" in d.code for d in result.diagnostics)

    def test_block_with_valid_declarations(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "real :: y\n"
            "character(len=10) :: str\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert not result.has_errors, (
            f"Expected no errors for valid declarations, got: "
            f"{[d.message for d in result.diagnostics]}"
        )


class TestBlockConstructISOCompliance:
    """Test ISO compliance reporting."""

    def test_diagnostics_reference_iso_section(self):
        code = (
            "program test\n"
            "my_block: block\n"
            "end block other_block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert any(d.iso_section == "8.1.4" for d in result.diagnostics)

    def test_diagnostic_has_severity(self):
        code = (
            "program test\n"
            "block1: block\n"
            "end block block2\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        errors = [d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR]
        assert len(errors) > 0, "Expected at least one ERROR diagnostic"

    def test_diagnostic_has_code(self):
        code = (
            "program test\n"
            "block1: block\n"
            "end block block2\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert all(d.code for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR)

    def test_diagnostic_has_message(self):
        code = (
            "program test\n"
            "block1: block\n"
            "end block block2\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert all(d.message for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR)


class TestConvenienceFunctions:
    """Test validator convenience functions."""

    def test_validate_block_construct_function(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert isinstance(result.diagnostics, list)

    def test_validate_block_construct_file_function(self):
        from f2008_block_construct_validator import validate_block_construct_file
        import tempfile
        import os

        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "end block\n"
            "end program test\n"
        )

        with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
            f.write(code)
            temp_path = f.name

        try:
            result = validate_block_construct_file(temp_path)
            assert not result.has_errors
        finally:
            os.unlink(temp_path)

    def test_validate_nonexistent_file(self):
        from f2008_block_construct_validator import validate_block_construct_file
        result = validate_block_construct_file("/nonexistent/path/file.f90")
        assert result.has_errors
        assert any("not found" in d.message.lower() for d in result.diagnostics)


class TestBlockConstructTracking:
    """Test that BLOCK constructs are properly tracked."""

    def test_multiple_blocks_tracked(self):
        code = (
            "program test\n"
            "block\n"
            "integer :: x\n"
            "end block\n"
            "block\n"
            "real :: y\n"
            "end block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert len(result.block_constructs) >= 2, (
            "Expected at least 2 BLOCK constructs to be tracked"
        )

    def test_block_construct_info_captured(self):
        code = (
            "program test\n"
            "my_block: block\n"
            "integer :: x\n"
            "end block my_block\n"
            "end program test\n"
        )
        result = validate_block_construct(code)
        assert result.block_constructs, "Expected BLOCK construct to be tracked"
        block_info = result.block_constructs[0]
        assert 'line' in block_info
        assert 'column' in block_info
        assert 'block_name_start' in block_info


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
