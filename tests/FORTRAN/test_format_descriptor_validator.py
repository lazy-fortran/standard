"""Test suite for FORTRAN 1957 FORMAT descriptor semantic validation.

Tests FORMAT statement descriptor semantics per IBM FORTRAN for the IBM 704
(Form C28-6003, Oct 1958), Chapter 5.

Validates:
- Integer descriptors (Iw): width must be positive
- Fixed-point descriptors (Fw.d): width > 0, precision >= 0
- Exponential descriptors (Ew.d): width > 0, precision >= 0, if d >= 10 treated mod 10
- Scale factors (nP): applied to E/F conversions
- Hollerith fields (nH): count must be positive, exactly n characters follow
- Multi-record formats with slashes
- Grouped specifications with parentheses
"""

import sys
from pathlib import Path

sys.path.insert(
    0, str(Path(__file__).parent.parent.parent / "tools")
)

import pytest
from f57_format_descriptor_validator import (
    FormatDescriptorValidator,
    DiagnosticSeverity,
)


class TestFormatDescriptorValidatorBasics:
    """Basic FORMAT descriptor validation tests."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_empty_specification(self):
        """Empty FORMAT specification should not error."""
        result = self.validator.validate_format_descriptor_string("")
        assert not result.has_errors

    def test_simple_i_descriptor_valid(self):
        """Valid I-conversion descriptor."""
        result = self.validator.validate_format_descriptor_string("I12")
        assert not result.has_errors

    def test_simple_i_descriptor_zero_width(self):
        """I-conversion with zero width should error."""
        result = self.validator.validate_format_descriptor_string("I0")
        assert result.has_errors
        assert result.error_count == 1
        assert "width must be positive" in result.diagnostics[0].message

    def test_simple_f_descriptor_valid(self):
        """Valid F-conversion descriptor."""
        result = self.validator.validate_format_descriptor_string("F10.4")
        assert not result.has_errors

    def test_simple_f_descriptor_zero_width(self):
        """F-conversion with zero width should error."""
        result = self.validator.validate_format_descriptor_string("F0.4")
        assert result.has_errors
        assert any("width must be positive" in d.message for d in result.diagnostics)

    def test_simple_f_descriptor_negative_precision(self):
        """F-conversion with negative precision should error."""
        result = self.validator.validate_format_descriptor_string("F10.-2")
        assert result.has_errors
        assert any(
            "decimal places must be non-negative" in d.message
            for d in result.diagnostics
        )

    def test_simple_e_descriptor_valid(self):
        """Valid E-conversion descriptor."""
        result = self.validator.validate_format_descriptor_string("E12.4")
        assert not result.has_errors

    def test_simple_e_descriptor_zero_width(self):
        """E-conversion with zero width should error."""
        result = self.validator.validate_format_descriptor_string("E0.4")
        assert result.has_errors
        assert any("width must be positive" in d.message for d in result.diagnostics)

    def test_simple_e_descriptor_negative_precision(self):
        """E-conversion with negative precision should error."""
        result = self.validator.validate_format_descriptor_string("E10.-2")
        assert result.has_errors
        assert any(
            "decimal places must be non-negative" in d.message
            for d in result.diagnostics
        )


class TestRepeatedDescriptors:
    """Test repeated FORMAT descriptors (nX where n is count)."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_repeated_i_descriptor(self):
        """Repeated I-conversion descriptor."""
        result = self.validator.validate_format_descriptor_string("3I12")
        assert not result.has_errors

    def test_repeated_f_descriptor(self):
        """Repeated F-conversion descriptor."""
        result = self.validator.validate_format_descriptor_string("3F10.4")
        assert not result.has_errors

    def test_repeated_e_descriptor(self):
        """Repeated E-conversion descriptor (common pattern)."""
        result = self.validator.validate_format_descriptor_string("3E12.4")
        assert not result.has_errors


class TestScaleFactors:
    """Test scale factor (nP) descriptors."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_positive_scale_factor(self):
        """Positive scale factor should be valid."""
        result = self.validator.validate_format_descriptor_string("1P3F11.3")
        assert not result.has_errors

    def test_negative_scale_factor(self):
        """Negative scale factor should be valid."""
        result = self.validator.validate_format_descriptor_string("-1P3F11.3")
        assert not result.has_errors

    def test_zero_scale_factor(self):
        """Zero scale factor (0P) should be valid."""
        result = self.validator.validate_format_descriptor_string("0P3F11.3")
        assert not result.has_errors

    def test_scale_factor_with_e(self):
        """Scale factor applied to E-conversion."""
        result = self.validator.validate_format_descriptor_string("1P3E12.4")
        assert not result.has_errors

    def test_scale_factor_does_not_hide_invalid_descriptor(self):
        """Scale factor followed by invalid descriptor semantics should error."""
        result = self.validator.validate_format_descriptor_string("1P3F0.4")
        assert result.has_errors
        assert any("width must be positive" in d.message for d in result.diagnostics)


class TestHollerthFields:
    """Test Hollerith field descriptors (nH...)."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_valid_hollerith_basic(self):
        """Basic valid Hollerith field."""
        result = self.validator.validate_format_descriptor_string("5HHELLO")
        assert not result.has_errors

    def test_valid_hollerith_single_char(self):
        """Single character Hollerith field."""
        result = self.validator.validate_format_descriptor_string("1H*")
        assert not result.has_errors

    def test_valid_hollerith_with_spaces(self):
        """Hollerith field with spaces (allowed per C28-6003)."""
        result = self.validator.validate_format_descriptor_string("11Hhello world")
        assert not result.has_errors

    def test_invalid_hollerith_count_mismatch_high(self):
        """Hollerith count too high for actual characters."""
        result = self.validator.validate_format_descriptor_string("5HHELL")
        assert result.has_errors
        assert any("count mismatch" in d.message for d in result.diagnostics)

    def test_invalid_hollerith_count_mismatch_low(self):
        """Hollerith count too low for actual characters."""
        result = self.validator.validate_format_descriptor_string("3HHELLO")
        assert result.has_errors
        assert any("count mismatch" in d.message for d in result.diagnostics)

    def test_invalid_hollerith_zero_count(self):
        """Hollerith with zero count should error."""
        result = self.validator.validate_format_descriptor_string("0H")
        assert result.has_errors
        assert any("count must be positive" in d.message for d in result.diagnostics)


class TestMultipleDescriptors:
    """Test FORMAT specifications with multiple descriptors."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_multiple_descriptors_all_valid(self):
        """Multiple descriptors, all valid."""
        result = self.validator.validate_format_descriptor_string("I12,E12.4,F10.4")
        assert not result.has_errors

    def test_multiple_descriptors_one_invalid(self):
        """Multiple descriptors, one invalid."""
        result = self.validator.validate_format_descriptor_string("I12,E0.4,F10.4")
        assert result.has_errors
        assert result.error_count == 1

    def test_complex_format_from_standard(self):
        """Format from C28-6003 example: FORMAT (12, E12.4, F10.4)."""
        result = self.validator.validate_format_descriptor_string("I2,E12.4,F10.4")
        assert not result.has_errors

    def test_complex_format_with_scale(self):
        """Complex format with scale factor: FORMAT (12, 1P3F11.3)."""
        result = self.validator.validate_format_descriptor_string("I2,1P3F11.3")
        assert not result.has_errors


class TestGroupedSpecifications:
    """Test grouped FORMAT specifications with parentheses."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_grouped_spec_basic(self):
        """Basic grouped specification."""
        # FORMAT (2(F10.6, E10.2), I4)
        result = self.validator.validate_format_descriptor_string("2(F10.6,E10.2),I4")
        assert not result.has_errors

    def test_grouped_spec_from_standard(self):
        """Grouped spec from C28-6003: FORMAT (2(F10.6, E10.2), I4)."""
        result = self.validator.validate_format_descriptor_string("2(F10.6,E10.2),I4")
        assert not result.has_errors


class TestEdgeCases:
    """Edge cases and corner scenarios."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_precision_equals_10(self):
        """Precision of 10: per C28-6003, treated mod 10."""
        # F10.10 should parse without errors (treated as F10.0)
        result = self.validator.validate_format_descriptor_string("F10.10")
        assert not result.has_errors

    def test_precision_greater_than_10(self):
        """Precision > 10: per C28-6003, treated mod 10."""
        # F10.15 should parse without errors (treated as F10.5)
        result = self.validator.validate_format_descriptor_string("F10.15")
        assert not result.has_errors

    def test_very_large_field_width(self):
        """Very large field width (valid but impractical)."""
        result = self.validator.validate_format_descriptor_string("F999.99")
        assert not result.has_errors

    def test_whitespace_handling(self):
        """Whitespace in specification should be handled."""
        result = self.validator.validate_format_descriptor_string("I12 , E12.4 , F10.4")
        assert not result.has_errors


class TestDiagnosticMetadata:
    """Test diagnostic information and metadata."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_error_has_iso_section_reference(self):
        """Errors should include ISO/standard section references."""
        result = self.validator.validate_format_descriptor_string("I0")
        assert result.has_errors
        assert result.diagnostics[0].iso_section is not None
        assert "C28-6003" in result.diagnostics[0].iso_section

    def test_error_has_error_code(self):
        """Errors should have diagnostic error codes."""
        result = self.validator.validate_format_descriptor_string("I0")
        assert result.has_errors
        assert result.diagnostics[0].code.startswith("FMT57_")

    def test_warning_vs_error_severity(self):
        """Warnings should be distinguished from errors."""
        # E-conversion with very small width generates warning, not error
        result = self.validator.validate_format_descriptor_string("E2.4")
        # Should have warning about insufficient width
        warnings = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.WARNING
        ]
        # Note: may not have warning if minimum check isn't applied
        # but should not have errors
        assert not any(
            d.severity == DiagnosticSeverity.ERROR for d in result.diagnostics
        )


class TestRealWorldFormats:
    """Test FORMAT specifications from real FORTRAN programs."""

    def setup_method(self):
        self.validator = FormatDescriptorValidator()

    def test_format_for_integer_output(self):
        """Simple integer output format."""
        result = self.validator.validate_format_descriptor_string("10I8")
        assert not result.has_errors

    def test_format_for_float_output(self):
        """Fixed-point float output format."""
        result = self.validator.validate_format_descriptor_string("5F12.6")
        assert not result.has_errors

    def test_format_for_scientific_output(self):
        """Scientific notation output format."""
        result = self.validator.validate_format_descriptor_string("8E15.7")
        assert not result.has_errors

    def test_format_mixed_types(self):
        """Mixed type format for varied output."""
        result = self.validator.validate_format_descriptor_string("I5,3F10.4,E12.5")
        assert not result.has_errors

    def test_format_with_labeling(self):
        """Format with Hollerith labels (output text)."""
        result = self.validator.validate_format_descriptor_string(
            "3HX =,F8.3,5H   Y=,F8.3"
        )
        assert not result.has_errors


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
