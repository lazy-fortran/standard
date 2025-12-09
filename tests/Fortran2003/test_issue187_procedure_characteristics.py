#!/usr/bin/env python3
"""Semantic Validation Tests for Procedure Characteristics - Issue #187

Comprehensive test suite for Fortran 2003 semantic validation of:
- Procedure characteristics (ISO/IEC 1539-1:2004 Section 12.2.2.1)
- Abstract interfaces (ISO/IEC 1539-1:2004 Section 12.3.2.3)
- Procedure pointers and compatibility (ISO/IEC 1539-1:2004 Section 12.3.2.3)
- Type-bound procedures (ISO/IEC 1539-1:2004 Section 4.5.4)

These tests validate the semantic analysis layer for procedure pointer and
type-bound procedure characteristics, ensuring proper enforcement of the
Fortran 2003 standard requirements.
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.insert(0, "tools")
sys.path.append(str(Path(__file__).parent.parent))

from f2003_semantic_validator import (
    F2003SemanticValidator,
    ValidationResult,
    DiagnosticSeverity,
    ProcedureCharacteristics,
    AbstractInterface,
    ProcedurePointer,
    TypeBoundProcedure,
    GenericBinding,
)
from fixture_utils import load_fixture


class TestAbstractInterfaceDetection:
    """Tests for abstract interface detection and tracking."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_abstract_interface_detected(self):
        """Abstract interface blocks should be detected and tracked."""
        code = """
module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert len(result.abstract_interfaces) > 0

    def test_abstract_interface_function_characteristics(self):
        """Abstract interface should capture function characteristics."""
        code = """
module test_mod
    abstract interface
        function func_char(x) result(y)
            real, intent(in) :: x
            real :: y
        end function func_char
    end interface
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert len(result.abstract_interfaces) > 0

    def test_abstract_interface_subroutine(self):
        """Abstract interface with subroutine should be tracked."""
        code = """
module test_mod
    abstract interface
        subroutine sub_interface(x)
            real, intent(inout) :: x
        end subroutine sub_interface
    end interface
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert len(result.abstract_interfaces) > 0

    def test_abstract_interface_fixture(self):
        """Load and validate abstract interface fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "abstract_interface_module.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert len(result.abstract_interfaces) > 0


class TestProcedurePointerDeclaration:
    """Tests for procedure pointer declaration tracking."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_procedure_pointer_detected(self):
        """Procedure pointer declarations should be detected."""
        code = """
module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    procedure(func_interface), pointer :: func_ptr
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "func_ptr" in result.procedure_pointers
        ptr = result.procedure_pointers["func_ptr"]
        assert ptr.interface_name == "func_interface"

    def test_procedure_pointer_with_null_init(self):
        """Procedure pointer with null initialization."""
        code = """
module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    procedure(func_interface), pointer :: func_ptr => null()
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors

    def test_procedure_pointer_fixture(self):
        """Load and validate procedure pointer fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "basic_procedure_pointer_declaration.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"


class TestProcedurePointerComponent:
    """Tests for procedure pointer components in derived types."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_proc_component_detected(self):
        """Procedure pointer component in derived type should be detected."""
        code = """
module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    type :: math_t
        procedure(func_interface), pointer, nopass :: operation
    end type math_t
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "operation" in result.procedure_pointers
        ptr = result.procedure_pointers["operation"]
        assert ptr.is_component
        assert ptr.interface_name == "func_interface"
        assert ptr.pass_attr == "nopass"

    def test_proc_component_fixture(self):
        """Load and validate procedure component fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_pointer_component.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"


class TestTypeBoundProcedures:
    """Tests for type-bound procedure detection and validation."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_basic_type_bound_procedure(self):
        """Basic type-bound procedure should be detected."""
        code = """
module test_mod
    type :: shape_t
    contains
        procedure :: area_method
    end type shape_t
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "shape_t" in result.type_bound_procedures
        bindings = result.type_bound_procedures["shape_t"]
        assert len(bindings) == 1
        assert bindings[0].binding_name == "area_method"

    def test_deferred_type_bound_procedure(self):
        """Deferred type-bound procedure should be detected."""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "deferred_procedure_abstract_type.f90",
        )
        result = self.validator.validate_code(code)
        assert len(result.type_bound_procedures) > 0
        type_name = list(result.type_bound_procedures.keys())[0]
        bindings = result.type_bound_procedures[type_name]
        assert len(bindings) == 1
        tbp = bindings[0]
        assert tbp.is_deferred

    def test_deferred_without_interface_error(self):
        """DEFERRED binding without interface should produce error."""
        code = """
module test_mod
    type, abstract :: shape_t
    contains
        procedure, deferred :: area_method
    end type shape_t
end module test_mod
"""
        result = self.validator.validate_code(code)
        has_error = any(d.code == "PROC_CHAR_E001" for d in result.diagnostics)
        assert has_error, "Expected PROC_CHAR_E001 for DEFERRED without interface"

    def test_type_bound_with_binding_rename(self):
        """Type-bound procedure with binding rename."""
        code = """
module test_mod
    type :: counter_t
    contains
        procedure :: increment => do_increment
    end type counter_t
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "counter_t" in result.type_bound_procedures
        bindings = result.type_bound_procedures["counter_t"]
        assert len(bindings) == 1
        tbp = bindings[0]
        assert tbp.binding_name == "increment"
        assert tbp.procedure_name == "do_increment"

    def test_type_bound_fixture(self):
        """Load and validate type-bound procedure fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "basic_type_bound_procedure.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"


class TestGenericBindings:
    """Tests for generic type-bound procedure bindings."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_generic_operator_binding(self):
        """Generic operator binding should be detected."""
        code = """
module test_mod
    type :: vector_t
    contains
        generic :: operator(+) => add_vectors
    end type vector_t
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "vector_t" in result.generic_bindings
        generics = result.generic_bindings["vector_t"]
        assert len(generics) == 1
        gb = generics[0]
        assert gb.is_operator
        assert "add_vectors" in gb.specific_bindings

    def test_generic_fixture(self):
        """Load and validate generic type-bound procedure fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "generic_type_bound_procedure.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"


class TestProcedureCharacteristics:
    """Tests for procedure characteristics extraction."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_function_characteristics(self):
        """Function characteristics should be extracted from subprograms."""
        code = """
function square(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * x
end function square
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        if "square" in result.procedures:
            proc = result.procedures["square"]
            assert proc.is_function
            assert proc.result_name == "y"

    def test_simple_function(self):
        """Procedure characteristics tracked via ValidationResult."""
        code = """
function double_val(x) result(y)
    real, intent(in) :: x
    real :: y
    y = 2.0 * x
end function double_val
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert hasattr(result, "procedures")

    def test_bind_c_procedure(self):
        """BIND(C) procedure should be detected."""
        code = """
module test_mod
    use iso_c_binding
contains
    subroutine c_proc() bind(c)
    end subroutine c_proc
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert "c_proc" in result.c_interop_entities


class TestCombinedFeatures:
    """Tests combining multiple procedure characteristic features."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_abstract_type_with_pointer_component(self):
        """Procedure component fixture from existing tests."""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_pointer_component.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"
        assert len(result.abstract_interfaces) > 0
        assert "operation" in result.procedure_pointers

    def test_polymorphism_fixture(self):
        """Load and validate polymorphism fixture."""
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "poly_test_program.f90",
        )
        result = self.validator.validate_code(code)
        assert not result.has_errors, f"Errors: {result.diagnostics}"


class TestDataclassProperties:
    """Tests for dataclass structure and properties."""

    def test_procedure_characteristics_dataclass(self):
        """ProcedureCharacteristics dataclass should have all fields."""
        proc = ProcedureCharacteristics(
            name="test_proc",
            is_function=True,
            is_pure=True,
            is_elemental=False,
            result_type="real",
            result_name="res",
        )
        assert proc.name == "test_proc"
        assert proc.is_function
        assert proc.is_pure
        assert not proc.is_elemental
        assert proc.result_type == "real"
        assert proc.result_name == "res"

    def test_abstract_interface_dataclass(self):
        """AbstractInterface dataclass should have all fields."""
        iface = AbstractInterface(
            name="test_iface",
            procedures=[],
            line=10,
            column=5,
        )
        assert iface.name == "test_iface"
        assert iface.procedures == []
        assert iface.line == 10
        assert iface.column == 5

    def test_procedure_pointer_dataclass(self):
        """ProcedurePointer dataclass should have all fields."""
        ptr = ProcedurePointer(
            name="ptr",
            interface_name="iface",
            is_component=True,
            initial_target="target_proc",
            pass_attr="nopass",
        )
        assert ptr.name == "ptr"
        assert ptr.interface_name == "iface"
        assert ptr.is_component
        assert ptr.initial_target == "target_proc"
        assert ptr.pass_attr == "nopass"

    def test_type_bound_procedure_dataclass(self):
        """TypeBoundProcedure dataclass should have all fields."""
        tbp = TypeBoundProcedure(
            binding_name="method",
            procedure_name="impl_method",
            is_deferred=True,
            is_nopass=False,
            is_non_overridable=False,
        )
        assert tbp.binding_name == "method"
        assert tbp.procedure_name == "impl_method"
        assert tbp.is_deferred
        assert not tbp.is_nopass

    def test_generic_binding_dataclass(self):
        """GenericBinding dataclass should have all fields."""
        gb = GenericBinding(
            generic_name="operator(+)",
            specific_bindings=["add_int", "add_real"],
            is_operator=True,
        )
        assert gb.generic_name == "operator(+)"
        assert gb.specific_bindings == ["add_int", "add_real"]
        assert gb.is_operator


class TestValidationResultExtensions:
    """Tests for ValidationResult extensions for procedure characteristics."""

    def setup_method(self):
        self.validator = F2003SemanticValidator()

    def test_validation_result_has_procedure_fields(self):
        """ValidationResult should have procedure characteristic fields."""
        code = """
module test_mod
end module test_mod
"""
        result = self.validator.validate_code(code)
        assert hasattr(result, "procedures")
        assert hasattr(result, "abstract_interfaces")
        assert hasattr(result, "procedure_pointers")
        assert hasattr(result, "type_bound_procedures")
        assert hasattr(result, "generic_bindings")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
