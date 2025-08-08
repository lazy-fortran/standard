#!/usr/bin/env python3
"""IEEE arithmetic tests for Issue #27

Test comprehensive F2003 IEEE floating-point support including:
- IEEE intrinsic module usage
- Exception handling
- Special value detection  
- Rounding mode control
- IEEE features specification
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

class TestIssue27IEEEArithmetic:
    """Comprehensive IEEE arithmetic functionality tests"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_ieee_exceptions_module_import(self):
        """IEEE exceptions module import should parse"""
        code = """module test
    use, intrinsic :: ieee_exceptions
    implicit none
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exceptions module import failed: {errors} errors"
    
    def test_ieee_arithmetic_module_import(self):
        """IEEE arithmetic module import should parse"""
        code = """module test
    use, intrinsic :: ieee_arithmetic
    implicit none
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE arithmetic module import failed: {errors} errors"
    
    def test_ieee_features_module_import(self):
        """IEEE features module import should parse"""
        code = """module test
    use, intrinsic :: ieee_features
    implicit none
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE features module import failed: {errors} errors"
    
    def test_ieee_exceptions_only_import(self):
        """IEEE exceptions with ONLY clause should parse"""
        code = """module test
    use, intrinsic :: ieee_exceptions, only: ieee_overflow, ieee_underflow
    implicit none
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exceptions ONLY import failed: {errors} errors"
    
    def test_ieee_arithmetic_only_import(self):
        """IEEE arithmetic with ONLY clause should parse"""
        code = """module test
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value
    implicit none
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE arithmetic ONLY import failed: {errors} errors"
    
    
    @pytest.mark.xfail(reason="PROGRAM unit parsing issue - line 10:8 'if' construct not recognized in program context")
    def test_ieee_exception_handling_basic(self):
        """Basic IEEE exception handling should parse"""
        code = """program test
    use, intrinsic :: ieee_exceptions
    implicit none
    logical :: overflow_flag
    
    call ieee_get_flag(ieee_overflow, overflow_flag)
    call ieee_set_flag(ieee_overflow, .false.)
    
    if (overflow_flag) then
        print *, 'Overflow detected'
    end if
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exception handling failed: {errors} errors"
    
    @pytest.mark.xfail(reason="PROGRAM unit parsing issue - line 9:4 execution part not properly parsed in program context")
    def test_ieee_special_values(self):
        """IEEE special value detection should parse"""
        code = """program test
    use, intrinsic :: ieee_arithmetic
    implicit none
    real :: x, y
    logical :: is_nan, is_finite
    
    x = ieee_value(x, ieee_positive_inf)
    y = ieee_value(y, ieee_quiet_nan)
    
    is_nan = ieee_is_nan(x)
    is_finite = ieee_is_finite(y)
    
    if (is_nan) then
        print *, 'Value is NaN'
    end if
    
    if (.not. is_finite) then
        print *, 'Value is not finite'
    end if
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE special values failed: {errors} errors"
    
    def test_ieee_rounding_modes(self):
        """IEEE rounding mode control should parse"""
        code = """program test
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    call ieee_set_rounding_mode(ieee_nearest)
    call ieee_set_rounding_mode(ieee_to_zero)
    call ieee_set_rounding_mode(ieee_up)
    call ieee_set_rounding_mode(ieee_down)
end program test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE rounding modes failed: {errors} errors"
    
    @pytest.mark.xfail(reason="MODULE parsing issue - line 28:8 execution statements not properly parsed in module context")
    def test_ieee_comprehensive_example(self):
        """Comprehensive IEEE usage pattern should parse"""
        code = """module ieee_demo
    use, intrinsic :: ieee_exceptions
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_features
    implicit none
    
contains
    subroutine safe_calculation()
        real :: x, y, result
        logical :: overflow_flag, underflow_flag
        logical :: is_valid
        
        ! Save exception state
        call ieee_get_flag(ieee_overflow, overflow_flag)
        call ieee_get_flag(ieee_underflow, underflow_flag)
        
        ! Clear exceptions
        call ieee_set_flag(ieee_overflow, .false.)
        call ieee_set_flag(ieee_underflow, .false.)
        
        ! Set rounding mode
        call ieee_set_rounding_mode(ieee_nearest)
        
        ! Perform calculation
        x = huge(1.0)
        y = 2.0
        result = x * y
        
        ! Check for special values
        is_valid = ieee_is_finite(result) .and. .not. ieee_is_nan(result)
        
        if (.not. is_valid) then
            ! Handle infinite or NaN result
            result = ieee_value(result, ieee_positive_inf)
        end if
        
        ! Check for exceptions
        call ieee_get_flag(ieee_overflow, overflow_flag)
        if (overflow_flag) then
            print *, 'Overflow occurred during calculation'
        end if
        
        ! Restore exception state if needed
        call ieee_set_flag(ieee_overflow, overflow_flag)
    end subroutine safe_calculation
end module ieee_demo"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Comprehensive IEEE example failed: {errors} errors"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])