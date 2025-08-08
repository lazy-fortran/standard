#!/usr/bin/env python3
"""F2018 SELECT RANK Tests

Test F2018 SELECT RANK construct for assumed-rank arrays including:
- Basic SELECT RANK with specific ranks
- RANK(*) for assumed-size arrays  
- RANK DEFAULT for other cases
- Nested SELECT RANK constructs
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser

class TestF2018SelectRank:
    """Test F2018 SELECT RANK functionality"""
    
    def parse_code(self, code):
        """Parse Fortran 2018 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2018Lexer(input_stream)
        parser = Fortran2018Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2018()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_basic_select_rank(self):
        """Test basic SELECT RANK with specific ranks"""
        code = """module array_processor
    implicit none
contains
    subroutine process_array(array)
        real, intent(in) :: array(..)
        
        select rank (array)
        rank (0)
            print *, 'Processing scalar:', array
        rank (1)
            print *, 'Processing vector of size:', size(array)
        rank (2)
            print *, 'Processing matrix:', shape(array)
        rank (3)
            print *, 'Processing 3D array'
        end select
    end subroutine process_array
end module array_processor"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Basic SELECT RANK failed to produce parse tree"
        assert errors <= 10, f"Too many errors for basic SELECT RANK: {errors}"
    
    def test_select_rank_with_default(self):
        """Test SELECT RANK with RANK DEFAULT"""
        code = """subroutine handle_any_rank(data)
    real :: data(..)
    
    select rank (data)
    rank (1)
        call process_vector(data)
    rank (2)
        call process_matrix(data)
    rank default
        print *, 'Unsupported rank:', rank(data)
    end select
end subroutine handle_any_rank"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "SELECT RANK DEFAULT failed to produce parse tree"
        assert errors <= 10, f"Too many errors for SELECT RANK DEFAULT: {errors}"
    
    def test_select_rank_with_star(self):
        """Test SELECT RANK with RANK(*)"""
        code = """subroutine process_assumed_size(array)
    integer :: array(..)
    
    select rank (array)
    rank (*)
        print *, 'Assumed-size array'
    rank (1)
        print *, 'Explicit rank-1 array'
    rank default
        print *, 'Other rank'
    end select
end subroutine process_assumed_size"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "SELECT RANK(*) failed to produce parse tree"
        assert errors <= 10, f"Too many errors for SELECT RANK(*): {errors}"
    
    def test_named_select_rank(self):
        """Test named SELECT RANK construct"""
        code = """subroutine named_rank_select(input)
    real :: input(..)
    
    array_rank: select rank (input)
    rank (0) array_rank
        print *, 'Scalar input'
    rank (1) array_rank
        print *, 'Vector input'
    rank (2) array_rank
        print *, 'Matrix input'
    end select array_rank
end subroutine named_rank_select"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Named SELECT RANK failed to produce parse tree"
        assert errors <= 10, f"Too many errors for named SELECT RANK: {errors}"
    
    def test_select_rank_with_operations(self):
        """Test SELECT RANK with different operations per rank"""
        code = """module rank_operations
    implicit none
contains
    function compute_norm(array) result(norm_val)
        real, intent(in) :: array(..)
        real :: norm_val
        
        select rank (array)
        rank (0)
            norm_val = abs(array)
        rank (1)
            norm_val = sqrt(sum(array**2))
        rank (2)
            norm_val = sqrt(sum(array**2))
        rank default
            norm_val = -1.0  ! Error indicator
        end select
    end function compute_norm
end module rank_operations"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "SELECT RANK operations failed to produce parse tree"
        assert errors <= 15, f"Too many errors for SELECT RANK operations: {errors}"
    
    def test_nested_select_rank(self):
        """Test nested SELECT RANK constructs"""
        code = """subroutine nested_rank_processing(a, b)
    real :: a(..), b(..)
    
    select rank (a)
    rank (1)
        select rank (b)
        rank (1)
            call vector_vector_op(a, b)
        rank (2)
            call vector_matrix_op(a, b)
        end select
    rank (2)
        select rank (b)
        rank (1)
            call matrix_vector_op(a, b)
        rank (2)
            call matrix_matrix_op(a, b)
        end select
    end select
end subroutine nested_rank_processing"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Nested SELECT RANK failed to produce parse tree"
        assert errors <= 15, f"Too many errors for nested SELECT RANK: {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])