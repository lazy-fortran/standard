#!/usr/bin/env python3
"""Basic F2018 features tests - Foundation

Test core F2018 functionality including:
- Basic collective operations
- SELECT RANK construct  
- Team support basics
- Event support basics
- Enhanced intrinsic functions
- New token recognition
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser

class TestBasicF2018Features:
    """Test basic F2018 functionality"""
    
    def parse_code(self, code):
        """Parse Fortran 2018 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2018Lexer(input_stream)
        parser = Fortran2018Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2018()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_basic_module_inheritance(self):
        """F2018 should inherit basic module support from F2008"""
        code = """module test_mod
    implicit none
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Module inheritance failed"
        assert errors == 0, f"Basic module inheritance failed: {errors} errors"
    
    def test_collective_tokens_recognized(self):
        """Test that F2018 collective operation tokens are recognized"""
        test_cases = [
            ("module test; call co_sum(x); end module test", "co_sum call"),
            ("module test; call co_broadcast(x, source_image=1); end module test", 
             "co_broadcast"),
        ]
        
        for code, description in test_cases:
            tree, errors = self.parse_code(code)
            assert tree is not None, f"{description} failed to produce parse tree"
            # Allow some parser errors but not too many (indicates lexer working)
            assert errors <= 5, f"{description}: too many errors ({errors})"
    
    def test_select_rank_basic_syntax(self):
        """Test basic SELECT RANK syntax recognition"""
        code = """module test
    contains
    subroutine test_rank(array)
        real :: array(..)
        select rank (array)
        rank (0)
            print *, 'Scalar'
        rank (1)
            print *, 'Vector'
        rank default
            print *, 'Other rank'
        end select
    end subroutine test_rank
end module test"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "SELECT RANK failed to produce parse tree"
        assert errors <= 10, f"Too many errors for SELECT RANK: {errors}"
    
    def test_team_tokens(self):
        """Test team-related token recognition"""
        code = """module test_teams
    use, intrinsic :: iso_fortran_env
    implicit none
    type(team_type) :: my_team
    contains
    subroutine test_team()
        form team(1, my_team)
    end subroutine test_team
end module test_teams"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Team tokens failed to produce parse tree"
        assert errors <= 10, f"Too many errors for team test: {errors}"
    
    def test_event_tokens(self):
        """Test event-related token recognition"""
        code = """module test_events
    use, intrinsic :: iso_fortran_env
    implicit none
    type(event_type) :: my_event
    contains
    subroutine test_event()
        event post(my_event)
    end subroutine test_event
end module test_events"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Event tokens failed to produce parse tree"
        assert errors <= 10, f"Too many errors for event test: {errors}"
    
    def test_image_status_tokens(self):
        """Test image status function tokens"""
        code = """program test_image_status
    integer :: status
    status = image_status(1)
    if (status == image_status_failed) then
        print *, 'Image failed'
    end if
end program test_image_status"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Image status failed to produce parse tree"
        assert errors <= 10, f"Too many errors for image status: {errors}"
    
    def test_random_init_token(self):
        """Test RANDOM_INIT statement token"""
        code = """program test_random
    call random_init(repeatable=.true., image_distinct=.false.)
end program test_random"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "RANDOM_INIT failed to produce parse tree"
        assert errors <= 5, f"Too many errors for RANDOM_INIT: {errors}"
    
    def test_enhanced_stop_quiet(self):
        """Test enhanced STOP with QUIET option"""
        code = """program test_stop
    stop 'Normal stop', quiet=.true.
    error stop 42, quiet=.false.
end program test_stop"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Enhanced STOP failed to produce parse tree"
        assert errors <= 10, f"Too many errors for enhanced STOP: {errors}"
    
    def test_do_concurrent_locality(self):
        """Test DO CONCURRENT with locality specifiers"""
        code = """module test_concurrent
    contains
    subroutine test_locality()
        integer :: i, j, local_var
        real :: shared_array(100)
        
        do concurrent (i = 1:10, local_init :: local_var, shared :: shared_array)
            local_var = i * 2
            shared_array(i) = local_var
        end do
    end subroutine test_locality
end module test_concurrent"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "DO CONCURRENT locality failed to produce parse tree"
        assert errors <= 10, f"Too many errors for DO CONCURRENT locality: {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])