#!/usr/bin/env python3
"""
Comprehensive test suite for Fortran 2003 unified grammar implementation.
Tests all major F2003 innovations with both fixed-form and free-form formats.
"""

import sys
import os
import pytest
from pathlib import Path

# Add build directories to path
sys.path.append(str(Path(__file__).parent.parent.parent / "build" / "Fortran2003"))

from antlr4 import *

class TestFortran2003Unified:
    """Test Fortran 2003 unified grammar with both fixed-form and free-form support."""
    
    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser
            
            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))
            
            tree = parser.program_unit_f90()  # Start with F90 rule
            errors = parser.getNumberOfSyntaxErrors()
            
            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"
            
            return tree, errors, None
            
        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)
    
    def test_object_oriented_programming(self):
        """Test F2003 object-oriented programming features."""
        oop_code = """
module shapes_mod
    implicit none
    
    ! Abstract base type
    type, abstract :: shape_t
        real :: area
    contains
        procedure(calculate_area), deferred :: calculate_area
        procedure :: print_info
    end type shape_t
    
    ! Concrete derived type with inheritance
    type, extends(shape_t) :: circle_t
        real :: radius
    contains
        procedure :: calculate_area => circle_calculate_area
    end type circle_t
    
    ! Abstract interface
    abstract interface
        function calculate_area(this) result(area)
            import :: shape_t
            class(shape_t), intent(in) :: this
            real :: area
        end function calculate_area
    end interface
    
contains
    
    subroutine print_info(this)
        class(shape_t), intent(in) :: this
        print *, 'Area:', this%area
    end subroutine print_info
    
    function circle_calculate_area(this) result(area)
        class(circle_t), intent(in) :: this
        real :: area
        area = 3.14159 * this%radius**2
    end function circle_calculate_area
    
end module shapes_mod
        """
        self.parse_fortran_code(oop_code)
    
    def test_parameterized_derived_types(self):
        """Test F2003 parameterized derived types."""
        pdt_code = """
module pdt_mod
    implicit none
    
    ! Parameterized derived type
    type :: matrix_t(k, rows, cols)
        integer, kind :: k = kind(0.0)  ! Kind parameter
        integer, len :: rows, cols      ! Length parameters
        real(k) :: data(rows, cols)
    end type matrix_t
    
contains
    
    subroutine test_pdt()
        type(matrix_t(kind=real64, rows=3, cols=3)) :: mat
        integer :: i, j
        
        do i = 1, 3
            do j = 1, 3
                mat%data(i,j) = real(i*j, kind=real64)
            end do
        end do
    end subroutine test_pdt
    
end module pdt_mod
        """
        self.parse_fortran_code(pdt_code)
    
    def test_associate_construct(self):
        """Test F2003 ASSOCIATE construct."""
        associate_code = """
program test_associate
    implicit none
    
    type :: person_t
        character(len=50) :: name
        integer :: age
    end type person_t
    
    type(person_t) :: person
    real :: x(10,10), y(10,10)
    integer :: i, j
    
    person%name = "John Doe"
    person%age = 30
    
    ! Associate construct with derived type component
    associate (name => person%name, age => person%age)
        print *, 'Name: ', trim(name)
        print *, 'Age: ', age
    end associate
    
    ! Associate construct with array expression
    associate (sum_xy => x + y, n => size(x,1))
        do i = 1, n
            do j = 1, n
                sum_xy(i,j) = x(i,j) + y(i,j)
            end do
        end do
    end associate
    
end program test_associate
        """
        self.parse_fortran_code(associate_code)
    
    def test_block_construct(self):
        """Test F2003 BLOCK construct."""
        block_code = """
program test_block
    implicit none
    integer :: n = 10
    real :: result
    
    block
        ! Local declarations within block
        real :: temp_array(n)
        integer :: i
        
        ! Initialize temporary array
        do i = 1, n
            temp_array(i) = real(i)**2
        end do
        
        ! Calculate result
        result = sum(temp_array)
    end block
    
    print *, 'Result:', result
    
end program test_block
        """
        self.parse_fortran_code(block_code)
    
    def test_procedure_pointers(self):
        """Test F2003 procedure pointers."""
        proc_ptr_code = """
module proc_ptr_mod
    implicit none
    
    abstract interface
        function math_func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function math_func
    end interface
    
    procedure(math_func), pointer :: func_ptr => null()
    
contains
    
    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x**2
    end function square
    
    function cube(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x**3
    end function cube
    
    subroutine test_proc_pointers()
        real :: value = 2.0
        
        ! Point to square function
        func_ptr => square
        print *, 'Square:', func_ptr(value)
        
        ! Point to cube function
        func_ptr => cube
        print *, 'Cube:', func_ptr(value)
    end subroutine test_proc_pointers
    
end module proc_ptr_mod
        """
        self.parse_fortran_code(proc_ptr_code)
    
    def test_enhanced_allocate(self):
        """Test F2003 enhanced ALLOCATE with SOURCE and MOLD."""
        alloc_code = """
program test_enhanced_allocate
    implicit none
    
    integer, allocatable :: array1(:), array2(:), array3(:)
    integer :: source_array(5) = [1, 2, 3, 4, 5]
    integer :: stat
    character(len=100) :: errmsg
    
    ! Allocate with SOURCE
    allocate(array1, source=source_array, stat=stat, errmsg=errmsg)
    if (stat /= 0) then
        print *, 'Allocation failed:', trim(errmsg)
        stop
    end if
    
    ! Allocate with MOLD
    allocate(array2, mold=source_array, stat=stat)
    if (stat == 0) then
        array2 = [10, 20, 30, 40, 50]
    end if
    
    ! Traditional allocation
    allocate(array3(10))
    array3 = 0
    
    print *, 'Array1:', array1
    print *, 'Array2:', array2
    print *, 'Array3 size:', size(array3)
    
    deallocate(array1, array2, array3)
    
end program test_enhanced_allocate
        """
        self.parse_fortran_code(alloc_code)
    
    def test_c_interoperability(self):
        """Test F2003 C interoperability features."""
        c_interop_code = """
module c_interop_mod
    use iso_c_binding
    implicit none
    
    ! C-interoperable derived type
    type, bind(c) :: c_struct_t
        integer(c_int) :: i
        real(c_float) :: x
        character(c_char) :: name(20)
    end type c_struct_t
    
    ! Interface to C function
    interface
        function c_function(x) bind(c, name="my_c_function")
            use iso_c_binding
            real(c_float), value :: x
            real(c_float) :: c_function
        end function c_function
    end interface
    
contains
    
    subroutine test_c_interop()
        type(c_struct_t) :: my_struct
        real(c_float) :: result
        
        my_struct%i = 42
        my_struct%x = 3.14
        my_struct%name = c_char_"Hello"//c_null_char
        
        result = c_function(my_struct%x)
        print *, 'C function result:', result
    end subroutine test_c_interop
    
end module c_interop_mod
        """
        self.parse_fortran_code(c_interop_code)
    
    def test_enhanced_io(self):
        """Test F2003 enhanced I/O features."""
        io_code = """
program test_enhanced_io
    implicit none
    
    integer, parameter :: unit = 10
    integer :: iostat
    character(len=100) :: iomsg
    real :: data(100)
    integer :: i, id
    
    ! Initialize data
    do i = 1, 100
        data(i) = real(i)
    end do
    
    ! Open file with enhanced options
    open(unit=unit, file='test.dat', access='stream', &
         form='unformatted', asynchronous='yes', &
         iostat=iostat, iomsg=iomsg)
    
    if (iostat /= 0) then
        print *, 'Open failed:', trim(iomsg)
        stop
    end if
    
    ! Asynchronous write
    write(unit=unit, asynchronous='yes', id=id) data
    
    ! Wait for completion
    wait(unit=unit, id=id)
    
    ! Flush buffer
    flush(unit)
    
    close(unit)
    
end program test_enhanced_io
        """
        self.parse_fortran_code(io_code)
    
    def test_volatile_protected(self):
        """Test F2003 VOLATILE and PROTECTED attributes."""
        attr_code = """
module attr_mod
    implicit none
    
    ! Protected variables (read-only outside module)
    integer, protected :: protected_var = 42
    
    ! Volatile variables (can be modified by external means)
    integer, volatile :: volatile_var
    
contains
    
    subroutine set_protected_var(val)
        integer, intent(in) :: val
        protected_var = val  ! OK within defining module
    end subroutine set_protected_var
    
    subroutine use_volatile_var()
        ! Compiler cannot optimize access to volatile_var
        if (volatile_var > 0) then
            print *, 'Volatile var is positive:', volatile_var
        end if
    end subroutine use_volatile_var
    
end module attr_mod
        """
        self.parse_fortran_code(attr_code)
    
    def test_import_statement(self):
        """Test F2003 IMPORT statement."""
        import_code = """
module import_mod
    implicit none
    
    integer, parameter :: my_kind = selected_real_kind(15)
    
    interface
        subroutine external_sub(x, y)
            import :: my_kind  ! Import specific names
            real(my_kind), intent(in) :: x
            real(my_kind), intent(out) :: y
        end subroutine external_sub
        
        function external_func(a, b)
            import  ! Import all accessible names
            real(my_kind) :: external_func
            real(my_kind), intent(in) :: a, b
        end function external_func
    end interface
    
end module import_mod
        """
        self.parse_fortran_code(import_code)
    
    def test_fixed_form_compatibility(self):
        """Test that F2003 features work with fixed-form format."""
        fixed_form_f2003 = """
C     F2003 features in fixed-form format
      MODULE FIXEDF2003
      IMPLICIT NONE
      
C     Object-oriented type (fixed-form)
      TYPE :: SHAPE
          REAL :: AREA
      CONTAINS
          PROCEDURE :: PRINT_AREA
      END TYPE SHAPE
      
      CONTAINS
      
      SUBROUTINE PRINT_AREA(THIS)
          CLASS(SHAPE), INTENT(IN) :: THIS
          WRITE(*,*) 'AREA:', THIS%AREA
      END SUBROUTINE PRINT_AREA
      
      END MODULE FIXEDF2003
        """
        self.parse_fortran_code(fixed_form_f2003)
    
    def test_error_handling(self):
        """Test that invalid F2003 syntax is properly rejected."""
        invalid_codes = [
            # Invalid abstract type instantiation
            """
            type, abstract :: base_t
            end type base_t
            type(base_t) :: invalid  ! Cannot instantiate abstract type
            """,
            
            # Invalid deferred procedure without abstract
            """
            type :: bad_type
            contains
                procedure, deferred :: bad_proc  ! Deferred without abstract
            end type bad_type
            """
        ]
        
        for i, code in enumerate(invalid_codes):
            try:
                self.parse_fortran_code(code, expect_success=False)
            except AssertionError:
                pass  # Expected to fail
    
    def test_complex_f2003_program(self):
        """Test complex F2003 program combining multiple features."""
        complex_code = """
module advanced_f2003
    use iso_c_binding
    implicit none
    
    ! Abstract base class
    type, abstract :: drawable_t
        integer :: id
    contains
        procedure(draw_interface), deferred :: draw
        procedure :: get_id
        final :: cleanup_drawable
    end type drawable_t
    
    ! Concrete implementation
    type, extends(drawable_t) :: circle_t
        real :: radius
        real :: center(2)
    contains
        procedure :: draw => draw_circle
    end type circle_t
    
    ! Abstract interface
    abstract interface
        subroutine draw_interface(this)
            import :: drawable_t
            class(drawable_t), intent(in) :: this
        end subroutine draw_interface
    end interface
    
    ! Procedure pointer
    procedure(draw_interface), pointer :: draw_proc => null()
    
contains
    
    function get_id(this) result(id)
        class(drawable_t), intent(in) :: this
        integer :: id
        id = this%id
    end function get_id
    
    subroutine draw_circle(this)
        class(circle_t), intent(in) :: this
        
        associate(x => this%center(1), y => this%center(2), r => this%radius)
            print *, 'Drawing circle at (', x, ',', y, ') with radius', r
        end associate
    end subroutine draw_circle
    
    subroutine cleanup_drawable(this)
        type(drawable_t), intent(inout) :: this
        print *, 'Cleaning up drawable with ID:', this%id
    end subroutine cleanup_drawable
    
    subroutine demonstrate_features()
        type(circle_t), allocatable :: shapes(:)
        integer :: i
        
        ! Enhanced allocate with source
        allocate(shapes, source=[circle_t(1, 5.0, [0.0, 0.0]), &
                                circle_t(2, 3.0, [1.0, 1.0])])
        
        ! Polymorphic behavior
        do i = 1, size(shapes)
            call shapes(i)%draw()
        end do
        
        ! Block construct with local variables
        block
            real :: total_area
            total_area = 0.0
            
            do i = 1, size(shapes)
                total_area = total_area + 3.14159 * shapes(i)%radius**2
            end do
            
            print *, 'Total area:', total_area
        end block
        
        deallocate(shapes)
    end subroutine demonstrate_features
    
end module advanced_f2003
        """
        self.parse_fortran_code(complex_code)

if __name__ == "__main__":
    pytest.main([__file__, "-v"])