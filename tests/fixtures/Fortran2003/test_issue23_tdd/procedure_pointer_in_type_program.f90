program test_prog
    abstract interface
        subroutine operation_interface(x, y)
            real, intent(in) :: x
            real, intent(out) :: y
        end subroutine
    end interface

    type :: processor_t
        procedure(operation_interface), pointer, nopass :: operation
    end type processor_t

    type(processor_t) :: obj

    obj%operation => some_procedure
end program test_prog

