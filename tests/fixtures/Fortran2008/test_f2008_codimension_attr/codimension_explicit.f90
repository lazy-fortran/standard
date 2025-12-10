module coarray_explicit
    implicit none

    ! Explicit codimension with CODIMENSION attribute
    ! ISO/IEC 1539-1:2010 R511: explicit-coshape-spec
    integer, codimension[0:*] :: my_coarray
    real :: matrix_coarray(100)[1:10,*]

contains

    subroutine example_coarray_use()
        integer, codimension[*] :: local_coarray
        real :: large_array(100)[*]
    end subroutine

end module coarray_explicit
