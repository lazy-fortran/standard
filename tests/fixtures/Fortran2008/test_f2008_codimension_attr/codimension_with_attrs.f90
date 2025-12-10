module coarray_with_attrs
    implicit none

    ! CODIMENSION with other attributes
    ! ISO/IEC 1539-1:2010 Section 5.3.6
    integer, allocatable, codimension[*] :: alloc_coarray(:)
    real, codimension[*] :: input_coarray
    integer, parameter :: n = 10
    real, codimension[*] :: fixed_coarray

end module coarray_with_attrs
