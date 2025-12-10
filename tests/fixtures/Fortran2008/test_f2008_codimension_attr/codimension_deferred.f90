module coarray_deferred
    implicit none

    ! Deferred codimension with CODIMENSION attribute
    ! ISO/IEC 1539-1:2010 R509: coarray-spec -> deferred-coshape-spec-list
    integer, codimension[*] :: shared_int
    real(8), codimension[*] :: shared_real
    integer :: shared_array(10)[*]

end module coarray_deferred
