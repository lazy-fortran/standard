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

