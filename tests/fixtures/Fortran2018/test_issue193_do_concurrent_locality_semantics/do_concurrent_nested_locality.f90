module do_concurrent_nested_locality
    ! ISO/IEC 1539-1:2018 Section 11.1.7.5 - Nested DO CONCURRENT with locality
    implicit none
contains
    subroutine matrix_init(mat, n, m)
        real, intent(out) :: mat(:, :)
        integer, intent(in) :: n, m
        integer :: i, j
        real :: row_val, col_val

        do concurrent(i=1:n) local(row_val) shared(n)
            row_val = real(i)
            do concurrent(j=1:m) local(col_val) shared(m)
                col_val = real(j)
                mat(i, j) = row_val + col_val
            end do
        end do
    end subroutine matrix_init
end module do_concurrent_nested_locality
