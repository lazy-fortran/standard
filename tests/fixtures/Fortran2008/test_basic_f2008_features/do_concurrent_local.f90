! Test Fortran 2008 DO CONCURRENT LOCAL specifier
! ISO/IEC 1539-1:2010 Section 8.1.6.6, R821: concurrent-locality

module test_do_concurrent_local
    implicit none
contains

    subroutine test_local_basic()
        real :: temp
        integer :: i, n
        real, allocatable :: data(:), results(:)

        n = 100
        allocate(data(n), results(n))

        ! Basic LOCAL specifier with single variable
        ! Section 8.1.6.6, R821: LOCAL ( local-variable-list )
        DO CONCURRENT (i = 1:n, LOCAL(temp))
            temp = data(i) * 2.0
            results(i) = temp
        END DO

        deallocate(data, results)
    end subroutine test_local_basic

    subroutine test_local_multiple()
        real :: tmp1, tmp2, result
        integer :: i, n
        real, allocatable :: data(:)

        n = 100
        allocate(data(n))

        ! Multiple LOCAL variables
        DO CONCURRENT (i = 1:n, LOCAL(tmp1, tmp2, result))
            tmp1 = data(i)
            tmp2 = tmp1 * tmp1
            result = sqrt(tmp2)
            data(i) = result
        END DO

        deallocate(data)
    end subroutine test_local_multiple

    subroutine test_local_with_mask()
        real :: temp
        integer :: i, n
        real, allocatable :: data(:)

        n = 100
        allocate(data(n))

        ! LOCAL with mask expression
        DO CONCURRENT (i = 1:n, data(i) > 50.0, LOCAL(temp))
            temp = data(i) / 2.0
            data(i) = temp
        END DO

        deallocate(data)
    end subroutine test_local_with_mask

    subroutine test_local_multiple_indices()
        real :: temp_val
        integer :: i, j
        real :: matrix(10, 10)

        ! LOCAL with multiple loop indices
        DO CONCURRENT (i = 1:10, j = 1:10, LOCAL(temp_val))
            temp_val = matrix(i, j) * 2.0
            matrix(i, j) = temp_val
        END DO
    end subroutine test_local_multiple_indices

    subroutine test_local_as_identifier()
        real :: local

        ! LOCAL variable is used as identifier elsewhere (not in DO CONCURRENT context)
        local = 3.14
    end subroutine test_local_as_identifier

end module test_do_concurrent_local
