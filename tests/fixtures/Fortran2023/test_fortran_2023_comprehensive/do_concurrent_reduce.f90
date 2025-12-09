program test_do_concurrent_reduce
    ! ISO/IEC 1539-1:2023 Section 11.1.7.5
    ! REDUCE locality specifier for DO CONCURRENT
    implicit none

    integer :: i
    integer :: n = 100
    real :: a(100)
    real :: total, maximum, minimum
    integer :: product_val
    logical :: all_positive, any_negative

    ! Initialize array
    do i = 1, n
        a(i) = real(i)
    end do

    ! Sum reduction with REDUCE(+:total)
    total = 0.0
    do concurrent (i = 1:n) reduce(+:total)
        total = total + a(i)
    end do

    ! Product reduction with REDUCE(*:product_val)
    product_val = 1
    do concurrent (i = 1:5) reduce(*:product_val)
        product_val = product_val * i
    end do

    ! Maximum reduction with REDUCE(max:maximum)
    maximum = a(1)
    do concurrent (i = 2:n) reduce(max:maximum)
        maximum = max(maximum, a(i))
    end do

    ! Minimum reduction with REDUCE(min:minimum)
    minimum = a(1)
    do concurrent (i = 2:n) reduce(min:minimum)
        minimum = min(minimum, a(i))
    end do

    ! Logical AND reduction with REDUCE(.and.:all_positive)
    all_positive = .true.
    do concurrent (i = 1:n) reduce(.and.:all_positive)
        all_positive = all_positive .and. (a(i) > 0.0)
    end do

    ! Logical OR reduction with REDUCE(.or.:any_negative)
    any_negative = .false.
    do concurrent (i = 1:n) reduce(.or.:any_negative)
        any_negative = any_negative .or. (a(i) < 0.0)
    end do

    print *, total, maximum, minimum, product_val
    print *, all_positive, any_negative

end program test_do_concurrent_reduce
