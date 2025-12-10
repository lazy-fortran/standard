module do_concurrent_typed_header
    ! ISO/IEC 1539-1:2018 R1125 - DO CONCURRENT with optional integer-type-spec
    implicit none
    integer :: n = 10
contains
    subroutine process_typed(a)
        ! Test DO CONCURRENT with explicit INTEGER type spec
        integer, intent(inout) :: a(:)
        integer :: temp

        ! Simple form with INTEGER type spec
        do concurrent (integer :: i = 1:n)
            a(i) = a(i) + 1
        end do
    end subroutine process_typed

    subroutine process_typed_kind(a)
        ! Test DO CONCURRENT with INTEGER with kind selector
        integer, intent(inout) :: a(:)
        integer :: temp

        ! INTEGER with kind selector
        do concurrent (integer(kind=4) :: i = 1:n)
            a(i) = a(i) + 2
        end do
    end subroutine process_typed_kind

    subroutine process_typed_with_mask(a)
        ! Test DO CONCURRENT with type spec and mask
        integer, intent(inout) :: a(:)
        integer :: temp

        ! WITH mask and type spec
        do concurrent (integer :: i = 1:n, a(i) > 0)
            a(i) = a(i) + 3
        end do
    end subroutine process_typed_with_mask

    subroutine process_typed_with_locality(a)
        ! Test DO CONCURRENT with type spec and locality specs
        integer, intent(inout) :: a(:)
        integer :: temp

        ! WITH locality and type spec
        do concurrent (integer :: i = 1:n) local(temp) shared(n)
            temp = a(i)
            a(i) = temp + 4
        end do
    end subroutine process_typed_with_locality

    subroutine process_untyped(a)
        ! Test backward compatibility - untyped form still works
        integer, intent(inout) :: a(:)

        do concurrent (i = 1:n)
            a(i) = a(i) + 5
        end do
    end subroutine process_untyped
end module do_concurrent_typed_header
