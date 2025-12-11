! Test Fortran 2008 advanced bit manipulation intrinsics (ISO/IEC 1539-1:2010 Section 13.7)
! - DSHIFTL(I,J,SHIFT): Combined left shift (Section 13.7.50)
! - DSHIFTR(I,J,SHIFT): Combined right shift (Section 13.7.51)
! - LEADZ(I): Leading zero count (Section 13.7.103)
! - MERGE_BITS(I,J,MASK): Merge bits under mask (Section 13.7.112)
! - POPCNT(I): Population count (Section 13.7.133)
! - POPPAR(I): Population parity (Section 13.7.134)
! - TRAILZ(I): Trailing zero count (Section 13.7.168)

module test_advanced_bit_intrinsics
    implicit none
contains
    subroutine test_double_shift_functions()
        ! Test DSHIFTL and DSHIFTR (Section 13.7.50-51)
        integer :: a, b, shift, result_val

        a = 15
        b = 240
        shift = 4

        ! Section 13.7.50: DSHIFTL - Combined left shift
        result_val = dshiftl(a, b, shift)

        ! Section 13.7.51: DSHIFTR - Combined right shift
        result_val = dshiftr(a, b, shift)
    end subroutine test_double_shift_functions

    subroutine test_bit_counting_functions()
        ! Test LEADZ, POPCNT, POPPAR, TRAILZ (Section 13.7.103, 133-134, 168)
        integer :: a, count_val

        a = 15

        ! Section 13.7.103: LEADZ - Leading zero count
        count_val = leadz(a)
        count_val = leadz(0)

        ! Section 13.7.133: POPCNT - Population count
        count_val = popcnt(a)
        count_val = popcnt(-1)

        ! Section 13.7.134: POPPAR - Population parity
        count_val = poppar(a)
        count_val = poppar(7)

        ! Section 13.7.168: TRAILZ - Trailing zero count
        count_val = trailz(a)
        count_val = trailz(16)
    end subroutine test_bit_counting_functions

    subroutine test_merge_bits_function()
        ! Test MERGE_BITS (Section 13.7.112)
        integer :: a, b, mask, result_val

        a = 15
        b = 240
        mask = 255

        ! Section 13.7.112: MERGE_BITS - Merge under mask
        ! Result = IOR(IAND(I, MASK), IAND(J, NOT(MASK)))
        result_val = merge_bits(a, b, mask)
        result_val = merge_bits(a, b, 0)
        result_val = merge_bits(a, b, -1)
    end subroutine test_merge_bits_function

    subroutine test_array_operations()
        ! Test elemental behavior with arrays
        integer :: arr_a(4), arr_b(4), arr_result(4)
        integer :: arr_counts(4)
        integer :: shift

        arr_a = [1, 3, 7, 15]
        arr_b = [16, 32, 64, 128]
        shift = 2

        ! Double shift with arrays
        arr_result = dshiftl(arr_a, arr_b, shift)
        arr_result = dshiftr(arr_a, arr_b, shift)

        ! Bit counting with arrays
        arr_counts = popcnt(arr_a)
        arr_counts = leadz(arr_a)
        arr_counts = trailz(arr_b)
        arr_counts = poppar(arr_a)
    end subroutine test_array_operations

end module test_advanced_bit_intrinsics
