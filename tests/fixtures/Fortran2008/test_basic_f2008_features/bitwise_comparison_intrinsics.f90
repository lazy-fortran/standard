! Test Fortran 2008 bitwise comparison intrinsics (ISO/IEC 1539-1:2010 Section 13.7.28-31)
module test_bitwise_comparison
    implicit none
contains
    subroutine test_bitwise_comparison_functions()
        integer :: a, b
        logical :: ge_result, gt_result, le_result, lt_result

        a = 5
        b = 3

        ! Section 13.7.28: BGE - Bitwise greater-or-equal
        ge_result = BGE(a, b)

        ! Section 13.7.29: BGT - Bitwise greater-than
        gt_result = BGT(a, b)

        ! Section 13.7.30: BLE - Bitwise less-or-equal
        le_result = BLE(a, b)

        ! Section 13.7.31: BLT - Bitwise less-than
        lt_result = BLT(a, b)

        ! Test with negative numbers
        ge_result = BGE(-1, 0)
        gt_result = BGT(-1, 0)
        le_result = BLE(-1, 0)
        lt_result = BLT(-1, 0)

        ! Test with arrays (elemental)
        call test_elemental_bitwise_comparison()
    end subroutine test_bitwise_comparison_functions

    subroutine test_elemental_bitwise_comparison()
        integer :: x(3), y(3)
        logical :: results(3)

        x = [1, 2, 3]
        y = [3, 2, 1]

        results = BGE(x, y)
        results = BGT(x, y)
        results = BLE(x, y)
        results = BLT(x, y)
    end subroutine test_elemental_bitwise_comparison

end module test_bitwise_comparison
