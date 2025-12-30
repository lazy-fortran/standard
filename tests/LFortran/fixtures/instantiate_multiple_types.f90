! Test: INSTANTIATE statement with multiple type parameters
! Reference: J3/24-107r1 INSTANTIATE syntax

program test_instantiate_multiple
    use pair_module
    implicit none

    ! Instantiation with multiple type parameters
    instantiate pair_t(integer, real(8)), only: pair_ir => pair
    instantiate pair_t(real(4), real(8)), only: pair_rr => pair

    type(pair_ir) :: p1
    type(pair_rr) :: p2

    p1%first = 42
    p1%second = 3.14d0

    p2%first = 1.0
    p2%second = 2.71828d0
end program test_instantiate_multiple
