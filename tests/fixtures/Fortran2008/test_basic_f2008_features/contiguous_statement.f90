module test_contiguous_stmt
    implicit none
    real, pointer :: a(:), b(:)
    contiguous :: a, b
end module test_contiguous_stmt

