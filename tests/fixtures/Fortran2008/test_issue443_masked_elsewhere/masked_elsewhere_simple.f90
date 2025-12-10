! Simple MASKED ELSEWHERE construct
! ISO/IEC 1539-1:2010 Section 8.1.4.3, R807

program test_masked_elsewhere
  implicit none
  real :: x(10)

  x = 0.0

  where (x > 0.0)
    x = 1.0
  masked elsewhere
    x = -1.0
  end where

end program test_masked_elsewhere
