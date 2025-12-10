! Mixed regular ELSEWHERE and MASKED ELSEWHERE
! ISO/IEC 1539-1:2010 Section 8.1.4.3, R807
! Both forms can be used together in F2008

program test_masked_elsewhere_mixed
  implicit none
  real :: x(10), y(10)

  x = 0.0
  y = 0.0

  where (x > 1.0)
    y = 2.0
  elsewhere (x > 0.5)
    y = 1.5
  masked elsewhere
    y = -1.0
  end where

end program test_masked_elsewhere_mixed
