! MASKED ELSEWHERE with construct name
! ISO/IEC 1539-1:2010 Section 8.1.4.3, R807

program test_masked_elsewhere_named
  implicit none
  real :: x(10)

  x = 0.0

  array_op: where (x > 0.0)
    x = 1.0
  masked elsewhere array_op
    x = -1.0
  end where array_op

end program test_masked_elsewhere_named
