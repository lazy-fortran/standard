program co_reduce_test
  implicit none
  integer :: result_val[*]

  result_val = 7
  call co_reduce(result_val, operation=add_op)

contains
  subroutine add_op(a, b)
    integer, intent(inout) :: a
    integer, intent(in) :: b
    a = a + b
  end subroutine add_op

end program co_reduce_test
