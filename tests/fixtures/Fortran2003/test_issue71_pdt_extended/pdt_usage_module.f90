module pdt_usage
  implicit none

  type :: matrix_t(k, m, n)
    integer, kind :: k
    integer, len  :: m, n
    real(k)       :: data(m, n)
  end type matrix_t

  type :: system_t
    type(matrix_t(8,3,3)) :: mass
    type(matrix_t(8,3,3)) :: stiffness
  end type system_t

contains

  subroutine init_system(sys)
    type(system_t), intent(out) :: sys
    ! Simple whole-object assignments; detailed component assignment
    ! semantics are outside the scope of this syntax-focused test.
    sys%mass      = sys%mass
    sys%stiffness = sys%stiffness
  end subroutine init_system

  function norm_matrix(m) result(res)
    type(matrix_t(8,3,3)), intent(in) :: m
    real(8)                            :: res
    res = sum(abs(m%data))
  end function norm_matrix

end module pdt_usage

