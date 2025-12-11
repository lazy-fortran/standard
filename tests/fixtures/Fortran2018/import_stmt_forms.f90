module import_stmt_forms
  type :: host_type
    integer :: value
  end type host_type

contains

  interface
    module subroutine import_default(x)
      import
      integer, intent(inout) :: x
    end subroutine import_default

    module subroutine import_with_double_colon(x)
      import :: host_type
      type(host_type), intent(inout) :: x
    end subroutine import_with_double_colon

    module subroutine import_without_colon(x)
      import host_type
      type(host_type), intent(inout) :: x
    end subroutine import_without_colon

    module subroutine import_with_only(x)
      import, only: host_type
      type(host_type), intent(inout) :: x
    end subroutine import_with_only

    module subroutine import_with_none(value)
      import, none
      integer, intent(inout) :: value
    end subroutine import_with_none

    module subroutine import_with_all(value)
      import, all
      integer, intent(inout) :: value
    end subroutine import_with_all
  end interface

end module import_stmt_forms
