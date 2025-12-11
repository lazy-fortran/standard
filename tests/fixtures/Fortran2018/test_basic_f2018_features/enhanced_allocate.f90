module enhanced_allocate_specifiers
  implicit none

contains

  subroutine demonstrate_allocate_specifiers()
    integer, allocatable :: arr_stat(:)
    integer, allocatable :: arr_source(:)
    integer, allocatable :: arr_mold(:)
    integer, allocatable :: arr_errmsg(:)
    integer :: stat_code
    character(len=32) :: errmsg
    real, allocatable :: source_template(:)
    real :: mold_template(3) = [1.0, 2.0, 3.0]

    allocate(source_template(3))
    allocate(arr_stat(5), stat=stat_code)
    allocate(arr_source, source=source_template)
    allocate(arr_mold, mold=mold_template)
    allocate(arr_errmsg(2), errmsg=errmsg)
  end subroutine demonstrate_allocate_specifiers

end module enhanced_allocate_specifiers
