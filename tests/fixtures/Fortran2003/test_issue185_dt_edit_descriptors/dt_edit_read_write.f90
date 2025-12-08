! DT edit descriptors in READ and WRITE statements (ISO 1539-1:2004 10.2.2)
!
! Tests both READ and WRITE statements with DT edit descriptors, including
! file I/O operations.

module complex_types
  implicit none

  type :: vector_t
    real :: x, y, z
  contains
    procedure :: write_fmt => vector_write_formatted
    procedure :: read_fmt => vector_read_formatted
    generic :: write(formatted) => write_fmt
    generic :: read(formatted) => read_fmt
  end type vector_t

contains

  subroutine vector_write_formatted(dtv, u, iotype, vlist, ios, msg)
    class(vector_t), intent(in) :: dtv
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: ios
    character(*), intent(inout) :: msg

    write(u, '(3G15.7)') dtv%x, dtv%y, dtv%z
    ios = 0
    msg = ''
  end subroutine vector_write_formatted

  subroutine vector_read_formatted(dtv, u, iotype, vlist, ios, msg)
    class(vector_t), intent(inout) :: dtv
    integer, intent(in)            :: u
    character(*), intent(in)       :: iotype
    integer, intent(in)            :: vlist(:)
    integer, intent(out)           :: ios
    character(*), intent(inout)    :: msg
    real :: tmp_x, tmp_y, tmp_z

    read(u, '(3G15.7)') tmp_x, tmp_y, tmp_z
    dtv%x = tmp_x
    dtv%y = tmp_y
    dtv%z = tmp_z
    ios = 0
    msg = ''
  end subroutine vector_read_formatted

end module complex_types

program test_dt_read_write
  use complex_types
  implicit none

  type(vector_t) :: v1, v2
  integer :: ios
  character(100) :: msg

  v1%x = 1.0
  v1%y = 2.0
  v1%z = 3.0

  ! WRITE with DT to standard output
  write(*, '(DT)') v1
  write(*, '(DT"vec3d")') v1
  write(*, '(DT(12,4))') v1

  ! WRITE with DT to file unit
  open(unit=10, file='vector.dat', status='replace', action='write')
  write(10, '(DT"vector"(15,6))') v1
  close(10)

  ! WRITE with IOSTAT and DT
  write(*, '(DT)', iostat=ios) v1

  ! WRITE with IOMSG and DT
  write(*, '(DT"vector_output")', iomsg=msg) v1

  ! READ with DT from file (syntactic test)
  ! The actual read would require proper input data
  open(unit=20, file='input.dat', status='old', action='read', iostat=ios)
  if (ios == 0) then
    read(20, '(DT)', iostat=ios) v2
    read(20, '(DT"vec3d"(15,6))', iostat=ios) v2
    close(20)
  end if

  ! Multiple DT descriptors with array of derived types
  write(*, '(2(DT,1X))') v1, v2

end program test_dt_read_write
