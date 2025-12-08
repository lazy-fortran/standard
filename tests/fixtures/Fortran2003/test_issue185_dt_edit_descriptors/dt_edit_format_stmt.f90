! DT edit descriptors in complex FORMAT strings (ISO 1539-1:2004 10.2.2)
!
! Tests various DT descriptor forms within complex format specifications.
! The grammar treats FORMAT strings as opaque character literals; the DT
! descriptor syntax inside them is not parsed structurally.

module matrix_io
  implicit none

  type :: matrix_t
    integer :: rows, cols
  contains
    procedure :: write_fmt
    procedure :: read_fmt
    generic :: write(formatted) => write_fmt
    generic :: read(formatted) => read_fmt
  end type matrix_t

contains

  subroutine write_fmt(dtv, u, iotype, vlist, ios, msg)
    class(matrix_t), intent(in) :: dtv
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: ios
    character(*), intent(inout) :: msg

    write(u, '(I0,A,I0)') dtv%rows, 'x', dtv%cols
    ios = 0
    msg = ''
  end subroutine write_fmt

  subroutine read_fmt(dtv, u, iotype, vlist, ios, msg)
    class(matrix_t), intent(inout) :: dtv
    integer, intent(in)            :: u
    character(*), intent(in)       :: iotype
    integer, intent(in)            :: vlist(:)
    integer, intent(out)           :: ios
    character(*), intent(inout)    :: msg

    ios = 0
    msg = ''
  end subroutine read_fmt

end module matrix_io

program test_dt_format_stmt
  use matrix_io, only: matrix_t
  implicit none

  type(matrix_t) :: m1, m2

  m1%rows = 3
  m1%cols = 4

  m2%rows = 2
  m2%cols = 2

  ! Basic DT descriptor
  write(*, '(DT)') m1

  ! DT with type-name
  write(*, '(DT"matrix")') m1

  ! DT with v-list
  write(*, '(DT(15,4))') m1

  ! DT with type-name and v-list
  write(*, '(DT"matrix_fmt"(10,2,0))') m1

  ! Multiple DT descriptors
  write(*, '(DT,1X,DT)') m1, m2

  ! DT mixed with other edit descriptors
  write(*, '(A8,1X,DT"mat"(5),1X,A3)') 'Matrix:', m1, 'end'

  ! DT with repeat count
  write(*, '(2DT)') m1, m2

  ! DT in parenthesized group with repeat
  write(*, '(2(DT,1X))') m1, m2

end program test_dt_format_stmt
