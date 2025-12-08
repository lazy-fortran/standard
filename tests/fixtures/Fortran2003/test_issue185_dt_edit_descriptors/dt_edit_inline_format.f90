! DT edit descriptors in inline format strings (ISO 1539-1:2004 10.2.2)
!
! DT edit descriptors have the form: DT [ char-literal-constant ] [ ( v-list ) ]
! where:
!   - char-literal-constant is an optional type-name string
!   - v-list is an optional list of integer values passed to the DTIO procedure
!
! This grammar treats FORMAT strings as opaque character literals; the DT
! descriptor syntax inside them is not parsed structurally.

module dt_types
  implicit none

  integer, parameter :: dp = kind(1.0d0)

  type :: point_t
    real(dp) :: x, y
  contains
    procedure :: write_fmt => point_write_formatted
    generic :: write(formatted) => write_fmt
  end type point_t

contains

  subroutine point_write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(point_t), intent(in)    :: dtv
    integer, intent(in)           :: unit
    character(*), intent(in)      :: iotype
    integer, intent(in)           :: v_list(:)
    integer, intent(out)          :: iostat
    character(*), intent(inout)   :: iomsg

    write(unit, '(A,2G12.5)', iostat=iostat) iotype, dtv%x, dtv%y
    iomsg = ''
  end subroutine point_write_formatted

end module dt_types

program test_dt_inline
  use dt_types, only: point_t
  implicit none

  type(point_t) :: p1, p2

  p1%x = 1.0d0
  p1%y = 2.0d0
  p2%x = 3.0d0
  p2%y = 4.0d0

  ! Basic DT edit descriptor
  write(*, '(DT)') p1

  ! DT with type-name string
  write(*, '(DT"point_t")') p1

  ! DT with v-list (integer parameters)
  write(*, '(DT(10,2))') p1

  ! DT with both type-name and v-list
  write(*, '(DT"custom_format"(5,3,1))') p1

  ! Multiple DT descriptors in one format
  write(*, '(DT,A,DT)') p1, ' | ', p2

  ! DT mixed with regular edit descriptors
  write(*, '(A,DT"point"(8,2),A)') 'Point: ', p1, ' units'

  ! Single-quoted type-name
  write(*, "(DT'point_type')") p1

end program test_dt_inline
