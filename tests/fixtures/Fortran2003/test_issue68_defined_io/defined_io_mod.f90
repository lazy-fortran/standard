module defined_io_mod
  implicit none

  type :: point_t
    real :: x, y
  contains
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
  end type point_t

contains

  subroutine write_formatted(dtv, u, iotype, v_list, ios, msg)
    class(point_t), intent(in) :: dtv
    integer, intent(in)        :: u
    character(*), intent(in)   :: iotype
    integer, intent(in)        :: v_list(:)
    integer, intent(out)       :: ios
    character(*), intent(inout):: msg

    write(u, '(2F10.4)') dtv%x, dtv%y
    ios    = 0
    msg    = ''
  end subroutine write_formatted

end module defined_io_mod

