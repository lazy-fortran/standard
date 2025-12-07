module interface_defined_io
  implicit none

  type :: point_t
    real :: x, y
  end type point_t

  interface write(formatted)
     subroutine write_point_formatted(dtv, u, iotype, v_list, ios, msg)
       import :: point_t
       class(point_t), intent(in) :: dtv
       integer, intent(in)        :: u
       character(*), intent(in)   :: iotype
       integer, intent(in)        :: v_list(:)
       integer, intent(out)       :: ios
       character(*), intent(inout):: msg
     end subroutine write_point_formatted
  end interface

  interface write(unformatted)
     subroutine write_point_unformatted(dtv, u, iotype, v_list, ios, msg)
       import :: point_t
       class(point_t), intent(in) :: dtv
       integer, intent(in)        :: u
       character(*), intent(in)   :: iotype
       integer, intent(in)        :: v_list(:)
       integer, intent(out)       :: ios
       character(*), intent(inout):: msg
     end subroutine write_point_unformatted
  end interface

end module interface_defined_io

