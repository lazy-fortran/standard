! DT edit descriptors with interface block defined I/O (ISO 1539-1:2004)
!
! Tests DT edit descriptors used with defined I/O established via
! interface blocks rather than type-bound procedures.

module interface_dtio
  implicit none

  type :: person_t
    character(50) :: name
    integer :: age
  end type person_t

  interface read(formatted)
     subroutine person_read_formatted(dtv, u, iotype, vlist, ios, msg)
       import :: person_t
       type(person_t), intent(inout) :: dtv
       integer, intent(in)           :: u
       character(*), intent(in)      :: iotype
       integer, intent(in)           :: vlist(:)
       integer, intent(out)          :: ios
       character(*), intent(inout)   :: msg
     end subroutine person_read_formatted
  end interface

  interface write(formatted)
     subroutine person_write_formatted(dtv, u, iotype, vlist, ios, msg)
       import :: person_t
       type(person_t), intent(in)  :: dtv
       integer, intent(in)         :: u
       character(*), intent(in)    :: iotype
       integer, intent(in)         :: vlist(:)
       integer, intent(out)        :: ios
       character(*), intent(inout) :: msg
     end subroutine person_write_formatted
  end interface

end module interface_dtio

program test_dt_interface
  use interface_dtio, only: person_t
  implicit none

  type(person_t) :: p1, p2

  p1%name = 'Alice'
  p1%age = 30
  p2%name = 'Bob'
  p2%age = 25

  ! DT with interface-block defined I/O
  write(*, '(DT)') p1
  write(*, '(DT"person")') p1
  write(*, '(DT(30,4))') p1
  write(*, '(DT"person_record"(50,3))') p1

  ! Multiple DT for array-like output
  write(*, '(DT,/,DT)') p1, p2

  ! DT with repeat count (grammar allows, semantics in DTIO)
  write(*, '(2DT)') p1, p2

end program test_dt_interface
