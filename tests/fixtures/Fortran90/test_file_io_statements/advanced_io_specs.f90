program test_advanced_io
    implicit none
    integer :: unit_num, ios, reclen, nextrec_val
    logical :: exists, opened_flag
    character(len=100) :: fname, access_type, form_type, seq_type, direct_type

    unit_num = 30

    open(unit=unit_num, file='direct.dat', access='direct', recl=100, iostat=ios)

    if (ios == 0) then
        inquire(unit=unit_num, exist=exists, opened=opened_flag)
        inquire(unit=unit_num, access=access_type, form=form_type, recl=reclen)
        inquire(file='direct.dat', sequential=seq_type, direct=direct_type)
        close(unit=unit_num, status='delete', iostat=ios)
    end if

    open(unit=40, file='formatted.txt', status='replace', form='formatted')
    close(40)

end program
