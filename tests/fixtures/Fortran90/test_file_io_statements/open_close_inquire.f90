program test_file_io
    implicit none
    integer :: unit_num, ios
    logical :: file_exists, is_opened
    character(len=100) :: filename

    unit_num = 10
    filename = 'test_data.txt'

    inquire(file=filename, exist=file_exists)

    if (file_exists) then
        open(unit=unit_num, file=filename, status='old', iostat=ios)
        if (ios == 0) then
            write(unit_num, *) 'Hello from Fortran 90'
            close(unit=unit_num)
        end if
    else
        open(unit=unit_num, file=filename, status='new', iostat=ios)
        if (ios == 0) then
            write(unit_num, *) 'New file created'
            close(unit=unit_num, status='keep')
        end if
    end if

    inquire(unit=unit_num, opened=is_opened)
    inquire(file=filename, number=unit_num)

end program
