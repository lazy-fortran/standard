program test_file_positioning
    implicit none
    integer :: unit_num, ios, i
    real :: x, values(5)

    unit_num = 20
    open(unit=unit_num, file='sequential.dat', status='replace', iostat=ios)

    do i = 1, 5
        x = i * 1.5
        write(unit_num, *) x
    end do

    rewind(unit_num)

    read(unit_num, *) values(1)

    backspace(unit_num)
    read(unit_num, *) values(1)

    endfile(unit_num)

    rewind(unit=unit_num, iostat=ios)
    backspace(unit=unit_num, iostat=ios)
    endfile(unit=unit_num, iostat=ios)

    close(unit_num)

end program
