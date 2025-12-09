program test_leading_zero_f2023
    implicit none
    integer :: unit_num, ios
    character(len=100) :: iomsg_val
    character(len=20) :: lz_status
    integer :: value

    value = 42

    open(unit=10, file='output.txt', leading_zero='YES')

    open(unit=11, file='output2.txt', leading_zero='NO', status='replace')

    open(unit=12, file='output3.txt', leading_zero='PROCESSOR_DEFINED')

    write(10, '(I8)', leading_zero='YES') value

    write(11, '(I8)', leading_zero='NO') value

    read(10, '(I8)', leading_zero='YES') value

    inquire(unit=10, leading_zero=lz_status)

    inquire(file='output.txt', leading_zero=lz_status)

    close(10)
    close(11)
    close(12)

end program test_leading_zero_f2023
