program test_enhanced_io
    implicit none

    integer, parameter :: unit = 10
    integer :: iostat
    character(len=100) :: iomsg
    real :: data(100)
    integer :: i, id

    ! Initialize data
    do i = 1, 100
        data(i) = real(i)
    end do

    ! Open file with enhanced options
    open(unit=unit, file='test.dat', access='stream', &
         form='unformatted', asynchronous='yes', &
         iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) then
        print *, 'Open failed:', trim(iomsg)
        stop
    end if

    ! Asynchronous write
    write(unit=unit, asynchronous='yes', id=id) data

    ! Wait for completion
    wait(unit=unit, id=id)

    ! Flush buffer
    flush(unit)

    close(unit)

end program test_enhanced_io

