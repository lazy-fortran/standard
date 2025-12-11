program test_inquire_iolength
    implicit none
    integer :: iolength_value, ios, direct_unit
    real :: data_values(5)
    complex :: complex_data(2)
    character(len=12) :: label

    direct_unit = 20
    data_values = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)
    complex_data = (/ cmplx(1.0, 1.0), cmplx(2.0, -1.0) /)
    label = 'F90RECORD'

    inquire(iolength=iolength_value) data_values, complex_data, label

    open(unit=direct_unit, file='record.bin', access='direct', form='unformatted', recl=iolength_value, status='replace', iostat=ios)

    if (ios == 0) then
        write(direct_unit, rec=1) data_values, complex_data, label
        close(unit=direct_unit)
    end if

end program test_inquire_iolength
