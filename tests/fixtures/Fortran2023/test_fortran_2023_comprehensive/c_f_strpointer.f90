program test_c_f_strpointer_f2023
    use, intrinsic :: iso_c_binding
    implicit none

    character :: c_string
    character :: fstr
    integer :: nchars

    call c_f_strpointer(c_string, fstr)

    call c_f_strpointer(c_string, fstr, 5)

    nchars = 10
    call c_f_strpointer(c_string, fstr, nchars)

end program test_c_f_strpointer_f2023
