program test_f_c_string_f2023
    use, intrinsic :: iso_c_binding
    implicit none

    character :: fstr
    character :: cstr
    logical :: preserve_nulls

    fstr = "Hello from Fortran"

    cstr = f_c_string(fstr)

    cstr = f_c_string(fstr, .false.)

    preserve_nulls = .true.
    cstr = f_c_string(fstr, preserve_nulls)

end program test_f_c_string_f2023
