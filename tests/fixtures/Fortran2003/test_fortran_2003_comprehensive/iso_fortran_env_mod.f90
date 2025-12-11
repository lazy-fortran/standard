module iso_fortran_env_mod
    ! Test ISO_FORTRAN_ENV intrinsic module support
    ! ISO/IEC 1539-1:2004 Section 13.8.2
    use, intrinsic :: iso_fortran_env
    implicit none

contains

    subroutine write_to_output(msg)
        implicit none
        character(len=*), intent(in) :: msg

        ! Use OUTPUT_UNIT named constant from ISO_FORTRAN_ENV
        write(output_unit, '(A)') msg
    end subroutine write_to_output

    subroutine write_to_error(msg)
        implicit none
        character(len=*), intent(in) :: msg

        ! Use ERROR_UNIT named constant from ISO_FORTRAN_ENV
        write(error_unit, '(A)') msg
    end subroutine write_to_error

    subroutine check_eof(stat)
        implicit none
        integer, intent(in) :: stat
        integer :: end_code, eor_code

        ! Use IOSTAT_END and IOSTAT_EOR named constants for EOF/EOR detection
        end_code = iostat_end
        eor_code = iostat_eor

        if (stat == end_code) then
            write(output_unit, '(A)') 'End of file reached'
        end if
    end subroutine check_eof

end module iso_fortran_env_mod

program test_iso_fortran_env
    ! Direct use of ISO_FORTRAN_ENV with ONLY list
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, &
                                             input_unit
    implicit none

    write(output_unit, '(A)') 'Testing ISO_FORTRAN_ENV support'
    write(error_unit, '(A)') 'Error unit message'

end program test_iso_fortran_env
