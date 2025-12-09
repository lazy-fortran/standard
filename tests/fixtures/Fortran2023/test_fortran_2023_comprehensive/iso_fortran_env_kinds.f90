! Test ISO_FORTRAN_ENV intrinsic module constants: LOGICAL_KINDS, CHARACTER_KINDS
! ISO/IEC 1539-1:2023 Section 16.10.2

program test_iso_fortran_env_kinds
    use, intrinsic :: iso_fortran_env, only: logical_kinds, character_kinds
    implicit none

    print *, 'Logical kinds:', logical_kinds
    print *, 'Character kinds:', character_kinds

end program test_iso_fortran_env_kinds
