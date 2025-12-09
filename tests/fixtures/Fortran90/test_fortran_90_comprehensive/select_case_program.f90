program test_select
    implicit none
    integer :: grade
    character(len=1) :: letter
    grade = 85
    letter = 'b'

    ! Integer SELECT CASE - ISO/IEC 1539:1991 Section 8.1.3
    select case (grade)
    case (90:100)
        print *, "A"
    case (80:89)
        print *, "B"
    case (70:79)
        print *, "C"
    case default
        print *, "F"
    end select

    ! Character SELECT CASE - ISO/IEC 1539:1991 Section 8.1.3
    ! Per issue #319: Test character selector and character ranges
    select case (letter)
    case ('a':'z')
        print *, "lowercase"
    case ('A':'Z')
        print *, "uppercase"
    case ('0':'9')
        print *, "digit"
    case default
        print *, "other"
    end select
end program

