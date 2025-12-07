program test_select
    integer :: grade
    grade = 85

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
end program

