program test_associate
    implicit none

    type :: person_t
        character(len=50) :: name
        integer :: age
    end type person_t

    type(person_t) :: person
    real :: x(10,10), y(10,10)
    integer :: i, j

    person%name = "John Doe"
    person%age = 30

    ! Associate construct with derived type component
    associate (name => person%name, age => person%age)
        print *, 'Name: ', trim(name)
        print *, 'Age: ', age
    end associate

    ! Associate construct with array expression
    associate (sum_xy => x + y, n => size(x,1))
        do i = 1, n
            do j = 1, n
                sum_xy(i,j) = x(i,j) + y(i,j)
            end do
        end do
    end associate

end program test_associate

