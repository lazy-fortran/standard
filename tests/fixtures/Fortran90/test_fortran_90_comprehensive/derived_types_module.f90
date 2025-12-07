program test_types
    type :: person
        character(len=50) :: name
        integer :: age
        real :: height
    end type person

    type(person) :: john
    john%name = "John Doe"
    john%age = 30
    john%height = 5.9
end program

