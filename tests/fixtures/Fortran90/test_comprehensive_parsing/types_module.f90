module types
    type :: person_t
        character(len=50) :: name
        integer :: age
        real :: height
    end type person_t
contains
    subroutine print_person(p)
        type(person_t), intent(in) :: p
        write(*,*) p%name, p%age, p%height
    end subroutine print_person
end module types

