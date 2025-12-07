program test
    integer :: a
    associate (b => a)
        stop
    end associate
end program test

