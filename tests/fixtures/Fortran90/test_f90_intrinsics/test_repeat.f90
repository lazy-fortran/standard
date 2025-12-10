program test_repeat
    implicit none
    character(len=30) :: result
    character(len=2) :: str
    str = "ab"
    result = repeat(str, 5)
    print *, result
end program test_repeat
