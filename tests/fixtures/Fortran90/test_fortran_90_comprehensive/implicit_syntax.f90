program test_implicit_syntax
    implicit integer (i-n), real (a-h, o-z)
    real :: x, y
    i = 10
    a = 3.14
    x = a + float(i)
end program
