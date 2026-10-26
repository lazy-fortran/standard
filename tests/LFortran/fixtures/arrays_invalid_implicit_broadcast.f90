! Invalid neighbor: no implicit broadcasting. Shapes (2,2) vs (2) do not conform.
program invalid_implicit_broadcast
    implicit none
    real(dp), shape(2, 2) :: a, b
    real(dp), shape(2)    :: v
    a = b + v
end program invalid_implicit_broadcast
