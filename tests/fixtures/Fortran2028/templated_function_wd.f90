template function mysum(T)(x, y) result(z)
    type, deferred :: T
    type(T), intent(in) :: x, y
    type(T) :: z
    z = x + y
end function mysum
