template scale_t(T, N)
    type, deferred :: T
    integer, constant :: N
contains
    function scale(x) result(y)
        type(T), intent(in) :: x
        type(T) :: y
        y = x
    end function scale
end template scale_t
