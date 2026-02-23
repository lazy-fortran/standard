template subroutine swap_t(T)(a, b)
    type, deferred :: T
    type(T), intent(inout) :: a, b
    type(T) :: tmp
    tmp = a
    a = b
    b = tmp
end subroutine swap_t
