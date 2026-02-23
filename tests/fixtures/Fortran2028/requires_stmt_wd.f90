template function mysum(T, add)(x, y) result(z)
    type, deferred :: T
    procedure(add_sig) :: add
    requires :: addable{T, add}
    type(T), intent(in) :: x, y
    type(T) :: z
    z = add(x, y)
end function mysum
