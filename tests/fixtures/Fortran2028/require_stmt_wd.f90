template function mysum_req(T, add)(x, y) result(z)
    type, deferred :: T
    procedure(add_sig) :: add
    require :: addable{T, add}
    type(T), intent(in) :: x, y
    type(T) :: z
    z = add(x, y)
end function mysum_req
