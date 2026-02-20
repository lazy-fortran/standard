type :: point_t
    real :: x, y
end type

arr := [1, 2, 3]
p := point_t(1.0, 2.0)
print *, size(arr), p%x, p%y
