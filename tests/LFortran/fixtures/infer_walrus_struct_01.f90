program infer_walrus_struct_01
    implicit none
    type :: point_t
        real :: x, y
    end type
    p := point_t(1.0, 2.0)
    print *, p%x, p%y
end program infer_walrus_struct_01
