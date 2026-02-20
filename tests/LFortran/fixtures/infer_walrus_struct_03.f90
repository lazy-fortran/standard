program infer_walrus_struct_03
    implicit none
    type :: vec_t
        integer :: x, y, z
    end type
    p = vec_t(1, 2, 3)
    print *, p%x, p%y, p%z
end program infer_walrus_struct_03
