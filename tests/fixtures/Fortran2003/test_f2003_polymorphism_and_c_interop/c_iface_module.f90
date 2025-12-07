module c_iface
    use iso_c_binding
    implicit none

    interface
        subroutine c_sub(x) bind(c)
            import :: c_int
            integer(c_int), value :: x
        end subroutine c_sub
    end interface

end module c_iface

