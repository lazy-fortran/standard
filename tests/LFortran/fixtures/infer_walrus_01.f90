program infer_walrus_01
    implicit none
    call test()
contains
    subroutine test()
        x := 42
        y := 3.14d0
        z := (1.0d0, 2.0d0)
        flag := .true.
        s := "hello"
        x = 100
        print *, x, y, z, flag, s
    end subroutine
end program infer_walrus_01
