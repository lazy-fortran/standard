program infer_walrus_shadow_02
    implicit none
    x := 10
    block
        x := 3.14d0
        block
            x := .true.
            print *, x
        end block
        print *, x
    end block
    print *, x
end program infer_walrus_shadow_02
