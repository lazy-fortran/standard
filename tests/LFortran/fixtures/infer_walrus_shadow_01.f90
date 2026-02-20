program infer_walrus_shadow_01
    implicit none
    x := 5
    block
        x := "hello"
        print *, x
    end block
    print *, x
end program infer_walrus_shadow_01
