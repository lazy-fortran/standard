program infer_walrus_shadow_03
    implicit none
    x = 42
    block
        x = 2.718d0
        print *, x
    end block
    print *, x
end program infer_walrus_shadow_03
