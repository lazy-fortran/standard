program infer_walrus_array_02
    implicit none
    x := reshape([1, 2, 3, 4, 5, 6], [2, 3])
    print *, size(x, 1), size(x, 2), x(1, 1), x(2, 1), x(1, 2)
end program infer_walrus_array_02
