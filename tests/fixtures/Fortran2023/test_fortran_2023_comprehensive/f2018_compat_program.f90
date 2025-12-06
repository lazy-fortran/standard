program test_f2018_compat
    integer :: images[*]
    integer :: values(10)

    call co_sum(values, result_image=1)

    select rank (values)
    rank (1)
        print *, 'Rank 1 array'
    rank default
        print *, 'Other rank'
    end select
end program test_f2018_compat

