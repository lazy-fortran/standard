submodule (math_mod) implementation_sub
    implicit none
contains
    module subroutine calculate_result()
        print *, 'Calculation performed in submodule'
    end subroutine calculate_result

    module function compute_value() result(val)
        real :: val
        val = 42.0
    end function compute_value
end submodule implementation_sub

