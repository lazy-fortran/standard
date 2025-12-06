module test
    contains
    subroutine test_proc()
        do concurrent (i = 1:10)
        end do
    end subroutine test_proc
end module test

