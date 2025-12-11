module test_combined_volatile_protected
    implicit none
    integer :: async_var, read_only_var, regular_var
    volatile async_var
    protected :: read_only_var
    ! regular_var has no attributes
end module test_combined_volatile_protected
