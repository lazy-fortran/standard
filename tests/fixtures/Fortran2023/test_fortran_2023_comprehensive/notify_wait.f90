program test_notify_wait_f2023
    use iso_fortran_env, only: notify_type
    implicit none
    type(notify_type) :: notify_var[*]
    integer :: stat_val
    character(len=100) :: errmsg_val

    notify wait(notify_var)

    notify wait(notify_var, stat=stat_val)

    notify wait(notify_var, errmsg=errmsg_val)

    notify wait(notify_var, stat=stat_val, errmsg=errmsg_val)

end program test_notify_wait_f2023
