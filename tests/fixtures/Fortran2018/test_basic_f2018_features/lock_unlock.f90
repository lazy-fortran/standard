program test_lock_unlock
    use iso_fortran_env
    implicit none
    type(lock_type) :: my_lock[*]
    integer :: stat_var
    character(len=128) :: errmsg_var
    logical :: acquired

    lock(my_lock)
    unlock(my_lock)

    lock(my_lock, stat=stat_var)
    unlock(my_lock, stat=stat_var)

    lock(my_lock, acquired_lock=acquired)
    lock(my_lock, stat=stat_var, errmsg=errmsg_var)
    lock(my_lock, acquired_lock=acquired, stat=stat_var)

    unlock(my_lock, stat=stat_var, errmsg=errmsg_var)
end program test_lock_unlock
