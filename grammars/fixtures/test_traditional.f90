! Traditional Fortran - REQUIRES PROGRAM BLOCK
! This file uses the .f90 extension

PROGRAM TRADITIONAL_TEST
    IMPLICIT NONE
    REAL :: x, y, z, average, max_val
    REAL, DIMENSION(5) :: data
    
    ! Variable assignments
    x = 5.0
    y = 10.0
    z = x + y
    
    ! Print statement
    PRINT *, 'Sum is:', z
    
    ! Array initialization
    data = [1.0, 2.0, 3.0, 4.0, 5.0]
    
    ! Use the function
    average = compute_average(data)
    PRINT *, 'Average:', average
    
    ! Call the subroutine
    CALL show_info('Result', average)
    
    ! Conditional expression (F2023 feature)
    max_val = (x > y ? x : y)
    PRINT *, 'Max:', max_val
    
CONTAINS  ! REQUIRED in traditional Fortran
    
    FUNCTION compute_average(arr) RESULT(avg)
        REAL, INTENT(IN) :: arr(:)
        REAL :: avg
        avg = SUM(arr) / SIZE(arr)
    END FUNCTION compute_average
    
    SUBROUTINE show_info(name, value)
        CHARACTER(*), INTENT(IN) :: name
        REAL, INTENT(IN) :: value
        PRINT *, name, '=', value
    END SUBROUTINE show_info
    
END PROGRAM TRADITIONAL_TEST