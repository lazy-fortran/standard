        ! Comprehensive test program using all implemented features
        
        ! Variable declarations and initialization
        N_SIZE = 10
        PI_CONSTANT = 3.14159
        TOLERANCE = 1.0E-6
        
        ! Array operations with complex subscripts
        DO 100 I = 1, N_SIZE
        DO 200 J = 1, N_SIZE
        INDEX_CALC = I * N_SIZE + J
        MATRIX_A(I, J) = PI_CONSTANT * INDEX_CALC
        200 CONTINUE
        100 CONTINUE
        
        ! Complex arithmetic with proper precedence
        RESULT = (MATRIX_A(1, 1) + MATRIX_A(2, 2)) ** 2 - PI_CONSTANT / 4.0
        
        ! Conditional logic with relational operators
        IF (RESULT .GT. TOLERANCE) 10, 20, 10
        10 STATUS_FLAG = 1
        GOTO 30
        20 STATUS_FLAG = 0
        30 CONTINUE
        
        ! Subroutine calls with complex arguments
        CALL PROCESS_DATA(MATRIX_A, N_SIZE, STATUS_FLAG)
        CALL CALCULATE_RESULT(RESULT + PI_CONSTANT, TOLERANCE * 2.0)
        CALL FINALIZE()
        
        ! I/O operations
        WRITE RESULT, STATUS_FLAG
        READ NEXT_VALUE
        
        ! Program termination
        STOP
        END

