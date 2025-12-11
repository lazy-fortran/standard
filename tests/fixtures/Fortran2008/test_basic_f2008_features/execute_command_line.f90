program test_execute_command_line
    implicit none
    integer :: exitstat, cmdstat
    character(len=256) :: cmdmsg

    ! Section 13.7.55: Basic command execution
    call EXECUTE_COMMAND_LINE('echo "Test basic execution"')

    ! With exit status capture
    call EXECUTE_COMMAND_LINE('echo "Test with EXITSTAT"', EXITSTAT=exitstat)
    print *, 'Exit status:', exitstat

    ! With command status capture
    call EXECUTE_COMMAND_LINE('echo "Test with CMDSTAT"', CMDSTAT=cmdstat)
    print *, 'Command status:', cmdstat

    ! With error message capture
    call EXECUTE_COMMAND_LINE('echo "Test with CMDMSG"', CMDMSG=cmdmsg)

    ! Multiple parameters with EXITSTAT and CMDSTAT
    call EXECUTE_COMMAND_LINE('echo "Test multiple"', &
                               EXITSTAT=exitstat, &
                               CMDSTAT=cmdstat, &
                               CMDMSG=cmdmsg)

    if (cmdstat /= 0) then
        print *, 'Error:', trim(cmdmsg)
    else
        print *, 'Success with exit code:', exitstat
    end if

    ! Test with different commands
    call EXECUTE_COMMAND_LINE('pwd', EXITSTAT=exitstat)
    call EXECUTE_COMMAND_LINE('ls -l', CMDSTAT=cmdstat)

    print *, 'All EXECUTE_COMMAND_LINE tests completed'
end program test_execute_command_line
