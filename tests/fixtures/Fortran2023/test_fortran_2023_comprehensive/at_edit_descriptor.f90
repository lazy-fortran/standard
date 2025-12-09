! AT edit descriptor for automatic trimming of trailing blanks
! ISO/IEC 1539-1:2023 Section 13 (Input/output editing)
! J3/22-007 R1307: char-string-edit-desc adds AT edit descriptor
!
! The AT edit descriptor outputs character values with trailing blanks
! automatically removed. It takes no width specification.
!
! Syntax: AT (no width parameter)
! Effect: Outputs character value with trailing blanks trimmed

program test_at_edit_descriptor
    implicit none
    character(len=20) :: padded_name
    character(len=30) :: message
    character(len=10) :: empty_str
    character(len=15) :: values(3)

    padded_name = 'Hello'
    message = 'World       '
    empty_str = '          '
    values(1) = 'One'
    values(2) = 'Two'
    values(3) = 'Three'

    write(*, '(AT)') padded_name

    write(*, '(A,AT)') 'Name: ', padded_name

    write(*, '(AT,A,AT)') padded_name, ' and ', message

    write(*, '("Result: ",AT)') padded_name

    write(*, '(3(AT,1X))') values(1), values(2), values(3)

    write(*, '(A,AT,A,AT)') 'First: ', padded_name, ' Second: ', message

    write(*, '(AT)') empty_str

    print '(AT)', padded_name

    print '(A,": ",AT)', 'Value', message

end program test_at_edit_descriptor
