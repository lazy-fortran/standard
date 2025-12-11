! Test Fortran 2008 NEWUNIT I/O specifier (ISO/IEC 1539-1:2010 Section 9.5.6.10)
! NEWUNIT provides automatic assignment of unique unit numbers for OPEN statements,
! eliminating manual unit number management and avoiding conflicts.

program test_newunit_io
    implicit none

    ! Automatic unit number assignment via NEWUNIT
    integer :: unit1, unit2, unit3

    ! Test 1: Basic NEWUNIT with file
    open(newunit=unit1, file='test1.txt', status='replace', action='write')
    write(unit1, *) 'Test file 1'
    close(unit1)

    ! Test 2: NEWUNIT with status and action
    open(newunit=unit2, file='test2.txt', status='new', action='write')
    write(unit2, *) 'Test file 2'
    close(unit2)

    ! Test 3: NEWUNIT with form specifier
    open(newunit=unit3, file='test3.dat', status='replace', &
         action='write', form='formatted')
    write(unit3, '(A)') 'Formatted output'
    close(unit3)

    ! Test 4: NEWUNIT with access specifier
    open(newunit=unit1, file='test4.txt', status='replace', &
         action='write', access='sequential')
    write(unit1, *) 'Sequential access'
    close(unit1)

    ! Test 5: NEWUNIT with blank specifier
    open(newunit=unit2, file='test5.txt', status='replace', &
         action='write', form='formatted', blank='zero')
    write(unit2, *) 'Blank handling'
    close(unit2)

    ! Test 6: NEWUNIT with position specifier
    open(newunit=unit3, file='test6.txt', status='replace', &
         action='write', position='rewind')
    write(unit3, *) 'Position test'
    close(unit3)

    ! Test 7: NEWUNIT with RECL specifier
    open(newunit=unit1, file='test7.txt', status='replace', &
         action='write', recl=100)
    write(unit1, '(A)') 'Record length test'
    close(unit1)

    ! Test 8: NEWUNIT with multiple specifiers
    open(newunit=unit2, file='test8.txt', status='replace', &
         action='write', form='formatted', access='sequential')
    write(unit2, *) 'Multiple specifiers'
    close(unit2)

    print *, 'All NEWUNIT tests completed successfully'

end program test_newunit_io
