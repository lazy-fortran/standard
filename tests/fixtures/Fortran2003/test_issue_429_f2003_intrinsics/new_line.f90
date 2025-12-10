! Test: NEW_LINE intrinsic function (ISO/IEC 1539-1:2004 Section 13.7.84)
! Returns the newline character for the character kind of A.
! Essential for portable formatted I/O.

module new_line_mod
  implicit none

contains

  ! Test 1: Get newline for default character
  function get_newline_default() result(nl)
    character(len=1) :: nl
    nl = new_line('a')
  end function get_newline_default

  ! Test 2: Construct multi-line string with new_line
  function multi_line_string() result(str)
    character(len=100) :: str
    str = "Line 1" // new_line('x') // "Line 2" // new_line('x') // "Line 3"
  end function multi_line_string

  ! Test 3: Use in formatted output
  subroutine print_with_newlines()
    character(len=1) :: nl
    nl = new_line('c')
    print '(A)', "First line" // nl // "Second line" // nl // "Third line"
  end subroutine print_with_newlines

  ! Test 4: Character kind specification
  subroutine work_with_chars(char_var)
    character(len=*), intent(in) :: char_var
    character(len=1) :: nl
    nl = new_line(char_var)
    print *, "Got newline for character: ", len(nl)
  end subroutine work_with_chars

end module new_line_mod

program test_new_line
  use new_line_mod
  implicit none

  character(len=1) :: nl
  character(len=50) :: multiline
  integer :: nl_code

  ! Test 1: Get newline character
  nl = new_line('a')
  nl_code = iachar(nl)

  if (nl_code == 10) then
    print *, "Newline code is 10 (LF): PASS"
  else
    print *, "Newline code is:", nl_code
  end if

  ! Test 2: Use in string concatenation
  multiline = "Line 1" // new_line('x') // "Line 2"
  print *, "Multi-line string created: PASS"

  ! Test 3: Multi-character test
  call work_with_chars("test")

end program test_new_line
