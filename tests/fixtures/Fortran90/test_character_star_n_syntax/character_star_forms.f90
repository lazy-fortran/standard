program character_star_forms
    implicit none
    character*80 :: line
    character*20 :: name
    character*(*) :: assumed_str
    character*(100) :: expr_form
    character*1 :: single_char
    character*(80+20) :: computed_len

    line = "This is a test line"
    name = "Test"
    single_char = "X"

end program character_star_forms
