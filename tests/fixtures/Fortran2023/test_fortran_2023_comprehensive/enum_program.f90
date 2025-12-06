program test_enum
    enum, bind(c) :: color_type
        enumerator :: red = 1, green = 2, blue = 3
    end enum

    enum :: status
        enumerator :: success, failure, pending
    end enum
end program test_enum

