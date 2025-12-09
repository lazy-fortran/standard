program test_enum
    ! F2023 R766-R771: enum-type-def with optional enum-type-name on end-enum-stmt
    ! R767: enum-type-stmt is ENUM, BIND(C) :: enum-type-name
    ! R771: end-enum-type-stmt is END ENUM [ enum-type-name ]

    ! BIND(C) enum with optional type name on END ENUM (F2023 feature)
    enum, bind(c) :: color_type
        enumerator :: red = 1, green = 2, blue = 3
    end enum color_type

    ! Fortran-specific enum without BIND(C), with type name
    enum :: status
        enumerator :: success, failure, pending
    end enum status

    ! Backwards-compatible: enum without type name on END ENUM
    enum, bind(c) :: error_code
        enumerator :: ok = 0, warning = 1, error = 2
    end enum
end program test_enum

