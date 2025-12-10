program compiler_info
    implicit none
    character(len=256) :: version, options

    version = compiler_version()
    options = compiler_options()

    print *, "Compiled with: ", version
    print *, "Options: ", options
end program compiler_info
