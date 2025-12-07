        module test_mod
            private
            integer :: private_var
            public :: public_namelist
            namelist /public_namelist/ private_var
        end module

