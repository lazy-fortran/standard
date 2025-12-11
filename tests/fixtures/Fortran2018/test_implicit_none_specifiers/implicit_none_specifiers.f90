module implicit_none_specs
  implicit none
end module implicit_none_specs

program implicit_none_type
  implicit none (TYPE)
end program implicit_none_type

program implicit_none_external
  implicit none (EXTERNAL)
end program implicit_none_external

program implicit_none_type_external
  implicit none (TYPE, EXTERNAL)
end program implicit_none_type_external

program implicit_none_external_type
  implicit none (EXTERNAL, TYPE)
end program implicit_none_external_type
