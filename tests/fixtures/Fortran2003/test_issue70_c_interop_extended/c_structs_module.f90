module c_structs
  use iso_c_binding
  implicit none

  type, bind(c) :: particle_t
    integer(c_int)      :: id
    real(c_double)      :: mass
    real(c_double)      :: position(3)
    type(c_ptr)         :: payload
  end type particle_t

  type, bind(c) :: pair_t
    type(particle_t) :: a
    type(particle_t) :: b
  end type pair_t

end module c_structs

