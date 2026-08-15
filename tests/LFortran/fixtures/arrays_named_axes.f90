! Array contracts proposal: named axes / index spaces.
module forces
    implicit none
    integer, parameter :: dp = kind(1.0d0)
contains
    subroutine force(positions, masses, forces)
        real(dp), shape(particle, xyz), intent(in)    :: positions
        real(dp), shape(particle),      intent(in)    :: masses
        real(dp), shape(particle, xyz), intent(inout) :: forces
        forces = broadcast(masses, over=particle) * positions
    end subroutine force
end module forces
