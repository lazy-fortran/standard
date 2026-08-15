! Array contracts proposal: shape/rank annotation and explicit broadcast.
module particles
    implicit none
    integer, parameter :: dp = kind(1.0d0)
contains
    subroutine step_positions(x, v, dt)
        real(dp), shape(n, 3), intent(inout) :: x
        real(dp), shape(n, 3), intent(in)    :: v
        real(dp),             intent(in)    :: dt
        x = x + broadcast(dt * v, over=particle)
    end subroutine step_positions
end module particles
