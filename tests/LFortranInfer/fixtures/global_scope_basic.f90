! Test: Global scope - bare statements without program wrapper
! Reference: LFortran interactive mode / script mode

integer :: x, y
real(8) :: pi

x = 42
y = x * 2
pi = 3.14159265358979d0

print *, "x =", x
print *, "y =", y
print *, "pi =", pi
