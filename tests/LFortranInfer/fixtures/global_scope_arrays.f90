! Test: Global scope - arrays and expressions
! Reference: LFortran interactive mode / script mode

integer, allocatable :: arr(:)
real(8) :: result

arr = [1, 2, 3, 4, 5]
result = sum(arr) / size(arr)

print *, "Array:", arr
print *, "Mean:", result

! Bare expression (valid in REPL)
2 + 2
