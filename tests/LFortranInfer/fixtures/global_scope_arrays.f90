! Test: Infer mode - arrays and expressions without program wrapper
! Reference: LFortran infer mode (--infer flag)

integer, allocatable :: arr(:)
real(8) :: result

arr = [1, 2, 3, 4, 5]
result = sum(arr) / size(arr)

print *, "Array:", arr
print *, "Mean:", result

! Bare expression (valid in REPL)
2 + 2
