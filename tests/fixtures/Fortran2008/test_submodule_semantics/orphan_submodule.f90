submodule(nonexistent_mod) orphan_sub
   implicit none
contains
   module subroutine orphan_proc()
      print *, 'Orphan procedure'
   end subroutine orphan_proc
end submodule orphan_sub
