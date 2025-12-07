program team_event_semantics
   use, intrinsic :: iso_fortran_env, only: dp => real64
   implicit none

   type(team_type) :: team_handle
   type(event_type) :: e
   real(dp) :: x
   integer :: stat_var
   integer :: count_value
   integer :: team_index
   integer :: result_image_var
   character(len=80) :: msg

   interface
      subroutine reducer(x)
         use, intrinsic :: iso_fortran_env, only: dp => real64
         implicit none
         real(dp), intent(inout) :: x
      end subroutine reducer
   end interface

   call co_sum(x)
   call co_min(x, stat=stat_var)
   call co_max(x, stat=stat_var, errmsg=msg)
   call co_broadcast(x, source_image=1, stat=stat_var, &
                     errmsg=msg)
   call co_reduce(x, reducer, result_image=result_image_var, &
                  stat=stat_var, errmsg=msg)

   form team(1, team_handle, new_index=team_index, stat=stat_var, &
             errmsg=msg)
   change team(team_handle, stat=stat_var)
   event post(e, stat=stat_var, errmsg=msg)
   event wait(e, until_count=5, stat=stat_var, &
              errmsg=msg)
   event query(e, count=count_value, stat=stat_var, &
               errmsg=msg)
   end team(stat=stat_var, errmsg=msg)
end program team_event_semantics
