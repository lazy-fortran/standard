program fail_image_stmt
  implicit none
  logical :: triggered
  triggered = .true.
  if (triggered) then
    fail image
  end if
end program fail_image_stmt
