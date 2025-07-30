program check
  use, intrinsic :: iso_fortran_env
  use :: precision_module
  use :: mixed_type_module
  implicit none

  print *, "Running tests..."

contains

  subroutine print_result(test_name, condition)
    character(len=*), intent(in) :: test_name
    logical, intent(in) :: condition

    if (condition) then
      print *, "PASS: ", trim(test_name)
    else
      print *, "FAIL: ", trim(test_name)
    end if
  end subroutine print_result

end program check
