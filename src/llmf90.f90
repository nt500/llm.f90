module llmf90
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, llmf90!"
  end subroutine say_hello
end module llmf90
