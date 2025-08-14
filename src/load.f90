! load.f90

module load_precision_module
  implicit none
  integer, parameter :: wp = kind(1.0)
  ! not portable?
  integer, parameter :: ip4 = 1
  integer, parameter :: qk4 = 32
end module load_precision_module

module load_mixed_type_module
  use load_precision_module, only: wp
  implicit none
  type mixed_type
    class(*), allocatable :: item
  end type mixed_type

  type multi_type
        integer :: type_num
        integer(4) :: i32
        !integer(2) :: i16
        real(4) :: f32
        character(64)  :: string 
        type(multi_type), allocatable :: a(:)
  end type

  type ggml_tensor_info
        character(64) :: tname
        integer(4) :: ndim, ttype
        integer(8) :: offset
        integer(8), allocatable :: dims(:)
  end type

  type generic_tensor
        integer :: ndims
        integer :: ttype
        integer(2), allocatable :: f161d(:)
        integer(2), allocatable :: f162d(:,:)
        real(kind=wp), allocatable :: f321d(:)
        real(kind=wp), allocatable :: f322d(:,:)
        ! can add fp4
  end type


end module

module load_arg_parse
        implicit none

        type args
                character(:), allocatable :: infile
                character(:), allocatable :: outfile
                character(:), allocatable :: tokenizer_file
                logical :: verbose
        end type args

        contains

                subroutine parse_args(arg_values)
                        type(args), intent(inout) :: arg_values
                        integer :: i, num_args
                        character(256) :: arg



                        !defaults 
                        arg_values%verbose = .false.
                        arg_values%infile = ""
                        arg_values%outfile = ""
                        arg_values%tokenizer_file = ""

                        num_args = command_argument_count()

                        i = 1
                        do while (i <= num_args)
                                call get_command_argument(i, arg)
                                        select case (arg)
                                                case ('-i', '--infile')
                                                ! input ggml file
                                                call get_command_argument(i+1, arg)
                                                arg_values%infile = trim(arg)
                                                i = i + 2
                                                case ('-o', '--outfile')
                                                ! output model file
                                                call get_command_argument(i+1, arg)
                                                arg_values%outfile = trim(arg)
                                                i = i + 2
                                                case ('-t', '--tokenizer_file')
                                                ! optionally write tokenizer file
                                                call get_command_argument(i+1, arg)
                                                arg_values%tokenizer_file = trim(arg)
                                                i = i + 2
                                                case ('-v', '--verbose')
                                                ! print additional information
                                                arg_values%verbose = .true.
                                                i = i + 1
                                                case default
                                                print *, 'Unrecognized option:', trim(arg)
                                                stop
                                                end select
                        end do

                        if (arg_values%infile .eq. "") then
                                print *, "Input file required"
                                stop
                        !else if (arg_values%outfile .eq. "") then
                        !        print *, "Output file required"
                        !        stop
                        end if 


                end subroutine

end module load_arg_parse


!module conversions
!        use iso_c_binding
!        !use quants
!        use load_precision_module
!        implicit none
!
!        interface
!                pure function float_to_half_c(x) bind(C, name="float_to_half")
!                        use iso_c_binding
!                        real(c_float), value :: x
!                        integer(c_int16_t) :: float_to_half_c
!                end function float_to_half_c
!
!               pure function half_to_float_c(h) bind(C, name="half_to_float")
!                        use iso_c_binding
!                        integer(c_int16_t), value :: h
!                        real(c_float) :: half_to_float_c
!                end function half_to_float_c
!
!                function pack2x4_c(xi0, xi1) bind(C, name="pack2x4")
!                        use iso_c_binding
!                        integer(c_int8_t), value :: xi0, xi1
!                        integer(c_int8_t) :: pack2x4_c
!                end function pack2x4_c
!
!                function unpack_high_c(x) bind(C, name="unpack_high")
!                        use iso_c_binding
!                        integer(c_int8_t), value :: x
!                        integer(c_int8_t) :: unpack_high_c
!                end function unpack_high_c
!
!                function unpack_low_c(x) bind(C, name="unpack_low")
!                        use iso_c_binding
!                        integer(c_int8_t), value :: x
!                        integer(c_int8_t) :: unpack_low_c
!                end function unpack_low_c
!
!        end interface
!
!end module 
