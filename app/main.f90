program main
    implicit none
    
    call parse_and_run()
    
contains

    subroutine parse_and_run()
        implicit none
        integer :: nargs, i, ios
        character(len=512) :: arg
        character(len=512) :: model_file, prompt
        real :: temperature
        integer :: max_tokens
        logical :: verbose
        
        ! Initialize defaults
        model_file = ""
        prompt = ""
        temperature = 0.8
        max_tokens = 128
        verbose = .false.
        
        nargs = command_argument_count()
        
        if (nargs == 0) then
            call print_usage()
            return
        end if
        
        ! Parse arguments
        i = 1
        do while (i <= nargs)
            call get_command_argument(i, arg)
            
            select case (trim(arg))
            case ('-m', '--model')
                if (i >= nargs) then
                    print *, "Error: -m requires a filename"
                    return
                end if
                i = i + 1
                call get_command_argument(i, model_file)
                
            case ('-p', '--prompt')
                if (i >= nargs) then
                    print *, "Error: -p requires a prompt"
                    return
                end if
                i = i + 1
                call get_command_argument(i, prompt)
                
            case ('-t', '--temperature')
                if (i >= nargs) then
                    print *, "Error: -t requires a value"
                    return
                end if
                i = i + 1
                call get_command_argument(i, arg)
                read(arg, *, iostat=ios) temperature
                if (ios /= 0) then
                    print *, "Error: Invalid temperature value"
                    return
                end if
                
            case ('-n', '--max-tokens')
                if (i >= nargs) then
                    print *, "Error: -n requires a value"
                    return
                end if
                i = i + 1
                call get_command_argument(i, arg)
                read(arg, *, iostat=ios) max_tokens
                if (ios /= 0) then
                    print *, "Error: Invalid max tokens value"
                    return
                end if
                
            case ('-v', '--verbose')
                verbose = .true.
                
            case ('-h', '--help')
                call print_usage()
                return
                
            case default
                print *, "Unknown option: ", trim(arg)
                call print_usage()
                return
            end select
            
            i = i + 1
        end do
        
        ! Validate required arguments
        if (len_trim(model_file) == 0) then
            print *, "Error: Model file (-m) is required"
            return
        end if
        
        if (len_trim(prompt) == 0) then
            print *, "Error: Prompt (-p) is required"
            return
        end if
        
        ! Run the inference
        call run_llm(model_file, prompt, temperature, max_tokens, verbose)
        
    end subroutine parse_and_run
    
    subroutine print_usage()
        print *, "Usage: llm -m <model> -p <prompt> [options]"
        print *, ""
        print *, "Required:"
        print *, "  -m, --model <file>      GGUF model file"
        print *, "  -p, --prompt <text>     Input prompt"
        print *, ""
        print *, "Options:"
        print *, "  -t, --temperature <val> Sampling temperature (default: 0.8)"
        print *, "  -n, --max-tokens <num>  Maximum tokens to generate (default: 128)"
        print *, "  -v, --verbose           Verbose output"
        print *, "  -h, --help              Show this help"
    end subroutine print_usage
    
    subroutine run_llm(model_file, prompt, temperature, max_tokens, verbose)
        character(len=*), intent(in) :: model_file, prompt
        real, intent(in) :: temperature
        integer, intent(in) :: max_tokens
        logical, intent(in) :: verbose
        
        if (verbose) then
            print *, "Configuration:"
            print *, "  Model: ", trim(model_file)
            print *, "  Temperature: ", temperature
            print *, "  Max tokens: ", max_tokens
            print *, "  Prompt: ", trim(prompt)
            print *, ""
        end if
        
        ! TODO: Replace with actual llm.f90 inference calls
        print *, "This is llm.f90's response!"
        
    end subroutine run_llm

end program main

