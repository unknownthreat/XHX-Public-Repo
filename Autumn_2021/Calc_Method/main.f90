program Calc_Method
    use universal
    use Q1
    use Q2
    use Q3
    implicit none
    
    call acquire_DIR_by_file
10  call print_header(print_unit)
    write(*,'(A)') ' 0 >> Run all three sub programs '
    write(*,'(A)') ' 1 >> Run sub program No. 1 (Price for riverside display)'
    write(*,'(A)') ' 2 >> Run sub program No. 2 (Movie box office prediction)'
    write(*,'(A)') ' 3 >> Run sub program No. 3 (Gauss on large scale matrix)'
    write(*,'(A)') ' 7 >> CLEAR customized input file directory'
    write(*,'(A)') ' 8 >>  SET  customized input file directory'
    write(*,'(A)') ' 9 >> EXIT'
    write(*,*)
    call check_DIR(dir_error)
    if(dir_error) goto 20
    
    write(*,'(A,$)') ' Please input Action Number: '
    read(*,*) question_flag
    write(*,*)
    select case (question_flag)
    case(0)
        call SYSTEM('CLS')
        call print_header(print_unit)
        open(unit=output_unit,file='output_TOTAL.txt',status='replace')
        call print_header(output_unit)
        call Run_Q1
        call Run_Q2
        call Run_Q3
        close(output_unit)
    case(1)
        call SYSTEM('CLS')
        call print_header(print_unit)
        call Run_Q1
    case(2)
        call SYSTEM('CLS')
        call print_header(print_unit)
        call Run_Q2
    case(3)
        call SYSTEM('CLS')
        call print_header(print_unit)
        call Run_Q3
    case(7)
        INPUT_DIR = './'
        write(*,*) 'Customized DIR CLEARED !'
        call SYSTEM('del '//ADJUSTL(TRIM(CUSTOM_DIR_FILENAME)))
        call sleep(1)
        call SYSTEM('CLS')
        go to 10
    case(8)
20      write(*,'(A)') ' Please input your customized input file directory below: (use "\" in the middle and at the end)'
        write(*,'(A,$)') ' '
        read(*,*) INPUT_DIR
        call DIR_convert
        write(*,*)
        write(*,*) 'Customized DIR SET !'
        call write_DIR_by_file
        call sleep(1)
        call SYSTEM('CLS')
        go to 10
    case(9)
        stop
    case default
        write(*,*) 'Wrong Question Number !'
        call sleep(1)
        call SYSTEM('CLS')
        goto 10
    end select
    
    contains
    subroutine acquire_DIR_by_file
        implicit none
        integer     :: u, i, io_error
        logical     :: is_exist
        
        inquire(file=CUSTOM_DIR_FILENAME,exist=is_exist)
        if(is_exist) then
            open(newunit=u,file=CUSTOM_DIR_FILENAME)
            read(u,*,iostat=io_error) INPUT_DIR
            close(u)
        end if
        call DIR_convert
    end subroutine acquire_DIR_by_file
    
    subroutine write_DIR_by_file
        implicit none
        integer     :: u, i
        character(len=180)  :: write_dir
        
        open(newunit=u,file=CUSTOM_DIR_FILENAME,status='replace')
        write_dir = INPUT_DIR
        do i = 1,len(write_dir)
            if (write_dir(i:i)=='/') write_dir(i:i) = '\'
        end do
        
        write(u,'(A)') ADJUSTL(TRIM(write_dir))
        close(u)
    end subroutine write_DIR_by_file
    
    subroutine check_DIR(error)
        logical, intent(out)    :: error
        integer                 :: i
        do i = len(INPUT_DIR),1,-1
            if(INPUT_DIR(i:i)/=' ') exit
        end do
        if(INPUT_DIR(i:i)/='/') then
            write(*,*) ' *ERROR* Illegal customized input file directory is found'
            error = .true.
        else
            error = .false.
        end if
        if(ADJUSTL(TRIM(INPUT_DIR))/='./' .and. (.not.error)) then
            write(*,*) 'Using customized input file directory : '//ADJUSTL(TRIM(INPUT_DIR))
            write(*,*)
        end if
    end subroutine check_DIR
    
    subroutine DIR_convert
        integer i
        do i = 1,len(INPUT_DIR)
            if (INPUT_DIR(i:i)=='\') INPUT_DIR(i:i) = '/'
        end do
    end subroutine DIR_convert
end program