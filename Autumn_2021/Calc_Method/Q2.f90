module Q2
    use universal
    implicit none
    real(8), allocatable    :: box_office(:), office_total(:)
    integer                 :: uo, scale, total_day = 365
    character(len=50)       :: file_name
    
    contains
    subroutine Run_Q2
        if (question_flag==2) then
            open(newunit=uo, file='output_Q2.txt',status='replace')
            call print_header(uo)
        else
            uo = output_unit
        end if
        call print_seperator(uo,'Question 2')
        call print_seperator(print_unit,'Question 2')
        
        file_name = '长津湖每日票房.csv'
        call read_q2
        call calc_q2
        call output_q2(31)
        file_name = '战狼II每日票房.csv'
        call read_q2
        call calc_q2
        call output_q2
        if (question_flag==2) close(uo)
    end subroutine Run_Q2
    
    subroutine read_q2
        character(len=100)              :: word
        integer     :: u, io_error, i
        logical     :: is_exist
        
        inquire(file=ADJUSTL(TRIM(INPUT_DIR))//file_name,exist=is_exist)
        if(.not. is_exist) then
            write(*,*) ' *ERROR* No input file '//TRIM(file_name)//' found in dir '//ADJUSTL(TRIM(INPUT_DIR))
            write(*,*) 'Program Stopped'
            stop
        end if
        
        open(newunit=u,file=ADJUSTL(TRIM(INPUT_DIR))//TRIM(file_name))
        read(u,*)
        scale = 0
        do
            read(u,*,iostat=io_error)
            if(io_error<0) exit
            scale = scale + 1
        end do
        rewind(u)
        read(u,*)
        allocate(box_office(scale))
        do i = 1,scale
            read(u,*) word, box_office(i)
        end do
        
    end subroutine read_q2
    
    subroutine calc_q2
        real(8), allocatable    :: matrix(:,:), c(:)
        integer                 :: i
        real(8)                 :: i_r, a, b
        
        do i = scale,1,-1
            box_office(i) = sum(box_office(1:i))
        end do
        
        allocate(matrix(2,3),c(2))
        matrix = 0.
        matrix(1,1) = scale
        do i = 1,scale
            i_r = real(i)
            matrix(1,2) = matrix(1,2) + 1.0/i_r
            matrix(2,2) = matrix(2,2) + 1.0/i_r/i_r
            matrix(1,3) = matrix(1,3) + log(box_office(i))
            matrix(2,3) = matrix(2,3) + 1.0/i_r * log(box_office(i))
        end do
        matrix(2,1) = matrix(1,2)
        call matrix_chase(matrix,c)
        
        allocate(office_total(total_day))
        office_total(1:scale) = box_office
        do i = scale+1,total_day
            i_r = real(i)
            office_total(i) = exp(c(1) + c(2)/i_r)
        end do
        deallocate(box_office)
        
    end subroutine calc_q2
    
    subroutine output_q2(tgt_day)
        integer, intent(in), optional   :: tgt_day
        character(len=5)   :: day_str
        
        
        call output_u(uo)
        call output_u(print_unit)
        deallocate(office_total)
    contains
    subroutine output_u(unit)
        integer, intent(in) :: unit
        
        write(unit,'(2A)') 'File Name : ', TRIM(file_name)
        write(day_str,'(I5)') total_day
        write(unit,'(A40,F10.2)') 'Total box office on DAY '//TRIM(ADJUSTL(day_str))//' is :              ', office_total(total_day)
        if (present(tgt_day)) then
            write(day_str,'(I5)') tgt_day
            write(unit,'(A40,F10.2)') 'Total box office on '//TRIM(ADJUSTL(day_str))//' more days is :    ', office_total(scale+tgt_day)
        end if
        write(unit,*)
    end subroutine output_u
    end subroutine output_q2
    
end module Q2