module Q3
    use universal
    implicit none
    integer                 :: uo, scale, p, q
    real, allocatable       :: matrix(:,:) !(column,row) 
    real, allocatable       :: B(:)
    integer(8)                 :: time(3), rate
    logical                 :: is_compressed
    
    contains
    
    subroutine Run_Q3
        if (question_flag==3) then
            open(newunit=uo, file='output_Q3.txt',status='replace')
            call print_header(uo)
        else
            uo = output_unit
        end if
        call print_seperator(uo,'Question 3')
        call print_seperator(print_unit,'Question 3')
        
        call run_q3_file('FuncData20211.dat')
        call run_q3_file('FuncData20212.dat')
        call run_q3_file('FuncData20213.dat')
        call run_q3_file('FuncData20214.dat')
        call run_q3_file('FuncData20215.dat')
        
        if (question_flag==3) close(uo)
    end subroutine
    
    subroutine run_q3_file(file_name)
        character(len=*), intent(in)    :: file_name
        
        call SYSTEM_CLOCK(time(1),rate)
        call read_q3(file_name)
        call SYSTEM_CLOCK(time(2),rate)
        call matrix_gauss
        deallocate(matrix)
        call SYSTEM_CLOCK(time(3),rate)
        call output_q3(file_name)
    end subroutine
    
    subroutine read_q3(file_name)
        implicit none
        character(len=*), intent(in)    :: file_name
        real, allocatable   :: matrix_line(:)
        integer     :: u, i, j, j_
        integer     :: compress_flag
        real        :: real_4
        logical     :: is_exist
        
        inquire(file=ADJUSTL(TRIM(INPUT_DIR))//file_name,exist=is_exist)
        if(.not. is_exist) then
            write(*,*) ' *ERROR* No input file '//file_name//' found in dir '//ADJUSTL(TRIM(INPUT_DIR))
            write(*,*) 'Program Stopped'
            stop
        end if
        
        open(newunit=u,file=ADJUSTL(TRIM(INPUT_DIR))//file_name,access='stream',form='unformatted',action='read')
        read(u,pos=5) compress_flag
        select case (compress_flag)
        case(258)
            is_compressed = .false.
        case(514)
            is_compressed = .true.
        case default
            write(*,*) 'file compress indicator error'
            stop
        end select
        read(u,pos=13) scale,q,p
        print*, 'scale=', scale
        allocate(matrix(p+q+1,scale))
        matrix = 0.
        allocate(B(scale))
        if (.not. is_compressed) then
            allocate(matrix_line(scale))
            do i = 1,scale
                ! new method introduced, more efficient on time
                !do j = 1,scale
                !    read(u) real_4
                !    if(i==j) then
                !        matrix(p+1,i) = real_4
                !    else if(j>i .and. j-i<=q) then
                !        matrix(p+1+j-i,i) = real_4
                !    else if(i>j .and. i-j<=p) then
                !        matrix(p+1+j-i,i) = real_4
                !    end if
                !end do
                read(u) matrix_line
                if(i<1+p) then
                    matrix(p+2-i:p+q+1,i) = matrix_line(1:i+q)
                elseif(i>scale-q) then
                    matrix(1:scale-i+p+1,i) = matrix_line(i-p:scale)
                else
                    matrix(:,i) = matrix_line(i-p:i+q)
                end if
            end do
            deallocate(matrix_line)
        else
            do i = 1,scale
                read(u) matrix(:,i)
            end do
        end if
        read(u) B
        
    end subroutine read_q3
    
    subroutine output_q3(file_name)
        character(len=*), intent(in)    :: file_name
        real    :: r
        
        r = ( maxval(B) + minval(B) )/2
        call output_u(uo)
        call output_u(print_unit)
        deallocate(B)
    contains
    subroutine output_u(unit)
        integer, intent(in) :: unit
        
        write(unit,'(A,F6.3)') 'The solution for file "'//file_name//'" is : ', r
        write(unit,'(A,F6.3,A)') 'Total Time Cost                 :', (time(3)-time(1))/real(rate), ' seconds'
        write(unit,'(A,F6.3,A)') 'Time cost for reading input     :', (time(2)-time(1))/real(rate), ' seconds'
        write(unit,'(A,F6.3,A)') 'Time cost for calulating matrix :', (time(3)-time(2))/real(rate), ' seconds'
        write(unit,*)
    end subroutine output_u
    end subroutine output_q3
    
    subroutine matrix_gauss
        real    k
        integer i,j, d_line
        
        do i = 1,scale-1
            do j = i+1,min(i+p,scale)
                d_line = j-i
                k = matrix(p+1-d_line,j)/matrix(p+1,i)
                matrix(1:p+q+1-d_line,j) = matrix(1:p+q+1-d_line,j) - matrix(1+d_line:p+q+1,i)*k
                B(j) = B(j) - B(i)*k
            end do
        end do
        
        do i = scale,2,-1
            do j = i-1,max(1,i-q),-1
                d_line = i-j
                k = matrix(p+1+d_line,j)/matrix(p+1,i)
                matrix(p+1+d_line,j) = matrix(p+1+d_line,j) - matrix(p+1,i)*k
                B(j) = B(j) - B(i)*k
            end do
        end do
        
        do i = 1,scale
            B(i) = B(i)/matrix(p+1,i)
        end do
    end subroutine matrix_gauss
    
end module Q3