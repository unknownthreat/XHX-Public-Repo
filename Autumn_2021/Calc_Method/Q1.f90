module Q1
    use universal
    implicit none
    integer                 :: scale, uo
    real(8), allocatable    :: data_x(:), data_y(:)
    real(8)                 :: length_side(2)
    
    contains
    
    subroutine Run_Q1
        if (question_flag==1) then
            open(newunit=uo, file='output_Q1.txt',status='replace')
            call print_header(uo)
        else
            uo = output_unit
        end if
        call print_seperator(uo,'Question 1')
        call print_seperator(print_unit,'Question 1')
        
        call read_q1('river1.csv')
        call calc_q1(length_side(1))
        deallocate(data_x,data_y)
        call read_q1('river2.csv')
        call calc_q1(length_side(2))
        call output_q1
        
        if (question_flag==1) close(uo)
    end subroutine Run_Q1
    
    subroutine read_q1(file_name)
        character(len=*)    :: file_name
        real(8)             :: r1,r2
        integer             :: io_error, i, u
        logical     :: is_exist
        
        inquire(file=ADJUSTL(TRIM(INPUT_DIR))//file_name,exist=is_exist)
        if(.not. is_exist) then
            write(*,*) ' *ERROR* No input file '//file_name//' found in dir '//ADJUSTL(TRIM(INPUT_DIR))
            write(*,*) 'Program Stopped'
            stop
        end if
        
        open(newunit=u,file=ADJUSTL(TRIM(INPUT_DIR))//file_name)
        read(u,*)
        scale = 0
        do
            read(u,*,iostat=io_error)
            if(io_error<0) exit
            scale = scale + 1
        end do
        allocate(data_x(0:scale-1),data_y(0:scale-1))
        rewind(u)
        read(u,*)
        do i = 0,scale-1
            read(u,*) data_x(i), data_y(i)
        end do
        close(u)
    end subroutine read_q1
    
    subroutine calc_q1(length_side)
        real(8), intent(out)    :: length_side
        integer                 :: i
        real(8), allocatable    :: h(:),mu(:),lambda(:),d(:),M(:)
        real(8), allocatable    :: matrix(:,:)
        real(8)                 :: length, length_i
        
        allocate(h(scale-1),mu(scale-2),lambda(scale-2),d(scale-2),M(0:scale-1))
        allocate(matrix(scale-2,scale-1))
        
        do i = 1,scale-1
            h(i) = data_x(i) - data_x(i-1)
        end do
        do i = 1,scale-2
            mu(i) = h(i)/(h(i)+h(i+1))
            lambda(i) = 1-mu(i)
        end do
        do i = 1,scale-2
            d(i) = 6/(h(i)+h(i+1))* ( (data_y(i+1)-data_y(i))/h(i+1) - (data_y(i)-data_y(i-1))/h(i) )
        end do
        
        matrix = 0
        matrix(1,1) = 2; matrix(1,2) = lambda(1); matrix(1,scale-1) = d(1)
        do i = 2,scale-3
            matrix(i,i) = 2
            matrix(i,i-1) = mu(i)
            matrix(i,i+1) = lambda(i)
            matrix(i,scale-1) = d(i)
        end do
        matrix(scale-2,scale-3) = mu(scale-2); matrix(scale-2,scale-2) = 2; matrix(scale-2,scale-1) = d(scale-2)
        
        call matrix_chase(matrix,M(1:scale-2))
        M(0) = 0; M(scale-1) = 0
        
        length = 0
        do i = 1,scale-1
            call calc_length(data_x(i-1),data_x(i),data_y(i-1),data_y(i),M(i-1),M(i),length_i)
            length = length + length_i
        end do
        
        length_side = length
        deallocate(h,mu,lambda,d,M,matrix)
    end subroutine calc_q1
    
    subroutine output_q1
        integer :: sum_p
        
        sum_p = ceiling(sum(length_side)*4500.0)
        call output_u(uo)
        call output_u(print_unit)
    contains
    subroutine output_u(unit)
        integer, intent(in) :: unit
        write(unit,'(A,2F12.2,A)') 'The length of both sides are (m) : ', length_side
        write(unit,'(A,I12,A)') 'The total price is : ', sum_p, ' RMB'
        write(unit,*)
    end subroutine output_u
    end subroutine output_q1
    
    subroutine calc_length(x1,x2,y1,y2,M1,M2,length)
        real(8), intent(in)     :: x1,x2,y1,y2,M1,M2
        real(8), intent(out)    :: length
        real(8)     :: h, A,B,C, delta_h
        integer     :: i,n
        
        h = x2-x1
        A = (M2-M1)/h*0.5
        B = (x2*M1-x1*M2)/h
        C = (3*(x1*x1*M2-x2*x2*M1)+6*(y2-y1)+h*h*(M1-M2))/(6*h)
        
        delta_h = 1.0
        n = INT( h/delta_h )
        
        length = 0
        length = length + Sx(x1)
        do i = 1,n-1
            length = length + 2*Sx(x1+delta_h*i)
        end do
        do i = 1,n
            length = length + 4*Sx(x1+delta_h*(i-0.5))
        end do
        length = length + Sx(x2)
        
        length = length*delta_h/6
        
    contains
    function Sx(x)
        real(8)                 :: Sx
        real(8), intent(in)     :: x
        
        Sx = SQRT(A*A*x**4+2*A*B*x**3+(2*A*C+B*B)*x**2+2*B*C*x+C*C+1)
        
    end function Sx
    
    end subroutine calc_length
    
end module