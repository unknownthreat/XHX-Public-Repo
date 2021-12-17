module frame
    implicit none
    integer :: scale, delay_time=3, flag, max_cap=0, output_unit=101, f_width = 100
    real(8), allocatable :: num(:), skew(:,:)
    
    contains
    subroutine read_input
        implicit none
        write(*,*) '>> Reading Input...'
        write(*,*)
        write(*,*) 'Following methods are supported:'
        write(*,*) '1: Forsythe'
        write(*,*) '2: LCG'
        write(*,*) '3: Classic Fibonacci'
10      continue
        write(*,*)
        write(*,'((A),$)') ' Please select generation method: '
        read(*,*) flag
        select case(flag)
        case(1)
            write(*,*) '>> Method Selected: Forsythe'
            write(*,*) 'Seed Number Requirement: 2*s-digit Positive'
        case(2)
            write(*,*) '>> Method Selected: LCG'
            write(*,*) 'Seed Number Requirement: Positive'
        case(3)
            write(*,*) '>> Method Selected: Classic Fibonacci'
            write(*,*) 'Seed Number Requirement: Positive'
        case default
            write(*,*) 'Wrong Input!!!'
            goto 10
        end select
        write(*,*)
        write(*,'((A),$)') ' Please input amount of random number: '
        read(*,*) scale
        if(scale<3) then
            scale=3
            write(*,*) ' >>Amount is changed to 3'
        end if
        allocate(num(scale), skew(2,scale))
        skew=0
        write(*,'((A),$)') ' Please input x0: '
        read(*,*) num(1)
        if (num(1)<=0) then
            write(*,*) ' Illegal x0!'
            deallocate(num,skew)
            goto 10
        end if
    end subroutine read_input
    
    subroutine LCG(a,c,m)
        implicit none
        real, intent(in) :: a,c,m
        integer i
        
        write(*,*) '>> Performing LCG Generation...'
        write(*,*)
        do i = 2,scale
            num(i) = mod(a*num(i-1)+c,m)
        end do
        num = num/m
    end subroutine LCG
    
    subroutine Forsythe(s)
        implicit none
        integer, intent(in) :: s
        integer i
        
        write(*,*) '>> Performing Forsythe Generation...'
        write(*,*)
        if(num(1)<10**(2*s-1).or.num(1)>=10**(2*s)) then
            write(*,*) 'Wrong Seed Number!'
            write(*,'(1X,(A),I1,(A))') 'Please input a ',2*s,'-digit number as seed number!'
            stop
        end if
        num(2) = mod(int(0.1**s*num(1)**2),100**s)
        do i = 3,scale
            num(i) = mod(int(0.1**s*num(i-1)*num(i-2)),100**s)
        end do
        num = num/(100**s)
    end subroutine Forsythe
    
    subroutine Fibonacci(M)
        implicit none
        integer, intent(in)     :: M
        integer i
        
        write(*,*) '>> Performing Fibonacci Generation...'
        write(*,*)
        num(2) = mod(2*int(num(1)),M)
        do i = 3,scale
            num(i) = mod(int(num(i-1)+num(i-2)),M)
        end do
        num = num/(M+0.)
    end subroutine Fibonacci
    
    subroutine Calc_N
        implicit none
        integer i,j
        
        write(*,*) '>> Calculating Capacity...'
        write(*,*)
        main:do i = 3,scale-1
            do j = 1,i-2
                if(num(i)==num(j).and.num(i+1)==num(j+1)) then
                    max_cap = i-1
                    exit main
                end if
            end do
        end do main
        if(max_cap==0) max_cap = scale
    end subroutine Calc_N
    
    subroutine Calc_Skewness
        implicit none
        integer i,j,k,n,nx,ny
        integer guage(1)
        real x,y,max, temp_cap
        
        write(*,*) '>> Calculating Skewness...'
        write(*,*)
        do i = 1,scale
            temp_cap = min(max_cap,i)
            if (mod((i+1)/2,scale/20)==1) then
                guage = floor( (i+0.)/(scale+0.)*(f_width+0.) )
                call draw_bar('Uniform Skewness Calcutaion',guage,f_width)
            end if
            max = 0.
            do j = 1,20
                x = j/20.
                call count_amount(i,n,x)
                if ( abs((n+0.)/(temp_cap+0.)-x+0.)>max ) max = abs((n+0.)/(temp_cap+0.)-x+0.)
            end do
            skew(1,i) = max
        end do
        do i = 1,scale,2
            temp_cap = min(max_cap,i+1)
            if (mod((i+1)/2,scale/20)==1) then
                guage = floor( (i+0.)/(scale+0.)*(f_width+0.) )
                call draw_bar('Independence Skewness Calcutaion',guage,f_width)
            end if
            max = 0.
            do j = 1,20
                x = j/20.
                call count_amount(i+1,nx,x,1.)
                do k = 1,20
                    y = k/20.
                    call count_amount(i+1,n,x,y)
                    call count_amount(i+1,ny,1.,y)
                    if ( abs((2.*n/(temp_cap+0.)-4.*nx*ny/temp_cap**2)) >max ) then
                        max = abs((2.*n/(temp_cap+0.)-4.*nx*ny/temp_cap**2))
                    end if
                end do
            end do
            skew(2,i+1) = max
            skew(2,i) = max
        end do
        write(*,*) '>> Skewness Calculation Finished'
        write(*,*)
    contains
    subroutine count_amount(n,count,x,y)
        integer, intent(in)         :: n
        integer, intent(inout)      :: count
        real, intent(in)            :: x
        real, intent(in), optional  :: y
        
        integer i
        count=0
        if(.not. present(y)) then
            do i = 1,n
                if(num(i)<x) count = count+1
            end do
        else
            do i = 1,n,2
                if(num(i)<x.and.num(i+1)<y) count = count+1
            end do
        end if
    end subroutine count_amount
    end subroutine Calc_Skewness
    
    subroutine print_summary(unit)
        implicit none
        integer, intent(in), optional   :: unit
        
        if(.not.present(unit)) then
            call SYSTEM('CLS')
            write(*,*)
            write(*,*) '>> Calculation Finished'
            write(*,*)
            write(*,*) '==========<<SUMMARY>>=========='
            write(*,'(X,A,I8)') 'Random Number Scale:', scale
            select case(flag)
            case(1)
                write(*,*) 'Generation Method: Forsythe'
            case(2)
                write(*,*) 'Generation Method: LCG'
            case(3)
                write(*,*) 'Generation Method: Classic Fibonacci'
            end select
            write(*,*)
            if(max_cap==scale) then
                write(*,*) 'The Max Capacity is NOT Reached'
            else
                write(*,*) 'The Max Capacity IS Reached'
                write(*,'(X,A,I8)') 'Max Capacity:        ', max_cap
            end if
            write(*,*) '==============================='
            write(*,*)
        else
            write(unit,*)
            write(unit,'(A)') '==========<<SUMMARY>>=========='
            write(unit,'(A,I8)') 'Random Number Scale:', scale
            select case(flag)
            case(1)
                write(unit,'(A)') 'Generation Method: Forsythe'
            case(2)
                write(unit,'(A)') 'Generation Method: LCG'
            case(3)
                write(unit,'(A)') 'Generation Method: Classic Fibonacci'
            end select
            write(unit,*)
            if(max_cap==scale) then
                write(unit,'(A)') 'The Max Capacity is NOT Reached'
            else
                write(unit,'(A)') 'The Max Capacity IS Reached'
                write(unit,'(A,I8)') 'Max Capacity:        ', max_cap
            end if
            write(unit,'(A)') '==============================='
            write(unit,*)
        end if
            
    end subroutine print_summary
    
    subroutine delay_msg(msg,time)
        implicit none
        character(len=*), intent(in)    :: msg
        integer, intent(in)             :: time
        character(len=5) str_time
        integer i
        
        do i = 1,time
            write(str_time,'(I5)') time+1-i
            write(*,*) msg//' in '//TRIM(ADJUSTL(str_time))//' seconds'
            call sleep(1)
        end do
    end subroutine delay_msg
    
    subroutine draw_figure(title,array)
        implicit none
        character(len=*), intent(in)    :: title
        real(8), intent(in)             :: array(:)
        integer :: i,j,k,f_height=20,n,axial_line=0, vertical_line=-1
        integer, allocatable            :: x(:),y(:)
        real max,min
        logical draw_point
        logical, allocatable            :: point_drawn(:)
        
        call delay_msg(title//'Distribution Figure Will be Shown',3)
        call SYSTEM('CLS')
        allocate(point_drawn(f_width))
        point_drawn = .false.
        n = size(array)
        allocate(x(n),y(n))
        max = maxval(array)
        min = minval(array)
        if (min>=0) then
            y(:) = int( array(:)/max*f_height )
        else
            y(:) = int( (array(:)-min)/(max-min)*f_height )
            axial_line = int( -min/(max-min)*f_height )
        end if
        do i = 1,n
            x(i) = int( (i+0.)/(n+0.)*(f_width+0.) )
            if (i==max_cap.and.i/=scale) vertical_line = x(i)
        end do
        write(*,*)
        write(*,'((A))') '--------------------'//title//' Distribution Figure--------------------'
        write(*,*)
        write(*,'((A),$)') '        '
        write(*,*) '/\'
        
        do i = f_height,0,-1
            if (i==f_height) then
                write(*,'(ES8.1,$)') max
            elseif(i==0) then
                write(*,'(ES8.1,$)') min
            else
                write(*,'((A),$)') '        '
            end if
            write(*,'((A),$)') ' |'
            do j = 1,f_width
                draw_point = .false.
                if(.not. point_drawn(j)) then
                    do k = 1,n
                        if(j==x(k).and.i==y(k)) then
                            draw_point = .true.
                            exit
                        end if
                    end do
                end if
                if(draw_point) then
                    write(*,'((A),$)') '+'
                    point_drawn(j) = .true.
                else
                    if(j==vertical_line) then
                        write(*,'((A),$)') '.'
                    elseif(i/=axial_line) then
                        write(*,'((A),$)') ' '
                    else
                        write(*,'((A),$)') '-'
                    end if
                end if
            end do
            if(i/=axial_line) then
                write(*,*)
            else
                write(*,'((A))') '--->'
            end if
        end do
        write(*,'((A),$)') '        '
        do j = 1,f_width
            if(j+1==vertical_line) then
                write(*,'((A),$)') 'Max Cap'
            else
                write(*,'((A),$)') ' '
            end if
        end do
        write(*,*)
        deallocate(x,y,point_drawn)
    end subroutine draw_figure
    
    subroutine draw_bar(title,guage,guage_cap)
        implicit none
        character(len=*), intent(in)    :: title
        integer, intent(in)             :: guage(:), guage_cap
        integer i,j
        
        call SYSTEM('CLS')
        
        write(*,*)
        write(*,*) '========'//title//' Progress Indication Bar========'
        write(*,*)
        write(*,'((A),$)') ' |'
        do i = 1,guage_cap
            write(*,'((A),$)') '-'
        end do
        write(*,*) '|'
        do i = 1,size(guage)
            write(*,'((A),$)') ' |'
            do j = 1,guage_cap
                if (j<=guage(i)) then
                    write(*,'((A),$)') 'X'
                else
                    write(*,'((A),$)') ' '
                end if
            end do
            write(*,*) '|'
        end do
        write(*,'((A),$)') ' |'
        do i = 1,guage_cap
            write(*,'((A),$)') '-'
        end do
        write(*,*) '|'
        write(*,*)
    end subroutine draw_bar
    
    subroutine print_header(unit)
        implicit none
        integer, intent(in), optional   :: unit
        
        if(.not.present(unit)) then
            write(*,'(1X,(A))') '============================================================'
            write(*,'(1X,(A))') '------This Is A Small Random Number Generator Program.------'
            write(*,'(1X,(A))') '------------Coded By Xu Haoxiang. ID 3121103330.------------'
            write(*,'(1X,(A))') '============================================================'
            write(*,*)
        else
            write(unit,'((A))') '============================================================'
            write(unit,'((A))') '------This Is A Small Random Number Generator Program.------'
            write(unit,'((A))') '------------Coded By Xu Haoxiang. ID 3121103330.------------'
            write(unit,'((A))') '============================================================'
            write(unit,*)
        end if
    end subroutine print_header
    
    subroutine print_num
        implicit none
        integer i
        
        do i = 1,scale
            print*, i, num(i)
        end do
    end subroutine print_num
    
    subroutine output_data(unit)
        implicit none
        integer, intent(in), optional   :: unit
        integer i
        
        write(unit,'((A))') 'Array Length  |  Random Num  |  Uni. Skewness  |  Ind. Skewness'
        do i = 1,scale
            write(unit,'(X,I8,F18.8,F16.8,F18.8)') i, num(i), skew(1,i), skew(2,i)
        end do
        write(*,*) ">> Output data is stored in 'RNG_output.txt'"
        call delay_msg('File Will Open',3)
    end subroutine output_data
    
    subroutine generate_seed(seed)
        implicit none
        integer, intent(inout)          :: seed
        integer     :: time(8)
        
        call DATE_AND_TIME(VALUES=time)
        seed = MOD(time(8),10)
    end subroutine generate_seed
    
end module