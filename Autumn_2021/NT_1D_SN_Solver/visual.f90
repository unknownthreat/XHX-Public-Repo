module visual
	implicit none
	integer, parameter :: print_unit = 6
	
	contains
    subroutine print_header(unit)
        implicit none
        integer, intent(in)             :: unit
        
        write(unit,'(A)') '============================================================'
        write(unit,'(A)') '                   <--  NT 1D SN Solver -->                 '
        write(unit,'(A)') '         Original Author: Xu Haoxiang  ID: 3121103330       '
        write(unit,'(A)') '         Lisenced Under CC BY-NC-SA 4.0 International       '
        write(unit,'(A)') '============================================================'
        write(unit,'(A)')
        
	end subroutine print_header
	
    subroutine delay_msg(msg,time)
        implicit none
        character(len=*), intent(in)    :: msg
        integer, intent(in)             :: time
        character(len=5) str_time
        integer i
        
        write(print_unit,*)
        do i = 1,time
            write(str_time,'(I5)') time+1-i
            write(print_unit,'(A)') msg//' in '//TRIM(ADJUSTL(str_time))//' seconds'
            call sleep(1)
        end do
	end subroutine delay_msg
	
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
	
    subroutine draw_figure(title,array, f_height, f_width, unit)
        implicit none
        character(len=*), intent(in)    :: title
        real(8), intent(in)             :: array(:)
        integer, intent(in)             :: f_height, f_width
        integer, intent(in)             :: unit
        integer :: i,j,k,n,axial_line=0, vertical_line=-1
        integer, allocatable            :: x(:),y(:)
        real max,min
        logical draw_point
        logical, allocatable            :: point_drawn(:)
        
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
        end do
        if (unit == print_unit) then
            call delay_msg(title//' Distribution Figure Will be Shown',3)
            call SYSTEM('CLS')
        end if
        write(unit,'(A)')
        write(unit,'(A)') '--------------------'//title//' Distribution Figure--------------------'
        write(unit,'(A)')
        write(unit,'(A,$)') '        '
        write(unit,'(A)') ' /\'
        
        do i = f_height,0,-1
            if (i==f_height) then
                write(unit,'(ES8.1,$)') max
            elseif(i==0) then
                write(unit,'(ES8.1,$)') min
            else
                write(unit,'((A),$)') '        '
            end if
            write(unit,'((A),$)') ' |'
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
                    write(unit,'((A),$)') '+'
                    point_drawn(j) = .true.
                else
                    if(j==vertical_line) then
                        write(unit,'((A),$)') '.'
                    elseif(i/=axial_line) then
                        write(unit,'((A),$)') ' '
                    else
                        write(unit,'((A),$)') '-'
                    end if
                end if
            end do
            if(i/=axial_line) then
                write(unit,'((A))')
            else
                write(unit,'((A))') '--->'
            end if
        end do
        write(unit,'((A),$)') '        '
        do j = 1,f_width
            if(j+1==vertical_line) then
                write(unit,'((A),$)') 'Max Cap'
            else
                write(unit,'((A),$)') ' '
            end if
        end do
        write(unit,'((A))')
        
        deallocate(x,y,point_drawn)
    end subroutine draw_figure
end module visual