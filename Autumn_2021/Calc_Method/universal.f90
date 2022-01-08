module universal
    implicit none
    integer     :: question_flag, output_unit = 999, print_unit = 6
    character(len=180)  :: INPUT_DIR = './', CUSTOM_DIR_FILENAME = 'CUSTOM_DIR.txt'
    logical     :: dir_error = .false.
    
    contains
    subroutine print_header(unit)
        implicit none
        integer, intent(in)     :: unit
        
        write(unit,'(A)') '============================================================'
        write(unit,'(A)') '              <--  Calculation Method Program -->           '
        write(unit,'(A)') '         Original Author: Xu Haoxiang  ID: 3121103330       '
        write(unit,'(A)') '         Lisenced Under CC BY-NC-SA 4.0 International       '
        write(unit,'(A)') '============================================================'
        write(unit,'(A)')
        
    end subroutine print_header
    
    subroutine print_seperator(unit,title)
        implicit none
        integer, intent(in)             :: unit
        character(len=*), intent(in)    :: title
        
        write(unit,'(A)') '------------'//ADJUSTL(TRIM(title))//'------------'
    end subroutine print_seperator
    
    subroutine matrix_chase(matrix,results)
        real(8), intent(in)     :: matrix(:,:) !(row,column)
        real(8), intent(inout)  :: results(:)
        real(8), allocatable    :: u(:), y(:), l(:)
        integer     :: scale, i
        
        scale = size(matrix,dim=1)
        allocate(u(scale),y(scale),l(scale))
        u(1) = matrix(1,1)
        y(1) = matrix(1,scale+1)
        do i = 2,scale
            l(i) = matrix(i,i-1)/u(i-1)
            u(i) = matrix(i,i) - l(i)*matrix(i-1,i)
            y(i) = matrix(i,scale+1) - l(i)*y(i-1)
        end do
        results(scale) = y(scale)/u(scale)
        do i = scale-1,1,-1
            results(i) = ( y(i) - matrix(i,i+1)*results(i+1) )/u(i)
        end do
        deallocate(u,y,l)
    end subroutine matrix_chase
    
end module