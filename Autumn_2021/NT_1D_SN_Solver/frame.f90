module frame
    use visual
	implicit none
	
	integer				:: read_unit = 101, iter_write_unit= 102, write_unit = 103, io_error
	character(len=20)	:: read_file = 'input.txt', iter_write_file = 'temp.txt', write_file = 'output.txt'
	integer				:: scale, mat_num, sn_order, bc_cond(2), iter_out
	real(8), allocatable    :: grid_length(:), grid_mat(:), xsec_f_nu(:), xsec_t(:), xsec_s(:), source(:)
	real(8), allocatable    :: flux_ave(:)
    real                :: iter_error(2) = (/1.0e-6,1.0e-6/), error_k = 0., error_f, keff
    integer             :: iter_limit(2) = (/10,1000/)
    character(len=20)   :: problem_type='EIGEN', method_type='FMFD'
	
	contains
	
	subroutine read_input
		implicit none
		
		character(len=200)		:: line, keyword
        character(len=20)       :: dummy_char
		integer					:: i, i_mat
        logical                 :: keyword_exist(7) = .false., file_exist
        real, allocatable       :: mat_t(:), mat_s(:), mat_f_nu(:), mat_q(:), temp_xsec(:,:)
        real, allocatable       :: container(:)
		
        inquire(file=read_file,exist=file_exist)
        if (.not.file_exist) then
            write(*,*) 'No Input File Is Found !' 
            write(*,*) 'Please Provide A Input File Named "'&
                & //TRIM(ADJUSTL(read_file))//'" at the Same Folder as the Executable.'
            stop
        end if
		open(unit=read_unit,file=read_file)
		write(*,*) '>> Reading input...'
		write(*,*)
		do
			line = ''
			read(read_unit,fmt='(A)',iostat = io_error) line
			if (io_error<0) then
				do i = 1,size(keyword_exist)
					if (.not. keyword_exist(i)) then
                        print*, 'Keyword Missing In Input Data !'
                        stop
					end if
				end do
				write(*,*) '>- Reading input finished'
				write(*,*)
				exit
			end if
			if (line(1:1)==' '.or.line(1:1)=='!') cycle
            do i = 1,200
                if ( LGE(line(i:i),'a') .and. (LLE(line(i:i),'z')) ) line(i:i) = ACHAR(IACHAR(line(i:i))-32)
				if (line(i:i)=='!') line(i:200) = ''
			end do
			read(line,fmt=*,iostat = io_error) keyword
			
			select case (ADJUSTL(TRIM(keyword)))
            case('PROBLEM_TYPE')
                keyword_exist(1) = .true.
                read(line,*,iostat = io_error) keyword, problem_type
                
            case('METHOD_TYPE')
                keyword_exist(2) = .true.
                read(line,*,iostat = io_error) keyword, method_type
                
            case('GRID')
                keyword_exist(3) = .true.
                allocate(container(100000000))
                container = -1.
                read(line,*,iostat = io_error) keyword, container
                do i = 1,size(container)
                    if(container(i)<0) exit
                end do
                scale = i-1
                deallocate(container)
				allocate(grid_length(scale),grid_mat(scale),xsec_t(scale),xsec_f_nu(scale),xsec_s(scale),source(scale))
                allocate(flux_ave(scale))
				read(line,*,iostat = io_error) keyword, grid_length
                
			case('BC')
                keyword_exist(4) = .true.
				read(line,*,iostat = io_error) keyword, bc_cond
                
			case('MATERIAL')
                keyword_exist(5) = .true.
                if(.not.keyword_exist(3)) then
                    write(*,*) 'PLZ Input GRID Before '//ADJUSTL(TRIM(keyword))//' !'
                    stop
                end if
				read(line,*,iostat = io_error) keyword, grid_mat
                
            case('MAT_XSEC')
                keyword_exist(6) = .true.
                if(.not.keyword_exist(3)) then
                    write(*,*) 'PLZ Input GRID Before '//ADJUSTL(TRIM(keyword))//' !'
                    stop
                end if
                mat_num = mat_num + 1
				read(line,*,iostat = io_error) keyword, i_mat
                if (mat_num/=i_mat) then
                    write(*,*) 'Non-sequential numbering found in MAT_XSEC !'
                    stop
                end if
                if (mat_num==1) then
                    allocate(mat_t(1),mat_f_nu(1),mat_s(1),mat_q(1))
                    read(line,*,iostat = io_error) keyword, i_mat, mat_t(1), mat_s(1), mat_f_nu(1)
                else
                    allocate(temp_xsec(3,mat_num-1))
                    temp_xsec(1,:) = mat_t
                    temp_xsec(2,:) = mat_s
                    temp_xsec(3,:) = mat_f_nu
                    deallocate(mat_t,mat_f_nu,mat_s)
                    allocate(mat_t(mat_num),mat_s(mat_num),mat_f_nu(mat_num))
                    mat_t(1:mat_num-1) = temp_xsec(1,:)
                    mat_s(1:mat_num-1) = temp_xsec(2,:)
                    mat_f_nu(1:mat_num-1) = temp_xsec(3,:)
                    read(line,*,iostat = io_error) keyword, i_mat, mat_t(i_mat), mat_s(i_mat), mat_f_nu(i_mat)
                    deallocate(temp_xsec)
                end if
                
            case('SN_ORDER')
                keyword_exist(7) = .true.
                read(line,*,iostat = io_error) keyword, sn_order
                
            case('ITER_LIMIT')
                read(line,*,iostat = io_error) keyword, iter_limit, iter_error
                
            case default
                write(*,*) 'Wrong Keyword Input Detected :'//ADJUSTL(TRIM(keyword))//' !'
                stop
                
            end select
            if (io_error/=0) then
                write(*,*) 'Warning: Input Error Found in Keyword '//ADJUSTL(TRIM(keyword))//' !'
                stop
            end if
        end do
        write(*,*) '>- Selected Problem Type: '//TRIM(problem_type)
        write(*,*) '>- Selected Method Type : '//TRIM(method_type)
        write(*,*)
        write(*,*) '>> Assigning xsec...'
        write(*,*)
        do i = 1,scale
            i_mat = grid_mat(i)
            xsec_t(i) = mat_t(i_mat)
            xsec_s(i) = mat_s(i_mat)
            xsec_f_nu(i) = mat_f_nu(i_mat)
            source(i) = mat_f_nu(i_mat)
        end do
        if (TRIM(problem_type)=='EIGEN') then
            deallocate(source)
        else if (TRIM(problem_type)=='FIXED-SOURCE') then
            deallocate(xsec_f_nu)
        end if
        write(*,*) '>- Assigning xsec finished'
        write(*,*)
    end subroutine read_input
    
    subroutine output_iter_title
        implicit none
        
        open(unit=iter_write_unit,file=iter_write_file,status='replace')
        
        write(iter_write_unit,'(A)') '--------------Iteration Process Information-----------------'
        if(TRIM(problem_type)=='EIGEN') then
            write(iter_write_unit,'(A)') 'iter_cycle  |   keff   |   error_k   |  error_flux'
        else
            write(iter_write_unit,'(A)') 'iter_cycle  |  error_flux'
        end if
        
    end subroutine output_iter_title
    
    subroutine output_summary
        implicit none
        integer i
        
        write(write_unit,'(A)')
        write(write_unit,'(A)') '------------------------SUMMARY-----------------------------'
        write(write_unit,'(A)') 'Problem Type   :   '//TRIM(problem_type)
        write(write_unit,'(A)') 'Method Type    :   '//TRIM(method_type)
        write(write_unit,'(A,I6)') 'Total Mesh Num :', scale
        write(write_unit,'(A,I3)') 'SN Order       : ',sn_order
        write(write_unit,'(A,ES10.2)') 'Flux Cov. Error: ', iter_error(1)
        if(TRIM(problem_type)=='EIGEN') write(write_unit,'(A,ES10.2)') 'Keff Cov. Error: ', iter_error(2)
        write(write_unit,'(A)')
        write(write_unit,'(A,I3)') 'Iteration Cycle:  ', iter_out
        if(TRIM(problem_type)=='EIGEN') then
            write(write_unit,'(A,F8.5)') 'Keff Result    :  ', keff
            write(write_unit,'(A,F8.5)') 'Keff Error     :  ', error_k
        end if
        write(write_unit,'(A,ES12.4)') 'Max Flux Error : ', error_f
        write(write_unit,'(A)') '------------------------------------------------------------'
        write(write_unit,'(A)')
        
        call draw_figure('Flux', flux_ave, 40,120, write_unit)
        
        write(write_unit,'(A)') '--------------------Flux Distribution-----------------------'
        write(write_unit,'(A)') 'Grid Number  |  Gird Width  |  Flux Ave'
        do i = 1,scale
            write(write_unit,'(X,I6,11X,F6.4,5X,ES12.4)') i, grid_length(i), flux_ave(i)
        end do
        write(write_unit,'(A)')
        
        call SYSTEM('CLS')
        write(print_unit,'(A)') '----------------------SUMMARY-------------------------------'
        write(print_unit,'(A)') 'Problem Type   :   '//TRIM(problem_type)
        write(print_unit,'(A)') 'Method Type    :   '//TRIM(method_type)
        write(print_unit,'(A,I3)') 'SN Order       : ',sn_order
        write(print_unit,'(A)')
        write(print_unit,'(A,I3)') 'Iteration Cycle:  ', iter_out
        if(TRIM(problem_type)=='EIGEN') then
            write(print_unit,'(A,F8.5)') 'Keff Result    :  ', keff
            write(print_unit,'(A,F8.5)') 'Keff Error     :  ', error_k
        end if
        write(print_unit,'(A,ES12.4)') 'Max Flux Error : ', error_f
        write(print_unit,'(A)') '------------------------------------------------------------'
        write(write_unit,'(A)')
    end subroutine output_summary
    
    subroutine output_iter_info(iter_out, error_flux, keff, error_k)
        implicit none
        integer, intent(in)     :: iter_out
        real, intent(in)        :: error_flux
        real, intent(in), optional  :: keff, error_k
        
        if(present(keff)) then
            write(iter_write_unit,'(I6,7X,F8.5,2X,ES12.4,3X,ES12.4)') iter_out, keff, error_k, error_f
            write(*,'(I6,7X,F8.5,2X,ES12.4,3X,ES12.4)') iter_out, keff, error_k, error_f
        else
            write(iter_write_unit,'(X,I5,7X,ES12.4)') iter_out, error_f
            write(*,'(X,I5,7X,ES12.4)') iter_out, error_f
        end if
    end subroutine output_iter_info
    
    subroutine transfer_iter_info
        implicit none
        character(len=200)  :: line
        integer i
        
        rewind(iter_write_unit)
        do
            read(iter_write_unit,'(A)',iostat=io_error) line
            if (io_error<0) exit
            do i = len(line),1,-1
                if(line(i:i)/=' ') exit
            end do
            write(write_unit,'(A)') line(1:i)
        end do
        close(iter_write_unit)
        call SYSTEM('del '//ADJUSTL(TRIM(iter_write_file)))
        close(write_unit)
    end subroutine transfer_iter_info
    
end module frame