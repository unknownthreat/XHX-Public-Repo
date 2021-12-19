program NT_1D_SN_Solver
	use frame
	use visual
    use solver
	implicit none
	
    call print_header(print_unit)
	call read_input
    open(unit=write_unit,file=write_file,status='replace')
    call print_header(write_unit)
    call output_iter_title
    call solver_init
    call delay_msg('Iteration Start ',2)
    select case(TRIM(method_type))
    case('FMFD')
        call solver_exec_FMFD
    case('CMNM')
        call solver_exec_CMNM
    end select
    call output_summary
    call transfer_iter_info
    call draw_figure('Flux', flux_ave, 20,100, print_unit)
    call delay_msg('Output File Will Open ',2)
    call SYSTEM('.\'//ADJUSTL(TRIM(write_file)))
    
    
end program NT_1D_SN_Solver