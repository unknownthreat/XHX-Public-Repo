program Random_Number_Generator
    use frame
    implicit none
    
    call print_header
    call read_input
    select case(flag)
    case(1)
        call Forsythe(2)
    case(2)
        call LCG(19.,11.,2.**31-1.)
    case(3)
        call Fibonacci(12345)
    end select
    call Calc_N
    call Calc_Skewness
    call print_summary
    call draw_figure('Uniform Skewness',skew(1,:))
    call draw_figure('Independence Skewness',skew(2,:))
    open(unit=output_unit,file='RNG_output.txt')
    call print_header(output_unit)
    call print_summary(output_unit)
    call output_data(output_unit)
    close(output_unit)
    call SYSTEM('.\RNG_output.txt')
end program