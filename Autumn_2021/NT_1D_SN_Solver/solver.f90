module solver
    use frame
    implicit none
    
    real, allocatable       :: mu(:), w(:), error_flux(:), error_in(:,:)
    integer                 :: iter_in, iter_sweep = 0
    
    contains
    subroutine solver_init
        implicit none
        real    :: mu_2(2) = (/0.57735, -0.57735/), weight_2(2) = 1
        real    :: mu_4(4) = (/0.33998, 0.86114, -0.86114, -0.33998/)
        real    :: weight_4(4) = (/0.65215, 0.34785, 0.34785, 0.65215/)
        real    :: mu_6(6) = (/0.23862, 0.66121, 0.93247, -0.93247, -0.66121, -0.23862/)
        real    :: weight_6(6) = (/0.46791, 0.36076, 0.17132, 0.17132, 0.36076, 0.46791/)
        
        allocate(mu(sn_order),w(sn_order))
        allocate(error_flux(scale),error_in(scale,sn_order))
        select case(sn_order)
        case (2)
            mu = mu_2
            w = weight_2
        case (4)
            mu = mu_4
            w = weight_4
        case (6)
            mu = mu_6
            w = weight_6
        case default
            write(*,*) 'Invalid SN_Order Input !'
            stop
        end select
        
    end subroutine solver_init
    
    subroutine solver_exec_FMFD
        implicit none
        integer i,j, i_sn,i_sn_, i_grid
        real, allocatable       :: flux_ave_ang(:,:), flux_l_ang(:,:), q_ave(:,:), q_scat(:), q_ext(:)
        real, allocatable       :: flux_ave_old(:)
        real                    :: k_old, k_new
        real    :: norm(2)
        
        allocate(flux_ave_ang(scale,sn_order),flux_l_ang(scale+1,sn_order))
        allocate(q_ave(scale,sn_order),q_scat(scale),q_ext(scale))
        allocate(flux_ave_old(scale))
        flux_ave_ang = 1.
        flux_l_ang = 1.
        iter_out = 0
        k_old = 1.
        flux_ave_old = 2.
        
        
        outer: do
            iter_out = iter_out + 1
            if(mod(iter_out-1,20)==0) then
                call SYSTEM('CLS')
                if(TRIM(problem_type)=='EIGEN') then
                    write(*,'(A)') 'iter_cycle  |  keff  |  error_k  |  error_flux'
                else
                    write(*,'(A)') 'iter_cycle  |  error_flux'
                end if
            end if
            
            if(TRIM(problem_type)=='EIGEN') then
                do i_grid = 1,scale
                    q_ext(i_grid) = 0.5/k_old*flux_ave_old(i_grid)*xsec_f_nu(i_grid)
                end do
            else
                do i_grid = 1,scale
                    q_ext(i_grid) = 0.5*source(i_grid)
                end do
            end if
            
            !do iter_in = 1,5
            
            
            do i_sn = 1,sn_order
                
                if(mu(i_sn)>0) then
                    if(bc_cond(1)==1) then
                        flux_l_ang(1,i_sn) = flux_l_ang(1,sn_order+1-i_sn)
                    else
                        flux_l_ang(1,i_sn) = 0.
                    end if
                    do i_grid = 1,scale
                        q_scat(i_grid) = 0.
                        do i_sn_ = 1,sn_order
                            q_scat(i_grid) = q_scat(i_grid) + &
                                & 0.5*xsec_s(i_grid)*flux_ave_ang(i_grid,i_sn_)*w(i_sn_)
                        end do
                        
                        q_ave(i_grid,i_sn) = q_scat(i_grid) + q_ext(i_grid)
                        
                        flux_ave_ang(i_grid,i_sn) = &
                            & ( q_ave(i_grid,i_sn) + 2*mu(i_sn)/grid_length(i_grid)*flux_l_ang(i_grid,i_sn) )/&
                            & (xsec_t(i_grid) + 2*mu(i_sn)/grid_length(i_grid))
                        flux_l_ang(i_grid+1,i_sn) = 2*flux_ave_ang(i_grid,i_sn) - flux_l_ang(i_grid,i_sn)
                        if(flux_l_ang(i_grid+1,i_sn)<0) flux_l_ang(i_grid+1,i_sn)=0.
                    end do
                else
                    if(bc_cond(2)==1) then
                        flux_l_ang(scale+1,i_sn) = flux_l_ang(scale+1,sn_order+1-i_sn)
                    else
                        flux_l_ang(scale+1,i_sn) = 0.
                    end if
                    do i_grid = scale,1,-1
                        q_scat(i_grid) = 0.
                        do i_sn_ = 1,sn_order
                            q_scat(i_grid) = q_scat(i_grid) + &
                                & 0.5*xsec_s(i_grid)*flux_ave_ang(i_grid,i_sn_)*w(i_sn_)
                        end do
                        
                        q_ave(i_grid,i_sn) = q_scat(i_grid) + q_ext(i_grid)
                        
                        flux_ave_ang(i_grid,i_sn) = &
                            & ( q_ave(i_grid,i_sn) - 2*mu(i_sn)/grid_length(i_grid)*flux_l_ang(i_grid+1,i_sn) )/&
                            & (xsec_t(i_grid) - 2*mu(i_sn)/grid_length(i_grid))
                        flux_l_ang(i_grid,i_sn) = 2*flux_ave_ang(i_grid,i_sn) - flux_l_ang(i_grid+1,i_sn)
                        if(flux_l_ang(i_grid,i_sn)<0) flux_l_ang(i_grid,i_sn)=0.
                    end do
                end if
            end do
            !end do
            flux_ave = 0.
            do i_sn = 1,sn_order
                flux_ave(:) = flux_ave(:) + flux_ave_ang(:,i_sn)*w(i_sn)
            end do
            error_flux = abs((flux_ave-flux_ave_old)/flux_ave)
            error_f = maxval(error_flux)
            
            if(TRIM(problem_type)=='EIGEN') then
                norm = 0.
                do i = 1,scale
                    norm(1) = norm(1) + (flux_ave(i)/k_old*0.5*xsec_f_nu(i))**2
                    norm(2) = norm(2) + (flux_ave_old(i)/k_old*0.5*xsec_f_nu(i))**2
                end do
                norm = SQRT(norm)
                k_new = k_old* norm(1)/norm(2)
                error_k = ABS((k_new-k_old)/k_new)
                k_old = k_new
                
                call output_iter_info(iter_out, error_f, k_new, error_k)
            else
                call output_iter_info(iter_out, error_f)
            end if
            
            flux_ave_old = flux_ave
            keff = k_new
            if(iter_out==iter_limit(2)) exit
            if(error_f<iter_error(1).and. error_k<iter_limit(2)) exit
        end do outer
        
    end subroutine solver_exec_FMFD
    
    subroutine solver_exec_CMNM
        implicit none
        integer i,j, i_sn,i_sn_, i_grid
        real, allocatable       :: flux_ave_ang(:,:), flux_ave_ang_old(:,:), flux_l_ang(:,:), q_scat(:)
        real, allocatable       :: flux_ave_old(:)
        real, allocatable       :: flux_ave_ang_highorder(:,:,:), flux_ave_highorder(:,:)
        real                    :: k_old, k_new
        real    :: norm(2), T, Qn(2), sum_C_Qn, exp_ne2T, A,B,C(2), q_ave
        
        allocate(flux_ave_ang(scale,sn_order),flux_ave_ang_old(scale,sn_order),flux_l_ang(scale+1,sn_order))
        allocate(flux_ave_old(scale))
        allocate(flux_ave_ang_highorder(2,scale,sn_order),flux_ave_highorder(2,scale))
        T = 0.
        Qn = 0.
        flux_ave_ang = 1.; flux_l_ang = 1.
        flux_ave_ang_highorder = 1.; flux_ave_highorder = 1.
        iter_out = 0
        k_old = 1.
        flux_ave_old = 1.
        
        outer: do
            iter_out = iter_out + 1
            if(iter_out==iter_limit(2)) exit
            
            inner: do iter_in = 1,iter_limit(1)
                do i_sn = 1,sn_order
                    if(mu(i_sn)>0) then
                        if(bc_cond(1)==1) then
                            flux_l_ang(1,i_sn) = flux_l_ang(1,sn_order+1-i_sn)
                        else
                            flux_l_ang(1,i_sn) = 0.
                        end if
                        
                        do i_grid = 1,scale
                            
                            T = xsec_t(i_grid)*grid_length(i_grid)/mu(i_sn)*0.5
                            exp_ne2T = exp(-2*T)
                            
                            Qn = 0.; q_ave = 0.
                            do i_sn_ = 1,sn_order
                                Qn(:) = Qn(:) + xsec_s(i_grid)*flux_ave_ang_highorder(:,i_grid,i_sn_)*w(i_sn_)*0.5
                                q_ave = q_ave + xsec_s(i_grid)*flux_ave_ang(i_grid,i_sn_)*w(i_sn_)*0.5
                            end do
                            if(TRIM(problem_type)=='EIGEN') then
                                Qn(:) = Qn(:) + 0.5/k_old*flux_ave_highorder(:,i_grid)*xsec_f_nu(i_grid)
                                q_ave = q_ave + 0.5/k_old*flux_ave_old(i_grid)*xsec_f_nu(i_grid)
                            else
                                Qn(:) = Qn(:) + source(i_grid)*0.5 ! ??????
                                q_ave = q_ave + source(i_grid)*0.5
                            end if
                            
                            A = 2*T*(1-exp_ne2T)/(2*T-1+exp_ne2T)
                            B = A
                            C(1) = 1.5*grid_length(i_grid)/mu(i_sn)/T * (T-1+(T+1)*exp_ne2T)/(2*T-1+exp_ne2T)
                            C(2) = 2.5*grid_length(i_grid)/mu(i_sn)/T/T * &
                                & (T**2-3*T+3-(T**2+3*T+3)*exp_ne2T)/(2*T-1+exp_ne2T)
                            
                            sum_C_Qn = C(1)*Qn(1) + C(2)*Qn(2)
                            
                            flux_ave_ang(i_grid,i_sn) = ( mu(i_sn)/grid_length(i_grid)*( A*flux_l_ang(i_grid,i_sn)-sum_C_Qn ) &
                                & + q_ave ) / ( B*mu(i_sn)/grid_length(i_grid)+xsec_t(i_grid) )
                            flux_l_ang(i_grid+1,i_sn) = (1-A)*flux_l_ang(i_grid,i_sn) + B*flux_ave_ang(i_grid,i_sn) + sum_C_Qn
                        end do
                    else
                        if(bc_cond(2)==1) then
                            flux_l_ang(scale+1,i_sn) = flux_l_ang(scale+1,sn_order+1-i_sn)
                        else
                            flux_l_ang(scale+1,i_sn) = 0.
                        end if
                        do i_grid = scale,1,-1
                            
                            T = xsec_t(i_grid)*grid_length(i_grid)/mu(i_sn)*0.5
                            exp_ne2T = exp(-2*T)
                            
                            Qn = 0.; q_ave = 0.
                            do i_sn_ = 1,sn_order
                                Qn(:) = Qn(:) + xsec_s(i_grid)*flux_ave_ang_highorder(:,i_grid,i_sn_)*w(i_sn_)*0.5
                                q_ave = q_ave + xsec_s(i_grid)*flux_ave_ang(i_grid,i_sn_)*w(i_sn_)*0.5
                            end do
                            if(TRIM(problem_type)=='EIGEN') then
                                Qn(:) = Qn(:) + 0.5/k_old*flux_ave_highorder(:,i_grid)*xsec_f_nu(i_grid)
                                q_ave = q_ave + 0.5/k_old*flux_ave_old(i_grid)*xsec_f_nu(i_grid)
                            else
                                Qn(:) = Qn(:) + source(i_grid)*0.5 ! ??????
                                q_ave = q_ave + source(i_grid)*0.5
                            end if
                            
                            A = 2*T*(1-exp_ne2T)/(2*T-1+exp_ne2T)
                            B = A
                            C(1) = 1.5*grid_length(i_grid)/mu(i_sn)/T * (T-1+(T+1)*exp_ne2T)/(2*T-1+exp_ne2T)
                            C(1) = -C(1)
                            C(2) = 2.5*grid_length(i_grid)/mu(i_sn)/T/T * &
                                & (T**2-3*T+3-(T**2+3*T+3)*exp_ne2T)/(2*T-1+exp_ne2T)
                            
                            sum_C_Qn = C(1)*Qn(1) + C(2)*Qn(2)
                            
                            flux_ave_ang(i_grid,i_sn) = (abs(mu(i_sn))/grid_length(i_grid)*( A*flux_l_ang(i_grid+1,i_sn)-sum_C_Qn ) &
                                & + q_ave ) / (B*abs(mu(i_sn))/grid_length(i_grid)+xsec_t(i_grid))
                            flux_l_ang(i_grid,i_sn) = (1-A)*flux_l_ang(i_grid+1,i_sn) + B*flux_ave_ang(i_grid,i_sn) + sum_C_Qn
                            
                        end do
                    end if
                end do
                
                !error_in = (flux_ave_ang-flux_ave_ang_old)/flux_ave_ang
                !if(maxval(error_in)<iter_error(2)) exit
            end do inner
            
            !do i_grid = 1,scale
            !    do i_sn = 1,sn_order
            !        flux_ave_ang_highorder(1,i_grid,i_sn) = 
            
            flux_ave = 0.; flux_ave_highorder = 0.
            do i_sn = 1,sn_order
                flux_ave(:) = flux_ave(:) + flux_ave_ang(:,i_sn)*w(i_sn)
                flux_ave_highorder(1,:) = flux_ave_highorder(1,:) + flux_ave_ang_highorder(1,:,i_sn)
                flux_ave_highorder(2,:) = flux_ave_highorder(2,:) + flux_ave_ang_highorder(2,:,i_sn)
            end do
            error_flux = abs(flux_ave-flux_ave_old)/flux_ave
            
            if(TRIM(problem_type)=='EIGEN') then
                norm = 0.
                do i = 1,scale
                    norm(1) = norm(1) + (flux_ave(i)/k_old*0.5*xsec_f_nu(i))**2
                    norm(2) = norm(2) + (flux_ave_old(i)/k_old*0.5*xsec_f_nu(i))**2
                end do
                norm = SQRT(norm)
                k_new = k_old* norm(1)/norm(2)
                error_k = ABS((k_new-k_old)/k_new)
                k_old = k_new
            end if
            
            if(TRIM(problem_type)=='EIGEN') then
                print*, iter_out, iter_sweep, k_new, error_k, maxval(error_flux)
            else
                print*, iter_out, maxval(error_flux)
            end if
            
            flux_ave_old = flux_ave
            if(iter_out>=iter_limit(2)) exit
            if(maxval(error_flux)<iter_error(2)) exit
        end do outer
    end subroutine solver_exec_CMNM
end module