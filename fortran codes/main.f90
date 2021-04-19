! PROGRAM CREATED TO APPLIED MATH COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR MARCIO VIOLANTE FERREIRA
! FIRST EXERCISE
! to compile code: gfortran -o result fourier.f90 parameters.f90

program main
    use fgsl
    use functions
    use fourierseries
    use newtoncotes
    
    implicit none

    ! Defining a name for Fourier Analysis Exercise
    character(kind=fgsl_char,len=*) :: file_name = "result.txt"
    integer(fgsl_int) :: iter=1,nmax=100,new_unit
    real(fgsl_double) :: dxvec,x0=-6.0_fgsl_double,x1=6.0_fgsl_double, itrap, ertrp, itsim, ersim
    real(fgsl_double), allocatable :: xvec(:), yvec(:)
    
    open(newunit=new_unit,file=file_name,status="replace",action="write")
    allocate(xvec(nmax))
    allocate(yvec(nmax))

    ! Size of spacing among elements of vector x
    dxvec = (x1-x0)/real(nmax-1,dp)

    do iter = 1, nmax
        xvec(iter) = x0 + (iter-1)*dxvec
    end do 

    ! Creating vector x and calculating piecewise_18pg556 function
    print "(a4,a11,a4)","x(n)"," ","y(n)"
    do iter = 1, nmax
        if (xvec(iter) < -2.0_wp) then
            call piecewise_18pg556(xvec(iter),yvec,-1,nmax)
        else if (xvec(iter) >= -2.0_wp .and. xvec(iter) < 2.0_wp) then
            call piecewise_18pg556(xvec(iter),yvec,0,nmax)
        else
            call piecewise_18pg556(xvec(iter),yvec,1,nmax)
        end if
        print "(f11.8,a3,f11.8)",xvec(iter)," ",yvec(iter)
    end do
    
    ! Integrating piecewise function and calculating error of each numerical-integration method
    print "(a19)", " "
    call trapzrep(x,y,h,n,itrap,ertrp)
    call simpson3rep(x,y,h,n,itsim,ersim)
    print "(a20,f15.22,a15,f15.22)", "Trapezoidal's rule: ",itrap,"maximum error: ",ertrp
    print "(a20,f15.22,a15,f15.22)", "Simpson's rule: ",itsim,"maximum error: ",ersim

    write(new_unit,"(f11.8,a6,f11.8)") (xvec(iter)," ",yvec(iter),iter=1,nmax)
    
    deallocate(xvec)
    deallocate(yvec)

    ! call execute_command_line() %% preparing a command-line for gnuplot
end program main