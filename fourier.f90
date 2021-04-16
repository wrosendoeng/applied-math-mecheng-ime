! PROGRAM CREATED TO APPLIED MATH COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR MARCIO VIOLANTE FERREIRA
! FIRST EXERCISE
! to compile code: gfortran -o result fourier.f90 parameters.f90

program fourierseries
    use parameters
    implicit none

    ! Defining a name for Fourier Analysis Exercise
    character(len=charlen) :: file_name = "result.txt"
    integer(sp) :: iter=1,nmax=100,new_unit
    real(dp) :: dxvec,x0=-6.0_wp,x1=6.0_wp
    real(dp), allocatable :: xvec(:), yvec(:)
    
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

    write(new_unit,"(f11.8,a6,f11.8)") (xvec(iter)," ",yvec(iter),iter=1,nmax)
    
    deallocate(xvec)
    deallocate(yvec)

    ! call execute_command_line() %% preparing a command-line for gnuplot
end program fourierseries

! Exercise 18 from page 556
! Calling a subroutine to calculate piecewise function
subroutine piecewise_18pg556(x,y,k,n)
    use parameters
    integer(sp) :: i, k
    real(dp), intent(in) :: x
    real(dp), intent(out) :: y(n)

    do i = 1, n
        if (x >= real(-2.0_wp + 4.0_wp*k) .and. x <= real(-1.0_wp + 4.0_wp*k)) then
            y(i) = 0.0
        else if (x > real(-1.0_wp + 4.0_wp*k) .and. x < real(1.0_wp + 4.0_wp*k)) then
            y(i) = x
        else if (x >= real(1.0_wp + 4.0_wp*k) .and. x < real(2.0_wp + 4.0_wp*k)) then
            y(i) = 0.0
        end if
    end do

end subroutine