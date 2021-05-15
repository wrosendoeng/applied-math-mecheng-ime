! PROGRAM CREATED TO APPLIED MATH COURSE OF IME`S POST-GRADUATION ENGINEERING COURSE
! WRITTEN BY WALLACE RAMOS ROSENDO DA SILVA
! PROFESSOR MARCIO VIOLANTE FERREIRA
! EXERCISE 18 FROM PAGE 556 - BOYCE & DIPRIMA'S BOOK
! COMPILING CODE: 
! Created a makefile to optimize compiling process
! Enter in Terminal and write:
! make ; ./ex18.exe argument1 ==> argument1 = number of repetitions for this exercise

program main
    use fgsl
    use exercise_boyce
    use fourierseries

    implicit none

    ! Defining a name for Fourier Analysis Exercise
    character(kind=fgsl_char,len=6) :: file_name = "result"
    character(kind=fgsl_char,len=6) :: unit1,arg1	
    integer(fgsl_int) :: iter=1,nmax=1000,mmax,new_unit1,period
    real(fgsl_double), parameter :: bar_length = 2.0e3, end_time = 10.0_fgsl_double ! Length in meters
    real(fgsl_double) :: dxvec, dtvec, start, finish, thermal_diff = 13.5865297
    real(fgsl_double), allocatable :: xvec(:), tvec(:), yvec(:), fft_res(:)
    
    ! Command-line input of how many iterations has to be done in Fourier's Series
    call get_command_argument(1,arg1)
    call cpu_time(start)
    read(arg1,*) mmax ! Transforming string into an integer

    write(unit1,'(I6.6)') mmax ! Creating a new string with 6 digits to store results
    open(newunit=new_unit1,file=file_name//"_"//trim(arg1)//".txt",status="replace",action="write")
    
    allocate(xvec(nmax))
    allocate(tvec(nmax))
    allocate(yvec(nmax))
    allocate(fft_res(nmax))

    ! Creating a linear spaced vector x
    dxvec = (bar_length - 0.0)/real(nmax-1,fgsl_double)
    dtvec = (end_time - 0.0)/real(nmax-1,fgsl_double)
    do iter = 1, nmax
        xvec(iter) = 0.0_fgsl_double + (iter-1)*dxvec
        tvec(iter) = 0.0_fgsl_double + (iter-1)*dtvec
    end do 

    ! Calculating f(x)
    do period = 0, 2
        yvec = piecewise_18pg556(xvec,period,nmax)
    end do
    
    ! Fourier Series Approximation
    fft_res = fouriertransformation(xvec,tvec, thermal_diff, bar_length, mmax,nmax) 

    ! Creating a text file with results of f(x) and Fourier Series approximation
    ! write(new_unit,'(a11,a6,a11,a6,a11)') &
    ! & "x"," ","f(x)"," ","fft(x)"    
    write(new_unit1,'(f11.8,a10,f11.8,a10,f11.8)') &
    & (xvec(iter)," ",tvec(iter)," ",fft_res(iter),iter=1,nmax)
    
    deallocate(xvec)
    deallocate(yvec)
    deallocate(fft_res)

    ! Plotting chart with interface between Gnuplot and Fortran
    call execute_command_line("gnuplot -c animation.plt " &
    & //file_name//"_"//trim(arg1)//".txt") ! preparing a command-line for gnuplot

    call cpu_time(finish)
    print *, "CPU time spent in code with "//trim(arg1)//" parcels: ", finish-start, " seconds."
end program main
