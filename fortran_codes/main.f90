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
    use functions
    use fourierseries

    implicit none

    ! Defining a name for Fourier Analysis Exercise
    character(kind=fgsl_char,len=6) :: file_name = "result"
    character(kind=fgsl_char,len=6) :: unit1,arg1	
    integer(fgsl_int) :: iter=1,nmax=1000,mmax,new_unit,period
    real(fgsl_double) :: dxvec,x0=-6.0_fgsl_double,x1=6.0_fgsl_double
    real(fgsl_double), allocatable :: xvec(:), yvec(:), fft_res(:)
    
    ! Command-line input of how many iterations has to be done in Fourier's Series
    call get_command_argument(1,arg1)
    read(arg1,*) mmax ! Transforming string into an integer

    write(unit1,'(I6.6)') mmax ! Creating a new string with 6 digits to store results
    open(newunit=new_unit,file=file_name//"_"//trim(unit1)//".txt",status="replace",action="write")
    
    allocate(xvec(nmax))
    allocate(yvec(nmax))
    allocate(fft_res(nmax))

    ! Creating a linear spaced vector x
    dxvec = (x1-x0)/real(nmax-1,fgsl_double)
    do iter = 1, nmax
        xvec(iter) = x0 + (iter-1)*dxvec
    end do 

    ! Calculating f(x)
    do period = -1,1
        call piecewise_18pg556(xvec,yvec,period,nmax)
    end do
    
    ! Fourier Series Approximation
    call fouriertransformation(xvec,fft_res,mmax,nmax) 

    ! Creating a text file with results of f(x) and Fourier Series approximation
    ! write(new_unit,'(a11,a6,a11,a6,a11)') &
    ! & "x"," ","f(x)"," ","fft(x)"    
    write(new_unit,'(f11.8,a6,f11.8,a6,f11.8)') &
    & (xvec(iter)," ",yvec(iter)," ",fft_res(iter),iter=1,nmax)
    
    deallocate(xvec)
    deallocate(yvec)
    deallocate(fft_res)

    ! Plotting chart with interface between Gnuplot and Fortran
    call execute_command_line("gnuplot -c plot.plt " &
    & //file_name//"_"//trim(unit1)//".txt "//arg1) ! preparing a command-line for gnuplot
end program main