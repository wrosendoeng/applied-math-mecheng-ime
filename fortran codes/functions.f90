module functions
    use fgsl
    implicit none
    contains 

    ! Exercise 18 from page 556
    ! Calling a subroutine to calculate piecewise function
    subroutine piecewise_18pg556(x,y,k,n)
        integer(fgsl_int) :: i, k, n
        real(fgsl_double), intent(in) :: x
        real(fgsl_double), intent(out) :: y(n)

        do i = 1, n
            if (x >= real(-2.0_fgsl_double + 4.0_fgsl_double*k) .and. x <= real(-1.0_fgsl_double + 4.0_fgsl_double*k)) then
                y(i) = 0.0
            else if (x > real(-1.0_fgsl_double + 4.0_fgsl_double*k) .and. x < real(1.0_fgsl_double + 4.0_fgsl_double*k)) then
                y(i) = x
            else if (x >= real(1.0_fgsl_double + 4.0_fgsl_double*k) .and. x < real(2.0_fgsl_double + 4.0_fgsl_double*k)) then
                y(i) = 0.0
        end if
    end do

    end subroutine

end module functions