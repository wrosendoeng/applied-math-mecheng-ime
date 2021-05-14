module exercise_boyce
    use fgsl
    implicit none
    contains 

    ! Exercise 18 from page 556 (UPDATED)
    ! Calling a subroutine to calculate piecewise function
    function piecewise_18pg556(x,k,n) result(y)
        integer(fgsl_int) :: i, k, n
        real(fgsl_double), intent(in) :: x(n)

        do i = 1, n
            if (x(i) > real(0.0_fgsl_double + 2.0_fgsl_double*k) .and. x(i) < real(1.0_fgsl_double + 2.0_fgsl_double*k)) then
                y(i) = x(i) - 2.0_fgsl_double*k
            else if (x(i) >= real(1.0_fgsl_double + 2.0_fgsl_double*k) .and. x(i) < real(2.0_fgsl_double + 2.0_fgsl_double*k)) then
                y(i) = 0.0
            end if
        end do

    end function

end module exercise_boyce