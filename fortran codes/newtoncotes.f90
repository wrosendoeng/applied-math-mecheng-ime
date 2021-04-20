module newtoncotes
    use functions
    use fgsl

    implicit none 
    contains

    subroutine trapzrep(x,y,h,n,itrep,etrep)
        integer(fgsl_int) :: i, n
        real(fgsl_double) :: dxit,d2ydx2,diff
        real(fgsl_double), intent(in) :: h, x(n), y(n)
        real(fgsl_double), intent(out) :: itrep, etrep

        dxit = 0.0
        d2ydx2 = 0.0
        do i = 1, n 
            if (i == 1 .or. i == n) then
                dxit = dxit + y(i)
            else
                dxit = dxit + 2*y(i)
            end if
            diff = abs((y(i+2)-2*y(i+1)+y(i))/h**(2)) ! Forward-finite difference for 2nd-order derivative
            if (diff > d2ydx2) then
                d2ydx2 = diff
            end if
        end do
        itrep = h*dxit/2.0_fgsl_double
        etrep = -n*h**(3)*d2ydx2/1.2e1 

    end subroutine trapzrep

    subroutine simpson3rep(x,y,h,n,isrep,esrep)
        integer(fgsl_int) :: i, n
        real(fgsl_double) :: dxis,d4ydx4,diff
        real(fgsl_double), intent(in) :: h, x(n), y(n)
        real(fgsl_double), intent(out) :: isrep, esrep

        dxis = 0.0
        d4ydx4 = 0.0
        do i = 1, n 
            if (i == 1 .or. i == n) then 
                dxis = dxis + y(i)
            else if (mod(i,2) == 1) then ! For odd positions
                dxis = dxis + 4*y(i)
            else                         ! For even positions
                dxis = dxis + 2*y(i)
            end if
            diff = abs((y(i)-4*y(i+1)+6*y(i+2)-4*y(i+3)+y(i+4))/h**(4)) ! Forward-finite difference for 4nd-order derivative
            if (diff > d4ydx4) then
                d4ydx4 = diff
            end if
        end do
        isrep = h*dxis/3.0_fgsl_double
        esrep = -n*h**(5)*d4ydx4/1.8e2 
    end subroutine simpson3rep

end module newtoncotes