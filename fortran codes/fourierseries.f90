module fourierseries
    use fgsl
    use newtoncotes, only: trapzrep ! or simpson3rep
    
    implicit none
    contains

    subroutine fouriercoefs(x,y,h,m,n,fft)
        integer(fgsl_int) :: i, k, n, m
        real(fgsl_double) :: a0(n), an(n), bn(n), elem_a0, elem_an, elem_bn, error0, error1, error2, L
        real(fgsl_double), intent(in) :: h, x(n), y(n)
        real(fgsl_double), intent(out) :: fft

        !a0 = 1/L * integral of f(x) from -L to L
        !am = 1/L * integral of f(x)*cos(m*m_pi*x/L) from -L to L
        !bm = 1/L * integral of f(x)*sin(m*m_pi*x/L) from -L to L
        L = abs(x(1))

        do k = 1, m
            do i = 1, n
                call trapzrep(x,y,h,n,elem_a0,error0)
                a0(k+1) = a0(k) + elem_a0
                call trapzrep(x,y*cos(k*m_pi*x/L),h,n,elem_an,error1)
                an(k+1) = an(k) + elem_an*cos(k*m_pi*x(i)/L)
                call trapzrep(x,y*sin(k*m_pi*x/L),h,n,elem_bn,error2)
                bn(k+1) = bn(k) + elem_bn*sin(k*m_pi*x(i)/L)
            end do
        end do
        fft = 0.5*a0(m) + an(m) + bn(m)
    end subroutine
end module fourierseries