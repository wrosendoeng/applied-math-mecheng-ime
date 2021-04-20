module fourierseries
    use fgsl
    
    implicit none
    contains

    subroutine fouriertransformation(x,fft,m,n)
        integer(fgsl_int) :: i, k, n, m
        real(fgsl_double) :: bm(m), sin_series(m,n)
        real(fgsl_double), intent(in) :: x(n)
        real(fgsl_double), intent(out) :: fft(n)

        do k = 1, m
            bm(k) = -2.0_fgsl_double*(cos(k*m_pi/6.0) + cos(k*m_pi*5.0/6.0) + cos(k*m_pi/2.0))/(k*m_pi) &
            & + 12.0_fgsl_double*(sin(k*m_pi/6.0) + sin(k*m_pi*5.0/6.0) - sin(k*m_pi/2.0))/(k*m_pi)**(2)
            do i = 1, n
                sin_series(k,i) = sin(k*m_pi*x(i)/abs(x(1)))
            end do
        end do 
    
        fft = matmul(bm,sin_series)

    end subroutine 
end module fourierseries