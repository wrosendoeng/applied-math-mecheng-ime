module fourierseries
    use fgsl  
    implicit none
    contains

    function fouriertransformation(x,m,n) result(fft)
        integer(fgsl_int) :: i, k, n, m
        real(fgsl_double) :: cm(m), sin_exp(m,n)
        real(fgsl_double), intent(in) :: x(n), t(n)

        do k = 1, m
            cm(k) = 2.0_fgsl_double*bar_length/(k*m_pi)**2*(2*sin(k*m_pi/2.0) - (k*m_pi)*cos(k*m_pi*/2.0))
            do i = 1, n
                sin_exp(k,i) = sin(k*m_pi*x(i)/bar_length)*exp(-t(i)*(alpha*k*m_pi/bar_length)**2)
            end do
        end do 
    
        fft = matmul(bm,sin_exp)

    end function  
end module fourierseries