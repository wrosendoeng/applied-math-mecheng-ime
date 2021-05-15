module fourierseries
    use fgsl  
    implicit none
    contains

    function fouriertransformation(x, t, alpha, bar_length, m, n) result(fft)
        integer(fgsl_int) :: i, k, n, m
        real(fgsl_double) :: cm(m), sin_fun(m,n), exp_tim(m,n)
        real(fgsl_double), intent(in) :: x(n), t(n), alpha, bar_length
        real(fgsl_double)             :: fft(n)

        do k = 1, m
            cm(k) = 2.0_fgsl_double*bar_length*(2*sin(k*m_pi/2.0) - (k*m_pi)*cos(k*m_pi/2.0))/(k*m_pi)**2
            do i = 1, n
                sin_fun(k,i) = sin(k*m_pi*x(i)/bar_length)
		exp_tim(k,i) = 1.0_fgsl_double/exp(t(i)*(alpha*k*m_pi/bar_length)**2)
            end do
        end do 
    
        fft = matmul(cm,sin_fun*exp_tim)

    end function  
end module fourierseries
