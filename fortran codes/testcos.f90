program testcos
    use fgsl
    implicit none

    integer(4) :: i
    real(8) :: x(4) 
    real(8) :: c(4)

    x(1) = -0.5*m_pi
    do i = 1, 3
        x(i+1) = x(i) + 0.5*m_pi
        c(i) = cos(x(i))
    end do

    print *, c
end program testcos