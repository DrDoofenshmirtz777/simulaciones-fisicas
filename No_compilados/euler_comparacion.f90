program comparacion
    implicit none

    integer, parameter :: dp = selected_real_kind(16)
    real(dp) :: h, x, y, x_inicial, y_inicial, k
    
    integer :: i, N

    x_inicial = 0.0_dp
    y_inicial = 1.0_dp
    h = 0.5_dp
    N = 6
x = x_inicial
y = y_inicial
    do i = 1, N
k = -2.0_dp * x * y
y = y + h * k
x = x + h

    end do
    