program RK45
    implicit none

    integer, parameter :: dp = selected_real_kind(16)
    real(dp) :: x, y, x_inicial, y_inicial, h, k_1, k_2, k_3, k_4, k_5, k_6, y_exacta, y_4, y_5, error_est, tol, h_nuevo, x_final
    integer :: N, i

    x_inicial = -3.0_dp
    y_inicial = exp(-(x_inicial**2))
    h = 0.5_dp
    N = 6
    tol = 0.0000000001_dp
    x_final = 3.0

    x = x_inicial
    y = y_inicial
    y_exacta = exp(-x ** 2)

    open(unit=10, file='datos_RK45.txt', status='replace')

    write(10, *) x, y, y_exacta

    do while (x < x_final)

        k_1 = -2.0_dp * x * y

        k_2 = -2.0_dp * (x + (1.0_dp/4.0_dp) * h) * (y + (h * ((1.0_dp/4.0_dp) * k_1))) 

        k_3 = -2.0_dp * (x + (3.0_dp / 8.0_dp) * h) * &
              (y + (h * ((3.0_dp / 32.0_dp) * k_1 + (9.0_dp / 32.0_dp) * k_2)))

        k_4 = -2.0_dp * (x + (12.0_dp / 13.0_dp) * h) * &
              (y + h * ((1932.0_dp / 2197.0_dp) * k_1 + &
              (-7200.0_dp / 2197.0_dp) * k_2 + (7296.0_dp / 2197.0_dp) * k_3))

        k_5 = -2.0_dp * (x + 1.0_dp * h) * &
              (y + h * ((439.0_dp / 216.0_dp) * k_1 + (-8.0_dp) * k_2 + &
              (3680.0_dp / 513.0_dp) * k_3 + (-845.0_dp / 4104.0_dp) * k_4)) 

        k_6 = -2.0_dp * (x + (1.0_dp / 2.0_dp) * h) * &
              (y + h * ((-8.0_dp / 27.0_dp) * k_1 + 2.0_dp * k_2 + &
              (-3544.0_dp / 2565.0_dp) * k_3 + (1859.0_dp / 4104.0_dp) * k_4 + &
              (-11.0_dp / 40.0_dp) * k_5))

        y_4 = y + h * (k_1 * (25.0_dp / 216.0_dp) + k_3 * (1408.0_dp / 2565.0_dp) + &
              k_4 * (2197.0_dp / 4104.0_dp) + k_5 * (-1.0_dp / 5.0_dp))

        y_5 = y + h * (k_1 * (16.0_dp / 135.0_dp) + k_3 * (6656.0_dp / 12825.0_dp) + &
              k_4 * (28561.0_dp / 56430.0_dp) + k_5 * (-9.0_dp / 50.0_dp) + &
              k_6 * (2.0_dp / 55.0_dp))

        error_est = abs(y_5 - y_4)

        h_nuevo = 0.9_dp * h * (tol / error_est + 1.0e-20_dp) ** 0.2_dp

        if (error_est <= tol) then
            x = x + h
            y = y_5
            y_exacta = exp(-x ** 2.0_dp)

            write(10, *) x, y, y_exacta
        end if

        h = h_nuevo

    end do

    close(10)

end program RK45