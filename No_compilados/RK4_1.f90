program RK4
implicit none

integer, parameter :: dp = selected_real_kind(16)
real(dp) :: x_inicial, y_inicial, y_exacta, k_1, k_2, k_3, k_4, h, x, y, pendiente, y_euler, x_final


x_inicial = -3.0_dp
y_inicial = exp(-(x_inicial**2))
y_exacta = exp(-x_inicial ** 2.0_dp)
h = 0.5_dp

x_final = 3.0_dp
x = x_inicial
y = y_inicial
y_euler = y_inicial

open(unit=10, file='Datos_RK4.txt', status='replace')
write(10,*) x, y, y_euler, y_exacta

do while (x < x_final)

k_1 = -2.0_dp * x * y

k_2 = -2.0_dp * (x + (h / 2.0_dp)) * (y + (h / 2.0_dp) * k_1)

k_3 = -2.0_dp * (x + (h / 2.0_dp)) * (y + (h / 2.0_dp) * k_2)

k_4 = -2.0_dp * (x + h) * (y + (h * k_3))

pendiente = -2.0_dp * x * y_euler

y_euler = y_euler + h * pendiente

y = y + (h / 6.0_dp) * (k_1 + (2.0_dp * k_2) + (2.0_dp * k_3) + k_4)

x = x + h

y_exacta = exp(-x ** 2.0_dp)

write(10,*) x, y, y_euler, y_exacta

end do


print *, "los resultados estan en Datos_RK4.txt"

end program RK4