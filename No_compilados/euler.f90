program euler_basico
 implicit none

integer, parameter :: doble_precision = selected_real_kind(15)

real(doble_precision) :: x, y, x_final, h, pendiente, y_exacta, error
integer :: i, N 

x = 0.0_doble_precision
y = 1.0_doble_precision
x_final = 3.0_doble_precision
y_exacta = exp(x)
error = abs(y_exacta - y)

N = 100000

h = (x_final - x) / real(N, doble_precision)

open(unit=10, file='datos_euler.txt', status='replace')
write(10, *) x, y, y_exacta, error

do i = 1, N
    pendiente = y
y = y + h * pendiente
x = x + h
y_exacta = exp(x)
error = abs(y_exacta - y)
write(10, *) x, y, y_exacta, error

end do

close(10)

print *, "Codigito listo, en datos_euler.txt"

end program euler_basico