program punto_flotante_Parte_A
implicit none

integer,parameter :: dp = Selected_real_kind(15,307)
real(dp) :: x, valor_anterior_x, valor_nuevo_x
integer :: iteracion

x = 1
iteracion = 1
valor_anterior_x = x
valor_nuevo_x = x

print *, "Valor de x:", x
print *, "Numero de iteracion:", iteracion

do while (x > 0)
valor_anterior_x = x
print *, "Valor anterior de x:", valor_anterior_x

x = x / 2.0_dp
iteracion = iteracion + 1
valor_nuevo_x = x

 
print *, "Valor de nuevo de x:", x
print *, "Numero de iteracion:", iteracion

end do

end program punto_flotante_Parte_A
