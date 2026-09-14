program epsilon_Aura
    implicit none

    integer, parameter :: dp = Selected_real_kind(15,307)
real(dp) :: epsilon, epsilon_anterior
integer :: iteracion

epsilon = 1.0_dp
iteracion = 0

print *, "Iter |      Epsilon actual      |     Epsilon anterior"


do while (1.0_dp + epsilon > 1.0_dp)
   

epsilon_anterior = epsilon
    epsilon = epsilon / 2.0_dp
iteracion = iteracion + 1


print *, iteracion, "   | ", epsilon, " | ", epsilon_anterior
end do

print *, iteracion, "   | ", epsilon, " | ", epsilon_anterior
end program