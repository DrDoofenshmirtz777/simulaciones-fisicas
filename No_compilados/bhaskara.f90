program bhaskara
    implicit none

    integer, parameter :: dp = Selected_Real_Kind(15,307)

    real(dp) :: a, b, x_1, x_2
    integer :: i

    print *, "Que coeficiente acompaña a tu termino cuadratico?"
    read *, a

   print *, "Que coeficiente acompaña a tu termino lineal?"
   read *, b


print *, "x_1                        |                x_2"

do i = 1, 17

x_1 = (2.0_dp * (10.0_dp ** (-i))) /&
 ((-b) - (sqrt((b ** 2) - (4.0_dp*a*10.0_dp**(-i)))))

x_2 = ((-b) - (sqrt((b ** 2) - (4.0_dp * a * 10.0_dp**(-i)))))

print *, x_1," | ", x_2

end do

end program bhaskara