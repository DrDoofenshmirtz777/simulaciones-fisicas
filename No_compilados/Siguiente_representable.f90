program Siguiente_numero
    implicit none

    integer, parameter :: dp = selected_real_kind(15, 307)
    integer :: i
    real(dp) :: delta_x, x_siguiente, relacion

    real(dp), dimension(7) :: valores = [1.0_dp, 2.0_dp, 4.0_dp, 8.0_dp, 100.0_dp, 1000.0_dp, 106.0_dp]
    real(dp) :: x

    print *, "x | Siguiente numero representable | Delta x | Relacion"

    do i = 1, size(valores)
        x = valores(i)
        x_siguiente = nearest(x, 1.0_dp)
        delta_x = x_siguiente - x
        relacion = delta_x / abs(x)

        print *, x, " | ", x_siguiente, " | ", delta_x, " | ", relacion
    end do

end program Siguiente_numero