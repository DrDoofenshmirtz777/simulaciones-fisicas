program sumas_sucesivas
    implicit none
    
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp) :: suma, producto, x
    integer :: i

    x = 0.1_dp
    suma = 0.0_dp

    do i = 1, 10
        suma = suma + x
        print '(I2, A, F35.30)', i, "   ", suma
    end do

    producto = x * 10.0_dp

    print *, ""
    print '(A, F35.30)', "Suma final: ", suma
    print '(A, F35.30)', "Producto:   ", producto
    print *, ""

    if (suma == 1.0_dp) then
        print *, "Comparar suma con 1: Iguales"
    else
        print *, "Comparar suma con 1: Diferentes"
    end if

    if (producto == 1.0_dp) then
        print *, "Comparar producto con 1: Iguales"
    else
        print *, "Comparar producto con 1: Diferentes"
    end if

    if (suma == producto) then
        print *, "Comparar suma con producto: Iguales"
    else
        print *, "Comparar suma con producto: Diferentes"
    end if

end program sumas_sucesivas