program Pendulo_Doble_Elastico_Aura
    implicit none

    integer, parameter :: dp = selected_real_kind(15)

    !Vectores de estado para cada metodo
    real(dp), dimension(8) :: M_euler, M_RK4, M_RK45

    integer, parameter :: N = 8

    ! Vectores temporales y coeficientes k 
    real(dp), dimension(N) :: M_temp_1, M_temp_2, M_temp_3

    real(dp), dimension(N) :: k1, k2, k3, k4, k5, k6

    real(dp), dimension(N) :: M_4_RK45, M_5_RK45

    !Sistema Matricial Ax=b
    real(dp), dimension(4,4) :: A !Matriz de masas
    real(dp), dimension(4) :: b !Vector de fuerzass
    real(dp), dimension(4) :: Aura
    
    !Constantes
    real(dp) :: m1, m2, k_resorte_1, k_resorte_2, &
    long_inicial_1, long_inicial_2, g

    !Tiempesongos
    real(dp) :: t, h, t_max

    !RK45
    real(dp) :: h_RK45, t_RK45, tol_RK45, error_RK45, &
    h_nuevo_RK45

    !Declarando variables
    m1 = 6.0_dp
    m2 = 7.0_dp     
    k_resorte_1 = 25.0_dp
    k_resorte_2 = 15.0_dp
    long_inicial_1 = 1.0_dp
    long_inicial_2 = 2.0_dp
    g = 9.81_dp
    t = 0.0_dp
    t_max = 10.0_dp
    h = 0.0001_dp
    

    !Condiciones iniciales del vector de estado de Euler
    !El orden es el mismo que el del documento, elongacion1,teta1, &
    !elongacion2,teta2

    M_euler(1) = long_inicial_1 + 0.5_dp 
    M_euler(2) = 1.1693706_dp
    M_euler(3) = long_inicial_2 + 0.2_dp
    M_euler(4) = 0.785398_dp

    M_euler(5:8) = 0.0_dp

    M_RK4 = M_euler
    M_RK45 = M_euler

    

open(unit=10, file="Pendulo_doble_elastico.txt", status='replace')

    write(10, *) t, M_euler, M_RK4


    do while(t_max > t)

        !Metodo de euler
        call Matriz_Gigante(t, M_euler, k1)
        M_euler = M_euler + h * k1

        !RK4 god
        call Matriz_Gigante(t, M_RK4, k1)
        
        M_temp_1 = M_RK4 + (h / 2.0_dp) * k1
        call Matriz_Gigante(t + (h / 2.0_dp), M_temp_1, k2)
        
        M_temp_1 = M_RK4 + (h / 2.0_dp) * k2
        call Matriz_Gigante(t + (h / 2.0_dp), M_temp_1, k3)

        M_temp_1 = M_RK4 + h * k3
        call Matriz_Gigante(t + h, M_temp_1, k4)

        !Promedio y actualizacion del estado RK4
        M_RK4 = M_RK4 + (h / 6.0_dp) * (k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)

        
        !Terminando ciclo
        t = t + h
        
        write(10, *) t, M_euler, M_RK4

    end do

    close(10)


!RK45
    t_RK45 = 0.0_dp
    h_RK45 = h
    tol_RK45 = 1.0e-10_dp

open(unit=11, file="Pendulo_RK45.txt", status='replace')
   
do while (t_RK45 < t_max)
    write(11, *) t_RK45, M_RK45

    !6 coeficientes de RK45

    call Matriz_Gigante(t_RK45, M_RK45, k1)
        
        M_temp_1 = M_RK45 + h_RK45 * (1.0_dp / 4.0_dp) * k1
        call Matriz_Gigante(t_RK45, M_temp_1, k2)
        
        M_temp_1 = M_RK45 + ((3.0_dp / 32.0_dp) * h_RK45 * k1) + &
         ((9.0_dp / 32.0_dp) * h_RK45 * k2)
        call Matriz_Gigante(t_RK45, M_temp_1, k3)
        
        M_temp_1 = M_RK45 + ((1932.0_dp / 2197.0_dp) * h_RK45 * k1) - &
        ((7200.0_dp / 2197.0_dp) * h_RK45 * k2) + &
        ((7296.0_dp / 2197.0_dp) * h_RK45 * k3)
        call Matriz_Gigante(t_RK45, M_temp_1, k4)
        
        M_temp_1 = M_RK45 + ((439.0_dp / 216.0_dp) * h_RK45 * k1) - &
        (8.0_dp * h_RK45 * k2) + ((3680.0_dp / 513.0_dp) * h_RK45 * k3) &
        - ((845.0_dp / 4104.0_dp) * h_RK45 * k4)
        call Matriz_Gigante(t_RK45, M_temp_1, k5)
        
        M_temp_1 = M_RK45 - ((8.0_dp / 27.0_dp) * h_RK45 * k1) + &
         (2.0_dp * h_RK45 * k2) - ((3544.0_dp / 2565.0_dp) * h_RK45 * k3) &
          + ((1859.0_dp / 4104.0_dp) * h_RK45 * k4) - &
          ((11.0_dp / 40.0_dp) * h_RK45 * k5)
        call Matriz_Gigante(t_RK45, M_temp_1, k6)

        !Estado de orden 4 y 5
        M_4_RK45 = M_RK45 + h_RK45 * ((25.0_dp / 216.0_dp) * k1 + &
        (1408.0_dp / 2565.0_dp) * k3 + &
        (2197.0_dp / 4104.0_dp) * k4 - (1.0_dp / 5.0_dp) * k5)
       
       
        M_5_RK45 = M_RK45 + h_RK45 * ((16.0_dp / 135.0_dp) * k1 + &
        (6656.0_dp / 12825.0_dp) * k3 + (28561.0_dp / 56430.0_dp) * k4 &
        - (9.0_dp / 50.0_dp) * k5 + (2.0_dp / 55.0_dp) * k6)
        
        !Adaptacion de paso
        error_RK45 = maxval(abs(M_5_RK45 - M_4_RK45))
        
        h_nuevo_RK45 = 0.9_dp * h_RK45 * &
        (tol_RK45 / (error_RK45 + 1.0e-20_dp))**0.2_dp

        if (error_RK45 <= tol_RK45) then

            M_RK45 = M_5_RK45
            t_RK45 = t_RK45 + h_RK45
        end if

        h_RK45 = h_nuevo_RK45

    end do

    close(11)

    print *, "Elpepe"

Contains  
!Subrutina de la matriz gigante
    SUBROUTINE Matriz_Gigante(t_actual, Y, dy)
    implicit none
        
    !Conexion
    real(dp), intent(in) :: t_actual
    real(dp), dimension(8), intent(in) :: Y 
    real(dp), dimension(8), intent(out) :: dy
    !Gauss-JOrdan
    integer :: i, j , k
    real(dp) :: pivote, factor
    real(dp), dimension(4,5) :: Laura 



    !Variables simplificadas
    real(dp) ::R1, R2, Delta

    A = 0.0_dp
    b= 0.0_dp

    R1 = long_inicial_1 + Y(1)
    R2 = long_inicial_2 + Y(3)
    Delta = Y(2) - Y(4)

    !Empezando Gauss Jordan

    !Llenando la matriz A
    A(1,1) = m1 + m2
    A(1,2) = 0.0_dp
    A(1,3) = m2 * cos(Delta)
    A(1,4) = m2 * R2 * sin(Delta)

    b(1) = (m1 + m2) * R1 * Y(6) ** 2 &
    - k_resorte_1 * Y(1)  &
    + (m1 + m2) * g * cos(Y(2)) &
    - 2.0_dp * m2 * Y(7) * Y(8) * sin(Delta) & 
    + m2 * (R2) * Y(8) ** 2 * cos(Delta)

    A(2,1) = 0.0_dp
    A(2,2) = (m1 + m2) * R1
    A(2,3) = -m2 * sin(Delta)
    A(2,4) = m2 * R2 * cos(Delta)

    b(2) = -(m1 + m2) * (2.0_dp * Y(5) * Y(6) &
    + g * sin(Y(2))) - m2 * R2 * Y(8) ** 2 &
    * sin(Delta) - 2.0_dp * m2 * Y(7) * Y(8) &
    * cos(Delta)

    A(3,1) = m2 * cos(Delta) 
    A(3,2) = -m2 * R1 * sin(Delta)
    A(3,3) = m2
    A(3,4) = 0.0_dp
    
    b(3) = m2 * R2 * Y(8) ** 2 &
    + m2 * R1 * Y(6) ** 2 * cos(Delta) &
    + 2.0_dp * m2 * Y(5) * Y(6) * sin(Delta) &
    -k_resorte_2 * Y(3) + m2 * g * cos(y(4)) 

    A(4,1) = sin(Delta)
    A(4,2) = R1 * cos(Delta)
    A(4,3) = 0.0_dp
    A(4,4) = R2

    b(4) = -2.0_dp * Y(7) * Y(8) &
    -g * sin(Y(4)) + R1 * Y(6) ** 2 * sin(Delta) &
    - 2.0_dp * Y(5) * Y(6) * cos(Delta)

    !Resolviendo Ax=b con gauss-jrodan
    ![A / b ]
    do i = 1, 4
        do j = 1, 4
Laura(i,j) = A(i,j)
        end do
        Laura(i, 5) = b(i)
    end do

    !Eliminacion gauss-jordan
    do k = 1,4
        pivote = Laura(k, k)

        do j = 1, 5
            Laura(k, j) = Laura(k, j) / pivote
        end do

        do i = 1,4

        if (i /= k) then
            factor = Laura(i, k)

            do j= 1, 5
                Laura(i, j) = Laura(i, j) - factor * Laura(k, j)
            end do
        end if
    end do
end do
            
            do i = 1, 4
                Aura(i) = Laura (i, 5)
            end do

            !Terminando subrutina
            dy(1:4) = Y(5:8) !Derivada posicion = velocidad
            dy(5:8) = Aura   !Derivada velocidad = aceleracion
            end SUBROUTINE Matriz_Gigante


end program Pendulo_Doble_Elastico_Aura