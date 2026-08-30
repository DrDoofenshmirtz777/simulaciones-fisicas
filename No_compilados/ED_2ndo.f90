program segundo_orden
    implicit none

    integer, parameter :: dp = selected_real_kind(15)

    real(dp) :: x_euler, aceleracion_euler, h, velocidad_euler, &
     x_inicial, velocidad_inicial, tiempo_inicial, tiempo, k, &
      tiempo_final, &
     k_1_x, k_2_x, k_3_x, k_4_x, k_1_v, k_2_v, k_3_v, k_4_v, x_RK4, &
     velocidad_RK4, aceleracion_RK4, error_abs, error_rel, &
     x_exacta, error_real_RK4, error_real_euler, &
     error_RK45, tolerancia_RK45, h_nuevo_RK45, h_RK45, x_exacta_RK45 &
     , error_relativo_RK4, error_relativo_euler, error_real_RK45, &
     error_relativo_RK45    

     real(dp), dimension(2) :: M, k1, k2, k3, k4, k5, k6, M_temp, &
     M_4, M_5


     
      
    tiempo_inicial = 0.0_dp
    tiempo_final = 300.0_dp
    x_inicial = 1.0_dp
    velocidad_inicial = 0.0_dp
    k = 200.0_dp
    tiempo = tiempo_inicial
    tolerancia_RK45 = 0.0000000001_dp
    
    h = 0.001_dp
    h_RK45 = h
    error_abs = 0.0_dp
    error_rel = 0.0_dp
    error_real_euler = 0.0_dp
    error_real_RK4 = 0.0_dp
    error_relativo_euler = 0.0_dp
    error_relativo_RK4 = 0.0_dp
    error_relativo_RK45 = 0.0_dp

    x_exacta = 0.0_dp
    x_exacta_RK45 = 0.0_dp
    x_euler = x_inicial
    velocidad_euler = velocidad_inicial
    aceleracion_euler = -k * x_euler 

    x_RK4 = x_inicial
    velocidad_RK4 = velocidad_inicial
    aceleracion_RK4 = -k * x_RK4

    M(1) = 1.0_dp
    M(2) = 0.0_dp

    open(unit=10,file="Resortito.txt",status='replace')
write(10,*) tiempo, x_euler, x_RK4, error_abs, &
error_relativo_euler, error_relativo_RK4, &
x_exacta, error_real_euler, error_real_RK4, &
velocidad_euler, velocidad_RK4


open(unit=11,file="Resortito_RK45.txt", status='replace')
write(11,*) tiempo, M, x_exacta_RK45, error_relativo_RK45

    do while (tiempo < tiempo_final)

x_exacta = cos(sqrt(k) * tiempo)

        k_1_x = velocidad_RK4
        k_1_v = aceleracion_RK4
        
        k_2_x = velocidad_RK4 + ((h / 2.0_dp) * k_1_v)
        k_2_v = -k * (x_RK4 + (h / 2.0_dp) * k_1_x)

        k_3_x = velocidad_RK4 + ((h / 2.0_dp) * k_2_v)
        k_3_v = -k * (x_RK4 + ((h / 2.0_dp) * k_2_x))

        k_4_x = velocidad_RK4 + (h * k_3_v)
        k_4_v = -k * (x_RK4 + (h * k_3_x))

        x_RK4 = x_RK4 + (h / 6.0_dp) * (k_1_x + 2.0_dp * k_2_x + &
        2.0_dp * k_3_x + k_4_x)

        velocidad_RK4 = velocidad_RK4 + (h / 6.0_dp) * (k_1_v + &
        2.0_dp * k_2_v + 2.0_dp * k_3_v + k_4_v)

        aceleracion_RK4 = -k * x_RK4

        aceleracion_euler = -k * x_euler

        x_euler = x_euler + (h * velocidad_euler)

        velocidad_euler = velocidad_euler + (h * aceleracion_euler)

        error_abs = abs(x_RK4 - x_euler)

        error_real_euler = abs(x_exacta - x_euler)
        error_real_RK4 = abs(x_exacta - x_RK4)

        if (abs(x_exacta) > (1.0d-5)) then
          error_relativo_RK4 = error_real_RK4 / abs(x_exacta)
          error_relativo_euler = error_real_euler / abs(x_exacta)
        else
            error_relativo_RK4 = 0.0_dp
            error_relativo_euler = 0.0_dp
        end if


   tiempo = tiempo + h

        write(10,*) tiempo, x_euler, x_RK4, error_abs, &
        error_relativo_euler, error_relativo_RK4, &
         x_exacta, error_real_euler, error_real_RK4, &
         velocidad_euler, velocidad_RK4

    end do

    tiempo = tiempo_inicial
    
    do while(tiempo_final > tiempo)
        

k1(1) = M(2)
k1(2) = -k * M(1)

M_temp = M + h_RK45 * (1.0_dp / 4.0_dp) * k1
k2(1) = M_temp(2)
k2(2) = -k * M_temp(1)

M_temp = M + ((3.0_dp / 32.0_dp) * h_RK45 * k1) + & 
((9.0_dp / 32.0_dp)) * h_RK45 * k2
k3(1) = M_temp(2)
k3(2) = -k * M_temp(1)

M_temp = M + & 
((1932.0_dp / 2197.0_dp) * h_RK45 * k1) - &
((7200.0_dp / 2197.0_dp) * h_RK45 * k2) + ((7296.0_dp &
/ 2197.0_dp) * h_RK45 * k3)

k4(1) = M_temp(2)
k4(2) = -k * M_temp(1)

M_temp = M + ((439.0_dp / 216.0_dp) * h_RK45 * k1) &
- 8.0_dp * h_RK45 * k2 + &
((3680.0_dp / 513.0_dp) * h_RK45 * k3) &
- ((845.0_dp / 4104.0_dp) * h_RK45 * k4)

k5(1) = M_temp(2)
k5(2) = -k * M_temp(1)

M_temp = M - &
((8.0_dp / 27.0_dp) * h_RK45 * k1) + &
2.0_dp * h_RK45 * k2 - &
((3544.0_dp / 2565.0_dp) * h_RK45 * k3) &
+ ((1859.0_dp / 4104.0_dp) * h_RK45 * k4) & 
- ((11.0_dp / 40.0_dp) * h_RK45 * k5)

k6(1) = M_temp(2)
k6(2) = -k * M_temp(1)  

M_4 = M & 
+ h_RK45 * ((25.0_dp / 216.0_dp) * k1 + &
(1408.0_dp / 2565.0_dp) * k3 + (2197.0_dp / &
4104.0_dp) * k4 - (1.0_dp / 5.0_dp) * k5)

M_5 = M + h_RK45 * & 
((16.0_dp / 135.0_dp) * k1 + &
(6656.0_dp / 12825.0_dp) * k3 + (28561.0_dp / &
56430.0_dp) * k4 - (9.0_dp / 50.0_dp) * k5 &
+ (2.0_dp / 55.0_dp) * k6)

error_RK45 = maxval(abs(M_5 - M_4))

h_nuevo_RK45 = 0.9_dp * h_RK45 * &
(tolerancia_RK45 / error_RK45 + 1.0e-20_dp) &
** 0.2_dp

if (error_RK45 <= tolerancia_RK45) then

M = M_5

x_exacta_RK45 = cos(sqrt(k) * tiempo)
error_real_RK45 = abs(x_exacta_RK45 - M(1))

if (abs(x_exacta_RK45) > 1.0d-5) then
    error_relativo_RK45 = error_real_RK45 / abs(x_exacta_RK45)
else
    error_relativo_RK45 = 0.0_dp
end if


write(11,*) tiempo, M, x_exacta_RK45, error_relativo_RK45

    tiempo = tiempo + h_RK45

    end if

    h_RK45 = h_nuevo_RK45

end do


    print *, "programa auroso"

end program segundo_orden