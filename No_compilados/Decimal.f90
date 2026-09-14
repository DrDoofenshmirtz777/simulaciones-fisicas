program propagacion_error
    implicit none
    
   
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp) :: x 

    x = 0.1_dp

   
    print *, "Salida habitual: ", x

   
    print '(A, F35.30)', "Salida con 30 cifras significativas: ", x

end program propagacion_error