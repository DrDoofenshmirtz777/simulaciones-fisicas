program valores_reales
    implicit none

    integer, parameter :: dp = Selected_real_kind(15,307)

    real(dp) :: x

    print *, "Mayor numero finito representable:", huge(x)
    print *, "Menor numero normal positivo:", tiny(x)
    print *, "El epsilon de la maquina:", epsilon(x)

end program valores_reales