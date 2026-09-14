program sumatoria
implicit none

integer, parameter :: dp = Selected_Real_kind(15,307)
real(dp) :: s1, s2, s2_1, s2_2, s3
integer :: n, i

s1 = 0.0_dp
s2 = 0.0_dp
s3 = 0.0_dp
n=0
i=0

print*, "Cuantos terminos deseas sumar?"
read*, n

do i = 1, 2 * n

    s1 = s1 + (((-1.0_dp)**i) * i) / (i + 1.0_dp) 
    
end do

do i = 1, n

    s2_1 = s2_1 - ((2.0_dp * i) - 1.0_dp) / (2.0_dp * i)

end do

do i = 1, n

    s2_2 = s2_2 + (2.0_dp * i) / ((2.0_dp * i) + 1.0_dp)

end do

s2 = s2_1 + s2_2

do i= 1, n

    s3 = s3 + (1.0_dp) / ((2.0_dp * i) * ((2.0_dp * i) + 1))

end do

print *, "S1      |     S2    |      S3     "

print*, s1 , " | ", s2, " | ", s3

end program sumatoria


