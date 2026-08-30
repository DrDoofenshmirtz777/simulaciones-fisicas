program Conditional
    real :: Speed

    print *, "What is the maximum speed that reach your car?"
    read *, Speed

If (Speed < 200.0 .and. Speed >= 100.0) then
    print*, "You have a normal car"

   else if (Speed >= 200.0 .and. Speed < 300.0 ) then
        print *, "Wow, your car is realy fast"

     else if (Speed >= 300.0) then
        print *, "You have a Sport Car"

     else
        print *, "Your car is broken down"

     end if

    end program


     
