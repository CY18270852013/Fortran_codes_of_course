program main
        implicit none
        real, parameter :: weight = 45.0 !answer
        real, parameter :: e = 0.001 !error
        real :: guess = 0.0 !guess value
        integer :: counter = 0

        do while(counter < 5 .and. abs(guess - weight) > e)
                write(*, *)"Weight:"
                read(*, *)guess
                counter = counter + 1
                if(abs(guess - weight) <= e)then
                        write(*, *)"You're right"
                        exit
                else
                        write(*, *)"Sorry, you are wrong, now you still have ", 5 - counter, " chances."
                end if
         end do

stop
end
