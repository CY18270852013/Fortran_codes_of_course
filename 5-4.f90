program main
        implicit none
        integer :: days, year
        write(*, *)"Please input the number of a year:"
        read(*, *)year
        if((mod(year,4) == 0 .and. mod(year,100) /= 0) .or. mod(year,400) == 0)then
                days = 366
        else
                days = 365
        end if
        write(*, *)"This year has", days, "days."
stop
end
