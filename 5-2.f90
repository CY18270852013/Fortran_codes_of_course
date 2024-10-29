program main
        integer :: day = 0
        write(*, *)"Please input one number of a week, I can tell you the show of that day, for example 1 refers to Monday"
        read(*, *)day
        if(day == 1 .or. day == 4) then
                write(*, *)"The show is: News."
        else if(day == 2 .or. day == 5)then
                write(*, *)"The show is: Teleplay."
        else if(day == 3 .or. day == 6)then
                write(*, *)"The show is: Animated carton."
        else if(day == 7)then
                write(*, *)"The show is: Movies."
        else
                write(*, *)"Please input a right number!"
        end if
stop 
end
