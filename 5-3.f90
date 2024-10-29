program main
        integer(kind = 4) :: age, yincome
        real(kind = 4)rtax
        write(*, *)"Please input your age:"
        read(*, *)age
        write(*, *)"Please input your income of this year:"
        read(*, *)yincome
        if(age < 50)then
                if(yincome > 5000)then
                        rtax = 0.15
                else if(yincome >= 1000)then
                        rtax = 0.1
                else
                        rtax = 0.03
                end if
         else
                 if(yincome > 5000)then
                         rtax = 0.1
                 else if(yincome >= 1000)then
                         rtax = 0.07
                 else
                         rtax = 0.05
                 end if
         end if
         write(*, *)"(Every year)Your tax is: ", yincome * rtax
stop
end
