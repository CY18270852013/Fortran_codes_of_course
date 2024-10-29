program main
        implicit none
        real(kind = 4) :: income, tax
        write(*, *)"Please input your income:"
        read(*, *)income
        if(income > 5000) then
                tax = 0.15
        else if(income <= 5000 .and. income >= 1000)then
                tax = 0.1
        else if(income > 0 .and. income < 1000) then
                tax = 0.03
        else
                write(*, *)"Go to make money!"
        end if
        write(*, *)"You need to pay taxes:",income * tax
        
stop
end
