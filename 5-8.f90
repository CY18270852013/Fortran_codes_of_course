program main
        implicit none
        integer(kind = 4) :: counter = 1
        real(kind = 4) :: divisor = 1, sum_result = 0
        do counter = 1, 10, 1
                divisor = divisor / counter
                sum_result = sum_result + divisor
        end do
        write(*, *)"The rusult is:", sum_result
stop
end

