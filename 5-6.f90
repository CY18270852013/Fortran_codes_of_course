program main
        integer :: counter, s
        do counter = 1, 99, 2
                s = s + counter
        end do
        write(*, *)"The result is:", s
stop
end
