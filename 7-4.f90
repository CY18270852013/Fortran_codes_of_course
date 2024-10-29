program main
        implicit none
        integer :: array(10), i = 1, s = 0
        do i = 1, 10
                array(i) = 2 * i
                s = s + array(i)
        end do
        write(*, *)s / 10
stop
end
