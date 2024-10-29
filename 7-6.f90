PROGRAM main
        implicit none
        integer :: f(0:9)
        integer :: i
        f(0) = 0
        f(1) = 1
        do i = 2, 9
                f(i) = f(i - 1) + f(i - 2)
        end do
        
        !result:
        do i = 0, 9
                write(*, *)f(i)
        end do
stop
end
