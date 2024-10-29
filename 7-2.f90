program main
        implicit none
        integer :: A(1:5,1:3)
        integer i
        integer j
        integer :: s = 0, p = 1

        do i = 1, 5
                do j = 1, 3
                        write(*, "('A(', I1, ',', I1, ')=')")i, j
                        read(*, *)A(i, j)
                end do
        end do

        do i = 1, 5
                do j = 1, 3
                        s = s + A(i, j)
                        p = p * A(i, j)
                end do
        end do

        write(*, *)"The sum is:", s
        write(*, *)"The product is:", p
stop
end

