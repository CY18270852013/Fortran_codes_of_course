program main
        implicit none
        integer :: i = 1, strlen
        character(len = 100) string
        write(*, *)"Please input a string that contains Spaces:"
        read(*, "(A100)")string
        strlen = len_trim(string)

        do i = 1, strlen
                if (string(i: i) == " ")string(i: i) = ""
        end do

        write(*, *)string
stop
end
