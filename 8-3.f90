program main
    implicit none
    integer(kind = 4) :: n = 0, m = 0
    integer, external :: greatest_common_divisor
    write(*, *)"Please input two numbers:"
    read(*, *)m, n
    write(*, *)"The greatest common divisor of m and n is:", greatest_common_divisor(m, n)
end program main

recursive integer function greatest_common_divisor(m, n) result(ans)
    implicit none
    integer, intent(in) :: m, n

    if(n == 0)then
        ans = m
        return
    end if
    ans = greatest_common_divisor(n, mod(m, n))
    return
end
