program main
    implicit none
    integer(kind = 4) :: n = 4
    integer, external :: summerize
    write(*, *)"The result of sum is:", summerize(n)
end program main

recursive integer function summerize(n) result(ans)
    implicit none
    integer, intent(in) :: n

    if(n > 100)then
        ans = 0
        return
    end if
    ans = n + summerize(n + 4)
    return
end