program main
    implicit none
    integer(kind = 4) :: a = 0, b = 0, c = 0, d = 0
    interface
        function area(a, b, c, d)
            integer, intent(in) :: a, b, c
            integer, optional :: d
            integer :: area
        end function area
    end interface

    write(*, *)"Please input parameter:"
    read(*, *) a, b, c, d
    write(*, *)"The area is:", area(a, b, c, d)
end program main

integer function area(a, b, c, d)
    implicit none
    integer, intent(in) :: a, b, c
    integer, optional :: d
    integer :: result = 0

    if (present(d)) then
        result = a + b + c + d
    else
        result = a + b + c
    end if
    area = result
end function area
