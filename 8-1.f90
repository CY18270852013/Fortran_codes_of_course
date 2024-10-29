program main
    implicit none
    real, external :: func
    real :: x
    x = 0.5
    write(*, *)"When x = 0.5, the result is:", func(x)
    x = sqrt(15.0)
    write(*, *)"When x = sqrt(15), the result is:", func(x)
    x = sin(0.3)
    write(*, *)"When x = sin(0.3), the result is:", func(x)

end program main

real function func(x)
    implicit none
    real, intent(in) :: x
    real :: result
    if(x < 0)then
        result = 1.0 + sqrt(1.0 + x**2)
    elseif(x == 0)then
        result = 0.0
    else
        result = 1.0 - sqrt(1.0 + x**2)
    end if
    func = result
    return
end