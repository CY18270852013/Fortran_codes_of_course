program main
        type :: distance
                real :: m, cm, inch
        end type distance

        type(distance) :: example

        write(*, *)"Please input the data of m:"
        read(*, *)example%m
        example%cm = 100 * example%m
        example%inch = example%m / 0.0254
        write(*, *)"Sir, if the data of m is ", example%m, ", then the &
                &data of cm is:", example%cm, "and the data of inch is:",&
                & example%inch
        end
