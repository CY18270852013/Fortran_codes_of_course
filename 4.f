c     fixed format demo
      program main
      real pi
      parameter(pi = 3.1415926)
      real radius, area
      write(*, *)"Please input the radius:"
      read(*, *)radius
      write(*, *)"The area of this circle is:", pi * radius * radius
      end
