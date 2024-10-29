c     fixed format demo
      program main
      character(len = 20)str1, str2
      str1 = "Have a good time"
      str2 = str1(8: 11)
      write(*, *)str1
      write(*, *)"That's good"
      write(*, *)"""Dept.Atmos.Sci."""
      write(*, *)"New string is:", str2
      end
