program bilinear
  implicit none

  !define dimension
  real, dimension(361,361)::sfcwind1
  real, dimension(361,361)::sfcwind2

  !read data from files
  open(unit=10,file='sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_210012.txt')
  read(10,*)
  do i=1,361
     read(10,*) (sfcwind1(i,j),j=1,361)  
  end do
  close(10)

  open(unit=11,file='sfcWind_Amon_FGOALS-s2_rcp85_r1i1p1_210012.txt')
  read(11,*)
  do i=1,361
     read(11,*) (sfcwind2(i,j),j=1,361)  
  end do
  close(11)

  !define position
  real :: lat,lon
  print*, 'please input latitude and longitude'
  read*,lat,lon  

  !bilinear interpolation
  i=floor(lat)+1
  j=floor(lon)+1
  dx=lat-i
  dy=lon-j
  sfcwind=sfcwind1(i,j)*(1-dx)*(1-dy)+sfcwind1(i,j+1)*dx*(1-dy)...
         +sfcwind1(i+1,j)*dy*(1-dx)+sfcwind1(i+1,j+1)*dx*dy

  print*, 'the sfc wind speed is:', sfcwind

  end program bilinear