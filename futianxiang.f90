
program main !fill the data to 1*1 using double linear insert method
!author:221840303
!include three main parts:data_read;data_process;data_write
!subroutine data_read: read the data from input file,consisting of longitude,latitude,windspeed
!subroutine data_interp: to interpolate the data
!subtoutine data_write: write the interpolation data to relative file
!subtoutine lat_search: find the two latitudes that most close to the target points
!subtoutine lon_search: find the two longitudes that most close to the target points

	  interface
	    subroutine data_read(filename,lat_in,lon_in,v_in)
		character(len=*)::filename
	        real,allocatable::lat_in(:),lon_in(:) !latitude and longitude input
	        real,allocatable::v_in(:,:)   !windspeed input
	    end subroutine
	    subroutine data_interp(lat_in,lon_in,v_in,v_out)
	        real,allocatable::lat_in(:),lon_in(:) !latitude and longitude input
	        real,allocatable::v_in(:,:)   !windspeed input
		real v_out(360,180) !windspeed output
	    end subroutine
	  end interface

	real,allocatable::lat_in(:),lon_in(:) !latitude and longitude input
	real,allocatable::v_in(:,:)   !windspeed input
	real::v_out(360,180)  !windspeed output

!GFDL-ESM2M
	allocate(lat_in(90))
	allocate(lon_in(144))
	allocate(v_in(144,90))
	call data_read("sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_210012.txt",lat_in,lon_in,v_in)
	call data_interp(lat_in,lon_in,v_in,v_out)
	call data_write("out1.txt",v_out)
	deallocate(lat_in)
	deallocate(lon_in)
	deallocate(v_in)


!FGOALS
	allocate(lat_in(108))
	allocate(lon_in(128))
	allocate(v_in(128,108))
	call data_read("sfcWind_Amon_FGOALS-s2_rcp85_r1i1p1_210012.txt",lat_in,lon_in,v_in)
	call data_interp(lat_in,lon_in,v_in,v_out)
	call data_write("out2.txt",v_out)
	deallocate(lat_in)
	deallocate(lon_in)
	deallocate(v_in)
end


subroutine data_read(filename_in,lat_in,lon_in,v_in)
	implicit none
	character(len=*)::filename_in
	character(27) buffer !for the first line of the file
	integer::error,i,j
	real,allocatable::lat_in(:),lon_in(:) 
	real,allocatable::v_in(:,:)
	real::a,b,c
	integer::len_lat,len_lon !the numbers of known latitudes and longitudes
	len_lat = size(lat_in) 
	len_lon = size(lon_in)	
   
     open(unit=10,file = filename_in,form = "formatted")
	read(10,"(A100)",iostat = error) buffer !skip the first line
	do j = 1,len_lat
	    do i =1,len_lon 
	        read(10,"(F6.2,1X,F7.2,1X,F8.4)",iostat = error)  lat_in(j),lon_in(i),v_in(i,j)
                !windspeed =v(i,j) for longitude = lon_in(i),latitude = lat_in(j) 
		    if(error .ne. 0) exit
	    enddo
	enddo
   
end subroutine data_read

subroutine data_interp(lat_in,lon_in,v_in,v_out)
	implicit none
	  interface    
	    subroutine lat_search(targetpnt,lat_in,lat1_index,lat2_index) 
		real::targetpnt
		integer::lat1_index,lat2_index !the location of  the two nearest lat
		real,allocatable::lat_in(:)
	    end subroutine lat_search
	    subroutine lon_search(targetpnt,lon_in,lon1_index,lon2_index) 
		real::targetpnt
		integer::lon1_index,lon2_index !the location of  the two nearest lon
		real,allocatable::lon_in(:)
	    end subroutine lon_search
	  end interface

	integer i,j
	integer lat1_index,lat2_index,lon1_index,lon2_index,len_lat
	real,allocatable::lat_in(:),lon_in(:)
	real::lat_out(180)=(/(i+0.5,i=-90,89)/) !lat_out = (-89.5,-88.5,...,89.5)
	real::lon_out(360)=(/(i+0.5,i=0,359)/)  !lon_out = (0.5,1.5,...,359.5)
	real::v_out(360,180)
	real,allocatable::v_in(:,:)
	real::x1,x2,x,y1,y2,y! interp formula variables
	real::FQ11,FQ21,FQ12,FQ22,FP1,FP2 ! interp formula temp variables

	len_lat = size(lat_in)

	do j = 2,179  !for points of latitude -88.5 to 88.5
		do i = 1,360
			call lat_search(lat_out(j),lat_in,lat1_index,lat2_index)
			call lon_search(lon_out(i),lon_in,lon1_index,lon2_index)
			x1 = lat_in(lat1_index)
			x2 = lat_in(lat2_index)
			x = lat_out(j)
			y1 = lon_in(lon1_index)
			y2 = lon_in(lon2_index)
			y =lon_out(i)

			if (y1 .gt. y2) then !for points near 0 or 360
				if (y1 .gt. 180) y1 = y1 - 360
				if (y .gt. 180) y = y -360
			end if

			fq11 = (x2-x)/(x2-x1) *v_in(lon1_index,lat1_index)
			fq21 = (x-x1)/(x2-x1) *v_in(lon1_index,lat2_index)
			fq12 = (x2-x)/(x2-x1) *v_in(lon2_index,lat1_index)
			fq22 = (x-x1)/(x2-x1) *v_in(lon2_index,lat2_index)
			fp1 = (y2-y)/(y2-y1)*(fq11 + fq21)
			fp2 = (y-y1)/(y2-y1)*(fq12 + fq22)
			v_out(i,j) = fp1 + fp2

		enddo	
	enddo

	do i = 1,360 !for points of latitude -89.5
		call lon_search(lon_out(i),lon_in,lon1_index,lon2_index)
		y1 = lon_in(lon1_index)
		y2 = lon_in(lon2_index)
		y =lon_out(i)

		if (y1 .gt. y2) then
			if (y1 .gt. 180) y1 = y1 - 360
			if (y .gt. 180) y = y -360
		end if
		
		fp1 = (y2-y)/(y2-y1)*v_in(lon1_index,1)
		fp2 =(y-y1)/(y2-y1)*v_in(lon2_index,1)
		v_out(i,1) = fp1 + fp2
	enddo

	do i = 1,360 !for points of latitude 89.5
		call lon_search(lon_out(i),lon_in,lon1_index,lon2_index)
		y1 = lon_in(lon1_index)
		y2 = lon_in(lon2_index)
		y =lon_out(i)

		if (y1 .gt. y2) then
			if (y1 .gt. 180) y1 = y1 - 360
			if (y .gt. 180) y = y -360
		end if
		
		fp1 = (y2-y)/(y2-y1)*v_in(lon1_index,len_lat)
		fp2 =(y-y1)/(y2-y1)*v_in(lon2_index,len_lat)
		v_out(i,180) = fp1 + fp2
	enddo
end subroutine data_interp

subroutine lat_search(targetpnt,lat_in,lat1_index,lat2_index) !find the nearest latitude
	implicit none
	real::targetpnt
	integer::lat1_index,lat2_index,i,j
	integer::len_lat
	real,allocatable::lat_in(:),lat_distance(:)

	len_lat = size(lat_in)
	lat_distance = lat_in - targetpnt !the distance between target point and known points
!find the index of the nearest latitudes, one of which is higher ,the other is lower
	do i = 1,len_lat-1
	    if (lat_distance(i) * lat_distance(i+1) .lt. 0) then
		lat1_index = i
		lat2_index = i + 1
		exit
	    end if
	end do

end subroutine lat_search

subroutine lon_search(targetpnt,lon_in,lon1_index,lon2_index) !find the nearest longitude
	implicit none
	real::targetpnt
	integer::len_lon
	integer::lon1_index,lon2_index,i,j
	real,allocatable::lon_in(:),lon_distance(:)
	
	len_lon = size(lon_in)
		if(targetpnt.gt.lon_in(len_lon) .or. targetpnt.lt.lon_in(1)) then 
	!special points near 0 or 360
			lon1_index = len_lon
			lon2_index = 1
		else
	!normal points	
			lon_distance = lon_in - targetpnt !the distance between target point and known points
			do i = 1,len_lon-1
				if (lon_distance(i) * lon_distance(i+1) .lt. 0) then
					lon1_index = i
					lon2_index = i + 1
					exit
				endif
			end do
		endif
end subroutine lon_search

subroutine data_write(filename_out,v_out)
	implicit none
	character(len=*)::filename_out
	integer::i,j
	real::lat_out(180)=(/(i+0.5,i=-90,89)/) !lat_out = (-89.5,-88.5,...,89.5)
	real::lon_out(360)=(/(i+0.5,i=0,359)/)  !lon_out = (0.5,1.5,...,359.5)
	real::v_out(360,180)

   open(unit=11,file = filename_out,form = "formatted")
	write(11,"(A27)") '"LAT","LON","SFCWind (m/s)"'
	
	do j = 1,180
	    do i =1,360
	        write(11,"(F6.2,A1,F7.2,A1,F8.4)")  lat_out(j),',',lon_out(i),',',v_out(i,j)
	    enddo
	enddo
end subroutine data_write
	
