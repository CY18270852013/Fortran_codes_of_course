program main
    implicit none

	! Interface declarations define common interfaces between subroutines
	interface
		subroutine data_read(file_name, lat_ori, lon_ori, wind_srf_ori)
    		implicit none
    		character(len=*) :: file_name
    		real, allocatable ::lat_ori(:), lon_ori(:)
    		real, allocatable :: wind_srf_ori(:,:)
    		integer :: i, j, error
    		integer :: nlat, nlon
    		character(len=30) :: header
		end subroutine
		subroutine data_interp(lat_ori, lon_ori, wind_srf_ori, wind_srf_interp)
	        integer i,j
			integer lat1,lat2,lon1,lon2,nlat
			real,allocatable::lat_ori(:),lon_ori(:)
			real::lat_interp(180)=(/(i+0.5,i=-90,89)/)
			real::lon_interp(360)=(/(i+0.5,i=0,359)/)
			real::wind_srf_interp(360,180)
			real,allocatable::wind_srf_ori(:,:)
			real::x1,x2,x,y1,y2,y
			real::FQ11,FQ21,FQ12,FQ22,FP1,FP2
	    end subroutine
		subroutine data_output(file_name, wind_srf_interp)
    		implicit none
    		character(len=*) :: file_name
    		real :: wind_srf_interp(360, 180)
    		integer :: i, j
    		character(len=30) :: header = '"LAT","LON","SFCWind (m/s)"'
		end subroutine
	end interface
		
	real, allocatable :: wind_srf_ori_GFDL(:,:), wind_srf_ori_FGOALS(:,:)
    real, allocatable :: wind_srf_interp_GFDL(:,:), wind_srf_interp_FGOALS(:,:)
	real, allocatable :: lat_ori(:),lon_ori(:)
	real, allocatable :: wind_srf_ori(:,:)
	real :: wind_srf_interp(360,180)

!GFDL-ESM2M
	allocate(lat_ori(90))
	allocate(lon_ori(144))
	allocate(wind_srf_ori(144,90))
	call data_read("sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_210012.txt",lat_ori,lon_ori,wind_srf_ori)
	call data_interp(lat_ori,lon_ori,wind_srf_ori,wind_srf_interp)
	call data_output("output1.txt",wind_srf_interp)
	deallocate(lat_ori)
	deallocate(lon_ori)
	deallocate(wind_srf_ori)


!FGOALS
	allocate(lat_ori(108))
	allocate(lon_ori(128))
	allocate(wind_srf_ori(128,108))
	call data_read("sfcWind_Amon_FGOALS-s2_rcp85_r1i1p1_210012.txt",lat_ori,lon_ori,wind_srf_ori)
	call data_interp(lat_ori,lon_ori,wind_srf_ori,wind_srf_interp)
	call data_output("output2.txt",wind_srf_interp)
	deallocate(lat_ori)
	deallocate(lon_ori)
	deallocate(wind_srf_ori)
end

! Subroutine to read input data from file
subroutine data_read(file_name, lat_ori, lon_ori, wind_srf_ori)
    implicit none
    character(len=*) :: file_name
    real, allocatable ::lat_ori(:), lon_ori(:)
    real, allocatable :: wind_srf_ori(:,:)
    integer :: i, j, error
    integer :: nlat, nlon
    character(len=30) :: header
    nlat = size(lat_ori)
    nlon = size(lon_ori)
    open(10, file=file_name, form='formatted',status='old', action='read')
    read(10,'(A30)') header
    do j = 1, nlat
        do i = 1, nlon
            read(10, '(f8.2, f8.2, f8.4)',iostat = error) lat_ori(j), lon_ori(i), wind_srf_ori(i, j)
            if(error /= 0) exit
        end do
    end do
    close(10)
end subroutine data_read

! Subroutine to interpolate data to regular grid
subroutine data_interp(lat_ori, lon_ori, wind_srf_ori, wind_srf_interp)
	implicit none
		! Includes lat/lon search subroutines to find nearest points
		interface    
	    	subroutine lat_search(targetpnt,lat_ori,lat1,lat2)
				real::targetpnt
				integer::lat1,lat2
				real,allocatable::lat_ori(:)
	    	end subroutine lat_search
	    	subroutine lon_search(targetpnt,lon_ori,lon1,lon2) 
				real::targetpnt
				integer::lon1,lon2
				real,allocatable::lon_ori(:)
	    	end subroutine lon_search
	  	end interface
	integer i,j
	integer lat1,lat2,lon1,lon2,nlat
	real,allocatable::lat_ori(:),lon_ori(:)
	real::lat_interp(180)=(/(i+0.5,i=-90,89)/)
	real::lon_interp(360)=(/(i+0.5,i=0,359)/)
	real::wind_srf_interp(360,180)
	real,allocatable::wind_srf_ori(:,:)
	real::x1,x2,x,y1,y2,y
	real::FQ11,FQ21,FQ12,FQ22,FP1,FP2
	nlat = size(lat_ori)

	! Performs bilinear interpolation based on nearest points
	do j = 2,179
		do i = 1,360
			call lat_search(lat_interp(j),lat_ori,lat1,lat2)
			call lon_search(lon_interp(i),lon_ori,lon1,lon2)
			x1 = lat_ori(lat1)
			x2 = lat_ori(lat2)
			x = lat_interp(j)
			y1 = lon_ori(lon1)
			y2 = lon_ori(lon2)
			y =lon_interp(i)
			! Handles edge cases like near 0/360 longitudes
			if (y1 .gt. y2) then !for points near 0 or 360
				if (y1 .gt. 180) y1 = y1 - 360
				if (y .gt. 180) y = y -360
			end if
			fq11 = (x2-x)/(x2-x1) *wind_srf_ori(lon1,lat1)
			fq21 = (x-x1)/(x2-x1) *wind_srf_ori(lon1,lat2)
			fq12 = (x2-x)/(x2-x1) *wind_srf_ori(lon2,lat1)
			fq22 = (x-x1)/(x2-x1) *wind_srf_ori(lon2,lat2)
			fp1 = (y2-y)/(y2-y1)*(fq11 + fq21)
			fp2 = (y-y1)/(y2-y1)*(fq12 + fq22)
			wind_srf_interp(i,j) = fp1 + fp2
		end do 	
	end do
	do i = 1,360
		call lon_search(lon_interp(i),lon_ori,lon1,lon2)
		y1 = lon_ori(lon1)
		y2 = lon_ori(lon2)
		y =lon_interp(i)
		if (y1 .gt. y2) then
			if (y1 .gt. 180) y1 = y1 - 360
			if (y .gt. 180) y = y -360
		end if
		fp1 = (y2-y)/(y2-y1)*wind_srf_ori(lon1,1)
		fp2 =(y-y1)/(y2-y1)*wind_srf_ori(lon2,1)
		wind_srf_interp(i,1) = fp1 + fp2
	end do
	do i = 1,360
		call lon_search(lon_interp(i),lon_ori,lon1,lon2)
		y1 = lon_ori(lon1)
		y2 = lon_ori(lon2)
		y =lon_interp(i)
		if (y1 .gt. y2) then
			if (y1 .gt. 180) y1 = y1 - 360
			if (y .gt. 180) y = y -360
		end if
		fp1 = (y2-y)/(y2-y1)*wind_srf_ori(lon1,nlat)
		fp2 =(y-y1)/(y2-y1)*wind_srf_ori(lon2,nlat)
		wind_srf_interp(i,180) = fp1 + fp2
	end do
end subroutine data_interp

! Finds indices of nearest latitude points to target point
subroutine lat_search(targetpnt,lat_ori,lat1,lat2)
	implicit none
	real::targetpnt
	integer::lat1,lat2,i,j
	integer::nlat
	real,allocatable::lat_ori(:),lat_distance(:)
	nlat = size(lat_ori)
	lat_distance = lat_ori - targetpnt
	do i = 1,nlat-1
	    if (lat_distance(i) * lat_distance(i+1) .lt. 0) then
		lat1 = i
		lat2 = i + 1
		exit
	    end if
	end do
end subroutine lat_search

! Finds indices of nearest longitude points to target point
subroutine lon_search(targetpnt,lon_ori,lon1,lon2)
	implicit none
	real::targetpnt
	integer::nlon
	integer::lon1,lon2,i,j
	real,allocatable::lon_ori(:),lon_distance(:)
	nlon = size(lon_ori)
		if(targetpnt.gt.lon_ori(nlon) .or. targetpnt.lt.lon_ori(1)) then 
			lon1 = nlon
			lon2 = 1
		else
	
			lon_distance = lon_ori - targetpnt
			do i = 1,nlon-1
				if (lon_distance(i) * lon_distance(i+1) .lt. 0) then
					lon1 = i
					lon2 = i + 1
					exit
				endif
			end do
		end if
end subroutine lon_search

! Writes interpolated data to output file in formatted string
subroutine data_output(file_name, wind_srf_interp)
    implicit none
    character(len=*) :: file_name
    real :: wind_srf_interp(360, 180)
    integer :: i, j
    character(len=30) :: header = '"LAT","LON","SFCWind (m/s)"'
    open(20, file=file_name, status='replace', action='write')
    write(20,*)header
    do i = 1, 360
        write(20, '(f8.2, f8.2, f8.4)') (-89.0 + (i-1), 0.0 + (j-1), wind_srf_interp(i, j), j = 1, 180)
    end do
    close(20)
end subroutine data_output