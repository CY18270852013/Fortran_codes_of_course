program main
    implicit none
    real, allocatable :: wind_srf_ori_GFDL(:,:), wind_srf_ori_FGOALS(:,:)
    real, allocatable :: wind_srf_interp_GFDL(:,:), wind_srf_interp_FGOALS(:,:)
    integer :: i, j, k, n
    integer, parameter :: nx_in = 180, ny_in = 73, nx_out = 360, ny_out = 181 ! 输入和输出数据的网格大小
    real, parameter :: lon_min = -180.0, lon_max = 180.0, lat_min = -90.0, lat_max = 90.0 ! 经纬度范围
    real :: lon_in(nx_in), lat_in(ny_in), lon_out(nx_out), lat_out(ny_out)
    real :: lon_ratio, lat_ratio
    character(100) :: file_in_GFDL = "sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_210012.txt"
    character(100) :: file_in_FGOALS = "sfcWind_Amon_FGOALS-s2_rcp85_r1i1p1_210012.txt"
    character(20) :: file_out = "output.txt"

    ! 分配内存
    allocate(wind_srf_ori_GFDL(nx_in, ny_in), wind_srf_interp_GFDL(nx_out, ny_out))
    allocate(wind_srf_ori_FGOALS(nx_in, ny_in), wind_srf_interp_FGOALS(nx_out, ny_out))

    ! 读取输入数据
    call data_read(file_in_GFDL, wind_srf_ori_GFDL)
    call data_read(file_in_FGOALS, wind_srf_ori_FGOALS)

    ! 计算输入和输出网格的经纬度
    lon_ratio = (lon_max - lon_min) / (nx_out - 1)
    lat_ratio = (lat_max - lat_min) / (ny_out - 1)
    do i = 1, nx_out
        lon_out(i) = lon_min + (i - 1) * lon_ratio
    end do
    do j = 1, ny_out
        lat_out(j) = lat_min + (j - 1) * lat_ratio
    end do

    ! 双线性插值
    do i = 1, nx_out
        do j = 1, ny_out
            wind_srf_interp_GFDL(i, j) = bilinear_interp(lon_out(i), lat_out(j), lon_in, lat_in, wind_srf_ori_GFDL)
            wind_srf_interp_FGOALS(i, j) = bilinear_interp(lon_out(i), lat_out(j), lon_in, lat_in, wind_srf_ori_FGOALS)
        end do
    end do

    ! 将插值结果写入文件
    call data_output(file_out, wind_srf_interp_GFDL, wind_srf_interp_FGOALS)

    ! 释放内存
    deallocate(wind_srf_ori_GFDL, wind_srf_interp_GFDL)
    deallocate(wind_srf_ori_FGOALS, wind_srf_interp_FGOALS)

end program main

subroutine data_read(file_name, data)
    implicit none
    character(20), intent(in) :: file_name
    real, intent(out) :: data(:,:)
    ! 读取数据的代码
end subroutine data_read

subroutine data_output(file_name, data_GFDL, data_FGOALS)
    implicit none
    character(20), intent(in) :: file_name
    real, intent(in) :: data_GFDL(:,:), data_FGOALS(:,:)
    ! 将数据写入文件的代码
end subroutine data_output

function bilinear_interp(x, y, x_in, y_in, data_in) result(value)
    implicit none
    real, intent(in) :: x, y
    real, intent(in) :: x_in(:), y_in(:), data_in(:,:)
    real :: value
    ! 双线性插值的代码
end function bilinear_interp