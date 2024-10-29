program InterpolateData
    implicit none
  
    ! 参数定义
    integer, parameter :: nx_source = 180  ! 源数据经度格点数
    integer, parameter :: ny_source = 72   ! 源数据纬度格点数
    integer, parameter :: nx_target = 360 ! 目标插值后经度格点数
    integer, parameter :: ny_target = 180 ! 目标插值后纬度格点数
    real(8), dimension(nx_source, ny_source) :: sourceData
    real(8), dimension(nx_target, ny_target) :: targetData
    real(8) :: lon_source(nx_source), lat_source(ny_source)
    real(8) :: lon_target(nx_target), lat_target(ny_target)
    real(8) :: lon, lat, value
    integer :: i, j, i_source, j_source
    real(8) :: weight_lon(2), weight_lat(2)
  
    ! 读取源数据
    open(unit=1, file='sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_210012.txt', status='old')
    do j = 1, ny_source
      do i = 1, nx_source
        read(1, *) lon, lat, sourceData(i, j)
        lon_source(i) = lon
        lat_source(j) = lat
      end do
    end do
    close(1)
  
    ! 读取目标数据的经纬度
    open(unit=2, file='sfcWind_Amon_FGOALS-s2_rcp85_r1i1p1_210012.txt', status='old')
    do j = 1, ny_target
      do i = 1, nx_target
        read(2, *) lon, lat
        lon_target(i) = lon
        lat_target(j) = lat
      end do
    end do
    close(2)
  
    ! 执行插值
    do j = 1, ny_target
      do i = 1, nx_target
        ! 找到源数据中最接近的经度和纬度格点
        call FindNearestGrid(lon_target(i), lon_source, i_source, weight_lon)
        call FindNearestGrid(lat_target(j), lat_source, j_source, weight_lat)
  
        ! 执行双线性插值
        targetData(i, j) = BilinearInterpolation( &
          sourceData(i_source, j_source), sourceData(i_source + 1, j_source), &
          sourceData(i_source, j_source + 1), sourceData(i_source + 1, j_source + 1), &
          weight_lon, weight_lat)
      end do
    end do
  
    ! 将插值结果写入文件或进行其他操作
  
  contains
  
    subroutine FindNearestGrid(target, grid, index, weights)
      real(8), intent(in) :: target
      real(8), dimension(:), intent(in) :: grid
      integer, intent(out) :: index
      real(8), dimension(2), intent(out) :: weights
  
      ! 在格点数组中找到最接近目标值的格点索引和权重
      ! 实现略过，需要根据实际情况编写
  
    end subroutine FindNearestGrid
  
    function BilinearInterpolation(q11, q21, q12, q22, weights_lon, weights_lat) result(value)
      real(8), intent(in) :: q11, q21, q12, q22
      real(8), dimension(2), intent(in) :: weights_lon, weights_lat
      real(8) :: value
  
      ! 执行双线性插值
      value = q11 * (1.0 - weights_lon(1)) * (1.0 - weights_lat(1)) + &
              q21 * weights_lon(1) * (1.0 - weights_lat(1)) + &
              q12 * (1.0 - weights_lon(2)) * weights_lat(1) + &
              q22 * weights_lon(2) * weights_lat(2)
  
    end function BilinearInterpolation
  
  end program InterpolateData
  