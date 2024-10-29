program main
    implicit none
!    This program is designed for interpolating global surface wind data into
!    1-degree spatial resolution.Bilinear interpolation method is applied.
!
!    wind_srf_ori_GFDL: surface wind data from GFDL
!    wind_srf_ori_FGOALS: surface wind data from FGOALS
!    wind_srf_interp_GFDL: surface wind data (GFDL) after conduct ing interpolat ion
!    wind_srf_interp_FGOALS: surface wind data (FGOALS) after conducting interpolation
!    ...... description for other variables but not for ijkn which are easy to understand e.g. variables indicating
!    loop operation.
!

    real, allocatable :: wind_srf_ori_GFDL(:,:),wind_srf_ori_FGOALS(:,:)
    real, allocatable :: wind_srf_interp_GFDL(:,:),wind_srf_interp_FGOALS(:,:)
    
!variables with fixed length are also acceptable

    integer :: i,j,k,n


!    program is split into three parts : . .. . .. decription
    call data_read ( . . ....)
    call data_interp( . . . ...)
    call data_output( ......)

end

subroutine data_read( . . ....)
    implicit none
!decription for subroutine

end subroutine

subroutine data_interp( .. . ...)
    implicit none
!decription for subroutine

end subroutine

subroutine data_output( ......)
    implicit none
!decription for subroutine
    
end subrout ine
    