program BilinearInterpolation
    implicit none
    
    ! 声明变量
    real(8) :: x1, y1, f1
    real(8) :: x2, y2, f2
    real(8) :: x3, y3, f3
    real(8) :: x4, y4, f4
    real(8) :: x, y
    real(8) :: result
    
    ! 读取四个输入点的坐标和值
    write(*,*) 'Enter the coordinates and values of the four points:'
    read(*,*) x1, y1, f1
    read(*,*) x2, y2, f2
    read(*,*) x3, y3, f3
    read(*,*) x4, y4, f4
    
    ! 读取目标插值点的坐标
    write(*,*) 'Enter the coordinates of the target interpolation point:'
    read(*,*) x, y
    
    ! 计算双线性插值
    result = BilinearInterpolation(x1, y1, f1, x2, y2, f2, x3, y3, f3, x4, y4, f4, x, y)
    
    ! 打印结果
    write(*,*) 'Bilinear Interpolation Result:', result
    
  contains
  
    function BilinearInterpolation(x1, y1, f1, x2, y2, f2, x3, y3, f3, x4, y4, f4, x, y) result(result)
      real(8), intent(in) :: x1, y1, f1
      real(8), intent(in) :: x2, y2, f2
      real(8), intent(in) :: x3, y3, f3
      real(8), intent(in) :: x4, y4, f4
      real(8), intent(in) :: x, y
      real(8) :: result
      
      result = f1 * (x2 - x) * (y2 - y) &
             + f2 * (x - x1) * (y2 - y) &
             + f3 * (x2 - x) * (y - y1) &
             + f4 * (x - x1) * (y - y1)
             
      result = result / ((x2 - x1) * (y2 - y1))
    end function BilinearInterpolation
  
  end program BilinearInterpolation
  