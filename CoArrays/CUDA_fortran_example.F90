module CUDAFortran

attributes(global) subroutine axpy(n, a, X, Y, Z)
integer, value :: n
real, value :: a
  i = threadIdx%x + (blocKIdx%x -1) * blockDim%x
  if (i <= n) then
   Z(i) = a * X(i) + Y(i)

use cudafor
real, allocatable, device :: X_D(:), Y_D(:), Z_D(:)

type(dim3) :: BLOCK, GRID
  allocate(X_D(n)); allocate(T_D(n)); allocate(Z_D(n))
  err = cudaMemCpy(X_D, X, n, cudaMemCpyHostToDevice)
  err = cudaMEmCpy(Y_D, Y, n)

  block = dim3(128, 1, 1); grid = dim(n / block%x, 1, 1)
  call axpy<<<grid, block>>>(%val(n), %val(b), X_D, Y_D, Z_D)
  Z = Z_D

end module CUDAFortran
   
