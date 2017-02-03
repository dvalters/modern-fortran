module C_interop

function sum_f(x, n ) result (res ) &
  bind(C, name = 'sum_f')
  use iso_c_binding
  implicit none
  
  real(kind = C_FLOAT), intent(in) :: x(*)
  integer(kind = C_INT), intent(in) :: n
  real(kind = C_FLOAT), res

  res = sum( x(1:n) )

end function sum_f

end module C_interop  
