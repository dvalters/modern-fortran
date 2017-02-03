! sum_f.f90

program sum_f

  use iso_c_binding
  interface
    function sum_c( x, n) bind( C, name = 'sum')
      use iso_c_binding
      real(kind=C_FLOAT) :: sum_f
      real(kind=C_FLOAT) :: x(*)

      integer(kind=C_INT), value :: n
    end function sum_c
  end interface

  integer, parameter :: n = 4
  real(kind=C_FLOAT) :: x(n) = [ 1.0, 2.0, 3.0, 4.0 ]

  print *, sum_c(x,n)

end program sum_f

