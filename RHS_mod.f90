module RHS_mod

    function func( j, x_num, x ) result ( d )
      implicit none

      integer(kind=SI), intent(in) :: j, x_num
      real(kind=DP) :: d
      real(kind=DP), intent(in) :: x(x_num)

      d = 0.0_DP
    end function func

    public :: func

end module RHS_mod
