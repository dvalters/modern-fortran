module sum_mod

contains

  subroutine sumpy( array_f, result_f )
    real, dimension(:), intent(in) :: array_F
    real, intent(out) :: result_f
    result_f = sum ( array_f )
  end subroutine sumpy

  function fumpy( array_f ) result ( result_f )
    real, dimension(:), intent(in) :: array_f
    real :: result_f
    result_f = sum(array_f)
  end function fumpy

end module sum_mod

