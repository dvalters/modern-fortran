module coarrays

  real :: a_I[*]
  real, allocatable :: X_I(:)[:], Y_I(:)[:], Z_I(:)[:]
  integer :: n_I

    n_I = n / num_images()
    allocate(X_I(n_I)); allocate(Y_I(n_I)); allocate(Z_I(n_I))

    if (this_image() == 1 ) then
      do i = 1, num_images()
        a_I[i] = a
        X_I(:)[i] = X((i-1) * n_I + 1 : i*n_I)
        Y_I(:)[i] = Y((i-1) * n_I + 1 : i*n_I)
      end do
    end if
    sync all

    call axpy(n_I, a_I, X_I, X_I, Y_I, Z_I)
    sync all 

    if (this_image() == 1 ) then
      do i = 1, num_images()
        Z((i-1) * n_I+1 : i*n_I) = Z_I(:)[i]
      end do
    end if

end module coarrays


