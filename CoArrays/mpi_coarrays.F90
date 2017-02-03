module mpi_coarray

use mpi

  real :: a_P
  real, allocatable :: X_P(:), Y_P(:), Z_P(:)
  integer :: n_P
  integer :: nProcs, procN, err
    call mpi_init(err)
    call mpi_comm_size(mpi_comm_world, nProcs, err)
    call mpi_comm_rank(mpi_comm_world, procN, err)
    
    n_P = n / nProcs

    allocate(X_P(n_P)); allocate(Y_P(n_P)); allocate(Z_P(n_P))

    call mpi_bcast(a_P,1, mpi_real, 0, mpi_comm_world, err)
    call mpi_scatter(X, n_P, mpi_real, X_P, n_P,
                     mpi_real, 0, mpi_comm_world, err)
    call mpi_scatter(Y, n_P, mpi_real, Y_P, n_P,
                     mpi_real, 0, mpi_comm_world, err)
    call axpy(n_P, a_P, X_P, Y_P, Z_P)

    call mpi_gather(Z_P, n_P, mpi_real, Z, n_P,
                    mpi_real, 0, mpi_comm_world, err)
    call mpi_finalize(err)

end module mpi_coarray

