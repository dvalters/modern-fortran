program fd1d_heat_explicit_prb
      use Types_mod
      implicit none

      integer(kind=SI), parameter :: t_num = 201
      integer(kind=SI), parameter :: x_num = 21
      
      real(kind=DP) :: cfl
      real(kind=DP) :: dt
      real(kind=DP), dimension(:), allocatable :: h!(1:x_num)
      real(kind=DP), dimension(:), allocatable :: h_new!(1:x_num)
      ! the "matrix" stores all x-values for all t-values
      ! remember Fortran is column major, meaning that rows are contiguous
      real(kind=DP), dimension(:,:), allocatable :: hmat!(1:x_num,1: t_num)
      
      

      integer(kind=SI) :: i
      integer(kind=SI) :: j
      real(kind=DP) :: k

      real(kind=DP), dimension(:), allocatable :: t!(1:t_num)
      real(kind=DP) :: t_max
      real(kind=DP) :: t_min
      real(kind=DP), dimension(:), allocatable :: x!(1:x_num)
      real(kind=DP) :: x_max
      real(kind=DP) :: x_min
      allocate(h(1:x_num), h_new(1:x_num), hmat(1:x_num, 1:t_num), t(1:t_num), x(1:x_num))
      
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the FD1D_HEAT_EXPLICIT library.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_TEST01:'
      write ( *, '(a)' ) '  Compute an approximate solution to the time-dependent'
      write ( *, '(a)' ) '  one dimensional heat equation:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Run a simple test case.'

      ! heat coefficient
      k = 0.002_DP

      ! the x-range values
      x_min = 0.0_DP
      x_max = 1.0_DP
      ! x_num is the number of intervals in the x-direction
      call r8vec_linspace( x_num, x_min, x_max, x )

      ! the t-range values. integrate from t_min to t_max
      t_min = 0.0_DP
      t_max = 80.0_DP

      ! t_num is the number of intervals in the t-direction
      dt = ( t_max - t_min ) / dble( t_num - 1 )
      call r8vec_linspace( t_num, t_min, t_max, t )

      ! get the CFL coefficient
      call fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )

     if ( 0.5_DP .le. cfl ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_CFL - Fatal error!'
        write ( *, '(a)' ) '  CFL condition failed.'
        write ( *, '(a)' ) '  0.5 <= K * dT / dX / dX = CFL.'
        stop
      end if

      ! set the initial condition
      do j = 1, x_num
        h(j) = 50.0_DP
      end do

      ! set the bounday condition
      h(1) = 90.0_DP
      h(x_num) = 70.0_DP

      ! initialise the matrix to the initial condition
      do i = 1, x_num
        hmat(i, 1) = h(i)
      end do

      ! the main time integration loop 
      do j = 2, t_num
        call fd1d_heat_explicit( x_num, x, t(j-1), dt, cfl, h, h_new )

        do i = 1, x_num
          hmat(i, j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do

      ! write data to files
      call r8mat_write( 'h_test01.txt', x_num, t_num, hmat )
      call r8vec_write( 't_test01.txt', t_num, t )
      call r8vec_write( 'x_test01.txt', x_num, x )

    deallocate(h, h_new, hmat, t, x)

    contains

    use RHS_mod

    subroutine fd1d_heat_explicit( x_num, x, t, dt, cfl, h, h_new )
      implicit none

      integer(kind=SI), intent(in) :: x_num

      real(kind=DP), intent(in) :: cfl
      real(kind=DP), intent(in) :: dt
      real(kind=DP), intent(in) :: h(x_num)
      real(kind=DP), intent(inout) :: h_new(x_num)
      integer(kind=SI) :: j
      real(kind=DP), intent(in) :: t
      real(kind=DP), intent(in) :: x(x_num)
      real(kind=DP) :: f(x_num)

      do j = 1, x_num
        f(j) = func( j, x_num, x )
      end do

      h_new(1) = 0.0_DP

      do j = 2, x_num - 1
        h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0_DP * h(j) + h(j+1) )
      end do

      ! set the boundary conditions again
      h_new(1) = 90.0_DP
      h_new(x_num) = 70.0_DP
    end subroutine fd1d_heat_explicit
 
    use CFL_mod
    
    subroutine r8mat_write( output_filename, m, n, table )
      implicit none

      integer(kind=SI), intent(in) :: m
      integer(kind=SI), intent(in) :: n

      integer(kind=SI) :: j
      character * ( * ) :: output_filename
      integer(kind=SI) :: output_unit
      character * ( 30 ) :: string 
      real(kind=DP), intent(out) :: table(m,n)
 
      output_unit = 10
      open( unit = output_unit, file = output_filename, status = 'replace' )

      write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'

      do j = 1, n
        write ( output_unit, string ) table(1:m, j)
      end do

      close( unit = output_unit )
    end subroutine r8mat_write

    subroutine r8vec_linspace ( n, a_first, a_last, a )

      implicit none

      integer(kind=SI), intent(in) :: n
      real(kind=DP), intent(inout) :: a(n)
      real(kind=DP), intent(in) :: a_first
      real(kind=DP), intent(in) :: a_last
      integer(kind=SI) :: i

      do i = 1, n
        a(i) = ( dble( n - i ) * a_first + dble( i - 1 ) * a_last ) / dble( n - 1 )
      end do

    end subroutine r8vec_linspace

    subroutine r8vec_write ( output_filename, n, x )

      implicit none

      integer(kind=SI) :: m
      integer(kind=SI) :: n

      integer(kind=SI) :: j
      character * ( * ) :: output_filename
      integer(kind=SI) :: output_unit
      real(kind=DP) :: x(n)

      output_unit = 11
      open( unit = output_unit, file = output_filename, status = 'replace' )

      do j = 1, n
        write ( output_unit, '(2x,g24.16)' ) x(j)
      end do

      close ( unit = output_unit )
  end subroutine r8vec_write
  

end program fd1d_heat_explicit_prb

