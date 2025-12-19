!===============================================================================
! MODULE: constants_mod
!===============================================================================
!> @brief Physical and mathematical constants for the SVM calculation
!>
!> This module contains all fundamental constants used throughout the
!> Stochastic Variational Method (SVM) program for solving few-body problems
!> with correlated Gaussian basis functions.
!>
!> @author Original: K. Varga, Y. Suzuki (Phys. Rev. C52 (1995) 2885)
!> @author Modern Fortran version: Refactored for modularity
!===============================================================================
module constants_mod
    implicit none

    !---------------------------------------------------------------------------
    ! Public visibility for all module contents
    !---------------------------------------------------------------------------
    public

    !---------------------------------------------------------------------------
    ! Precision parameters
    !---------------------------------------------------------------------------
    !> Double precision kind parameter (64-bit floating point)
    integer, parameter :: dp = selected_real_kind(15, 307)

    !> Single precision kind parameter (32-bit floating point)
    integer, parameter :: sp = selected_real_kind(6, 37)

    !---------------------------------------------------------------------------
    ! Mathematical constants
    !---------------------------------------------------------------------------
    !> Pi = 3.14159265358979323846...
    real(dp), parameter :: PI = 3.1415926535897932384_dp

    !> Square root of Pi
    real(dp), parameter :: SQRT_PI = 1.7724538509055160273_dp

    !> Natural logarithm of 2
    real(dp), parameter :: LN2 = 0.6931471805599453094_dp

    !---------------------------------------------------------------------------
    ! Array dimension parameters
    !---------------------------------------------------------------------------
    !> Maximum number of particles in the system
    integer, parameter :: MNPAR = 6

    !> Maximum dimension of the basis set
    integer, parameter :: MNBAS = 400

    !> Maximum number of permutations (factorial storage)
    integer, parameter :: NFAC = 8000

    !> Maximum factorial index
    integer, parameter :: IFD = 12

    !> Maximum number of spin configurations
    integer, parameter :: MSPC = 4

    !> Number of points for potential interpolation
    integer, parameter :: NPNT = 10000

    !> Maximum index for factorial logarithm array
    integer, parameter :: NMAX = 100

    !> Maximum index for double factorial logarithm array
    integer, parameter :: NNMAX = 301

    !> Maximum index for powers of 2 array
    integer, parameter :: N2MAX = 100

    !---------------------------------------------------------------------------
    ! Numerical tolerance parameters
    !---------------------------------------------------------------------------
    !> Small number to avoid division by zero
    real(dp), parameter :: TINY = 1.0d-20

    !> Default accuracy for root finding
    real(dp), parameter :: DEFAULT_ACC = 1.0d-12

    !> Large energy value for initialization
    real(dp), parameter :: LARGE_ENERGY = 1.0d+10

    !> Very large energy for failed calculations
    real(dp), parameter :: HUGE_ENERGY = 100000.0_dp

    !---------------------------------------------------------------------------
    ! Factorial table (precomputed for efficiency)
    !---------------------------------------------------------------------------
    !> Factorials from 0! to 12!
    integer, parameter :: FACTORIAL(0:IFD) = &
        [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, &
         3628800, 39916800, 479001600]

end module constants_mod
