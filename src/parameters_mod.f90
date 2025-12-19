!===============================================================================
! MODULE: parameters_mod
!===============================================================================
!> @brief Global parameters and shared variables for SVM calculation
!>
!> This module replaces the COMMON blocks from the original Fortran 77 code
!> with module-level variables, providing better encapsulation and type safety.
!>
!> The module contains:
!> - Particle properties (masses, charges, spins, isospins)
!> - Basis function parameters (nonlinear variational parameters)
!> - Matrix storage (Hamiltonian and overlap matrices)
!> - Control parameters for the optimization procedure
!> - Coordinate transformation matrices (Jacobi coordinates)
!>
!> @note All arrays use allocatable types for flexibility
!===============================================================================
module parameters_mod
    use constants_mod
    implicit none

    !---------------------------------------------------------------------------
    ! Public visibility
    !---------------------------------------------------------------------------
    public

    !---------------------------------------------------------------------------
    ! Particle properties
    !---------------------------------------------------------------------------
    !> Number of particles in the system
    integer :: npar

    !> Number of interacting pairs: kpot = npar*(npar-1)/2
    integer :: kpot

    !> Particle masses (in units where hbar^2/(2*mass_unit) = h2m)
    real(dp) :: xm(MNPAR)

    !> Reduced masses for Jacobi coordinates
    real(dp) :: xred(MNPAR)

    !> hbar^2 / (2 * mass_unit) - kinetic energy coefficient
    real(dp) :: h2m

    !> Particle charges (for Coulomb interaction)
    real(dp) :: particle_charge(MNPAR)

    !> Coulomb potential coefficients for each pair: z(i)*z(j)
    real(dp) :: coulomb_coef((MNPAR*(MNPAR+1))/2)

    !---------------------------------------------------------------------------
    ! Spin and isospin configurations
    !---------------------------------------------------------------------------
    !> Number of spin configurations
    integer :: nspc

    !> Number of isospin configurations
    integer :: nisc

    !> Spin z-component for each particle in each spin configuration
    !> isp(particle, configuration)
    integer :: isp(MNPAR, MSPC)

    !> Isospin z-component for each particle in each isospin configuration
    !> iso(particle, configuration)
    integer :: iso(MNPAR, MSPC)

    !> Coefficients for spin configuration superposition
    real(dp) :: cspc(MSPC)

    !> Coefficients for isospin configuration superposition
    real(dp) :: cisc(MSPC)

    !---------------------------------------------------------------------------
    ! Basis function parameters
    !---------------------------------------------------------------------------
    !> Matrix of nonlinear parameters for each basis function
    !> a(basis_index, i, j) defines the Gaussian width matrix A
    !> The Gaussian is: exp(-r^T A r) where r are relative coordinates
    real(dp) :: a(MNBAS, MNPAR, MNPAR)

    !> Norm of each basis function
    real(dp) :: xnorm(MNBAS)

    !---------------------------------------------------------------------------
    ! Hamiltonian and overlap matrices
    !---------------------------------------------------------------------------
    !> Hamiltonian matrix elements H(i,j) = <phi_i|H|phi_j>
    real(dp) :: he(MNBAS, MNBAS)

    !> Overlap matrix elements O(i,j) = <phi_i|phi_j>
    real(dp) :: oe(MNBAS, MNBAS)

    !---------------------------------------------------------------------------
    ! Coordinate transformation matrices (Jacobi coordinates)
    !---------------------------------------------------------------------------
    !> Jacobi transformation matrix: r_jacobi = T * r_single_particle
    real(dp) :: t_jacobi(MNPAR, MNPAR)

    !> Inverse Jacobi transformation matrix
    real(dp) :: ti_jacobi(MNPAR, MNPAR)

    !> b vector for pair coordinate transformation
    !> b(m, k) relates pair k to Jacobi coordinate m
    real(dp) :: b_pair(MNPAR, (MNPAR*(MNPAR+1))/2)

    !> Lambda coefficients for kinetic energy: lambda(i) = h2m/xred(i)
    real(dp) :: xlambda(MNPAR)

    !---------------------------------------------------------------------------
    ! Determinant and inverse storage for matrix elements calculation
    !---------------------------------------------------------------------------
    !> Combined A + A' matrix for overlap calculation
    real(dp) :: aap(MNBAS, MNPAR, MNPAR)

    !> Inverse of A + A' matrix
    real(dp) :: aapi(MNBAS, MNPAR, MNPAR)

    !> Determinant of A + A' matrix
    real(dp) :: det_aap(MNBAS)

    !---------------------------------------------------------------------------
    ! Permutation and symmetrization data
    !---------------------------------------------------------------------------
    !> Permutation matrices: c(perm, i, j) is the matrix representation
    real(dp) :: perm_matrix(NFAC, MNPAR, MNPAR)

    !> Parity of each permutation (+1 or -1)
    real(dp) :: perm_parity(NFAC)

    !> Exchange indices for each permutation
    integer :: iexc(NFAC, MNPAR)

    !> Number of permutations
    integer :: nper

    !> Spin-isospin overlap for each permutation
    real(dp) :: spiso(NFAC)

    !> Transformed permutation matrices (in Jacobi coordinates)
    real(dp) :: trp(NFAC, MNPAR, MNPAR)

    !> Number of non-zero permutation terms after spin-isospin selection
    integer :: not_perm

    !> Operator matrix elements (Wigner, Majorana, Bartlett, Heisenberg)
    !> w(perm, pair, operator_type)
    real(dp) :: w_operator(NFAC, (MNPAR*(MNPAR+1))/2, 4)

    !---------------------------------------------------------------------------
    ! Control parameters
    !---------------------------------------------------------------------------
    !> Solution method selector:
    !> - ico = 1: Use predefined basis (read from file)
    !> - ico = 2: SVM step-by-step optimization
    !> - ico = 3: SVM refinement of existing basis
    integer :: ico

    !> Particle statistics:
    !> - ibf = 1: Fermions (antisymmetric wave function)
    !> - ibf = 2: Bosons (symmetric wave function)
    integer :: ibf

    !> Number of optimization cycles (mm0)
    integer :: mm0

    !> Number of random trials per parameter (kk0)
    integer :: kk0

    !> Starting basis dimension (from file)
    integer :: nbas0

    !> Index for refinement loop
    integer :: nbas1

    !> Target basis dimension
    integer :: mnb

    !> Random number generator seed
    integer :: irand

    !---------------------------------------------------------------------------
    ! Optimization interval parameters
    !---------------------------------------------------------------------------
    !> Minimum value of 1/sqrt(A) parameter
    real(dp) :: bmin

    !> Maximum value of 1/sqrt(A) parameter
    real(dp) :: bmax

    !> Mass-dependent scaling factors for parameter generation
    real(dp) :: xmr((MNPAR*(MNPAR+1))/2)

    !---------------------------------------------------------------------------
    ! Potential parameters
    !---------------------------------------------------------------------------
    !> Potential representation method:
    !> - ipcon = 1: Read potential from file (interpolation)
    !> - ipcon = 2: Use analytic form defined in subroutine
    integer :: ipcon

    !> Number of potential terms in the expansion
    integer :: npt

    !> Number of operators (Wigner, Majorana, Bartlett, Heisenberg)
    integer :: no

    !> Power of r in potential term (r^n * exp(-a*r^2 + b*r))
    integer :: np_pot(10, 4)

    !> Potential strength coefficients
    real(dp) :: vp(10, 4)

    !> Gaussian exponent in potential
    real(dp) :: ap(10, 4)

    !> Linear term coefficient in potential exponent
    real(dp) :: bp(10, 4)

    !> Selected potential component index
    integer :: ip_pot

    !---------------------------------------------------------------------------
    ! Potential interpolation data
    !---------------------------------------------------------------------------
    !> Lower limit for logarithmic interpolation
    real(dp) :: xll

    !> Upper limit for logarithmic interpolation
    real(dp) :: xul

    !> Grid points for interpolation
    real(dp) :: def_grid(NPNT)

    !> Number of grid points
    integer :: nv0

    !> Interpolated potential values for each operator
    real(dp) :: fff_pot(NPNT, 4)

    !---------------------------------------------------------------------------
    ! Diagonalization storage
    !---------------------------------------------------------------------------
    !> Eigenvalues from diagonalization
    real(dp) :: eigenvalues(MNBAS)

    !> Eigenvectors from diagonalization
    real(dp) :: eigenvectors(MNBAS, MNBAS)

    !> Diagonalization counter
    integer :: diag_counter

    !> Error flag for iterative eigenvalue solver
    integer :: diag_error

    !> Accuracy check flag
    integer :: accuracy_flag

    !---------------------------------------------------------------------------
    ! Special function tables (precomputed)
    !---------------------------------------------------------------------------
    !> Logarithm of factorials: f(n) = log(n!)
    real(dp) :: log_factorial(0:NMAX)

    !> Logarithm of double factorials: df(n) = log(n!!)
    real(dp) :: log_double_factorial(-1:NNMAX)

    !> Logarithm of powers of 2: x2n(n) = log(2^n)
    real(dp) :: log_power2(0:N2MAX)

    !---------------------------------------------------------------------------
    ! Eigenvalue solver workspace
    !---------------------------------------------------------------------------
    !> Eigenvalues for reduced problem
    real(dp) :: ee_work(MNBAS)

    !> Off-diagonal elements for reduced problem
    real(dp) :: q_work(MNBAS-1)

    !> Dimension of reduced problem
    integer :: mi_work

    !> Search interval bounds
    real(dp) :: x1_bound, x2_bound

    !---------------------------------------------------------------------------
    ! Random number generator state
    !---------------------------------------------------------------------------
    !> State variable for random generator
    integer :: iy_random

    !> Array for random generator state
    integer :: ir_random(97)

contains

    !===========================================================================
    !> @brief Initialize all parameter arrays to default values
    !>
    !> This subroutine should be called at the start of the program to
    !> ensure all arrays are properly initialized before use.
    !===========================================================================
    subroutine init_parameters()
        implicit none

        ! Initialize particle properties
        npar = 0
        kpot = 0
        xm = 0.0_dp
        xred = 0.0_dp
        h2m = 0.0_dp
        particle_charge = 0.0_dp
        coulomb_coef = 0.0_dp

        ! Initialize spin/isospin
        nspc = 0
        nisc = 0
        isp = 0
        iso = 0
        cspc = 0.0_dp
        cisc = 0.0_dp

        ! Initialize basis parameters
        a = 0.0_dp
        xnorm = 1.0_dp

        ! Initialize matrices
        he = 0.0_dp
        oe = 0.0_dp

        ! Initialize transformation matrices
        t_jacobi = 0.0_dp
        ti_jacobi = 0.0_dp
        b_pair = 0.0_dp
        xlambda = 0.0_dp

        ! Initialize control parameters
        ico = 1
        ibf = 1
        mm0 = 1
        kk0 = 100
        nbas0 = 0
        nbas1 = 0
        mnb = 100
        irand = 12345

        ! Initialize potential parameters
        ipcon = 1
        npt = 0
        no = 1

    end subroutine init_parameters

end module parameters_mod
