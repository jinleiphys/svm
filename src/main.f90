!===============================================================================
! PROGRAM: fbs (Few-Body System)
!===============================================================================
!> @brief Main program for Stochastic Variational Method calculation
!>
!> This program solves few-body quantum mechanical problems using the
!> Stochastic Variational Method (SVM) with correlated Gaussian basis
!> functions. The method is applicable to systems with central interactions
!> and L=0 orbital angular momentum.
!>
!> Reference:
!> K. Varga, Y. Suzuki, Phys. Rev. C52 (1995) 2885
!>
!> @section usage Usage
!>
!> The program reads input from two files:
!> - fbs.inp: System parameters (particles, masses, spins, etc.)
!> - pot.inp: Potential parameters
!>
!> Output:
!> - fbs.res: Optimized basis function parameters
!> - ener.dat: Energy eigenvalues
!>
!> @section modes Solution Modes
!>
!> The program supports three solution modes (controlled by ico parameter):
!>
!> - ico = 1: Use predefined basis from fbs.res
!>   Just computes matrix elements and diagonalizes
!>
!> - ico = 2: SVM step-by-step optimization
!>   Builds basis incrementally, optimizing each new function
!>
!> - ico = 3: SVM refinement
!>   Improves existing basis by replacing functions one at a time
!>
!> @author Original: K. Varga, Y. Suzuki
!> @author Modern Fortran version: Refactored for modularity
!===============================================================================
program few_body_system
    use constants_mod
    use parameters_mod
    use io_mod
    use coordinate_transform_mod
    use permutation_mod
    use svm_mod

    implicit none

    logical :: file_exists
    integer :: nbas_read, nbas_start

    !---------------------------------------------------------------------------
    ! Print program header
    !---------------------------------------------------------------------------
    write(*, '(A)') ''
    write(*, '(A)') '************************************************************'
    write(*, '(A)') '*                                                          *'
    write(*, '(A)') '*   FBS: Few-Body System Solver                            *'
    write(*, '(A)') '*   Stochastic Variational Method (SVM)                    *'
    write(*, '(A)') '*                                                          *'
    write(*, '(A)') '*   Based on: K. Varga, Y. Suzuki                          *'
    write(*, '(A)') '*   Phys. Rev. C52 (1995) 2885                             *'
    write(*, '(A)') '*                                                          *'
    write(*, '(A)') '*   Modern Fortran Version                                 *'
    write(*, '(A)') '*                                                          *'
    write(*, '(A)') '************************************************************'
    write(*, '(A)') ''

    !---------------------------------------------------------------------------
    ! Initialize parameters
    !---------------------------------------------------------------------------
    call init_parameters()

    !---------------------------------------------------------------------------
    ! Read input data from files
    !---------------------------------------------------------------------------
    write(*, '(A)') 'Reading input files...'
    call read_input_data()

    !---------------------------------------------------------------------------
    ! Compute coordinate transformation (Jacobi coordinates)
    !---------------------------------------------------------------------------
    write(*, '(A)') 'Computing Jacobi coordinate transformation...'
    call compute_jacobi_transform()

    !---------------------------------------------------------------------------
    ! Compute spin-isospin matrix elements and symmetrization
    !---------------------------------------------------------------------------
    write(*, '(A)') 'Computing spin-isospin matrix elements...'
    call compute_spin_isospin_elements()

    !---------------------------------------------------------------------------
    ! Read existing basis (if any)
    !---------------------------------------------------------------------------
    call read_basis_file(nbas_read, nbas_start, file_exists)

    if (file_exists .and. nbas_read > 0) then
        nbas0 = nbas_read
        nbas1 = nbas_start
    else
        nbas0 = 0
        nbas1 = 0
    end if

    !---------------------------------------------------------------------------
    ! Execute selected solution mode
    !---------------------------------------------------------------------------
    write(*, '(A)') ''
    write(*, '(A)') 'Starting calculation...'
    write(*, '(A)') ''

    select case (ico)

        case (1)
            !-------------------------------------------------------------------
            ! Mode 1: Calculate with predefined basis
            !-------------------------------------------------------------------
            write(*, '(A)') 'Mode: Predefined basis calculation'
            call preset_calculation(nbas0)

        case (2)
            !-------------------------------------------------------------------
            ! Mode 2: SVM step-by-step optimization
            !-------------------------------------------------------------------
            write(*, '(A)') 'Mode: SVM step-by-step optimization'

            ! First compute matrix elements for existing basis
            if (nbas0 > 0) then
                call preset_calculation(nbas0)
            end if

            ! Then optimize new basis functions
            call svm_step_by_step(nbas0, mnb)

        case (3)
            !-------------------------------------------------------------------
            ! Mode 3: SVM refinement of existing basis
            !-------------------------------------------------------------------
            write(*, '(A)') 'Mode: SVM refinement'

            ! Compute matrix elements for existing basis
            call preset_calculation(nbas0)

            ! Refine basis functions
            call svm_refinement(nbas1, nbas0)

        case default
            write(*, '(A,I3)') 'Error: Unknown solution mode ico = ', ico
            stop

    end select

    !---------------------------------------------------------------------------
    ! Print completion message
    !---------------------------------------------------------------------------
    write(*, '(A)') ''
    write(*, '(A)') '************************************************************'
    write(*, '(A)') '*   Calculation completed successfully                     *'
    write(*, '(A)') '************************************************************'
    write(*, '(A)') ''

end program few_body_system
