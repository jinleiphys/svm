!===============================================================================
! MODULE: io_mod
!===============================================================================
!> @brief Input/Output routines for the SVM program
!>
!> This module handles:
!> - Reading input parameters from fbs.inp
!> - Reading potential parameters from pot.inp
!> - Reading/writing basis functions from/to fbs.res
!> - Output of energy results
!===============================================================================
module io_mod
    use constants_mod
    use parameters_mod
    use special_functions_mod, only: init_factorial_tables
    use potential_mod, only: init_potential_interpolation
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: read_input_data
    public :: read_basis_file
    public :: write_basis_file

contains

    !===========================================================================
    !> @brief Read all input data from files
    !>
    !> Reads:
    !> - pot.inp: Potential parameters
    !> - fbs.inp: System and calculation parameters
    !>
    !> Also initializes:
    !> - Factorial tables for special functions
    !> - Potential interpolation tables (if ipcon = 1)
    !===========================================================================
    subroutine read_input_data()
        implicit none

        real(dp) :: z_charge(MNPAR)
        real(dp) :: hh
        integer :: i, j, k
        integer :: io_pot, io_fbs

        io_pot = 4
        io_fbs = 1

        !-----------------------------------------------------------------------
        ! Initialize factorial tables first
        !-----------------------------------------------------------------------
        call init_factorial_tables()

        !-----------------------------------------------------------------------
        ! Read potential parameters from pot.inp
        !-----------------------------------------------------------------------
        open(io_pot, file='pot.inp', status='old', action='read')

        ! Potential representation type
        read(io_pot, *) ipcon

        ! Number of potential terms and operators
        read(io_pot, *) npt, no

        ! Read potential parameters for each operator and term
        do k = 1, no
            do i = 1, npt
                ! V(r) = vp * r^np * exp(-ap*r^2 + bp*r)
                read(io_pot, *) vp(i, k), ap(i, k), bp(i, k), np_pot(i, k)
            end do
        end do

        if (no == 0) no = 1

        close(io_pot)

        !-----------------------------------------------------------------------
        ! Generate potential interpolation table if needed
        !-----------------------------------------------------------------------
        if (ipcon == 1) then
            call init_potential_interpolation()

            ! Read back the interpolation data
            open(10, file='potrep.dat', status='old')
            read(10, *) xll, xul, nv0

            do i = 1, nv0
                def_grid(i) = xll + (i - 1) * (xul - xll) / real(nv0, dp)
            end do

            do k = 1, no
                do i = 1, nv0
                    read(10, *) fff_pot(i, k)
                end do
            end do
            close(10)
        end if

        !-----------------------------------------------------------------------
        ! Read system parameters from fbs.inp
        !-----------------------------------------------------------------------
        open(io_fbs, file='fbs.inp', status='old', action='read')

        ! Number of particles
        read(io_fbs, *) npar

        ! Particle masses
        read(io_fbs, *) (xm(i), i = 1, npar)

        ! Particle charges
        read(io_fbs, *) (z_charge(i), i = 1, npar)

        ! Number of isospin configurations
        read(io_fbs, *) nisc

        ! Read isospin configurations
        do k = 1, nisc
            read(io_fbs, *) cisc(k), (iso(i, k), i = 1, npar)
        end do

        ! Number of spin configurations
        read(io_fbs, *) nspc

        ! Read spin configurations
        do k = 1, nspc
            read(io_fbs, *) cspc(k), (isp(i, k), i = 1, npar)
        end do

        ! Control parameters
        ! hh = hbar^2/m (kinetic energy coefficient)
        ! irand = random seed
        ! ico = solution type (1=preset, 2=step-by-step, 3=refinement)
        ! ibf = statistics (1=fermions, 2=bosons)
        read(io_fbs, *) hh, irand, ico, ibf

        h2m = 0.5_dp * hh

        ! Optimization parameters
        ! mm0 = number of optimization cycles
        ! kk0 = random trials per parameter
        ! mnb = target basis dimension
        read(io_fbs, *) mm0, kk0, mnb

        ! Parameter generation range
        read(io_fbs, *) bmin, bmax

        close(io_fbs)

        !-----------------------------------------------------------------------
        ! Compute Coulomb potential coefficients
        !-----------------------------------------------------------------------
        k = 0
        do i = 1, npar
            do j = i + 1, npar
                k = k + 1
                coulomb_coef(k) = z_charge(i) * z_charge(j)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Compute mass-dependent scaling factors for parameter generation
        !-----------------------------------------------------------------------
        k = 0
        do i = 1, npar
            do j = i + 1, npar
                k = k + 1
                if (coulomb_coef(k) == 0.0_dp) then
                    ! Neutral particles: scale by reduced mass
                    xmr(k) = 2.0_dp * xm(i) * xm(j) / (xm(i) + xm(j))
                else
                    ! Charged particles: no mass scaling
                    xmr(k) = 1.0_dp
                end if
            end do
        end do

        !-----------------------------------------------------------------------
        ! Print summary
        !-----------------------------------------------------------------------
        write(*, '(A)') '=================================================='
        write(*, '(A)') '  Stochastic Variational Method (SVM) Calculation'
        write(*, '(A)') '=================================================='
        write(*, '(A,I3)') 'Number of particles: ', npar
        write(*, '(A,I3)') 'Solution type (ico): ', ico
        write(*, '(A,I3)') 'Statistics (1=F,2=B): ', ibf
        write(*, '(A,I5)') 'Target basis size: ', mnb
        write(*, '(A)') '=================================================='

    end subroutine read_input_data

    !===========================================================================
    !> @brief Read basis functions from file
    !>
    !> Reads previously computed basis function parameters from fbs.res.
    !> Returns the number of basis functions read.
    !>
    !> @param[out] nbas_read   Number of basis functions read
    !> @param[out] nbas_start  Starting index for optimization
    !> @param[out] file_exists Whether the file exists
    !===========================================================================
    subroutine read_basis_file(nbas_read, nbas_start, file_exists)
        implicit none
        integer, intent(out) :: nbas_read, nbas_start
        logical, intent(out) :: file_exists

        integer :: i, j, k
        integer :: io_unit

        io_unit = 15

        ! Check if file exists
        inquire(file='fbs.res', exist=file_exists)

        if (.not. file_exists) then
            nbas_read = 0
            nbas_start = 0
            return
        end if

        ! Read basis functions
        open(io_unit, file='fbs.res', status='old', action='read')

        read(io_unit, *) nbas_read, nbas_start

        do i = 1, nbas_read
            read(io_unit, *) ((a(i, j, k), j = 1, npar), k = 1, npar)
        end do

        close(io_unit)

        write(*, '(A,I5,A)') 'Read ', nbas_read, ' basis functions from fbs.res'

    end subroutine read_basis_file

    !===========================================================================
    !> @brief Write basis functions to file
    !>
    !> Saves the current basis function parameters to fbs.res.
    !>
    !> @param[in] nbas_total  Total number of basis functions
    !> @param[in] nbas_index  Current optimization index
    !===========================================================================
    subroutine write_basis_file(nbas_total, nbas_index)
        implicit none
        integer, intent(in) :: nbas_total, nbas_index

        integer :: i, j, k
        integer :: io_unit

        io_unit = 10

        open(io_unit, file='fbs.res', status='replace', action='write')

        write(io_unit, *) nbas_total, nbas_index

        do i = 1, nbas_total
            write(io_unit, *) ((a(i, k, j), k = 1, npar), j = 1, npar)
        end do

        close(io_unit)

    end subroutine write_basis_file

end module io_mod
