!===============================================================================
! MODULE: svm_mod
!===============================================================================
!> @brief Stochastic Variational Method (SVM) optimization routines
!>
!> This module implements the SVM algorithm for optimizing the nonlinear
!> parameters of correlated Gaussian basis functions. The method was
!> developed by K. Varga and Y. Suzuki (Phys. Rev. C52 (1995) 2885).
!>
!> Two optimization modes are available:
!>
!> 1. Step-by-step (ico=2): Build basis incrementally
!>    - Start with a small basis
!>    - Add one optimized basis function at a time
!>    - Optimize each new function to minimize energy
!>
!> 2. Refinement (ico=3): Improve existing basis
!>    - Start with a given basis
!>    - Replace one function at a time with a better one
!>    - Continue until convergence
!===============================================================================
module svm_mod
    use constants_mod
    use parameters_mod
    use random_mod, only: ran2
    use linear_algebra_mod, only: choldc
    use coordinate_transform_mod, only: trcorr, trcorri
    use matrix_elements_mod, only: compute_matrix_element
    use diagonalization_mod, only: diagonalize
    use io_mod, only: write_basis_file
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: svm_step_by_step
    public :: svm_refinement
    public :: preset_calculation
    public :: exchange_basis

contains

    !===========================================================================
    !> @brief Calculate with preset basis (ico=1)
    !>
    !> Computes matrix elements for all basis functions already defined
    !> in the a(:,:,:) array and performs diagonalization.
    !>
    !> @param[in] nbas_start  Number of basis functions to use
    !===========================================================================
    subroutine preset_calculation(nbas_start)
        implicit none
        integer, intent(in) :: nbas_start

        real(dp) :: ener
        integer :: i, id

        if (nbas_start <= 0) return

        id = 0

        ! Compute matrix elements for each basis function
        do i = 1, nbas_start
            call compute_matrix_element(i)

            id = id + 1

            ! Diagonalize every 50 basis functions for progress report
            if (id == 50) then
                id = 0
                call diagonalize(ener, i, 2)
                write(*, '(A,I5,A,ES15.8)') 'Basis size: ', i, '  Energy: ', ener
            end if
        end do

        ! Final diagonalization
        if (nbas_start > 0) then
            call diagonalize(ener, nbas_start, 2)
            write(*, '(A,I5,A,ES15.8)') 'Final basis size: ', nbas_start, &
                '  Energy: ', ener
        end if

    end subroutine preset_calculation

    !===========================================================================
    !> @brief SVM step-by-step optimization (ico=2)
    !>
    !> Builds the basis incrementally by adding one optimized function at
    !> a time. Each new function is optimized to minimize the ground state
    !> energy.
    !>
    !> @param[in] nbas_init  Starting basis size
    !> @param[in] nbas_max   Target basis size
    !===========================================================================
    subroutine svm_step_by_step(nbas_init, nbas_max)
        implicit none
        integer, intent(in) :: nbas_init, nbas_max

        real(dp) :: z((MNPAR*(MNPAR+1))/2)
        real(dp) :: elo, elow, xxx
        integer :: nbas, i, j, kk, nvar, ich0

        elo = LARGE_ENERGY

        ! Loop over basis size
        do nbas = nbas_init + 1, nbas_max

            elow = elo

            ! Try to find a good new basis function
555         continue
            kk = 0

            ! Generate random initial parameters
            do i = 1, npar
                do j = i + 1, npar
                    kk = kk + 1
                    xxx = ran2(irand)
                    ! z(kk) = 1 / (bmin + bmax*xxx)^2
                    z(kk) = 1.0_dp / (bmin + bmax * xxx)**2
                end do
            end do

            nvar = kk
            elo = elow

            ! Optimize the new basis function
            call optimize_new_basis(z, nvar, nbas, elo, ich0)

            ! If failed (linear dependence), try again
            if (ich0 == 1) goto 555

            ! Save current basis
            call write_basis_file(nbas_max, 1)

        end do

    end subroutine svm_step_by_step

    !===========================================================================
    !> @brief SVM refinement optimization (ico=3)
    !>
    !> Improves an existing basis by replacing one function at a time
    !> with a better optimized one.
    !>
    !> @param[in] nbas_start  Starting index for refinement
    !> @param[in] nbas_total  Total basis size
    !===========================================================================
    subroutine svm_refinement(nbas_start, nbas_total)
        implicit none
        integer, intent(in) :: nbas_start, nbas_total

        real(dp) :: z((MNPAR*(MNPAR+1))/2)
        real(dp) :: at(MNPAR, MNPAR)
        real(dp) :: ene, elow
        integer :: nbas, i, j, nvar

        ! Loop over basis functions to refine
        do nbas = nbas_start, nbas_total

            ! Get current energy
            call diagonalize(ene, nbas_total, 2)
            write(*, '(A,I5,A,ES15.8)') 'Refining ', nbas, '  Current E: ', ene

            ! Exchange this function with the last one
            call exchange_basis(nbas, nbas_total)

            ! Get energy without this function
            call diagonalize(ene, nbas_total - 1, 2)
            elow = ene

            ! Get current parameters in pairwise form
            nvar = (npar * (npar + 1)) / 2
            do i = 1, npar - 1
                do j = 1, npar - 1
                    at(i, j) = a(nbas_total, i, j)
                end do
            end do

            call trcorri(z, at)

            ! Try to find a better function
            call optimize_refinement(z, nvar, nbas_total, elow)

            ! Save improved basis
            call write_basis_file(mnb, nbas)

        end do

    end subroutine svm_refinement

    !===========================================================================
    !> @brief Optimize a new basis function (step-by-step)
    !>
    !> Randomly searches for optimal parameters that minimize the energy.
    !>
    !> @param[inout] zs    Initial/final parameter values
    !> @param[in]    nvar  Number of parameters
    !> @param[in]    nbas  Current basis index
    !> @param[inout] elow  Best energy found
    !> @param[out]   ich0  Status: 1=failed, 2=success
    !===========================================================================
    subroutine optimize_new_basis(zs, nvar, nbas, elow, ich0)
        implicit none
        real(dp), intent(inout) :: zs((MNPAR*(MNPAR+1))/2)
        integer, intent(in) :: nvar, nbas
        real(dp), intent(inout) :: elow
        integer, intent(out) :: ich0

        real(dp) :: hes(MNBAS), oes(MNBAS)
        real(dp) :: as(MNPAR, MNPAR)
        real(dp) :: z((MNPAR*(MNPAR+1))/2)
        real(dp) :: at(MNPAR, MNPAR)
        real(dp) :: xns, ener, esel, zzs, xxx, yyy, ph, www
        integer :: mm, kk, k, i, j, ierr

        ! Store current values for restoration
        do i = 1, npar - 1
            do j = 1, npar - 1
                as(j, i) = a(nbas, j, i)
            end do
        end do

        do i = 1, nbas
            hes(i) = he(i, nbas)
            oes(i) = oe(i, nbas)
        end do
        xns = xnorm(nbas)

        !-----------------------------------------------------------------------
        ! Optimization cycles
        !-----------------------------------------------------------------------
        do mm = 1, mm0

            z = zs

            ! Optimize each parameter
            do kk = 1, nvar
                zzs = z(kk)
                esel = elow

                ! Random trials for this parameter
                do k = 1, kk0

555                 continue
                    xxx = ran2(irand)
                    yyy = ran2(irand)
                    ph = 1.0_dp - 2.0_dp * yyy

                    ! Generate new random parameter
                    z(kk) = 1.0_dp / (bmin + bmax * xxx / xmr(kk))**2

                    ! Transform to Jacobi representation
                    call trcorr(z, at)

                    ! Check positive definiteness
                    call choldc(at, ierr)
                    if (ierr == 1) goto 555

                    ! Store new parameters
                    do i = 1, npar - 1
                        do j = 1, npar - 1
                            a(nbas, j, i) = at(i, j)
                        end do
                        xxx = ran2(irand)  ! Extra random call for compatibility
                    end do

                    ! Compute matrix elements
                    call compute_matrix_element(nbas)

                    ! Check for linear dependence
                    ich0 = 1
                    if (nbas < 5) ich0 = 2

                    do i = 1, nbas - 1
                        www = abs(oe(i, nbas) / sqrt(oe(i, i) * oe(nbas, nbas)))
                        if (www > 0.00001_dp) ich0 = 2
                        if (www > 0.99_dp) goto 555
                    end do

                    if (ich0 == 1) return

                    ! Get ground state energy
                    call diagonalize(ener, nbas, 1)

                    ! Keep best parameter value
                    if (ener < esel) then
                        esel = ener
                        zzs = z(kk)
                    end if

                    ! Store if best overall
                    if (ener < elow) then
                        elow = ener
                        zs = z

                        do i = 1, nbas
                            hes(i) = he(i, nbas)
                            oes(i) = oe(i, nbas)
                        end do
                        xns = xnorm(nbas)

                        do i = 1, npar - 1
                            do j = 1, npar - 1
                                as(j, i) = a(nbas, j, i)
                            end do
                        end do
                    end if

                end do  ! k loop

                z(kk) = zzs

            end do  ! kk loop

        end do  ! mm loop

        !-----------------------------------------------------------------------
        ! Restore best parameters found
        !-----------------------------------------------------------------------
        do i = 1, npar - 1
            do j = 1, npar - 1
                a(nbas, j, i) = as(j, i)
            end do
        end do

        do i = 1, nbas
            he(i, nbas) = hes(i)
            oe(i, nbas) = oes(i)
            he(nbas, i) = hes(i)
            oe(nbas, i) = oes(i)
        end do
        xnorm(nbas) = xns

        ! Final energy with full diagonalization
        call diagonalize(ener, nbas, 0)
        write(*, '(A,I5,A,ES15.8)') 'Basis size: ', nbas, '  Energy: ', ener

    end subroutine optimize_new_basis

    !===========================================================================
    !> @brief Optimize replacement function (refinement)
    !>
    !> @param[inout] zs    Initial/final parameter values
    !> @param[in]    nvar  Number of parameters
    !> @param[in]    nbas  Basis index to optimize
    !> @param[inout] elow  Best energy found
    !===========================================================================
    subroutine optimize_refinement(zs, nvar, nbas, elow)
        implicit none
        real(dp), intent(inout) :: zs((MNPAR*(MNPAR+1))/2)
        integer, intent(in) :: nvar, nbas
        real(dp), intent(inout) :: elow

        real(dp) :: hes(MNBAS), oes(MNBAS)
        real(dp) :: as(MNPAR, MNPAR)
        real(dp) :: z((MNPAR*(MNPAR+1))/2)
        real(dp) :: at(MNPAR, MNPAR)
        real(dp) :: xns, ener, esel, zzs, xxx, yyy, ph, www
        integer :: mm, kk, k, i, j, ierr

        ! Store current values
        do i = 1, npar - 1
            do j = 1, npar - 1
                as(j, i) = a(nbas, j, i)
            end do
        end do

        do i = 1, nbas
            hes(i) = he(i, nbas)
            oes(i) = oe(i, nbas)
        end do
        xns = xnorm(nbas)

        esel = elow

        !-----------------------------------------------------------------------
        ! Optimization cycles
        !-----------------------------------------------------------------------
        do mm = 1, mm0

            z = zs

            do kk = 1, nvar
                zzs = z(kk)

                do k = 1, kk0

555                 continue
                    xxx = ran2(irand)
                    yyy = ran2(irand)
                    ph = 1.0_dp - 2.0_dp * yyy

                    if (mm * kk * k /= 1) then
                        z(kk) = 1.0_dp / (bmin + bmax * xxx)**2
                    end if

                    call trcorr(z, at)
                    call choldc(at, ierr)
                    if (ierr == 1) goto 555

                    do i = 1, npar - 1
                        do j = 1, npar - 1
                            a(nbas, j, i) = at(i, j)
                        end do
                        xxx = ran2(irand)
                    end do

                    call compute_matrix_element(nbas)

                    ! Check overlap
                    do i = 1, nbas - 1
                        www = abs(oe(i, nbas) / sqrt(oe(i, i) * oe(nbas, nbas)))
                        if (www > 0.99_dp) goto 555
                    end do

                    call diagonalize(ener, nbas, 1)

                    if (ener < esel) then
                        esel = ener
                        zzs = z(kk)
                    end if

                    if (ener < elow) then
                        elow = ener
                        zs = z

                        do i = 1, nbas
                            hes(i) = he(i, nbas)
                            oes(i) = oe(i, nbas)
                        end do
                        xns = xnorm(nbas)

                        do i = 1, npar - 1
                            do j = 1, npar - 1
                                as(j, i) = a(nbas, j, i)
                            end do
                        end do
                    end if

                end do

                z(kk) = zzs

            end do

        end do

        !-----------------------------------------------------------------------
        ! Restore best parameters
        !-----------------------------------------------------------------------
        do i = 1, npar - 1
            do j = 1, npar - 1
                a(nbas, j, i) = as(j, i)
            end do
        end do

        do i = 1, nbas
            he(i, nbas) = hes(i)
            oe(i, nbas) = oes(i)
            he(nbas, i) = hes(i)
            oe(nbas, i) = oes(i)
        end do
        xnorm(nbas) = xns

        call diagonalize(ener, nbas, 0)
        write(*, '(A,I5,A,ES15.8)') 'Refined basis: ', nbas, '  Energy: ', ener

    end subroutine optimize_refinement

    !===========================================================================
    !> @brief Exchange two basis functions
    !>
    !> Swaps basis functions i and j, including matrix elements.
    !>
    !> @param[in] i  First basis index
    !> @param[in] j  Second basis index
    !===========================================================================
    subroutine exchange_basis(i_idx, j_idx)
        implicit none
        integer, intent(in) :: i_idx, j_idx

        integer :: ind(MNBAS)
        real(dp) :: c(MNBAS, MNBAS)
        real(dp) :: xnc(MNBAS)
        real(dp) :: ac(MNBAS, MNPAR, MNPAR)
        integer :: k, l, m, kk, ll

        ! Build index mapping
        do k = 1, nbas0
            ind(k) = k
        end do
        ind(i_idx) = j_idx
        ind(j_idx) = i_idx

        ! Exchange nonlinear parameters
        do m = 1, npar
            do l = 1, npar
                do k = 1, nbas0
                    kk = ind(k)
                    ac(k, l, m) = a(kk, l, m)
                end do
            end do
        end do

        ! Exchange Hamiltonian matrix
        do k = 1, nbas0
            kk = ind(k)
            do l = 1, nbas0
                ll = ind(l)
                c(l, k) = he(ll, kk)
            end do
        end do
        he(1:nbas0, 1:nbas0) = c(1:nbas0, 1:nbas0)

        ! Exchange overlap matrix
        do k = 1, nbas0
            kk = ind(k)
            do l = 1, nbas0
                ll = ind(l)
                c(l, k) = oe(ll, kk)
            end do
        end do
        oe(1:nbas0, 1:nbas0) = c(1:nbas0, 1:nbas0)

        ! Copy back parameters
        a(1:nbas0, :, :) = ac(1:nbas0, :, :)

        ! Exchange norms
        do k = 1, nbas0
            kk = ind(k)
            xnc(k) = xnorm(kk)
        end do
        xnorm(1:nbas0) = xnc(1:nbas0)

    end subroutine exchange_basis

end module svm_mod
