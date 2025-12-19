!===============================================================================
! MODULE: diagonalization_mod
!===============================================================================
!> @brief Eigenvalue problem solver for the SVM calculation
!>
!> This module solves the generalized eigenvalue problem:
!>   H * c = E * O * c
!>
!> where H is the Hamiltonian matrix, O is the overlap matrix, and E
!> are the energy eigenvalues.
!>
!> The module provides:
!> - Full diagonalization for all eigenvalues
!> - Fast lowest eigenvalue only (for optimization)
!> - Iterative update when adding one basis function
!===============================================================================
module diagonalization_mod
    use constants_mod
    use parameters_mod
    use linear_algebra_mod, only: solve_geneig
    use special_functions_mod, only: zerus
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: diagonalize
    public :: eigval_lowest
    public :: eigval_update

    !---------------------------------------------------------------------------
    ! Module variables for reduced basis storage
    !---------------------------------------------------------------------------
    integer, parameter :: MINDI = MNBAS  ! Maximum reduced dimension

contains

    !===========================================================================
    !> @brief Main diagonalization routine
    !>
    !> Dispatches to appropriate solver based on info flag:
    !> - info = 0: Full diag with eigenvector update
    !> - info = 1: Lowest eigenvalue only (fast)
    !> - info = 2: Full diagonalization
    !>
    !> @param[out] ener  Lowest eigenvalue (ground state energy)
    !> @param[in]  miki  Dimension of the basis
    !> @param[in]  info  Solver selection flag
    !===========================================================================
    subroutine diagonalize(ener, miki, info)
        implicit none
        real(dp), intent(out) :: ener
        integer, intent(in) :: miki, info

        real(dp) :: hd(MNBAS), od(MNBAS)
        real(dp) :: dl(MNBAS), e(MNBAS)
        integer :: i, j, ifail

        !-----------------------------------------------------------------------
        ! Fast path: lowest eigenvalue only
        !-----------------------------------------------------------------------
        if (info == 1 .and. diag_error /= 1 .and. &
            accuracy_flag /= 1 .and. miki >= 1) then
            call eigval_lowest(ener, miki)
            return
        end if

        !-----------------------------------------------------------------------
        ! Intermediate: update eigenvectors incrementally
        !-----------------------------------------------------------------------
        if (info == 0 .and. diag_error /= 1 .and. &
            accuracy_flag /= 1 .and. miki >= 1) then
            diag_counter = diag_counter + 1
            call eigval_update(ener, miki)
        end if

        !-----------------------------------------------------------------------
        ! Full diagonalization needed
        !-----------------------------------------------------------------------
        if (info == 2 .or. diag_counter == 1 .or. accuracy_flag == 1 .or. &
            diag_error == 1 .or. miki < 1) then

            ! Save diagonal elements (destroyed by diagonalization)
            do i = 1, miki
                hd(i) = he(i, i)
                od(i) = oe(i, i)
            end do

            ifail = 0
            diag_counter = 0

            ! Solve generalized eigenvalue problem
            call solve_geneig(miki, he(1:miki, 1:miki), oe(1:miki, 1:miki), &
                             eigenvalues(1:miki), eigenvectors(1:miki, 1:miki), ifail)

            ! Output energy levels to file
            open(16, file='ener.dat', status='replace')
            write(16, '(A,I5)') 'Dimension: ', miki
            write(16, '(A,ES20.12)') 'E(1) = ', eigenvalues(1)
            if (miki >= 2) write(16, '(A,ES20.12)') 'E(2) = ', eigenvalues(2)
            if (miki >= 3) write(16, '(A,ES20.12)') 'E(3) = ', eigenvalues(3)
            close(16)

            ! Restore matrix elements
            do i = 1, miki
                he(i, i) = hd(i)
                oe(i, i) = od(i)
            end do

            ! Restore symmetric structure
            do i = 1, miki
                do j = i, miki
                    he(j, i) = he(i, j)
                    oe(j, i) = oe(i, j)
                end do
            end do

            ener = eigenvalues(1)

            ! Compress eigenvectors for large basis (optional)
            if (miki >= MINDI) then
                call compress_eigenvectors(miki)
            end if

        end if

    end subroutine diagonalize

    !===========================================================================
    !> @brief Compute lowest eigenvalue only (fast method)
    !>
    !> Uses the previously computed eigenvectors to estimate the new
    !> lowest eigenvalue when one basis function is added. This is much
    !> faster than full diagonalization during optimization.
    !>
    !> @param[out] ener  Lowest eigenvalue estimate
    !> @param[in]  miki  Current basis dimension
    !===========================================================================
    subroutine eigval_lowest(ener, miki)
        implicit none
        real(dp), intent(out) :: ener
        integer, intent(in) :: miki

        real(dp) :: psi(MNBAS), hpsi(MNBAS)
        real(dp) :: s, xnb, xacc, brac
        integer :: mi, i, j, ifa

        mi = min(miki, MINDI)

        psi = 0.0_dp

        !-----------------------------------------------------------------------
        ! Compute overlap of new basis state with previous eigenstates
        !-----------------------------------------------------------------------
        do i = 1, mi - 1
            s = 0.0_dp
            do j = 1, miki - 1
                s = s + eigenvectors(j, i) * oe(j, miki)
            end do
            psi(i) = s
        end do

        !-----------------------------------------------------------------------
        ! Compute norm of new state in orthogonalized basis
        !-----------------------------------------------------------------------
        s = oe(miki, miki)
        do i = 1, mi - 1
            s = s - psi(i)**2
        end do

        if (s <= 0.0_dp) then
            ener = HUGE_ENERGY
            return
        end if
        xnb = sqrt(s)

        !-----------------------------------------------------------------------
        ! Compute Hamiltonian matrix elements in reduced basis
        !-----------------------------------------------------------------------
        hpsi = 0.0_dp
        do i = 1, mi - 1
            s = 0.0_dp
            do j = 1, miki - 1
                s = s + eigenvectors(j, i) * he(j, miki)
            end do
            hpsi(i) = s
        end do

        ! Off-diagonal coupling
        do i = 1, mi - 1
            q_work(i) = (hpsi(i) - eigenvalues(i) * psi(i)) / xnb
        end do

        ! Diagonal element of new state
        s = he(miki, miki)
        do i = 1, mi - 1
            s = s + eigenvalues(i) * psi(i)**2 - 2.0_dp * hpsi(i) * psi(i)
        end do
        ee_work(mi) = s / xnb**2

        ! Copy previous eigenvalues
        do i = 1, mi - 1
            ee_work(i) = eigenvalues(i)
        end do

        ! Special case: first basis function
        if (miki == 1) then
            ener = ee_work(1)
            return
        end if

        !-----------------------------------------------------------------------
        ! Find root of characteristic polynomial
        !-----------------------------------------------------------------------
        mi_work = mi
        xacc = 1.0d-12
        brac = 0.05_dp

        do while (brac <= 1000.0_dp)
            brac = 2.0_dp * brac
            x1_bound = eigenvalues(1) - brac
            x2_bound = eigenvalues(1)
            ifa = 0

            ener = zerus(x1_bound, x2_bound, xacc, ifa)

            if (ifa == 0) return
        end do

        ! Failed to find root
        ener = HUGE_ENERGY

    end subroutine eigval_lowest

    !===========================================================================
    !> @brief Update eigenvectors after adding one basis function
    !>
    !> Performs a small eigenvalue problem to update the eigenvectors
    !> when one new basis function is added. More accurate than
    !> eigval_lowest but still faster than full diagonalization.
    !>
    !> @param[out] ener  Updated lowest eigenvalue
    !> @param[in]  miki  Current basis dimension
    !===========================================================================
    subroutine eigval_update(ener, miki)
        implicit none
        real(dp), intent(out) :: ener
        integer, intent(in) :: miki

        integer, parameter :: MINDI_UPDATE = 100
        real(dp) :: psi(MINDI_UPDATE), hpsi(MINDI_UPDATE)
        real(dp) :: a_small(MINDI_UPDATE, MINDI_UPDATE)
        real(dp) :: b_small(MINDI_UPDATE, MINDI_UPDATE)
        real(dp) :: w(MNBAS, MINDI_UPDATE)
        real(dp) :: u(MINDI_UPDATE, MINDI_UPDATE)
        real(dp) :: d_eig(MINDI_UPDATE), e_work(MINDI_UPDATE)
        real(dp) :: xx(MINDI_UPDATE)
        real(dp) :: s, sss
        integer :: mi, i, j, k, ifail

        mi = min(miki, MINDI_UPDATE)

        ! Initialize new eigenvector column
        eigenvectors(1:miki, mi) = 0.0_dp
        eigenvectors(miki, mi) = 1.0_dp

        psi = 0.0_dp

        !-----------------------------------------------------------------------
        ! Compute overlaps with previous eigenstates
        !-----------------------------------------------------------------------
        do i = 1, mi - 1
            s = 0.0_dp
            do j = 1, miki - 1
                s = s + eigenvectors(j, i) * oe(j, miki)
            end do
            psi(i) = s
        end do
        psi(mi) = oe(miki, miki)

        !-----------------------------------------------------------------------
        ! Compute Hamiltonian in reduced eigenvector basis
        !-----------------------------------------------------------------------
        hpsi = 0.0_dp
        do i = 1, mi - 1
            s = 0.0_dp
            do j = 1, miki - 1
                s = s + eigenvectors(j, i) * he(j, miki)
            end do
            hpsi(i) = s
        end do
        hpsi(mi) = he(miki, miki)

        !-----------------------------------------------------------------------
        ! Build small eigenvalue problem
        !-----------------------------------------------------------------------
        a_small = 0.0_dp
        b_small = 0.0_dp

        do i = 1, mi - 1
            a_small(i, i) = eigenvalues(i)
            b_small(i, i) = 1.0_dp
            a_small(mi, i) = hpsi(i)
            a_small(i, mi) = hpsi(i)
            b_small(i, mi) = psi(i)
            b_small(mi, i) = psi(i)
        end do
        a_small(mi, mi) = hpsi(mi)
        b_small(mi, mi) = psi(mi)

        !-----------------------------------------------------------------------
        ! Solve small eigenvalue problem
        !-----------------------------------------------------------------------
        ifail = 0
        call solve_geneig(mi, a_small(1:mi, 1:mi), b_small(1:mi, 1:mi), &
                         xx(1:mi), u(1:mi, 1:mi), ifail)

        if (ifail /= 0) then
            write(*, '(A,I5)') 'Warning: eigval_update failed, ifail = ', ifail
        end if

        ener = xx(1)

        !-----------------------------------------------------------------------
        ! Update full eigenvectors
        !-----------------------------------------------------------------------
        do i = 1, mi
            do j = 1, miki
                sss = 0.0_dp
                do k = 1, mi
                    sss = sss + eigenvectors(j, k) * u(k, i)
                end do
                w(j, i) = sss
            end do
        end do

        ! Store updated eigenvectors and eigenvalues
        if (mi < MINDI_UPDATE) then
            do i = 1, miki
                do j = 1, mi
                    eigenvectors(i, j) = w(i, j)
                end do
                eigenvalues(i) = xx(i)
            end do
        else
            ! Compress to MINDI_UPDATE-1 vectors
            do i = 1, miki
                do j = 1, mi - 2
                    eigenvectors(i, j) = w(i, j)
                end do
                eigenvectors(i, mi-1) = (w(i, mi) + w(i, mi-1)) / sqrt(2.0_dp)
            end do
            do i = 1, mi - 2
                eigenvalues(i) = xx(i)
            end do
            eigenvalues(mi-1) = (xx(mi) + xx(mi-1)) / 2.0_dp
        end if

    end subroutine eigval_update

    !===========================================================================
    !> @brief Compress eigenvectors for large basis
    !>
    !> Reduces storage by combining high-lying eigenvectors.
    !>
    !> @param[in] miki  Basis dimension
    !===========================================================================
    subroutine compress_eigenvectors(miki)
        implicit none
        integer, intent(in) :: miki

        real(dp) :: ss
        integer :: i, j

        ! Combine last few eigenvectors
        do i = 1, miki
            ss = 0.0_dp
            do j = MINDI - 1, miki
                ss = ss + eigenvectors(i, j)
            end do
            eigenvectors(i, MINDI-1) = ss / sqrt(real(miki - MINDI + 2, dp))
        end do

        ! Average eigenvalues
        ss = 0.0_dp
        do j = MINDI - 1, miki
            ss = ss + eigenvalues(j)
        end do
        eigenvalues(MINDI-1) = ss / real(miki - MINDI + 2, dp)

    end subroutine compress_eigenvectors

end module diagonalization_mod
