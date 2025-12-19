!===============================================================================
! MODULE: matrix_elements_mod
!===============================================================================
!> @brief Matrix element calculation for Hamiltonian and overlap
!>
!> This module computes the matrix elements of the Hamiltonian H and
!> overlap O between correlated Gaussian basis functions:
!>
!>   phi_i(r) = exp(-r^T A_i r)
!>
!> The matrix elements involve:
!> - Kinetic energy: T = -hbar^2/(2m) * nabla^2
!> - Potential energy: V(r_ij) for all pairs
!> - Overlap: <phi_i | phi_j>
!>
!> The calculation includes proper symmetrization for identical particles.
!===============================================================================
module matrix_elements_mod
    use constants_mod
    use parameters_mod
    use linear_algebra_mod, only: vdet, vinv, vtrafo
    use potential_mod, only: poten
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: compute_matrix_element
    public :: vove_mat
    public :: vkin_ene
    public :: vpot_ene

contains

    !===========================================================================
    !> @brief Compute matrix elements for a single basis state
    !>
    !> Computes all matrix elements <phi_m | H | phi_nbas> and
    !> <phi_m | phi_nbas> for m = 1, ..., nbas.
    !>
    !> The calculation loops over all contributing permutations to
    !> properly symmetrize the wave function.
    !>
    !> @param[in] nbas  Index of the new basis state
    !===========================================================================
    subroutine compute_matrix_element(nbas_in)
        implicit none
        integer, intent(in) :: nbas_in

        real(dp) :: xt(MNBAS)           ! Kinetic energy contributions
        real(dp) :: xo(MNBAS)           ! Overlap contributions
        real(dp) :: xv(MNBAS, 0:4)      ! Potential contributions
        real(dp) :: ap_temp(MNBAS, MNPAR, MNPAR)
        real(dp) :: tr_temp(MNPAR, MNPAR)
        real(dp) :: xxx, vvv
        integer :: np, m, ip, i, j, k

        np = npar - 1

        !-----------------------------------------------------------------------
        ! Initialize matrix elements to zero
        !-----------------------------------------------------------------------
        do m = 1, nbas_in
            oe(m, nbas_in) = 0.0_dp
            he(m, nbas_in) = 0.0_dp
            oe(nbas_in, m) = 0.0_dp
            he(nbas_in, m) = 0.0_dp
        end do

        !-----------------------------------------------------------------------
        ! Loop over all contributing permutations for symmetrization
        !-----------------------------------------------------------------------
        do ip = 1, not_perm

            ! Copy basis function parameters
            do j = 1, np
                do i = 1, np
                    do k = 1, nbas_in
                        ap_temp(k, i, j) = a(k, i, j)
                    end do
                end do
            end do

            ! Get transformation matrix for this permutation
            do j = 1, np
                do i = 1, np
                    tr_temp(i, j) = trp(ip, i, j)
                end do
            end do

            ! Transform last basis function by permutation
            call vtrafo(ap_temp, tr_temp, np, nbas_in)

            ! Construct A + A' for overlap/determinant calculation
            do j = 1, np
                do i = 1, np
                    do k = 1, nbas_in
                        aap(k, i, j) = a(k, i, j) + ap_temp(nbas_in, i, j)
                    end do
                end do
            end do

            ! Compute inverse and determinant of A + A'
            call vinv(np, nbas_in)
            call vdet(np, nbas_in)

            ! Compute matrix elements
            call vove_mat(nbas_in, xo)           ! Overlap
            call vkin_ene(nbas_in, xt)           ! Kinetic energy
            call vpot_ene(nbas_in, ip, xv)       ! Potential energy

            ! Store norm on first permutation
            if (ip == 1) xnorm(nbas_in) = 1.0_dp

            ! Accumulate contributions from this permutation
            do m = 1, nbas_in
                xxx = sqrt(xnorm(m) * xnorm(nbas_in))

                ! Overlap
                oe(m, nbas_in) = oe(m, nbas_in) + spiso(ip) * xo(m) / xxx

                ! Total potential (sum over operators)
                vvv = xv(m, 0) + xv(m, 1) + xv(m, 2) + xv(m, 3) + xv(m, 4)

                ! Hamiltonian = Kinetic + Potential
                he(m, nbas_in) = he(m, nbas_in) + &
                    (spiso(ip) * xt(m) + vvv) / xxx

                ! Symmetric matrix
                oe(nbas_in, m) = oe(m, nbas_in)
                he(nbas_in, m) = he(m, nbas_in)
            end do

        end do  ! End permutation loop

    end subroutine compute_matrix_element

    !===========================================================================
    !> @brief Compute overlap matrix elements
    !>
    !> The overlap between two Gaussians is:
    !> <exp(-r^T A r) | exp(-r^T A' r)> = (pi)^(n/2) / det(A+A')^(3/2)
    !>
    !> @param[in]  nbas  Number of basis states
    !> @param[out] sum_out  Overlap values for each pair
    !===========================================================================
    subroutine vove_mat(nbas_in, sum_out)
        implicit none
        integer, intent(in) :: nbas_in
        real(dp), intent(out) :: sum_out(MNBAS)

        integer :: k

        ! Overlap = 1 / det(A+A')^(3/2)
        ! (Factor of pi^(n/2) absorbed in normalization)
        do k = 1, nbas_in
            sum_out(k) = 1.0_dp / det_aap(k)**1.5_dp
        end do

    end subroutine vove_mat

    !===========================================================================
    !> @brief Compute kinetic energy matrix elements
    !>
    !> The kinetic energy for Gaussian basis functions:
    !> <phi | T | phi'> = sum_i lambda_i * [3*A_ii - 2*Tr(A (A+A')^{-1} lambda A)]
    !>                    / det(A+A')^(3/2)
    !>
    !> where lambda_i = hbar^2 / (2 * mu_i) is the kinetic energy coefficient
    !> for Jacobi coordinate i.
    !>
    !> @param[in]  nbas     Number of basis states
    !> @param[out] sum_out  Kinetic energy values
    !===========================================================================
    subroutine vkin_ene(nbas_in, sum_out)
        implicit none
        integer, intent(in) :: nbas_in
        real(dp), intent(out) :: sum_out(MNBAS)

        real(dp) :: x(MNBAS, MNPAR, MNPAR)
        real(dp) :: summ(MNBAS)
        real(dp) :: w
        integer :: np, i, j, k, n

        np = npar - 1

        !-----------------------------------------------------------------------
        ! Compute X = A * Lambda * A where Lambda is diagonal kinetic matrix
        !-----------------------------------------------------------------------
        do i = 1, np
            do j = 1, np
                sum_out(1:nbas_in) = 0.0_dp
                do k = 1, np
                    do n = 1, nbas_in
                        w = -a(n, i, k) * xlambda(k)
                        sum_out(n) = sum_out(n) + w * a(n, k, j)
                    end do
                end do
                x(1:nbas_in, i, j) = sum_out(1:nbas_in)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Compute Tr((A+A')^{-1} * X)
        !-----------------------------------------------------------------------
        sum_out(1:nbas_in) = 0.0_dp
        do i = 1, npar
            do k = 1, npar
                do n = 1, nbas_in
                    sum_out(n) = sum_out(n) + 3.0_dp * aapi(n, k, i) * x(n, k, i)
                end do
            end do
        end do

        !-----------------------------------------------------------------------
        ! Add diagonal contribution: 3 * sum_i lambda_i * A_ii
        !-----------------------------------------------------------------------
        summ(1:nbas_in) = 0.0_dp
        do k = 1, np
            do n = 1, nbas_in
                summ(n) = summ(n) + xlambda(k) * a(n, k, k) * 3.0_dp
            end do
        end do

        !-----------------------------------------------------------------------
        ! Final result with determinant factor
        !-----------------------------------------------------------------------
        do n = 1, nbas_in
            sum_out(n) = (summ(n) + sum_out(n)) / det_aap(n)**1.5_dp
        end do

    end subroutine vkin_ene

    !===========================================================================
    !> @brief Compute potential energy matrix elements
    !>
    !> The potential is a sum over pair interactions:
    !> V = sum_{i<j} V_ij(|r_i - r_j|)
    !>
    !> Each pair contribution is computed using the Gaussian integral
    !> formula and includes the appropriate operator weights for
    !> Wigner, Majorana, Bartlett, and Heisenberg terms.
    !>
    !> @param[in]  nbas     Number of basis states
    !> @param[in]  ip       Permutation index
    !> @param[out] v        Potential energy for each operator type
    !===========================================================================
    subroutine vpot_ene(nbas_in, ip, v)
        implicit none
        integer, intent(in) :: nbas_in, ip
        real(dp), intent(out) :: v(MNBAS, 0:4)

        real(dp) :: u(MNBAS)            ! 1/(2*a) parameter
        real(dp) :: p(MNBAS, 0:4)       ! Potential integrals
        real(dp) :: y(0:4)              ! Operator weights
        real(dp) :: w, w1, yy, www
        integer :: np, k, m, i, j, kk

        np = npar - 1

        ! Initialize
        v = 0.0_dp

        !-----------------------------------------------------------------------
        ! Loop over all pairs
        !-----------------------------------------------------------------------
        do k = 1, kpot
            ! Get operator weights for this pair and permutation
            y(0) = w_operator(ip, k, 1) * coulomb_coef(k)  ! Coulomb
            y(1) = w_operator(ip, k, 1)                     ! Wigner
            y(2) = w_operator(ip, k, 2)                     ! Majorana
            y(3) = w_operator(ip, k, 3)                     ! Bartlett
            y(4) = w_operator(ip, k, 4)                     ! Heisenberg

            ! Check if any contribution
            yy = abs(y(1)) + abs(y(2)) + abs(y(3)) + abs(y(4))
            if (yy == 0.0_dp) cycle

            !-------------------------------------------------------------------
            ! Compute u = (r_i - r_j)^T * (A+A')^{-1} * (r_i - r_j)
            ! This is the effective width parameter for this pair
            !-------------------------------------------------------------------
            u(1:nbas_in) = 0.0_dp
            do i = 1, np
                w1 = b_pair(i, k)
                do j = 1, np
                    w = w1 * b_pair(j, k)
                    do m = 1, nbas_in
                        u(m) = u(m) + w * aapi(m, i, j)
                    end do
                end do
            end do

            !-------------------------------------------------------------------
            ! Compute potential integrals
            !-------------------------------------------------------------------
            call poten(nbas_in, u, p)

            !-------------------------------------------------------------------
            ! Accumulate contributions
            !-------------------------------------------------------------------
            do kk = 0, no
                do m = 1, nbas_in
                    www = p(m, kk) / det_aap(m)**1.5_dp * y(kk)
                    v(m, kk) = v(m, kk) + www
                end do
            end do

        end do  ! End pair loop

    end subroutine vpot_ene

end module matrix_elements_mod
