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
    use linear_algebra_mod, only: vdet, vinv, vtrafo, trafo_single, &
                                   vdet_explicit, vinv_explicit
    use potential_mod, only: poten
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Detailed timing for matrix element breakdown
    !---------------------------------------------------------------------------
    real(dp), save :: time_vtrafo = 0.0_dp
    real(dp), save :: time_vinv = 0.0_dp
    real(dp), save :: time_vdet = 0.0_dp
    real(dp), save :: time_vove = 0.0_dp
    real(dp), save :: time_vkin = 0.0_dp
    real(dp), save :: time_vpot = 0.0_dp
    real(dp), save :: time_accum = 0.0_dp

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: compute_matrix_element
    public :: vove_mat
    public :: vkin_ene
    public :: vpot_ene
    public :: print_matrix_elem_timing

contains

    !===========================================================================
    !> @brief Print detailed timing breakdown for matrix elements
    !===========================================================================
    subroutine print_matrix_elem_timing()
        implicit none
        real(dp) :: total

        total = time_vtrafo + time_vinv + time_vdet + time_vove + time_vkin + time_vpot + time_accum

        if (total > 0.0_dp) then
            write(*,'(A)') ''
            write(*,'(A)') '  Matrix Element Breakdown:'
            write(*,'(A,F10.3,A,F5.1,A)') '    vtrafo (T^T*A*T): ', time_vtrafo, ' s (', 100.0_dp*time_vtrafo/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    vinv (inverse):   ', time_vinv, ' s (', 100.0_dp*time_vinv/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    vdet (determinant):', time_vdet, ' s (', 100.0_dp*time_vdet/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    vove (overlap):   ', time_vove, ' s (', 100.0_dp*time_vove/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    vkin (kinetic):   ', time_vkin, ' s (', 100.0_dp*time_vkin/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    vpot (potential): ', time_vpot, ' s (', 100.0_dp*time_vpot/total, '%)'
            write(*,'(A,F10.3,A,F5.1,A)') '    accumulation:     ', time_accum, ' s (', 100.0_dp*time_accum/total, '%)'
        end if
    end subroutine print_matrix_elem_timing

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

        ! Thread-local arrays for OpenMP parallelization
        real(dp) :: xt(MNBAS)           ! Kinetic energy contributions
        real(dp) :: xo(MNBAS)           ! Overlap contributions
        real(dp) :: xv(MNBAS, 0:4)      ! Potential contributions
        real(dp) :: ap_transformed(MNPAR, MNPAR)  ! Single transformed matrix
        real(dp) :: tr_temp(MNPAR, MNPAR)
        real(dp) :: xxx, vvv

        ! Thread-local copies of shared arrays (for thread safety)
        real(dp) :: aap_local(MNBAS, MNPAR, MNPAR)
        real(dp) :: aapi_local(MNBAS, MNPAR, MNPAR)
        real(dp) :: det_local(MNBAS)
        real(dp) :: det_inv15_local(MNBAS)

        ! Thread-local accumulators
        real(dp) :: oe_local(MNBAS)
        real(dp) :: he_local(MNBAS)

        integer :: np, m, ip, i, j, k

        np = npar - 1

        !-----------------------------------------------------------------------
        ! Initialize matrix elements to zero
        !-----------------------------------------------------------------------
        do m = 1, nbas_in
            oe(m, nbas_in) = 0.0_dp
            he(m, nbas_in) = 0.0_dp
        end do

        !-----------------------------------------------------------------------
        ! Loop over all contributing permutations for symmetrization
        ! OpenMP parallel loop - each thread accumulates to local arrays,
        ! then critical section merges results
        !-----------------------------------------------------------------------
        !$omp parallel default(none) &
        !$omp shared(nbas_in, np, not_perm, trp, a, spiso, xnorm, &
        !$omp        w_operator, coulomb_coef, b_pair, kpot, no, xlambda, npar, oe, he) &
        !$omp private(ip, i, j, k, m, tr_temp, ap_transformed, aap_local, aapi_local, &
        !$omp         det_local, det_inv15_local, xo, xt, xv, xxx, vvv, oe_local, he_local)

        ! Initialize thread-local accumulators
        oe_local = 0.0_dp
        he_local = 0.0_dp

        !$omp do schedule(dynamic)
        do ip = 1, not_perm

            ! Get transformation matrix for this permutation
            do j = 1, np
                do i = 1, np
                    tr_temp(i, j) = trp(ip, i, j)
                end do
            end do

            ! Transform only the last basis function by permutation (T^T * A * T)
            call trafo_single(a(nbas_in,:,:), tr_temp, np, ap_transformed)

            ! Construct A + A' for overlap/determinant calculation (thread-local)
            do j = 1, np
                do i = 1, np
                    do k = 1, nbas_in
                        aap_local(k, i, j) = a(k, i, j) + ap_transformed(i, j)
                    end do
                end do
            end do

            ! Compute inverse and determinant of A + A' (thread-safe versions)
            call vinv_explicit(aap_local, aapi_local, np, nbas_in)
            call vdet_explicit(aap_local, det_local, det_inv15_local, np, nbas_in)

            ! Compute matrix elements using thread-local arrays
            call vove_mat_local(nbas_in, det_inv15_local, xo)
            call vkin_ene_local(nbas_in, np, aapi_local, det_inv15_local, xt)
            call vpot_ene_local(nbas_in, np, ip, aapi_local, det_inv15_local, xv)

            ! Accumulate contributions from this permutation to thread-local arrays
            do m = 1, nbas_in
                xxx = sqrt(xnorm(m) * xnorm(nbas_in))

                ! Overlap
                oe_local(m) = oe_local(m) + spiso(ip) * xo(m) / xxx

                ! Total potential (sum over operators)
                vvv = xv(m, 0) + xv(m, 1) + xv(m, 2) + xv(m, 3) + xv(m, 4)

                ! Hamiltonian = Kinetic + Potential
                he_local(m) = he_local(m) + (spiso(ip) * xt(m) + vvv) / xxx
            end do

        end do  ! End permutation loop
        !$omp end do

        ! Merge thread-local results into global arrays
        !$omp critical
        do m = 1, nbas_in
            oe(m, nbas_in) = oe(m, nbas_in) + oe_local(m)
            he(m, nbas_in) = he(m, nbas_in) + he_local(m)
        end do
        !$omp end critical

        !$omp end parallel

        ! Set norm
        xnorm(nbas_in) = 1.0_dp

        ! Symmetric matrix
        do m = 1, nbas_in
            oe(nbas_in, m) = oe(m, nbas_in)
            he(nbas_in, m) = he(m, nbas_in)
        end do

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
        ! Use precomputed det^{-1.5}
        do k = 1, nbas_in
            sum_out(k) = det_aap_inv15(k)
        end do

    end subroutine vove_mat

    !===========================================================================
    !> @brief Thread-safe overlap computation with explicit arrays
    !===========================================================================
    subroutine vove_mat_local(nbas_in, det_inv15_in, sum_out)
        implicit none
        integer, intent(in) :: nbas_in
        real(dp), intent(in) :: det_inv15_in(MNBAS)
        real(dp), intent(out) :: sum_out(MNBAS)

        integer :: k

        do k = 1, nbas_in
            sum_out(k) = det_inv15_in(k)
        end do

    end subroutine vove_mat_local

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
        ! Final result with determinant factor (use precomputed det^{-1.5})
        !-----------------------------------------------------------------------
        do n = 1, nbas_in
            sum_out(n) = (summ(n) + sum_out(n)) * det_aap_inv15(n)
        end do

    end subroutine vkin_ene

    !===========================================================================
    !> @brief Thread-safe kinetic energy computation with explicit arrays
    !===========================================================================
    subroutine vkin_ene_local(nbas_in, np_in, aapi_in, det_inv15_in, sum_out)
        implicit none
        integer, intent(in) :: nbas_in, np_in
        real(dp), intent(in) :: aapi_in(MNBAS, MNPAR, MNPAR)
        real(dp), intent(in) :: det_inv15_in(MNBAS)
        real(dp), intent(out) :: sum_out(MNBAS)

        real(dp) :: x(MNBAS, MNPAR, MNPAR)
        real(dp) :: summ(MNBAS)
        real(dp) :: w
        integer :: i, j, k, n

        ! Compute X = A * Lambda * A
        do i = 1, np_in
            do j = 1, np_in
                sum_out(1:nbas_in) = 0.0_dp
                do k = 1, np_in
                    do n = 1, nbas_in
                        w = -a(n, i, k) * xlambda(k)
                        sum_out(n) = sum_out(n) + w * a(n, k, j)
                    end do
                end do
                x(1:nbas_in, i, j) = sum_out(1:nbas_in)
            end do
        end do

        ! Compute Tr((A+A')^{-1} * X)
        sum_out(1:nbas_in) = 0.0_dp
        do i = 1, npar
            do k = 1, npar
                do n = 1, nbas_in
                    sum_out(n) = sum_out(n) + 3.0_dp * aapi_in(n, k, i) * x(n, k, i)
                end do
            end do
        end do

        ! Add diagonal contribution
        summ(1:nbas_in) = 0.0_dp
        do k = 1, np_in
            do n = 1, nbas_in
                summ(n) = summ(n) + xlambda(k) * a(n, k, k) * 3.0_dp
            end do
        end do

        ! Final result with determinant factor
        do n = 1, nbas_in
            sum_out(n) = (summ(n) + sum_out(n)) * det_inv15_in(n)
        end do

    end subroutine vkin_ene_local

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
            ! Compute u = b^T * (A+A')^{-1} * b for all basis functions
            ! This is the effective width parameter for this pair
            ! Optimized: precompute b(i)*b(j) outer product and vectorize
            !-------------------------------------------------------------------
            call compute_u_vectorized(nbas_in, np, k, u)

            !-------------------------------------------------------------------
            ! Compute potential integrals
            !-------------------------------------------------------------------
            call poten(nbas_in, u, p)

            !-------------------------------------------------------------------
            ! Accumulate contributions (use precomputed det^{-1.5})
            !-------------------------------------------------------------------
            do kk = 0, no
                do m = 1, nbas_in
                    www = p(m, kk) * det_aap_inv15(m) * y(kk)
                    v(m, kk) = v(m, kk) + www
                end do
            end do

        end do  ! End pair loop

    end subroutine vpot_ene

    !===========================================================================
    !> @brief Thread-safe potential energy computation with explicit arrays
    !===========================================================================
    subroutine vpot_ene_local(nbas_in, np_in, ip, aapi_in, det_inv15_in, v)
        implicit none
        integer, intent(in) :: nbas_in, np_in, ip
        real(dp), intent(in) :: aapi_in(MNBAS, MNPAR, MNPAR)
        real(dp), intent(in) :: det_inv15_in(MNBAS)
        real(dp), intent(out) :: v(MNBAS, 0:4)

        real(dp) :: u(MNBAS)
        real(dp) :: p(MNBAS, 0:4)
        real(dp) :: y(0:4)
        real(dp) :: yy, www
        integer :: k, m, kk

        v = 0.0_dp

        do k = 1, kpot
            y(0) = w_operator(ip, k, 1) * coulomb_coef(k)
            y(1) = w_operator(ip, k, 1)
            y(2) = w_operator(ip, k, 2)
            y(3) = w_operator(ip, k, 3)
            y(4) = w_operator(ip, k, 4)

            yy = abs(y(1)) + abs(y(2)) + abs(y(3)) + abs(y(4))
            if (yy == 0.0_dp) cycle

            call compute_u_local(nbas_in, np_in, k, aapi_in, u)
            call poten(nbas_in, u, p)

            do kk = 0, no
                do m = 1, nbas_in
                    www = p(m, kk) * det_inv15_in(m) * y(kk)
                    v(m, kk) = v(m, kk) + www
                end do
            end do
        end do

    end subroutine vpot_ene_local

    !===========================================================================
    !> @brief Thread-safe u computation with explicit aapi array
    !===========================================================================
    subroutine compute_u_local(nbas_in, np_in, k_pair, aapi_in, u)
        implicit none
        integer, intent(in) :: nbas_in, np_in, k_pair
        real(dp), intent(in) :: aapi_in(MNBAS, MNPAR, MNPAR)
        real(dp), intent(out) :: u(MNBAS)

        real(dp) :: bb(MNPAR, MNPAR)
        integer :: i, j, m

        do j = 1, np_in
            do i = 1, np_in
                bb(i, j) = b_pair(i, k_pair) * b_pair(j, k_pair)
            end do
        end do

        u(1:nbas_in) = 0.0_dp
        do j = 1, np_in
            do i = 1, np_in
                do m = 1, nbas_in
                    u(m) = u(m) + bb(i, j) * aapi_in(m, i, j)
                end do
            end do
        end do

    end subroutine compute_u_local

    !===========================================================================
    !> @brief Vectorized computation of u = b^T * (A+A')^{-1} * b
    !>
    !> Computes the quadratic form for all basis functions simultaneously.
    !> This is more cache-efficient than the nested loop version.
    !>
    !> @param[in]  nbas_in  Number of basis functions
    !> @param[in]  np       Dimension of the matrix
    !> @param[in]  k_pair   Pair index
    !> @param[out] u        Result array
    !===========================================================================
    subroutine compute_u_vectorized(nbas_in, np, k_pair, u)
        implicit none
        integer, intent(in) :: nbas_in, np, k_pair
        real(dp), intent(out) :: u(MNBAS)

        real(dp) :: bb(MNPAR, MNPAR)  ! Outer product b * b^T
        integer :: i, j, m

        ! Precompute outer product: bb(i,j) = b_pair(i,k) * b_pair(j,k)
        do j = 1, np
            do i = 1, np
                bb(i, j) = b_pair(i, k_pair) * b_pair(j, k_pair)
            end do
        end do

        ! Vectorized sum: u(m) = sum_{i,j} bb(i,j) * aapi(m,i,j)
        u(1:nbas_in) = 0.0_dp
        do j = 1, np
            do i = 1, np
                do m = 1, nbas_in
                    u(m) = u(m) + bb(i, j) * aapi(m, i, j)
                end do
            end do
        end do

    end subroutine compute_u_vectorized

end module matrix_elements_mod
