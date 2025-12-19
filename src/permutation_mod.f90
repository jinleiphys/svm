!===============================================================================
! MODULE: permutation_mod
!===============================================================================
!> @brief Permutation and symmetrization routines for identical particles
!>
!> This module handles the symmetrization of the wave function for systems
!> of identical particles. For fermions, the wave function must be
!> antisymmetric under particle exchange; for bosons, it must be symmetric.
!>
!> The module computes:
!> - All permutations of N particles
!> - Parity of each permutation
!> - Transformation matrices for permuting basis function parameters
!> - Spin-isospin matrix elements for each permutation
!===============================================================================
module permutation_mod
    use constants_mod
    use parameters_mod
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: compute_permutations
    public :: compute_spin_isospin_elements
    public :: permut
    public :: pape

contains

    !===========================================================================
    !> @brief Compute all permutations and their properties
    !>
    !> Generates all N! permutations of N particles and computes:
    !> - The permutation matrix representation
    !> - The parity (sign) of each permutation
    !> - The transformed permutation matrix in Jacobi coordinates
    !===========================================================================
    subroutine compute_permutations()
        implicit none

        integer :: ia(IFD)
        integer :: k, i, ip_sign

        ! Number of permutations = N!
        nper = FACTORIAL(npar)

        do k = 1, nper
            ! Initialize permutation matrix to zero
            perm_matrix(k, :, :) = 0.0_dp

            ! Get the k-th permutation
            call permut(k, npar, ia)

            ! Compute parity
            call pape(ia, npar, ip_sign)
            perm_parity(k) = real(ip_sign, dp)

            ! Store exchange indices
            do i = 1, npar
                iexc(k, i) = ia(i)
                perm_matrix(k, i, ia(i)) = 1.0_dp
            end do
        end do

        ! Transform permutation matrices to Jacobi coordinates
        call transform_permutations_to_jacobi()

    end subroutine compute_permutations

    !===========================================================================
    !> @brief Transform permutation matrices to Jacobi coordinates
    !>
    !> Computes C' = T * C * T^{-1} for each permutation matrix C,
    !> where T is the Jacobi transformation matrix.
    !===========================================================================
    subroutine transform_permutations_to_jacobi()
        implicit none

        real(dp) :: x(NFAC, MNPAR, MNPAR)
        real(dp) :: sum_val(NFAC)
        integer :: i, j, k, n

        ! First multiplication: X = C * T^{-1}
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    do n = 1, not_perm
                        sum_val(n) = sum_val(n) + perm_matrix(n, i, k) * ti_jacobi(k, j)
                    end do
                end do
                do n = 1, not_perm
                    x(n, i, j) = sum_val(n)
                end do
            end do
        end do

        ! Second multiplication: C = T * X
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    do n = 1, not_perm
                        sum_val(n) = sum_val(n) + t_jacobi(i, k) * x(n, k, j)
                    end do
                end do
                do n = 1, not_perm
                    perm_matrix(n, i, j) = sum_val(n)
                end do
            end do
        end do

    end subroutine transform_permutations_to_jacobi

    !===========================================================================
    !> @brief Compute spin-isospin matrix elements for all permutations
    !>
    !> For each permutation P, compute the matrix element:
    !> <chi_spin chi_isospin | P | chi_spin chi_isospin>
    !>
    !> This determines which permutations contribute to the symmetrized
    !> wave function based on the spin-isospin quantum numbers.
    !>
    !> Also computes the Wigner, Majorana, Bartlett, and Heisenberg
    !> operator matrix elements for each pair interaction.
    !===========================================================================
    subroutine compute_spin_isospin_elements()
        implicit none

        integer :: ia(IFD)
        real(dp) :: wm_temp((MNPAR*(MNPAR+1))/2)
        real(dp) :: wb_temp((MNPAR*(MNPAR+1))/2)
        real(dp) :: wh_temp((MNPAR*(MNPAR+1))/2)
        real(dp) :: ww, ccc
        integer :: k, i, j, l, m
        integer :: is1, is2, ii1, ii2
        integer :: i1, i2, i3, i4, k1, k2
        integer :: ip_sign, kp
        integer :: iw, im, ib, ih

        k = 0

        ! Loop over all permutations
        do i = 1, nper
            call permut(i, npar, ia)

            iw = 0
            im = 0
            ib = 0
            ih = 0
            ww = 0.0_dp
            wm_temp = 0.0_dp
            wb_temp = 0.0_dp
            wh_temp = 0.0_dp

            ! Loop over spin configurations
            do is1 = 1, nspc
                do is2 = 1, nspc
                    ! Loop over isospin configurations
                    do ii1 = 1, nisc
                        do ii2 = 1, nisc
                            ccc = cspc(is1) * cspc(is2) * cisc(ii1) * cisc(ii2)

                            !---------------------------------------------------
                            ! Wigner operator (identity): <P>
                            !---------------------------------------------------
                            call check_wigner(ia, is1, is2, ii1, ii2, ccc, ww, iw)

                            if (no == 1) cycle

                            !---------------------------------------------------
                            ! Majorana operator: space exchange P_ij^r
                            !---------------------------------------------------
                            call check_majorana(ia, is1, is2, ii1, ii2, ccc, &
                                               wm_temp, im)

                            !---------------------------------------------------
                            ! Bartlett operator: spin exchange P_ij^sigma
                            !---------------------------------------------------
                            call check_bartlett(ia, is1, is2, ii1, ii2, ccc, &
                                               wb_temp, ib)

                            !---------------------------------------------------
                            ! Heisenberg operator: P_ij^sigma * P_ij^tau
                            !---------------------------------------------------
                            call check_heisenberg(ia, is1, is2, ii1, ii2, ccc, &
                                                 wh_temp, ih)

                        end do
                    end do
                end do
            end do

            ! Skip if all matrix elements are zero
            if (im + iw + ib + ih == 0) cycle

            k = k + 1

            ! Compute parity of permutation
            call pape(ia, npar, ip_sign)
            perm_parity(k) = real(ip_sign, dp)

            ! Store permutation matrix
            perm_matrix(k, :, :) = 0.0_dp
            do l = 1, npar
                perm_matrix(k, ia(l), l) = 1.0_dp
            end do

            ! Store spin-isospin overlap
            spiso(k) = perm_parity(k) * ww

            ! Store operator matrix elements
            do kp = 1, kpot
                w_operator(k, kp, 1) = ww * ip_sign                ! Wigner
                w_operator(k, kp, 2) = -wm_temp(kp) * ip_sign      ! Majorana
                w_operator(k, kp, 3) = wb_temp(kp) * ip_sign       ! Bartlett
                w_operator(k, kp, 4) = -wh_temp(kp) * ip_sign      ! Heisenberg
            end do

        end do

        not_perm = k

        write(*, '(A,I5)') 'Number of contributing permutations: ', not_perm

        ! Transform permutation matrices to Jacobi coordinates
        call transform_permutations_to_jacobi()

        ! Store transformed matrices for use in matrix element calculation
        do k = 1, not_perm
            do m = 1, npar - 1
                do l = 1, npar - 1
                    trp(k, l, m) = perm_matrix(k, l, m)
                end do
            end do
        end do

    end subroutine compute_spin_isospin_elements

    !===========================================================================
    !> @brief Check Wigner operator contribution
    !===========================================================================
    subroutine check_wigner(ia, is1, is2, ii1, ii2, ccc, ww, iw)
        implicit none
        integer, intent(in) :: ia(IFD), is1, is2, ii1, ii2
        real(dp), intent(in) :: ccc
        real(dp), intent(inout) :: ww
        integer, intent(inout) :: iw

        integer :: j, i1, i2

        do j = 1, npar
            i1 = j
            i2 = ia(j)
            if (isp(i1, is1) - isp(i2, is2) /= 0) return
            if (iso(i1, ii1) - iso(i2, ii2) /= 0) return
        end do

        ww = ww + ccc
        iw = 1

    end subroutine check_wigner

    !===========================================================================
    !> @brief Check Majorana operator contribution
    !===========================================================================
    subroutine check_majorana(ia, is1, is2, ii1, ii2, ccc, wm, im)
        implicit none
        integer, intent(in) :: ia(IFD), is1, is2, ii1, ii2
        real(dp), intent(in) :: ccc
        real(dp), intent(inout) :: wm((MNPAR*(MNPAR+1))/2)
        integer, intent(inout) :: im

        integer :: l, j, m, kp
        integer :: i1, i2, i3, i4, k1, k2

        kp = 0
        do l = 1, npar
            do j = l + 1, npar
                i1 = l
                i2 = j
                i3 = ia(l)
                i4 = ia(j)
                kp = kp + 1

                ! Check spin exchange condition
                if (isp(i1, is1) - isp(i4, is2) /= 0) cycle
                if (isp(i2, is1) - isp(i3, is2) /= 0) cycle
                if (iso(i1, ii1) - iso(i4, ii2) /= 0) cycle
                if (iso(i2, ii1) - iso(i3, ii2) /= 0) cycle

                ! Check remaining particles
                do m = 1, npar
                    if (m == l .or. m == j) cycle
                    k1 = m
                    k2 = ia(m)
                    if (isp(k1, is1) - isp(k2, is2) /= 0) goto 220
                    if (iso(k1, ii1) - iso(k2, ii2) /= 0) goto 220
                end do

                im = 1
                wm(kp) = wm(kp) + ccc

220             continue
            end do
        end do

    end subroutine check_majorana

    !===========================================================================
    !> @brief Check Bartlett operator contribution
    !===========================================================================
    subroutine check_bartlett(ia, is1, is2, ii1, ii2, ccc, wb, ib)
        implicit none
        integer, intent(in) :: ia(IFD), is1, is2, ii1, ii2
        real(dp), intent(in) :: ccc
        real(dp), intent(inout) :: wb((MNPAR*(MNPAR+1))/2)
        integer, intent(inout) :: ib

        integer :: l, j, m, kp
        integer :: i1, i2, i3, i4, k1, k2

        kp = 0
        do l = 1, npar
            do j = l + 1, npar
                i1 = l
                i2 = j
                i3 = ia(l)
                i4 = ia(j)
                kp = kp + 1

                ! Check Bartlett exchange condition
                if (isp(i1, is1) - isp(i4, is2) /= 0) cycle
                if (isp(i2, is1) - isp(i3, is2) /= 0) cycle
                if (iso(i1, ii1) - iso(i3, ii2) /= 0) cycle
                if (iso(i2, ii1) - iso(i4, ii2) /= 0) cycle

                ! Check remaining particles
                do m = 1, npar
                    if (m == l .or. m == j) cycle
                    k1 = m
                    k2 = ia(m)
                    if (isp(k1, is1) - isp(k2, is2) /= 0) goto 320
                    if (iso(k1, ii1) - iso(k2, ii2) /= 0) goto 320
                end do

                ib = 1
                wb(kp) = wb(kp) + ccc

320             continue
            end do
        end do

    end subroutine check_bartlett

    !===========================================================================
    !> @brief Check Heisenberg operator contribution
    !===========================================================================
    subroutine check_heisenberg(ia, is1, is2, ii1, ii2, ccc, wh, ih)
        implicit none
        integer, intent(in) :: ia(IFD), is1, is2, ii1, ii2
        real(dp), intent(in) :: ccc
        real(dp), intent(inout) :: wh((MNPAR*(MNPAR+1))/2)
        integer, intent(inout) :: ih

        integer :: l, j, m, kp
        integer :: i1, i2, i3, i4, k1, k2

        kp = 0
        do l = 1, npar
            do j = l + 1, npar
                i1 = l
                i2 = j
                i3 = ia(l)
                i4 = ia(j)
                kp = kp + 1

                ! Check Heisenberg exchange condition
                if (isp(i1, is1) - isp(i3, is2) /= 0) cycle
                if (isp(i2, is1) - isp(i4, is2) /= 0) cycle
                if (iso(i1, ii1) - iso(i4, ii2) /= 0) cycle
                if (iso(i2, ii1) - iso(i3, ii2) /= 0) cycle

                ! Check remaining particles
                do m = 1, npar
                    if (m == l .or. m == j) cycle
                    k1 = m
                    k2 = ia(m)
                    if (isp(k1, is1) - isp(k2, is2) /= 0) goto 420
                    if (iso(k1, ii1) - iso(k2, ii2) /= 0) goto 420
                end do

                ih = 1
                wh(kp) = wh(kp) + ccc

420             continue
            end do
        end do

    end subroutine check_heisenberg

    !===========================================================================
    !> @brief Generate the n-th permutation of N elements
    !>
    !> Uses factorial number system to generate permutations in
    !> lexicographic order.
    !>
    !> @param[in]  nrp  Permutation index (1 to N!)
    !> @param[in]  n    Number of elements
    !> @param[out] ia   The permutation (ia(i) = where element i goes)
    !===========================================================================
    subroutine permut(nrp, n, ia)
        implicit none
        integer, intent(in) :: nrp, n
        integer, intent(out) :: ia(IFD)

        integer :: iv(IFD+1)
        integer :: i, m, io_val, in_val

        ! Initialize identity permutation
        do i = 1, n
            iv(i) = i
        end do

        io_val = nrp - 1

        ! Generate permutation using factorial number system
        do m = n - 1, 1, -1
            in_val = io_val / FACTORIAL(m) + 1
            io_val = mod(io_val, FACTORIAL(m))
            ia(n - m) = iv(in_val)
            ! Remove used element
            do i = in_val, m
                iv(i) = iv(i + 1)
            end do
        end do
        ia(n) = iv(1)

    end subroutine permut

    !===========================================================================
    !> @brief Compute parity of a permutation
    !>
    !> Counts the number of transpositions needed to sort the permutation,
    !> and returns +1 for even, -1 for odd (fermions) or +1 always (bosons).
    !>
    !> @param[in]  ia  The permutation
    !> @param[in]  n   Number of elements
    !> @param[out] ip  Parity: +1 (even/boson) or -1 (odd fermion)
    !===========================================================================
    subroutine pape(ia, n, ip)
        implicit none
        integer, intent(in) :: ia(IFD), n
        integer, intent(out) :: ip

        integer :: ib(IFD)
        integer :: i, j, mm, ii

        ! Copy permutation
        ib(1:n) = ia(1:n)

        ! Count transpositions by bubble sort
        ii = 0
        do i = 1, n
            do j = i, n
                if (i == ib(j) .and. i /= j) then
                    mm = ib(i)
                    ib(i) = ib(j)
                    ib(j) = mm
                    ii = ii + 1
                end if
            end do
        end do

        ! Parity
        ip = 1
        if (ibf == 1) ip = (-1)**ii  ! Fermions

    end subroutine pape

end module permutation_mod
