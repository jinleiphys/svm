!===============================================================================
! MODULE: coordinate_transform_mod
!===============================================================================
!> @brief Coordinate transformation routines for Jacobi coordinates
!>
!> This module handles the transformation between single-particle coordinates
!> and Jacobi (relative) coordinates, which are essential for separating
!> the center-of-mass motion in few-body systems.
!>
!> Jacobi coordinates for N particles:
!> - xi_1 = r_2 - r_1 (relative coordinate of first two particles)
!> - xi_2 = r_3 - (m1*r1 + m2*r2)/(m1+m2) (third particle relative to CM of first two)
!> - ... and so on
!> - xi_N = center of mass of all particles
!>
!> The transformation is defined by: xi = T * r
!> where r = (r_1, r_2, ..., r_N)^T and xi = (xi_1, ..., xi_N)^T
!===============================================================================
module coordinate_transform_mod
    use constants_mod
    use parameters_mod
    use linear_algebra_mod, only: ludcmp, lubksb
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: compute_jacobi_transform
    public :: trcorr
    public :: trcorri

contains

    !===========================================================================
    !> @brief Compute Jacobi coordinate transformation matrices
    !>
    !> This subroutine computes:
    !> - T: The Jacobi transformation matrix (r_jacobi = T * r_cartesian)
    !> - T^{-1}: Its inverse
    !> - b: The pair-to-Jacobi transformation vectors
    !> - lambda: Kinetic energy coefficients for each Jacobi coordinate
    !>
    !> The reduced masses are also computed for each Jacobi coordinate.
    !===========================================================================
    subroutine compute_jacobi_transform()
        implicit none

        real(dp) :: xma(MNPAR)           ! Cumulative mass sum
        real(dp) :: a_matrix(MNPAR, MNPAR)  ! Working matrix
        real(dp) :: y(MNPAR, MNPAR)      ! For inverse calculation
        integer :: indx(MNPAR)           ! Pivot indices
        real(dp) :: d_sign               ! Determinant sign
        real(dp) :: sum_mass
        integer :: i, j, k, m

        !-----------------------------------------------------------------------
        ! Initialize transformation matrix to zero
        !-----------------------------------------------------------------------
        a_matrix = 0.0_dp

        !-----------------------------------------------------------------------
        ! Compute cumulative mass sums: xma(j) = sum_{i=1}^{j} m_i
        !-----------------------------------------------------------------------
        sum_mass = 0.0_dp
        do j = 1, npar
            sum_mass = sum_mass + xm(j)
            xma(j) = sum_mass
        end do

        !-----------------------------------------------------------------------
        ! Construct Jacobi transformation matrix T
        !
        ! For i = 1, ..., N-1:
        !   T(i, i+1) = 1  (new particle enters)
        !   T(i, k) = -m_k / xma(i)  for k = 1, ..., i
        !
        ! For i = N (center of mass):
        !   T(N, j) = m_j / xma(N)  (mass-weighted average)
        !-----------------------------------------------------------------------
        do j = 1, npar - 1
            ! New particle term
            a_matrix(j, j+1) = 1.0_dp

            ! CM of previous particles term
            do k = 1, j
                a_matrix(j, k) = -xm(k) / xma(j)
            end do

            ! Reduced mass for this Jacobi coordinate
            ! mu_j = M_j * m_{j+1} / M_{j+1}
            ! where M_j = sum of masses 1 to j
            xred(j) = xma(j) * xm(j+1) / xma(j+1)
        end do

        ! Center of mass row
        do j = 1, npar
            a_matrix(npar, j) = xm(j) / xma(npar)
        end do
        xred(npar) = xma(npar)  ! Total mass

        !-----------------------------------------------------------------------
        ! Store transformation matrix T
        !-----------------------------------------------------------------------
        do j = 1, npar
            do i = 1, npar
                t_jacobi(i, j) = a_matrix(i, j)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Compute inverse transformation T^{-1} using LU decomposition
        !-----------------------------------------------------------------------
        call ludcmp(a_matrix, npar, MNPAR, indx, d_sign)

        ! Initialize identity matrix
        y = 0.0_dp
        do i = 1, npar
            y(i, i) = 1.0_dp
        end do

        ! Solve for each column of the inverse
        do j = 1, npar
            call lubksb(a_matrix, npar, MNPAR, indx, y(1:npar, j))
        end do

        ! Store inverse
        do i = 1, npar
            do j = 1, npar
                ti_jacobi(i, j) = y(i, j)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Compute b vectors for pair coordinates
        !
        ! b(m, k) gives the coefficient of Jacobi coordinate m
        ! in the expression for pair distance r_ij (pair k)
        !
        ! r_i - r_j = sum_m b(m, k) * xi_m
        !-----------------------------------------------------------------------
        k = 0
        do i = 1, npar
            do j = i + 1, npar
                k = k + 1
                do m = 1, npar
                    b_pair(m, k) = ti_jacobi(i, m) - ti_jacobi(j, m)
                end do
            end do
        end do
        kpot = k  ! Number of pairs

        !-----------------------------------------------------------------------
        ! Compute kinetic energy coefficients
        ! lambda(j) = hbar^2 / (2 * mu_j) where mu_j is reduced mass
        !-----------------------------------------------------------------------
        do j = 1, npar
            xlambda(j) = h2m / xred(j)
        end do

    end subroutine compute_jacobi_transform

    !===========================================================================
    !> @brief Transform from pairwise to Jacobi representation (nonlinear params)
    !>
    !> Given the pairwise correlation matrix z(k) = 1/r_ij^2 (upper triangle),
    !> construct the Jacobi representation A matrix.
    !>
    !> The Gaussian basis function is:
    !> phi = exp(-sum_ij z_ij * (r_i - r_j)^2) = exp(-xi^T A xi)
    !>
    !> This subroutine computes A from z using the Jacobi transformation.
    !>
    !> @param[in]  z  Pairwise parameters (upper triangle, size npar*(npar-1)/2)
    !> @param[out] a  Jacobi representation matrix ((npar-1) x (npar-1))
    !===========================================================================
    subroutine trcorr(z, a_out)
        implicit none
        real(dp), intent(in) :: z((MNPAR*(MNPAR+1))/2)
        real(dp), intent(out) :: a_out(MNPAR, MNPAR)

        real(dp) :: g(MNPAR, MNPAR)
        real(dp) :: xn_matrix(MNPAR, MNPAR)
        real(dp) :: ss
        integer :: i, j, k

        !-----------------------------------------------------------------------
        ! Construct symmetric matrix xn from z (pairwise parameters)
        ! xn(i,j) = |z_ij| represents correlation between particles i and j
        !-----------------------------------------------------------------------
        k = 0
        do i = 1, npar
            xn_matrix(i, i) = 0.0_dp
            do j = i + 1, npar
                k = k + 1
                xn_matrix(i, j) = abs(z(k))
                xn_matrix(j, i) = abs(z(k))
            end do
        end do

        !-----------------------------------------------------------------------
        ! Construct Laplacian-like matrix g
        ! g(i,j) = -xn(i,j) for i ≠ j
        ! g(i,i) = sum_k xn(i,k)  (sum of row)
        !-----------------------------------------------------------------------
        do i = 1, npar
            do j = i + 1, npar
                g(i, j) = -xn_matrix(i, j)
                g(j, i) = -xn_matrix(i, j)
            end do

            ss = 0.0_dp
            do k = 1, npar
                ss = ss + xn_matrix(i, k)
            end do
            g(i, i) = ss
        end do

        !-----------------------------------------------------------------------
        ! Transform g to Jacobi coordinates: g -> T^{-T} * g * T^{-1}
        !-----------------------------------------------------------------------
        call trafo3(g, ti_jacobi)

        !-----------------------------------------------------------------------
        ! Extract (npar-1) x (npar-1) submatrix (excluding CM coordinate)
        !-----------------------------------------------------------------------
        do i = 1, npar - 1
            do j = 1, npar - 1
                a_out(i, j) = g(i, j)
            end do
        end do

    end subroutine trcorr

    !===========================================================================
    !> @brief Transform from Jacobi to pairwise representation (inverse)
    !>
    !> Given the Jacobi representation A matrix, compute the pairwise
    !> parameters z(k) = correlation for pair k.
    !>
    !> This is the inverse operation of trcorr.
    !>
    !> @param[out] z  Pairwise parameters (upper triangle)
    !> @param[in]  a  Jacobi representation matrix
    !===========================================================================
    subroutine trcorri(z, a_in)
        implicit none
        real(dp), intent(out) :: z((MNPAR*(MNPAR+1))/2)
        real(dp), intent(in) :: a_in(MNPAR, MNPAR)

        real(dp) :: g(MNPAR, MNPAR)
        real(dp) :: xn_matrix(MNPAR, MNPAR)
        integer :: i, j, k

        !-----------------------------------------------------------------------
        ! Copy A matrix to g (with zero CM row/column)
        !-----------------------------------------------------------------------
        g = 0.0_dp
        do i = 1, npar - 1
            do j = 1, npar - 1
                g(i, j) = a_in(i, j)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Transform back: g -> T^T * g * T
        !-----------------------------------------------------------------------
        call trafo3(g, t_jacobi)

        !-----------------------------------------------------------------------
        ! Extract pairwise parameters from off-diagonal elements
        ! z_ij = -g(i,j) for i < j
        !-----------------------------------------------------------------------
        do k = 1, npar
            do j = k + 1, npar
                xn_matrix(k, j) = -g(k, j)
                xn_matrix(j, k) = -g(k, j)
            end do
        end do

        !-----------------------------------------------------------------------
        ! Pack into upper triangle array
        !-----------------------------------------------------------------------
        k = 0
        do i = 1, npar
            do j = i + 1, npar
                k = k + 1
                z(k) = xn_matrix(i, j)
            end do
        end do

    end subroutine trcorri

    !===========================================================================
    !> @brief Matrix transformation A -> T^T * A * T (internal)
    !>
    !> Performs the similarity transformation for coordinate change.
    !>
    !> @param[inout] a  Matrix to transform
    !> @param[in]    t  Transformation matrix
    !===========================================================================
    subroutine trafo3(a_mat, t_mat)
        implicit none
        real(dp), intent(inout) :: a_mat(MNPAR, MNPAR)
        real(dp), intent(in) :: t_mat(MNPAR, MNPAR)

        real(dp) :: x(MNPAR, MNPAR)
        real(dp) :: sum_val
        integer :: i, j, k

        ! X = A * T
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    sum_val = sum_val + a_mat(i, k) * t_mat(k, j)
                end do
                x(i, j) = sum_val
            end do
        end do

        ! A = T^T * X
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    sum_val = sum_val + t_mat(k, i) * x(k, j)
                end do
                a_mat(i, j) = sum_val
            end do
        end do

    end subroutine trafo3

end module coordinate_transform_mod
