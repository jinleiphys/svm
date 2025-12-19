!===============================================================================
! MODULE: potential_mod
!===============================================================================
!> @brief Potential energy routines for few-body calculations
!>
!> This module handles the evaluation of two-body potentials in the SVM
!> framework. Potentials can be specified in two ways:
!>
!> 1. Analytic form (ipcon = 2):
!>    V(r) = sum_i V_i * r^n_i * exp(-a_i*r^2 + b_i*r)
!>    Parameters are read from pot.inp file
!>
!> 2. Numerical interpolation (ipcon = 1):
!>    The potential is tabulated and interpolated
!>
!> Supported exchange operators:
!> - Wigner (W): Central force
!> - Majorana (M): Space exchange
!> - Bartlett (B): Spin exchange
!> - Heisenberg (H): Spin-isospin exchange
!===============================================================================
module potential_mod
    use constants_mod
    use parameters_mod
    use special_functions_mod, only: potmat
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: init_potential_interpolation
    public :: potval
    public :: pot_function
    public :: poten
    public :: qgaus

contains

    !===========================================================================
    !> @brief Initialize potential interpolation table
    !>
    !> For ipcon = 1, this routine creates a lookup table of potential values
    !> for fast interpolation during the calculation. The table is stored on
    !> a logarithmic grid in the parameter space.
    !===========================================================================
    subroutine init_potential_interpolation()
        implicit none

        real(dp) :: zzz(NPNT)
        real(dp) :: sss, fff_value
        integer :: i, ip_save
        integer :: io_unit

        io_unit = 10

        open(io_unit, file='potrep.dat', status='replace')

        ! Set grid parameters (logarithmic scale)
        xll = -20.0_dp    ! Lower limit log scale
        xul = 20.0_dp     ! Upper limit log scale
        nv0 = 8000        ! Number of grid points

        write(io_unit, *) xll, xul, nv0

        ! Generate grid points
        do i = 1, nv0
            sss = xll + (i - 1) * (xul - xll) / real(nv0, dp)
            zzz(i) = -exp(sss)
            def_grid(i) = sss
        end do

        ! Compute potential on grid for each operator type
        ip_save = ip_pot
        do ip_pot = 1, no
            do i = 1, nv0
                call qqpot(zzz(i), fff_value)
                write(io_unit, *) fff_value
            end do
        end do
        ip_pot = ip_save

        close(io_unit)

    end subroutine init_potential_interpolation

    !===========================================================================
    !> @brief Numerical integration of the potential for interpolation table
    !>
    !> Computes the integral:
    !> integral_0^infinity r^2 * exp(-q*r^2) * V(r) dr
    !>
    !> This is used to build the interpolation table.
    !>
    !> @param[in]  zzz  Gaussian parameter (-q)
    !> @param[out] fff  Integrated potential value
    !===========================================================================
    subroutine qqpot(zzz, fff)
        implicit none
        real(dp), intent(in) :: zzz
        real(dp), intent(out) :: fff

        real(dp) :: bn, a_val, b_val, s, gi, h_step, eps_tol
        real(dp), save :: q_param
        integer, save :: k_power

        h_step = 0.5_dp
        eps_tol = 0.000001_dp
        bn = 4.0_dp * PI

        k_power = 2
        q_param = -zzz

        a_val = 0.0_dp
        b_val = 0.0_dp
        s = 0.0_dp

        ! Adaptive integration
        do
            a_val = b_val
            b_val = b_val + h_step
            call qgaus_internal(a_val, b_val, q_param, k_power, gi)
            if (abs(gi) < eps_tol) exit
            s = s + gi
        end do

        fff = bn * s

    end subroutine qqpot

    !===========================================================================
    !> @brief Gaussian quadrature integration
    !>
    !> 5-point Gaussian quadrature for integration of the potential
    !> weighted by Gaussian and power of r.
    !>
    !> @param[in]  a     Lower integration limit
    !> @param[in]  b     Upper integration limit
    !> @param[in]  q     Gaussian parameter
    !> @param[in]  n     Power of r
    !> @param[out] ss    Integral value
    !===========================================================================
    subroutine qgaus(a_val, b_val, ss)
        implicit none
        real(dp), intent(in) :: a_val, b_val
        real(dp), intent(out) :: ss

        ! 5-point Gaussian quadrature
        real(dp), parameter :: x(5) = [0.1488743389_dp, 0.4333953941_dp, &
            0.6794095682_dp, 0.8650633666_dp, 0.9739065285_dp]
        real(dp), parameter :: w(5) = [0.2955242247_dp, 0.2692667193_dp, &
            0.2190863625_dp, 0.1494513491_dp, 0.0666713443_dp]

        real(dp) :: xm, xr, dx
        integer :: j

        xm = 0.5_dp * (b_val + a_val)
        xr = 0.5_dp * (b_val - a_val)
        ss = 0.0_dp

        do j = 1, 5
            dx = xr * x(j)
            ss = ss + w(j) * (fv_integrand(xm + dx) + fv_integrand(xm - dx))
        end do

        ss = xr * ss

    contains

        function fv_integrand(r) result(f_val)
            real(dp), intent(in) :: r
            real(dp) :: f_val
            real(dp) :: w_exp, w1

            ! Gaussian weight
            w_exp = 1.0_dp * r**2  ! q=1 for this internal version
            if (w_exp > 300.0_dp) then
                w1 = 0.0_dp
            else
                w1 = r**2 * exp(-w_exp)
            end if

            ! Potential value
            f_val = w1 * pot_function(r)

        end function fv_integrand

    end subroutine qgaus

    !===========================================================================
    !> @brief Internal Gaussian quadrature with explicit parameters
    !===========================================================================
    subroutine qgaus_internal(a_val, b_val, q, n, ss)
        implicit none
        real(dp), intent(in) :: a_val, b_val, q
        integer, intent(in) :: n
        real(dp), intent(out) :: ss

        real(dp), parameter :: x(5) = [0.1488743389_dp, 0.4333953941_dp, &
            0.6794095682_dp, 0.8650633666_dp, 0.9739065285_dp]
        real(dp), parameter :: w(5) = [0.2955242247_dp, 0.2692667193_dp, &
            0.2190863625_dp, 0.1494513491_dp, 0.0666713443_dp]

        real(dp) :: xm, xr, dx
        integer :: j

        xm = 0.5_dp * (b_val + a_val)
        xr = 0.5_dp * (b_val - a_val)
        ss = 0.0_dp

        do j = 1, 5
            dx = xr * x(j)
            ss = ss + w(j) * (fv_int(xm + dx, q, n) + fv_int(xm - dx, q, n))
        end do

        ss = xr * ss

    contains

        function fv_int(r, q_val, n_pow) result(f_val)
            real(dp), intent(in) :: r, q_val
            integer, intent(in) :: n_pow
            real(dp) :: f_val
            real(dp) :: w_exp, w1

            w_exp = q_val * r**2
            if (w_exp > 300.0_dp) then
                w1 = 0.0_dp
            else
                w1 = r**n_pow * exp(-w_exp)
            end if

            f_val = w1 * pot_function(r)

        end function fv_int

    end subroutine qgaus_internal

    !===========================================================================
    !> @brief Interpolate potential from table
    !>
    !> Linear interpolation of the pre-computed potential values.
    !>
    !> @param[in] a   Gaussian parameter
    !> @param[in] ip  Operator index (1=W, 2=M, 3=B, 4=H)
    !>
    !> @return Interpolated potential value
    !===========================================================================
    function potval(a_val, ip) result(pot_value)
        implicit none
        real(dp), intent(in) :: a_val
        integer, intent(in) :: ip
        real(dp) :: pot_value

        real(dp) :: da, y1, y2, f1, f2
        integer :: k, k1, k2

        ! Logarithm of parameter
        da = log(a_val)

        ! Find grid index
        k = int((da - xll) / (xul - xll) * nv0)
        k1 = k + 1
        k2 = k + 2

        ! Ensure bounds
        k1 = max(1, min(k1, nv0))
        k2 = max(1, min(k2, nv0))

        ! Grid points and values
        y1 = def_grid(k1)
        y2 = def_grid(k2)
        f1 = fff_pot(k1, ip)
        f2 = fff_pot(k2, ip)

        ! Linear interpolation
        if (y2 /= y1) then
            pot_value = f1 + (f2 - f1) / (y2 - y1) * (da - y1)
        else
            pot_value = f1
        end if

    end function potval

    !===========================================================================
    !> @brief Radial potential function V(r)
    !>
    !> This function defines the two-body interaction potential.
    !> The user can modify this function to implement different potentials.
    !>
    !> Currently implemented: Poschl-Teller potential (example)
    !>
    !> @param[in] r  Interparticle distance
    !>
    !> @return Potential value V(r)
    !===========================================================================
    function pot_function(r) result(v)
        implicit none
        real(dp), intent(in) :: r
        real(dp) :: v

        !-----------------------------------------------------------------------
        ! Example: Poschl-Teller potential (from the original code)
        ! V(r) = V1/sinh^2(beta*r) - V2/cosh^2(beta*r)
        !-----------------------------------------------------------------------
        real(dp) :: hb2, hbarc, rn1, rn2, rn3
        real(dp) :: beta, v1, v2
        real(dp) :: csc_val, ssc_val

        ! Physical constants
        hb2 = 41.47_dp          ! hbar^2/m in MeV*fm^2
        hbarc = 197.32858_dp    ! hbar*c in MeV*fm

        ! Potential parameters
        rn1 = 3.0_dp
        rn2 = 3.0_dp
        rn3 = 3.0_dp

        beta = hbarc / (hb2 * rn3)
        v1 = hb2 * beta * beta * rn1 * (rn1 - 1)
        v2 = hb2 * beta * beta * rn2 * (rn2 + 1)

        ! Hyperbolic functions (with small r protection)
        if (abs(beta * r) < 1.0d-10) then
            v = 0.0_dp
        else
            csc_val = 1.0_dp / sinh(beta * r)
            ssc_val = 1.0_dp / cosh(beta * r)
            v = v1 * csc_val**2 - v2 * ssc_val**2
        end if

        !-----------------------------------------------------------------------
        ! Alternative: Minnesota potential (commented out)
        !-----------------------------------------------------------------------
        ! real(dp) :: u, vv1, vv2, vv3, xw, xm, xb, xh
        ! u = 1.0_dp
        ! vv1 = 200.0_dp * exp(-1.487_dp * r * r)
        ! vv2 = -178.0_dp * exp(-0.639_dp * r * r)
        ! vv3 = -91.85_dp * exp(-0.465_dp * r * r)
        ! xw = u * 0.25_dp * (2.0_dp * vv1 + vv2 + vv3)
        ! xm = (1.0_dp - 0.5_dp * u) * vv1 + (0.5_dp - 0.25_dp * u) * (vv2 + vv3)
        ! xb = 0.25_dp * u * (vv2 - vv3)
        ! xh = (0.5_dp - 0.25_dp * u) * vv2 + (-0.5_dp + 0.25_dp * u) * vv3
        ! select case (ip_pot)
        !     case (1); v = xw
        !     case (2); v = xm
        !     case (3); v = xb
        !     case (4); v = xh
        !     case default; v = 0.0_dp
        ! end select

    end function pot_function

    !===========================================================================
    !> @brief Compute potential matrix elements
    !>
    !> Evaluates the matrix element of the potential between Gaussian basis
    !> functions. Uses either interpolation (ipcon=1) or analytic formula
    !> (ipcon=2).
    !>
    !> @param[in]  nbas  Number of basis states to compute
    !> @param[in]  u     1/(2*a) parameter array for each basis
    !> @param[out] v     Potential matrix element for each operator type
    !===========================================================================
    subroutine poten(nbas_in, u, v)
        implicit none
        integer, intent(in) :: nbas_in
        real(dp), intent(in) :: u(MNBAS)
        real(dp), intent(out) :: v(MNBAS, 0:4)

        real(dp) :: a_val, p, ppp
        integer :: k, m, nn

        !-----------------------------------------------------------------------
        ! Coulomb potential (operator 0)
        !-----------------------------------------------------------------------
        do m = 1, nbas_in
            v(m, 0) = 0.0_dp
        end do

        nn = 1  ! r^1 for Coulomb
        do m = 1, nbas_in
            a_val = u(m)
            p = 0.5_dp / a_val
            ppp = potmat(p, 0.0_dp, nn) / (2.0_dp * a_val)**1.5_dp * 4.0_dp / SQRT_PI
            v(m, 0) = v(m, 0) + ppp
        end do

        !-----------------------------------------------------------------------
        ! Other operators (1 = Wigner, 2 = Majorana, 3 = Bartlett, 4 = Heisenberg)
        !-----------------------------------------------------------------------
        do k = 1, no
            do m = 1, nbas_in
                v(m, k) = 0.0_dp
            end do

            if (ipcon == 1) then
                ! Interpolation from table
                do m = 1, nbas_in
                    a_val = u(m)
                    p = 0.5_dp / a_val
                    ppp = potval(p, k) / (2.0_dp * a_val)**1.5_dp / PI**1.5_dp
                    v(m, k) = ppp
                end do
            else if (ipcon == 2) then
                ! Analytic form: sum over potential terms
                do nn = 1, npt
                    do m = 1, nbas_in
                        a_val = u(m)
                        p = 0.5_dp / a_val + ap(nn, k)
                        ppp = potmat(p, bp(nn, k), 2 + np_pot(nn, k)) / &
                              (2.0_dp * a_val)**1.5_dp * 4.0_dp / SQRT_PI
                        v(m, k) = v(m, k) + vp(nn, k) * ppp
                    end do
                end do
            end if
        end do

    end subroutine poten

end module potential_mod
