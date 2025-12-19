!===============================================================================
! MODULE: special_functions_mod
!===============================================================================
!> @brief Special mathematical functions for SVM calculations
!>
!> This module provides mathematical functions needed for computing
!> matrix elements of the Hamiltonian with Gaussian basis functions:
!>
!> - Factorial and double factorial (logarithmic form for numerical stability)
!> - Hermite polynomials
!> - Complementary error function (erfc)
!> - Gaussian integrals (potmat)
!> - Root finding (zerus)
!>
!> Most functions use precomputed tables for efficiency.
!===============================================================================
module special_functions_mod
    use constants_mod
    use parameters_mod, only: log_factorial, log_double_factorial, log_power2, &
                              ee_work, q_work, mi_work, x1_bound, x2_bound
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: init_factorial_tables
    public :: hermite
    public :: erfc0
    public :: potmat
    public :: zerus
    public :: eps_func
    public :: power_func

contains

    !===========================================================================
    !> @brief Initialize factorial lookup tables
    !>
    !> Computes and stores logarithms of:
    !> - Factorials: log(n!) for n = 0, 1, 2, ...
    !> - Double factorials: log(n!!) for n = -1, 0, 1, 2, ...
    !> - Powers of 2: log(2^n) for n = 0, 1, 2, ...
    !>
    !> Using logarithms allows computing large factorials without overflow.
    !===========================================================================
    subroutine init_factorial_tables()
        implicit none
        integer :: i
        real(dp) :: x

        !-----------------------------------------------------------------------
        ! Initialize factorial table: f(n) = log(n!)
        !-----------------------------------------------------------------------
        log_factorial(0) = 0.0_dp
        log_factorial(1) = 0.0_dp

        do i = 2, NMAX
            x = real(i, dp)
            log_factorial(i) = log_factorial(i-1) + log(x)
        end do

        !-----------------------------------------------------------------------
        ! Initialize double factorial table: df(n) = log(n!!)
        ! n!! = n * (n-2) * (n-4) * ... * 1 (or 2)
        !-----------------------------------------------------------------------
        log_double_factorial(-1) = 0.0_dp
        log_double_factorial(0) = 0.0_dp
        log_double_factorial(1) = 0.0_dp
        log_double_factorial(2) = log(2.0_dp)

        do i = 3, NNMAX
            x = real(i, dp)
            log_double_factorial(i) = log_double_factorial(i-2) + log(x)
        end do

        !-----------------------------------------------------------------------
        ! Initialize powers of 2: x2n(n) = log(2^n) = n * log(2)
        !-----------------------------------------------------------------------
        log_power2(0) = 0.0_dp
        do i = 1, N2MAX
            log_power2(i) = log_power2(i-1) + log(2.0_dp)
        end do

    end subroutine init_factorial_tables

    !===========================================================================
    !> @brief Evaluate Hermite polynomial H_n(x)
    !>
    !> Computes the physicist's Hermite polynomial using the explicit formula:
    !> H_n(x) = sum_{k=0}^{n/2} (-1)^k * n! / (k! * (n-2k)!) * (2x)^(n-2k)
    !>
    !> @param[in]  n  Order of the polynomial (n >= 0)
    !> @param[in]  x  Argument
    !> @param[out] y  Value of H_n(x)
    !===========================================================================
    subroutine hermite(n, x, y)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: x
        real(dp), intent(out) :: y

        integer :: i, n_half
        real(dp) :: x1, x2

        y = 0.0_dp
        n_half = n / 2  ! Integer division

        do i = 0, n_half
            ! Coefficient: (-1)^i * n! / (i! * (n-2*i)!)
            x1 = (-1)**i * exp(log_factorial(n) - log_factorial(n-2*i) - log_factorial(i))

            ! Power term: (2*x)^(n-2*i)
            if (x == 0.0_dp) then
                if (n - 2*i == 0) then
                    x2 = 1.0_dp
                else
                    x2 = 0.0_dp
                end if
            else
                x2 = (2.0_dp * x) ** (n - 2*i)
            end if

            y = y + x1 * x2
        end do

    end subroutine hermite

    !===========================================================================
    !> @brief Complementary error function erfc(x) approximation
    !>
    !> Uses a rational Chebyshev approximation for the complementary
    !> error function: erfc(x) = 1 - erf(x)
    !>
    !> The approximation uses a change of variable t = 1 - 7.5/(|x| + 3.75)
    !> and a polynomial in t.
    !>
    !> @param[in]  x  Argument
    !> @param[out] y  Value of erfc(x) * exp(x^2) (scaled form)
    !===========================================================================
    subroutine erfc0(x, y)
        implicit none
        real(dp), intent(in) :: x
        real(dp), intent(out) :: y

        real(dp) :: t

        ! Change of variable
        t = 1.0_dp - 7.5_dp / (abs(x) + 3.75_dp)

        ! Polynomial approximation (Chebyshev coefficients)
        y = (((((((((((((((+3.328130055126039d-10 &
            * t - 5.718639670776992d-10) * t - 4.066088879757269d-9) &
            * t + 7.532536116142436d-9) * t + 3.026547320064576d-8) &
            * t - 7.043998994397452d-8) * t - 1.822565715362025d-7) &
            * t + 6.575825478226343d-7) * t + 7.478317101785790d-7) &
            * t - 6.182369348098529d-6) * t + 3.584014089915968d-6) &
            * t + 4.789838226695987d-5) * t - 1.524627476123466d-4) &
            * t - 2.553523453642242d-5) * t + 1.802962431316418d-3) &
            * t - 8.220621168415435d-3) * t + 2.414322397093253d-2

        y = (((((y * t - 5.480232669380236d-2) * t + 1.026043120322792d-1) &
            * t - 1.635718955239687d-1) * t + 2.260080669166197d-1) &
            * t - 2.734219314954260d-1) * t + 1.455897212750385d-1

        ! Handle negative argument
        if (x < 0.0_dp) y = 2.0_dp * exp(x * x) - y

    end subroutine erfc0

    !===========================================================================
    !> @brief Compute Gaussian integral for potential matrix elements
    !>
    !> Evaluates the integral:
    !> I = integral_0^infinity r^n * exp(-p*r^2 + q*r) dr
    !>
    !> This is needed for computing matrix elements of potentials of the form
    !> V(r) ~ r^n * exp(-a*r^2 + b*r) with Gaussian basis functions.
    !>
    !> @param[in] p  Gaussian exponent (p > 0)
    !> @param[in] q  Linear term coefficient
    !> @param[in] n  Power of r (n >= 0)
    !>
    !> @return Value of the integral
    !===========================================================================
    function potmat(p, q, n) result(integral)
        implicit none
        real(dp), intent(in) :: p, q
        integer, intent(in) :: n
        real(dp) :: integral

        real(dp) :: a, s, x1, x2, x3
        integer :: k

        ! Prefactor
        a = 0.5_dp * (-1)**n * sqrt(PI / p)

        ! Sum over terms
        s = 0.0_dp
        do k = 0, n
            ! Binomial coefficient: n! / (k! * (n-k)!)
            x1 = exp(log_factorial(n) - log_factorial(k) - log_factorial(n-k))

            ! First derivative term
            call d1_func(k, p, q, x2)

            ! Second derivative term
            call d2_func(n-k, p, q, x3)

            s = s + x1 * x2 * x3
        end do

        integral = s * a

    end function potmat

    !===========================================================================
    !> @brief First derivative component for potmat
    !>
    !> @param[in]  n  Order
    !> @param[in]  p  Gaussian parameter
    !> @param[in]  q  Linear parameter
    !> @param[out] x  Result
    !===========================================================================
    subroutine d1_func(n, p, q, x)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: p, q
        real(dp), intent(out) :: x

        real(dp) :: b, c
        integer :: i, n_half

        b = (0.5_dp / p) ** n
        x = 0.0_dp
        n_half = n / 2

        do i = 0, n_half
            c = exp(log_factorial(n) - log_factorial(n-2*i) - log_factorial(i)) &
                * power_func(q, n-2*i)
            x = x + b * c * p**i
        end do

    end subroutine d1_func

    !===========================================================================
    !> @brief Second derivative component for potmat
    !>
    !> @param[in]  n  Order
    !> @param[in]  p  Gaussian parameter
    !> @param[in]  q  Linear parameter
    !> @param[out] x  Result
    !===========================================================================
    subroutine d2_func(n, p, q, x)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: p, q
        real(dp), intent(out) :: x

        real(dp) :: y

        y = q / (2.0_dp * sqrt(p))

        if (n == 0) then
            call erfc0(y, x)
        else
            call hermite(n-1, y, x)
            x = x / (2.0_dp * sqrt(p))**n * (-1)**n * 2.0_dp / SQRT_PI
        end if

    end subroutine d2_func

    !===========================================================================
    !> @brief Safe power function x^n
    !>
    !> Handles the special case x = 0 correctly.
    !>
    !> @param[in] x  Base
    !> @param[in] n  Exponent
    !>
    !> @return x^n (0^0 = 1, 0^n = 0 for n > 0)
    !===========================================================================
    function power_func(x, n) result(result_val)
        implicit none
        real(dp), intent(in) :: x
        integer, intent(in) :: n
        real(dp) :: result_val

        if (x == 0.0_dp) then
            if (n == 0) then
                result_val = 1.0_dp
            else
                result_val = 0.0_dp
            end if
        else
            result_val = x ** n
        end if

    end function power_func

    !===========================================================================
    !> @brief Machine epsilon shifted value
    !>
    !> Returns x + eps or x - eps where eps is the smallest value such that
    !> x ± eps ≠ x in floating point arithmetic.
    !>
    !> @param[in] x  Base value
    !> @param[in] k  Direction: k=1 for x-eps, k=2 for x+eps
    !>
    !> @return x ± machine_epsilon
    !===========================================================================
    function eps_func(x, k) result(result_val)
        implicit none
        real(dp), intent(in) :: x
        integer, intent(in) :: k
        real(dp) :: result_val

        real(dp) :: epsy

        epsy = 1.0d-8

        ! Find smallest eps where x ± eps ≠ x
        do while (x /= x + (-1)**k * epsy)
            epsy = epsy / 2.0_dp
        end do

        result_val = x + (-1)**k * epsy * 2.0_dp

    end function eps_func

    !===========================================================================
    !> @brief Find root of the characteristic polynomial
    !>
    !> Uses inverse quadratic interpolation with bisection fallback to find
    !> the smallest root of the characteristic polynomial for the eigenvalue
    !> problem. This is used in the iterative eigenvalue solver.
    !>
    !> @param[in]    aa   Lower bound of search interval
    !> @param[in]    ff   Upper bound of search interval
    !> @param[in]    acc  Desired accuracy
    !> @param[inout] ifa  Flag: 0 on entry, 1 if no root found
    !>
    !> @return Approximate root location
    !===========================================================================
    function zerus(aa, ff, acc, ifa) result(root)
        implicit none
        real(dp), intent(in) :: aa, ff, acc
        integer, intent(inout) :: ifa
        real(dp) :: root

        integer, parameter :: MAXF = 300
        real(dp) :: z(3)
        real(dp) :: g1, g2, g3, f2
        real(dp) :: z12, z23, z13, www, z4, xx
        integer :: ifu, ic1, ic2

        ! Initialize search interval
        z(1) = aa
        z(3) = ff
        z(2) = (aa + ff) * 0.5_dp

        ! Function values (inverse for stability)
        g1 = 1.0_dp / func_char(z(1))
        g2 = 1.0_dp / func_char(z(2))
        g3 = 1.0_dp / func_char(z(3))

        ! Main iteration loop
        do ifu = 1, MAXF
            ! Check sign changes
            ic1 = 1
            ic2 = 1
            if ((g1 < 0.0_dp .and. g2 < 0.0_dp) .or. &
                (g1 > 0.0_dp .and. g2 > 0.0_dp)) ic1 = 0
            if ((g2 < 0.0_dp .and. g3 < 0.0_dp) .or. &
                (g2 > 0.0_dp .and. g3 > 0.0_dp)) ic2 = 0

            ! No root in interval
            if (ic1 + ic2 == 0) then
                root = z(1)
                if (ifu == 1) ifa = 1
                return
            end if

            ! Inverse quadratic interpolation
            z12 = z(1) - z(2)
            z23 = z(2) - z(3)
            z13 = z(1) - z(3)
            www = g1 * z23 - g2 * z13 + g3 * z12

            z4 = 1.0d+30
            if (www /= 0.0_dp) then
                z4 = (g1 * z(1) * z23 - g2 * z(2) * z13 + g3 * z(3) * z12) / www
            end if

            ! Update interval based on sign change
            if (ic1 == 1) then
                if (z4 < z(1) .or. z4 > z(2)) then
                    ! Bisection
                    xx = z(2)
                    z(3) = z(2)
                    g3 = g2
                    z(2) = (z(1) + xx) * 0.5_dp
                else
                    ! Use interpolated value
                    xx = z(2)
                    z(3) = z(2)
                    g3 = g2
                    z(2) = z4
                end if
                f2 = func_char(z(2))
                if (abs(f2) < acc) then
                    root = z(2)
                    return
                end if
                g2 = 1.0_dp / f2
            end if

            if (ic2 == 1) then
                if (z4 < z(2) .or. z4 > z(3)) then
                    ! Bisection
                    z(1) = z(2)
                    g1 = g2
                    z(2) = (z(2) + z(3)) * 0.5_dp
                else
                    ! Use interpolated value
                    z(1) = z(2)
                    g1 = g2
                    z(2) = z4
                end if
                f2 = func_char(z(2))
                if (abs(f2) < acc) then
                    root = z(2)
                    return
                end if
                g2 = 1.0_dp / f2
            end if

            ! Check convergence
            if (abs(z4 - z(3)) < acc) then
                root = z4
                return
            end if
        end do

        root = z(1)

    end function zerus

    !===========================================================================
    !> @brief Characteristic polynomial for eigenvalue iteration
    !>
    !> Evaluates det(H - x*O) in a factored form for the reduced eigenvalue
    !> problem. Used by the zerus root finder.
    !>
    !> @param[in] x  Eigenvalue candidate
    !>
    !> @return Value of the characteristic polynomial
    !===========================================================================
    function func_char(x) result(w)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: w

        real(dp) :: prod, ww, x_safe
        integer :: j

        ! Avoid exact boundary values
        x_safe = x
        if (x == x1_bound) x_safe = eps_func(x, 2)
        if (x == x2_bound) x_safe = eps_func(x, 1)

        prod = 1.0_dp
        w = 1.0_dp
        w = w * (ee_work(mi_work) - x_safe)

        do j = 1, mi_work - 1
            ww = q_work(j) * q_work(j) * prod / (ee_work(j) - x_safe)
            w = w - ww
        end do

    end function func_char

end module special_functions_mod
