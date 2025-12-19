!===============================================================================
! MODULE: linear_algebra_mod
!===============================================================================
!> @brief Linear algebra routines for matrix operations
!>
!> This module provides essential linear algebra operations needed for
!> the SVM calculation. It can use either:
!> - Built-in routines (for portability)
!> - LAPACK/OpenBLAS (for performance)
!>
!> To use LAPACK, compile with -DUSE_LAPACK and link with -llapack -lblas
!> To use OpenBLAS, compile with -DUSE_LAPACK and link with -lopenblas
!>
!> The module provides:
!> - LU decomposition and back substitution
!> - Cholesky decomposition (for positive definite matrices)
!> - Matrix inversion
!> - Determinant calculation
!> - Matrix transformation T^T * A * T
!> - Generalized eigenvalue problem solver
!===============================================================================
module linear_algebra_mod
    use constants_mod
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: ludcmp, lubksb           ! LU decomposition
    public :: choldc                    ! Cholesky decomposition
    public :: vdet, vinv               ! Vectorized determinant/inverse
    public :: vtrafo, trafo3, trafo_single  ! Matrix transformations
    public :: solve_geneig             ! Generalized eigenvalue problem

#ifdef USE_LAPACK
    !---------------------------------------------------------------------------
    ! LAPACK external declarations
    !---------------------------------------------------------------------------
    external :: dgetrf, dgetrs, dgetri, dpotrf  ! LU/Cholesky decomposition/inverse
    external :: dsygv, dsygvx           ! Generalized eigenvalue
    external :: dgemm                    ! Matrix multiplication
#endif

contains

    !===========================================================================
    !> @brief Solve generalized eigenvalue problem A*x = lambda*B*x
    !>
    !> This is the main routine for diagonalization. It can use LAPACK
    !> for better performance if available.
    !>
    !> @param[in]  n       Dimension of the problem
    !> @param[in]  a       Hamiltonian matrix (symmetric)
    !> @param[in]  b       Overlap matrix (symmetric positive definite)
    !> @param[out] evals   Eigenvalues in ascending order
    !> @param[out] evecs   Eigenvectors (columns)
    !> @param[out] info    0 = success, non-zero = error
    !===========================================================================
    subroutine solve_geneig(n, a, b, evals, evecs, info)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(inout) :: a(n, n)
        real(dp), intent(inout) :: b(n, n)
        real(dp), intent(out) :: evals(n)
        real(dp), intent(out) :: evecs(n, n)
        integer, intent(out) :: info

#ifdef USE_LAPACK
        ! Use LAPACK dsygv for generalized eigenvalue problem
        real(dp) :: work(3*MNBAS)
        integer :: lwork

        lwork = 3 * n

        ! Copy a to evecs (dsygv overwrites input)
        evecs = a

        ! DSYGV: Computes all eigenvalues and eigenvectors
        ! itype=1: A*x = lambda*B*x
        ! jobz='V': Compute eigenvalues and eigenvectors
        ! uplo='L': Lower triangle is stored
        call dsygv(1, 'V', 'L', n, evecs, n, b, n, evals, work, lwork, info)

#else
        ! Use built-in NAG-style routines
        real(dp) :: dl(MNBAS), e(MNBAS)
        integer :: ia, ib, iv, ifail

        ia = n
        ib = n
        iv = n
        ifail = 0

        ! Copy matrices to preserve originals
        evecs = a

        ! Call the built-in generalized eigenvalue solver
        call f02aef_internal(evecs, ia, b, ib, n, evals, evecs, iv, dl, e, ifail)

        info = ifail
#endif

    end subroutine solve_geneig

    !===========================================================================
    !> @brief LU decomposition of a matrix
    !>
    !> Performs LU decomposition using LAPACK (if available) or built-in.
    !>
    !> @param[inout] a     Matrix to decompose (overwritten with L and U)
    !> @param[in]    n     Dimension of the matrix
    !> @param[in]    np    Leading dimension of array a
    !> @param[out]   indx  Permutation vector from pivoting
    !> @param[out]   d     +1 or -1 indicating even/odd row permutations
    !===========================================================================
    subroutine ludcmp(a, n, np, indx, d)
        implicit none
        integer, intent(in)    :: n, np
        real(dp), intent(inout) :: a(np, np)
        integer, intent(out)   :: indx(n)
        real(dp), intent(out)  :: d

#ifdef USE_LAPACK
        integer :: info, i

        ! LAPACK dgetrf for LU decomposition
        call dgetrf(n, n, a, np, indx, info)

        ! Compute sign of permutation
        d = 1.0_dp
        do i = 1, n
            if (indx(i) /= i) d = -d
        end do

        if (info /= 0) then
            write(*, '(A,I5)') 'LAPACK dgetrf error: ', info
        end if
#else
        ! Built-in LU decomposition
        call ludcmp_builtin(a, n, np, indx, d)
#endif

    end subroutine ludcmp

    !===========================================================================
    !> @brief Built-in LU decomposition (Crout's method with partial pivoting)
    !===========================================================================
    subroutine ludcmp_builtin(a, n, np, indx, d)
        implicit none
        integer, intent(in)    :: n, np
        real(dp), intent(inout) :: a(np, np)
        integer, intent(out)   :: indx(n)
        real(dp), intent(out)  :: d

        integer, parameter :: NMAX_LOCAL = 100
        real(dp) :: vv(NMAX_LOCAL)
        real(dp) :: aamax, sum_val, dum
        integer :: i, j, k, imax

        d = 1.0_dp

        ! Get implicit scaling
        do i = 1, n
            aamax = 0.0_dp
            do j = 1, n
                if (abs(a(i, j)) > aamax) aamax = abs(a(i, j))
            end do
            if (aamax == 0.0_dp) then
                write(*, '(A)') 'Error: Singular matrix in ludcmp'
                stop
            end if
            vv(i) = 1.0_dp / aamax
        end do

        ! Crout's method
        do j = 1, n
            ! Upper triangular part
            if (j > 1) then
                do i = 1, j - 1
                    sum_val = a(i, j)
                    if (i > 1) then
                        do k = 1, i - 1
                            sum_val = sum_val - a(i, k) * a(k, j)
                        end do
                        a(i, j) = sum_val
                    end if
                end do
            end if

            ! Find pivot
            aamax = 0.0_dp
            imax = j
            do i = j, n
                sum_val = a(i, j)
                if (j > 1) then
                    do k = 1, j - 1
                        sum_val = sum_val - a(i, k) * a(k, j)
                    end do
                    a(i, j) = sum_val
                end if
                dum = vv(i) * abs(sum_val)
                if (dum >= aamax) then
                    imax = i
                    aamax = dum
                end if
            end do

            ! Interchange rows
            if (j /= imax) then
                do k = 1, n
                    dum = a(imax, k)
                    a(imax, k) = a(j, k)
                    a(j, k) = dum
                end do
                d = -d
                vv(imax) = vv(j)
            end if

            indx(j) = imax

            ! Divide by pivot
            if (j /= n) then
                if (a(j, j) == 0.0_dp) a(j, j) = TINY
                dum = 1.0_dp / a(j, j)
                do i = j + 1, n
                    a(i, j) = a(i, j) * dum
                end do
            end if
        end do

        if (a(n, n) == 0.0_dp) a(n, n) = TINY

    end subroutine ludcmp_builtin

    !===========================================================================
    !> @brief LU back substitution to solve A*x = b
    !===========================================================================
    subroutine lubksb(a, n, np, indx, b)
        implicit none
        integer, intent(in)    :: n, np
        real(dp), intent(in)   :: a(np, np)
        integer, intent(in)    :: indx(n)
        real(dp), intent(inout) :: b(n)

#ifdef USE_LAPACK
        integer :: info
        real(dp) :: b_work(n, 1)

        b_work(:, 1) = b
        call dgetrs('N', n, 1, a, np, indx, b_work, n, info)
        b = b_work(:, 1)
#else
        ! Built-in back substitution
        integer :: i, j, ii, ll
        real(dp) :: sum_val

        ! Forward substitution
        ii = 0
        do i = 1, n
            ll = indx(i)
            sum_val = b(ll)
            b(ll) = b(i)
            if (ii /= 0) then
                do j = ii, i - 1
                    sum_val = sum_val - a(i, j) * b(j)
                end do
            else if (sum_val /= 0.0_dp) then
                ii = i
            end if
            b(i) = sum_val
        end do

        ! Backward substitution
        do i = n, 1, -1
            sum_val = b(i)
            if (i < n) then
                do j = i + 1, n
                    sum_val = sum_val - a(i, j) * b(j)
                end do
            end if
            b(i) = sum_val / a(i, i)
        end do
#endif

    end subroutine lubksb

    !===========================================================================
    !> @brief Cholesky decomposition of a symmetric positive definite matrix
    !>
    !> @param[in]  ac    Input matrix (n-1 x n-1)
    !> @param[out] ierr  Error flag: 0 = success, 1 = not positive definite
    !===========================================================================
    subroutine choldc(ac, ierr)
        use parameters_mod, only: npar
        implicit none
        real(dp), intent(in)  :: ac(MNPAR, MNPAR)
        integer, intent(out)  :: ierr

        real(dp) :: a_local(MNPAR, MNPAR)
        integer :: n, i, j, k, info
        real(dp) :: p(MNPAR), sum_val

        ierr = 0
        n = npar - 1

        ! Copy input matrix
        a_local(1:n, 1:n) = ac(1:n, 1:n)

#ifdef USE_LAPACK
        ! Use LAPACK dpotrf
        call dpotrf('L', n, a_local, MNPAR, info)
        if (info /= 0) ierr = 1
#else
        ! Built-in Cholesky
        do i = 1, n
            do j = i, n
                sum_val = a_local(i, j)
                do k = i - 1, 1, -1
                    sum_val = sum_val - a_local(i, k) * a_local(j, k)
                end do

                if (i == j) then
                    if (sum_val <= 0.0_dp) then
                        ierr = 1
                        return
                    end if
                    p(i) = sqrt(sum_val)
                else
                    a_local(j, i) = sum_val / p(i)
                end if
            end do
        end do
#endif

    end subroutine choldc

    !===========================================================================
    !> @brief Vectorized determinant calculation for multiple matrices
    !>
    !> @param[in]  n    Dimension of each matrix
    !> @param[in]  nvd  Number of matrices to process
    !===========================================================================
    subroutine vdet(n, nvd)
        use parameters_mod, only: aap, det_aap, det_aap_inv15
        implicit none
        integer, intent(in) :: n, nvd

        real(dp) :: d_local(MNBAS)
        integer :: i, j, k, l, ii, n1

        if (n == 1) then
            do l = 1, nvd
                det_aap(l) = aap(l, 1, 1)
                det_aap_inv15(l) = 1.0_dp / (det_aap(l) * sqrt(det_aap(l)))
            end do
            return
        end if

        ! Gaussian elimination
        n1 = n - 1
        do i = 1, n1
            ii = i + 1
            do k = ii, n
                do l = 1, nvd
                    d_local(l) = aap(l, i, k) / aap(l, i, i)
                end do
                do j = ii, n
                    do l = 1, nvd
                        aap(l, j, k) = aap(l, j, k) - aap(l, j, i) * d_local(l)
                    end do
                end do
            end do
        end do

        ! Product of diagonal
        do l = 1, nvd
            det_aap(l) = 1.0_dp
        end do
        do i = 1, n
            do l = 1, nvd
                det_aap(l) = det_aap(l) * aap(l, i, i)
            end do
        end do

        ! Precompute det^{-1.5} = 1 / (det * sqrt(det)) to avoid repeated power
        do l = 1, nvd
            det_aap_inv15(l) = 1.0_dp / (det_aap(l) * sqrt(det_aap(l)))
        end do

    end subroutine vdet

    !===========================================================================
    !> @brief Vectorized matrix inversion for multiple symmetric matrices
    !>
    !> @param[in]  n    Dimension of each matrix
    !> @param[in]  nvd  Number of matrices to process
    !===========================================================================
    subroutine vinv(n, nvd)
        use parameters_mod, only: aap, aapi
        implicit none
        integer, intent(in) :: n, nvd

        ! Built-in Gauss-Jordan elimination (faster than LAPACK for small matrices)
        real(dp) :: b_work(MNBAS, MNPAR, 2*MNPAR)
        real(dp) :: x(MNBAS)
        integer :: i, j, k, l, m, j2, iji

        if (n == 1) then
            do l = 1, nvd
                aapi(l, 1, 1) = 1.0_dp / aap(l, 1, 1)
            end do
            return
        end if

        ! Set up augmented matrix [A | I]
        j2 = n * 2

        do j = n + 1, j2
            do i = 1, n
                do l = 1, nvd
                    b_work(l, i, j) = 0.0_dp
                end do
            end do
        end do

        do i = 1, n
            iji = i + n
            do l = 1, nvd
                b_work(l, i, iji) = 1.0_dp
            end do
        end do

        do i = 1, n
            do j = 1, n
                do l = 1, nvd
                    b_work(l, j, i) = aap(l, j, i)
                end do
            end do
        end do

        ! Gauss-Jordan elimination
        do i = 1, n
            do l = 1, nvd
                x(l) = b_work(l, i, i)
            end do

            do k = i, j2
                do l = 1, nvd
                    b_work(l, i, k) = b_work(l, i, k) / x(l)
                end do
            end do

            do m = 1, n
                do l = 1, nvd
                    x(l) = b_work(l, m, i)
                end do
                if (m == i) cycle
                do k = i, j2
                    do l = 1, nvd
                        b_work(l, m, k) = b_work(l, m, k) - b_work(l, i, k) * x(l)
                    end do
                end do
            end do
        end do

        ! Extract inverse
        do j = 1, n
            do i = 1, n
                do l = 1, nvd
                    aapi(l, i, j) = b_work(l, i, j + n)
                end do
            end do
        end do

    end subroutine vinv

    !===========================================================================
    !> @brief Vectorized matrix transformation A -> T^T * A * T
    !===========================================================================
    subroutine vtrafo(a, t, npar_in, nvd)
        implicit none
        integer, intent(in) :: npar_in, nvd
        real(dp), intent(inout) :: a(MNBAS, MNPAR, MNPAR)
        real(dp), intent(in) :: t(MNPAR, MNPAR)

        real(dp) :: x(MNBAS, MNPAR, MNPAR)
        real(dp) :: sum_val(MNBAS)
        integer :: i, j, k, l

        ! X = A * T
        do i = 1, npar_in
            do j = 1, npar_in
                sum_val = 0.0_dp
                do k = 1, npar_in
                    do l = 1, nvd
                        sum_val(l) = sum_val(l) + a(l, i, k) * t(k, j)
                    end do
                end do
                x(1:nvd, i, j) = sum_val(1:nvd)
            end do
        end do

        ! A = T^T * X
        do i = 1, npar_in
            do j = 1, npar_in
                sum_val = 0.0_dp
                do k = 1, npar_in
                    do l = 1, nvd
                        sum_val(l) = sum_val(l) + t(k, i) * x(l, k, j)
                    end do
                end do
                a(1:nvd, i, j) = sum_val(1:nvd)
            end do
        end do

    end subroutine vtrafo

    !===========================================================================
    !> @brief Single matrix transformation: result = T^T * A * T
    !>
    !> Optimized version for transforming a single matrix, using BLAS dgemm
    !> when available.
    !>
    !> @param[in]  a_in    Input matrix (n x n)
    !> @param[in]  t       Transformation matrix (n x n)
    !> @param[in]  n       Dimension
    !> @param[out] a_out   Output matrix (n x n)
    !===========================================================================
    subroutine trafo_single(a_in, t, n, a_out)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: a_in(MNPAR, MNPAR)
        real(dp), intent(in) :: t(MNPAR, MNPAR)
        real(dp), intent(out) :: a_out(MNPAR, MNPAR)

#ifdef USE_LAPACK
        real(dp) :: x(MNPAR, MNPAR)
        real(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp

        ! X = A * T using BLAS dgemm
        call dgemm('N', 'N', n, n, n, alpha, a_in, MNPAR, t, MNPAR, beta, x, MNPAR)

        ! a_out = T^T * X
        call dgemm('T', 'N', n, n, n, alpha, t, MNPAR, x, MNPAR, beta, a_out, MNPAR)
#else
        real(dp) :: x(MNPAR, MNPAR)
        real(dp) :: sum_val
        integer :: i, j, k

        ! X = A * T
        do i = 1, n
            do j = 1, n
                sum_val = 0.0_dp
                do k = 1, n
                    sum_val = sum_val + a_in(i, k) * t(k, j)
                end do
                x(i, j) = sum_val
            end do
        end do

        ! a_out = T^T * X
        do i = 1, n
            do j = 1, n
                sum_val = 0.0_dp
                do k = 1, n
                    sum_val = sum_val + t(k, i) * x(k, j)
                end do
                a_out(i, j) = sum_val
            end do
        end do
#endif

    end subroutine trafo_single

    !===========================================================================
    !> @brief Matrix transformation A -> T^T * A * T (single matrix)
    !===========================================================================
    subroutine trafo3(a, t)
        use parameters_mod, only: npar
        implicit none
        real(dp), intent(inout) :: a(MNPAR, MNPAR)
        real(dp), intent(in) :: t(MNPAR, MNPAR)

#ifdef USE_LAPACK
        real(dp) :: x(MNPAR, MNPAR), c(MNPAR, MNPAR)
        real(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp

        ! X = A * T using BLAS dgemm
        call dgemm('N', 'N', npar, npar, npar, alpha, a, MNPAR, t, MNPAR, beta, x, MNPAR)

        ! C = T^T * X
        call dgemm('T', 'N', npar, npar, npar, alpha, t, MNPAR, x, MNPAR, beta, c, MNPAR)

        a(1:npar, 1:npar) = c(1:npar, 1:npar)
#else
        real(dp) :: x(MNPAR, MNPAR)
        real(dp) :: sum_val
        integer :: i, j, k

        ! X = A * T
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    sum_val = sum_val + a(i, k) * t(k, j)
                end do
                x(i, j) = sum_val
            end do
        end do

        ! A = T^T * X
        do i = 1, npar
            do j = 1, npar
                sum_val = 0.0_dp
                do k = 1, npar
                    sum_val = sum_val + t(k, i) * x(k, j)
                end do
                a(i, j) = sum_val
            end do
        end do
#endif

    end subroutine trafo3

    !===========================================================================
    ! Internal NAG-style eigenvalue routines (used when LAPACK not available)
    !===========================================================================

    !---------------------------------------------------------------------------
    ! f02aef_internal: Generalized symmetric eigenvalue problem
    !---------------------------------------------------------------------------
    subroutine f02aef_internal(a, ia, b, ib, n, r, v, iv, dl, e, ifail)
        implicit none
        integer, intent(in) :: ia, ib, iv, n
        real(dp), intent(inout) :: a(ia, n), b(ib, n)
        real(dp), intent(out) :: r(n), v(iv, n), dl(n), e(n)
        integer, intent(inout) :: ifail

        real(dp) :: tol
        integer :: isave

        isave = ifail
        ifail = 1

        ! Reduce to standard form
        call f01aef_internal(n, a, ia, b, ib, dl, ifail)
        if (ifail /= 0) return

        ! Tridiagonalize
        tol = 2.0_dp ** (-50)
        call f01ajf_internal(n, tol, a, ia, r, e, v, iv)

        ! Find eigenvalues/vectors
        tol = 2.0_dp ** (-55)
        ifail = 1
        call f02amf_internal(n, tol, r, e, v, iv, ifail)
        if (ifail /= 0) return

        ! Back transform eigenvectors
        call f01aff_internal(n, 1, n, b, ib, dl, v, iv)

    end subroutine f02aef_internal

    !---------------------------------------------------------------------------
    ! Helper routines for eigenvalue solver
    !---------------------------------------------------------------------------
    subroutine f01aef_internal(n, a, ia, b, ib, dl, ifail)
        implicit none
        integer, intent(in) :: n, ia, ib
        real(dp), intent(inout) :: a(ia, n), b(ib, n)
        real(dp), intent(out) :: dl(n)
        integer, intent(inout) :: ifail

        integer :: i, j, k, kk, i1
        real(dp) :: x, y

        do i = 1, n
            i1 = i - 1
            do j = i, n
                x = b(i, j)
                if (i1 > 0) then
                    do kk = 1, i1
                        k = i1 - kk + 1
                        x = x - b(i, k) * b(j, k)
                    end do
                end if
                if (i /= j) then
                    b(j, i) = x / y
                else
                    if (x <= 0.0_dp) then
                        ifail = 1
                        return
                    end if
                    y = sqrt(x)
                    dl(i) = y
                end if
            end do
        end do

        do i = 1, n
            y = dl(i)
            i1 = i - 1
            do j = i, n
                x = a(i, j)
                if (i1 > 0) then
                    do kk = 1, i1
                        k = i1 - kk + 1
                        x = x - b(i, k) * a(j, k)
                    end do
                end if
                a(j, i) = x / y
            end do
        end do

        do j = 1, n
            do i = j, n
                x = a(i, j)
                i1 = i - 1
                if (i1 >= j) then
                    do kk = j, i1
                        k = i1 - kk + j
                        x = x - a(k, j) * b(i, k)
                    end do
                end if
                if (j > 1) then
                    do kk = 1, j - 1
                        k = j - kk
                        x = x - a(j, k) * b(i, k)
                    end do
                end if
                a(i, j) = x / dl(i)
            end do
        end do

        ifail = 0

    end subroutine f01aef_internal

    subroutine f01aff_internal(n, im1, im2, b, ib, dl, z, iz)
        implicit none
        integer, intent(in) :: n, im1, im2, ib, iz
        real(dp), intent(in) :: b(ib, n), dl(n)
        real(dp), intent(inout) :: z(iz, im2)

        integer :: j, ii, i, i2, k
        real(dp) :: x

        do j = im1, im2
            do ii = 1, n
                i = n - ii + 1
                x = z(i, j)
                i2 = i + 1
                if (i2 <= n) then
                    do k = i2, n
                        x = x - b(k, i) * z(k, j)
                    end do
                end if
                z(i, j) = x / dl(i)
            end do
        end do

    end subroutine f01aff_internal

    subroutine f01ajf_internal(n, atol, a, ia, d, e, z, iz)
        implicit none
        integer, intent(in) :: n, ia, iz
        real(dp), intent(in) :: atol
        real(dp), intent(inout) :: a(ia, n)
        real(dp), intent(out) :: d(n), e(n), z(iz, n)

        integer :: i, j, k, l, ii, j1
        real(dp) :: f, g, h, hh

        do i = 1, n
            do j = 1, i
                z(i, j) = a(i, j)
            end do
        end do

        if (n == 1) then
            e(1) = 0.0_dp
            d(1) = z(1, 1)
            z(1, 1) = 1.0_dp
            return
        end if

        do ii = 2, n
            i = n - ii + 2
            l = i - 2
            f = z(i, i-1)
            g = 0.0_dp
            if (l > 0) then
                do k = 1, l
                    g = g + z(i, k) * z(i, k)
                end do
            end if
            h = g + f * f

            if (g <= atol) then
                e(i) = f
                h = 0.0_dp
            else
                l = l + 1
                g = sqrt(h)
                if (f >= 0.0_dp) g = -g
                e(i) = g
                h = h - f * g
                z(i, i-1) = f - g
                f = 0.0_dp

                do j = 1, l
                    z(j, i) = z(i, j) / h
                    g = 0.0_dp
                    do k = 1, j
                        g = g + z(j, k) * z(i, k)
                    end do
                    j1 = j + 1
                    if (j1 <= l) then
                        do k = j1, l
                            g = g + z(k, j) * z(i, k)
                        end do
                    end if
                    e(j) = g / h
                    f = f + g * z(j, i)
                end do

                hh = f / (h + h)
                do j = 1, l
                    f = z(i, j)
                    g = e(j) - hh * f
                    e(j) = g
                    do k = 1, j
                        z(j, k) = z(j, k) - f * e(k) - g * z(i, k)
                    end do
                end do
            end if
            d(i) = h
        end do

        e(1) = 0.0_dp
        d(1) = 0.0_dp

        do i = 1, n
            l = i - 1
            if (d(i) /= 0.0_dp) then
                do j = 1, l
                    g = 0.0_dp
                    do k = 1, l
                        g = g + z(i, k) * z(k, j)
                    end do
                    do k = 1, l
                        z(k, j) = z(k, j) - g * z(k, i)
                    end do
                end do
            end if
            d(i) = z(i, i)
            z(i, i) = 1.0_dp
            if (l > 0) then
                do j = 1, l
                    z(i, j) = 0.0_dp
                    z(j, i) = 0.0_dp
                end do
            end if
        end do

    end subroutine f01ajf_internal

    subroutine f02amf_internal(n, acheps, d, e, z, iz, ifail)
        implicit none
        integer, intent(in) :: n, iz
        real(dp), intent(in) :: acheps
        real(dp), intent(inout) :: d(n), e(n), z(iz, n)
        integer, intent(inout) :: ifail

        integer :: i, l, j, m, i1, m1, ii, k
        real(dp) :: b, f, h, g, p, r, c, s

        if (n > 1) then
            do i = 2, n
                e(i-1) = e(i)
            end do
        end if
        e(n) = 0.0_dp
        b = 0.0_dp
        f = 0.0_dp

        do l = 1, n
            j = 0
            h = acheps * (abs(d(l)) + abs(e(l)))
            if (b < h) b = h

            do m = l, n
                if (abs(e(m)) <= b) exit
            end do

            if (m == l) then
                d(l) = d(l) + f
                cycle
            end if

            do while (j < 30)
                j = j + 1
                g = d(l)
                h = d(l+1) - g
                if (abs(h) >= abs(e(l))) then
                    p = 2.0_dp * e(l) / h
                    r = sqrt(p * p + 1.0_dp)
                    d(l) = e(l) * p / (1.0_dp + r)
                else
                    p = h * 0.5_dp / e(l)
                    r = sqrt(p * p + 1.0_dp)
                    h = p + r
                    if (p < 0.0_dp) h = p - r
                    d(l) = e(l) / h
                end if

                h = g - d(l)
                i1 = l + 1
                if (i1 <= n) then
                    do i = i1, n
                        d(i) = d(i) - h
                    end do
                end if
                f = f + h

                p = d(m)
                c = 1.0_dp
                s = 0.0_dp
                m1 = m - 1

                do ii = l, m1
                    i = m1 - ii + l
                    g = c * e(i)
                    h = c * p
                    if (abs(p) >= abs(e(i))) then
                        c = e(i) / p
                        r = sqrt(c * c + 1.0_dp)
                        e(i+1) = s * p * r
                        s = c / r
                        c = 1.0_dp / r
                    else
                        c = p / e(i)
                        r = sqrt(c * c + 1.0_dp)
                        e(i+1) = s * e(i) * r
                        s = 1.0_dp / r
                        c = c / r
                    end if
                    p = c * d(i) - s * g
                    d(i+1) = h + s * (c * g + s * d(i))

                    do k = 1, n
                        h = z(k, i+1)
                        z(k, i+1) = s * z(k, i) + c * h
                        z(k, i) = c * z(k, i) - s * h
                    end do
                end do

                e(l) = s * p
                d(l) = c * p
                if (abs(e(l)) <= b) exit
            end do

            if (j >= 30) then
                ifail = 1
                return
            end if

            d(l) = d(l) + f
        end do

        ! Sort eigenvalues
        do i = 1, n
            k = i
            p = d(i)
            i1 = i + 1
            if (i1 <= n) then
                do j = i1, n
                    if (d(j) < p) then
                        k = j
                        p = d(j)
                    end if
                end do
            end if
            if (k /= i) then
                d(k) = d(i)
                d(i) = p
                do j = 1, n
                    p = z(j, i)
                    z(j, i) = z(j, k)
                    z(j, k) = p
                end do
            end if
        end do

        ifail = 0

    end subroutine f02amf_internal

end module linear_algebra_mod
