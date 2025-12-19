!===============================================================================
! MODULE: random_mod
!===============================================================================
!> @brief Random number generator for stochastic variational optimization
!>
!> This module provides a pseudo-random number generator based on the
!> linear congruential method with shuffling. The algorithm is designed
!> to produce uniformly distributed random numbers in the interval [0, 1).
!>
!> The generator uses:
!> - Modulus M = 714025
!> - Multiplier A = 1366
!> - Increment C = 150889
!>
!> The shuffling technique with a 97-element table improves the statistical
!> properties of the sequence compared to a simple linear congruential
!> generator.
!>
!> @note This is the original ran2 algorithm from Numerical Recipes
!===============================================================================
module random_mod
    use constants_mod
    implicit none

    private

    !---------------------------------------------------------------------------
    ! Public interface
    !---------------------------------------------------------------------------
    public :: ran2, init_random, set_seed

    !---------------------------------------------------------------------------
    ! Generator parameters (linear congruential)
    !---------------------------------------------------------------------------
    !> Modulus for the generator
    integer, parameter :: M_RAND = 714025

    !> Multiplier for the generator
    integer, parameter :: A_RAND = 1366

    !> Increment for the generator
    integer, parameter :: C_RAND = 150889

    !> Normalization factor: 1/M
    real(dp), parameter :: RM_RAND = 1.4005112d-6

    !---------------------------------------------------------------------------
    ! Generator state (module-level for persistence)
    !---------------------------------------------------------------------------
    !> Current y value for shuffling
    integer, save :: iy = 0

    !> Shuffle table (97 elements)
    integer, save :: ir(97) = 0

    !> Initialization flag
    integer, save :: initialized = 0

contains

    !===========================================================================
    !> @brief Initialize the random number generator
    !>
    !> This subroutine initializes the shuffle table and prepares the
    !> generator for producing random numbers. It should be called once
    !> at the start of the program.
    !>
    !> @param[in] seed  Initial seed value (positive integer)
    !===========================================================================
    subroutine init_random(seed)
        implicit none
        integer, intent(in) :: seed

        integer :: j, idum_local

        ! Initialize with the seed
        idum_local = mod(C_RAND - seed, M_RAND)

        ! Fill the shuffle table
        do j = 1, 97
            idum_local = mod(A_RAND * idum_local + C_RAND, M_RAND)
            ir(j) = idum_local
        end do

        ! Initialize y
        idum_local = mod(A_RAND * idum_local + C_RAND, M_RAND)
        iy = idum_local

        ! Mark as initialized
        initialized = 1

    end subroutine init_random

    !===========================================================================
    !> @brief Set a new seed for the random number generator
    !>
    !> This subroutine resets the generator with a new seed value.
    !> Useful for reproducible calculations.
    !>
    !> @param[in] seed  New seed value (positive integer)
    !===========================================================================
    subroutine set_seed(seed)
        implicit none
        integer, intent(in) :: seed

        call init_random(seed)

    end subroutine set_seed

    !===========================================================================
    !> @brief Generate a random number in [0, 1)
    !>
    !> This function generates a pseudo-random number uniformly distributed
    !> in the interval [0, 1). It uses a linear congruential generator with
    !> shuffle table for improved statistical properties.
    !>
    !> The algorithm:
    !> 1. Uses y to select an index j from the shuffle table
    !> 2. Returns the value at that position
    !> 3. Replaces the used value with a new random number
    !> 4. Updates y from the table
    !>
    !> @param[inout] idum  Seed variable (updated on each call)
    !>                     Set to negative value to reinitialize
    !>
    !> @return Random number in [0, 1)
    !===========================================================================
    function ran2(idum) result(random_value)
        implicit none
        integer, intent(inout) :: idum
        real(dp) :: random_value

        integer :: j

        !-----------------------------------------------------------------------
        ! Initialize if needed (negative seed or first call)
        !-----------------------------------------------------------------------
        if (idum < 0 .or. initialized == 0) then
            initialized = 1

            ! Start with seed
            idum = mod(C_RAND - idum, M_RAND)

            ! Fill shuffle table with 97 values
            do j = 1, 97
                idum = mod(A_RAND * idum + C_RAND, M_RAND)
                ir(j) = idum
            end do

            ! Initialize y
            idum = mod(A_RAND * idum + C_RAND, M_RAND)
            iy = idum
        end if

        !-----------------------------------------------------------------------
        ! Generate random number using shuffle
        !-----------------------------------------------------------------------
        ! Select random index j from 1 to 97
        j = 1 + (97 * iy) / M_RAND

        ! Safety check (should not happen, but ensures bounds)
        if (j > 97 .or. j < 1) then
            write(*, '(A)') 'Warning: Random generator index out of bounds'
            j = min(max(j, 1), 97)
        end if

        ! Get value from shuffle table
        iy = ir(j)

        ! Convert to floating point in [0, 1)
        random_value = iy * RM_RAND

        ! Replace used value with new random number
        idum = mod(A_RAND * idum + C_RAND, M_RAND)
        ir(j) = idum

    end function ran2

end module random_mod
