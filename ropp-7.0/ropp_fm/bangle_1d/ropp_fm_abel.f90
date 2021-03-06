! $Id: ropp_fm_abel.f90 3831 2013-08-27 15:22:48Z idculv $

SUBROUTINE ropp_fm_abel(nr, refrac, impact, bangle)

!****s* BendingAngle/ropp_fm_abel *
!
! NAME
!    ropp_fm_abel - Forward model calculating a one dimensional bending angle
!                   profile from refractivity / impact parameter profile at
!                   state vector levels using a Fast Abel Transform
!
! SYNOPSIS
!    call ropp_fm_abel(nr, refrac, impact, bangle)
! 
! DESCRIPTION
!    This routine calculates bending angles at a given set of impact parameters
!    from a vertical profile of rafractivity given at the state vector's set of
!    x = nr levels. 
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x = nr product
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: impact      ! Impact parameters
!
! OUTPUT
!    real(wp), dimension(:) :: bangle      ! Forward modelled bending angles
!
! NOTES
!    The interpolation of bending angles calculated at the state vector's
!    geopotential levels to the observation vector's impact parameters is
!    carried out assuming that bending angle varies exponentially with
!    impact parameter.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_1d
!    ropp_fm_abel_ad
!    ropp_fm_abel_tl
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils, ONLY: ropp_MDFV, ropp_ZERO
  USE ropp_fm_constants, ONLY: pi

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)  :: nr             ! x = nr product
  REAL(wp), DIMENSION(:), INTENT(in)  :: refrac         ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)  :: impact         ! Impact parameter
  REAL(wp), DIMENSION(:), INTENT(out) :: bangle         ! Bending angle

  REAL(wp), DIMENSION(:), ALLOCATABLE :: kval           ! Exponential decay rate
  REAL(wp)                            :: t_upper        ! Upper bound integral
  REAL(wp)                            :: t_lower        ! Lower bound integral
  REAL(wp)                            :: refrac_low     ! refrac at lower level
  REAL(wp)                            :: nr_low         ! x=nr at lower level

  REAL(wp)                            :: integral_erf   ! integral approximation
  REAL(wp)                            :: erf_up, erf_low
  REAL(wp)                            :: zt
  REAL(wp), PARAMETER                 :: a = 0.3480242_wp
  REAL(wp), PARAMETER                 :: b = 0.0958798_wp
  REAL(wp), PARAMETER                 :: c = 0.7478556_wp

  INTEGER                             :: n_lev, n_lower, n_impact
  INTEGER                             :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)
  
  ALLOCATE(kval(n_lev-1))

!-------------------------------------------------------------------------------
! 3. Calculate lowest usable level (because of superrefraction)
!-------------------------------------------------------------------------------

  n_lower = 1
  DO i = n_lev, 2, -1
     IF (nr(i) - nr(i-1) < 10.0_wp) THEN
        n_lower = i
        EXIT
     ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! 4. Calculate exponential decay rate between levels
!-------------------------------------------------------------------------------

  DO i = 1, n_lev - 1
     kval(i) = LOG(refrac(i)/refrac(i+1)) / MAX(1.0_wp, (nr(i+1)-nr(i)))
     kval(i) = MAX(1.0e-6_wp, kval(i))
 
! limit the maximum kval so that refractivity gradient is ~ half critical value    
     
     kval(i) = MIN(kval(i),0.157_wp/refrac(i))     
     
  ENDDO

!-------------------------------------------------------------------------------
! 5. Calculate bending angles for observational heights
!-------------------------------------------------------------------------------

  bangle(:) = ropp_MDFV

  DO l = 1, n_impact

     IF (impact(l) < nr(n_lower) .OR. impact(l) >= nr(n_lev)) THEN
        CYCLE
     ENDIF

!    5.1 Find bottom state vector level
!    ----------------------------------

     i_bot = n_lower
     DO WHILE (impact(l) >= nr(i_bot + 1) .AND. i_bot < n_lev)
        i_bot = i_bot + 1
     ENDDO

!    5.2 Loop over all levels above
!    ------------------------------

     bangle(l) = ropp_ZERO

     DO i = i_bot, n_lev - 1

!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        IF (i == i_bot) THEN
           refrac_low = refrac(i_bot)*EXP(-kval(i_bot)*(impact(l) - nr(i_bot)))
           nr_low = impact(l)
        ELSE
           refrac_low = refrac(i) 
           nr_low = nr(i) 
        ENDIF

!       5.2.2 Upper (100 km above top end) and lower bounds of the integral
!       -------------------------------------------------------------------

        IF (i == n_lev - 1) THEN
           t_upper = SQRT(kval(i) * (nr(i+1) + 1.0d5 - impact(l))) 
        ELSE
           t_upper = SQRT(kval(i) * (nr(i+1) - impact(l)))
        ENDIF

        IF (i == i_bot) THEN
           t_lower = 0.0_wp
        ELSE
           t_lower = SQRT(kval(i) * (nr(i) - impact(l)))
        ENDIF

!       5.2.3 Integral
!       --------------

        ! Approximate error function with polynomial
        zt = 1.0_wp / (1.0_wp + 0.47047_wp * t_lower)
        erf_low = 1.0_wp - (a-(b-c*zt)*zt) * zt * EXP(-(t_lower*t_lower))
        zt = 1.0_wp / (1.0_wp + 0.47047_wp*t_upper)
        erf_up = 1.0_wp - (a-(b-c*zt)*zt) * zt * EXP(-(t_upper*t_upper))

        IF (i == n_lev - 1) erf_up = 1.0_wp ! To be fully consistent with ROPP documentation

        integral_erf = erf_up - erf_low

!       5.2.4 Bending angle value
!       -------------------------

        bangle(l) = bangle(l) &
                     + 1.0e-6_wp * SQRT(2.0_wp *pi*impact(l))*SQRT(kval(i)) & 
                     * refrac_low * EXP(kval(i) * (nr_low - impact(l)))     &
                     * integral_erf

     ENDDO
  ENDDO

  DEALLOCATE(kval)

END SUBROUTINE ropp_fm_abel
