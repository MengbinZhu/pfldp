! $Id: ropp_fm_abel_ad.f90 3831 2013-08-27 15:22:48Z idculv $

SUBROUTINE ropp_fm_abel_ad(nr, refrac, impact, nr_ad, refrac_ad, bangle_ad)

!****s* BendingAngle/ropp_fm_abel_ad *
!
! NAME
!    ropp_fm_abel_ad - Adjoint of ropp_fm_abel().
!
! SYNOPSIS
!    call ropp_fm_abel_ad(nr, refrac, impact, nr_ad, refrac_ad, bangle_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_abel.
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x=nr product of state vector
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: impact      ! Observation's impact parameters
!    real(wp), dimension(:) :: nr_ad       ! x=nr adjoint
!    real(wp), dimension(:) :: bangle_ad   ! Adjoint forcing
!
! OUTPUT
!    real(wp), dimension(:) :: nr_ad       ! Updated x=nr adjoint
!    real(wp), dimension(:) :: refrac_ad   ! Refractivity adjoint
!    real(wp), dimension(:) :: bangle_ad   ! Bending angle adjoint
!
! NOTES
!    The lengths of the arrays nr, nr_ad, refrac and refrac_ad must be
!    equal.
!
! SEE ALSO
!    ropp_fm_types
!    rpp_fm_bangle_1d
!    ropp_fm_abel
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
  USE ropp_fm_constants

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)    :: nr          ! x=nr product state
  REAL(wp), DIMENSION(:), INTENT(in)    :: refrac      ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)    :: impact      ! Impact observations
  REAL(wp), DIMENSION(:), INTENT(inout) :: nr_ad       ! x=nr adjoint
  REAL(wp), DIMENSION(:), INTENT(inout) :: refrac_ad   ! Refractivity adjoint
  REAL(wp), DIMENSION(:), INTENT(inout) :: bangle_ad   ! Bending angle adjoint

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval        ! Exponential decay rate
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval_tmp    ! 
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval_ad
  REAL(wp)                              :: t_lower     ! Lower bound integral
  REAL(wp)                              :: t_lower_ad 
  REAL(wp)                              :: t_upper     ! Upper bound integral
  REAL(wp)                              :: t_upper_ad  
  REAL(wp)                              :: refrac_low  ! Refractivity lower lvl
  REAL(wp)                              :: refrac_low_ad
  REAL(wp)                              :: nr_low      ! x=nr at lower level
  REAL(wp)                              :: nr_low_ad
  REAL(wp)                              :: factor

  REAL(wp)                              :: integral_erf ! Integral approx
  REAL(wp)                              :: integral_erf_ad
  REAL(wp)                              :: erf_up
  REAL(wp)                              :: erf_low
  REAL(wp)                              :: zt_up
  REAL(wp)                              :: zt_low
  REAL(wp), PARAMETER                   :: a=0.3480242_wp
  REAL(wp), PARAMETER                   :: b=0.0958798_wp
  REAL(wp), PARAMETER                   :: c=0.7478556_wp
  REAL(wp)                              :: erf_up_ad
  REAL(wp)                              :: erf_low_ad
  REAL(wp)                              :: zt_up_ad
  REAL(wp)                              :: zt_low_ad
  REAL(wp), PARAMETER                   :: missing_value = -999.9_wp

  INTEGER                               :: n_lev, n_lower, n_impact
  INTEGER                               :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables; also setting adjoint variables to zero
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)

  ALLOCATE(kval(n_lev-1))
  ALLOCATE(kval_tmp(n_lev-1))
  ALLOCATE(kval_ad(n_lev-1))

  kval_ad(:)      = 0.0_wp

  integral_erf_ad = 0.0_wp
  nr_low_ad   = 0.0_wp
  refrac_low_ad   = 0.0_wp
  t_lower_ad      = 0.0_wp
  t_upper_ad      = 0.0_wp
  erf_low_ad      = 0.0_wp
  erf_up_ad       = 0.0_wp
  zt_up_ad        = 0.0_wp
  zt_low_ad       = 0.0_wp

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
     kval(i) = LOG(refrac(i)/refrac(i+1))/MAX(1.0_wp, (nr(i+1)-nr(i)))
     kval(i) = MAX(1.0e-6_wp, kval(i))

! save for adjoint      
     
     kval_tmp(i) = kval(i)

! limit the size if the refractivity gradient

     kval(i) = MIN(kval(i),0.157_wp/refrac(i))     
          
  ENDDO

!-------------------------------------------------------------------------------
! 5. Adjoint code - bending angle calculation
!-------------------------------------------------------------------------------

  DO l = n_impact, 1, -1

     IF (impact(l) < nr(n_lower) .OR. impact(l) > nr(n_lev)) THEN
        CYCLE
     ENDIF

!    5.1 Find bottom state vector level
!    ----------------------------------

     i_bot = n_lower
     DO WHILE (impact(l) >= nr(i_bot + 1))
        i_bot = i_bot + 1
     ENDDO

!    5.2 Loop over all levels above
!    ------------------------------

     DO i = n_lev - 1, i_bot, -1

!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        IF (i == i_bot) THEN
           refrac_low = refrac(i_bot) * EXP(-kval(i_bot)*(impact(l)-nr(i_bot)))
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
        zt_low = 1.0_wp / (1.0_wp + 0.47047_wp * t_lower)
        erf_low = 1.0_wp-(a-(b-c*zt_low)*zt_low)*zt_low*EXP(-(t_lower*t_lower))
        zt_up = 1.0_wp / (1.0_wp + 0.47047_wp * t_upper)
        erf_up = 1.0_wp - (a-(b-c*zt_up)*zt_up)*zt_up*EXP(-(t_upper*t_upper))

        IF (i == n_lev - 1) erf_up = 1.0_wp ! To be fully consistent with ROPP documentation

        integral_erf = erf_up - erf_low

!       5.2.4 Bending angle value
!       -------------------------

!        alpha(l) = alpha(l) &
!                     + 1.0e-6_wp * sqrt(2.0_wp*pi*impact(l)) * sqrt(kval(i)) & 
!                          * refrac_low * exp(kval(i) * (nr_low - impact(l))) &
!                          * integral_erf

!       5.2.5 Adjoint bending angle value
!       ---------------------------------

        factor  = 1.0e-6_wp * SQRT(2.0_wp * pi * impact(l)) &
                         * EXP(kval(i) * (nr_low - impact(l)))

        integral_erf_ad = integral_erf_ad &
                          + factor * SQRT(kval(i)) * refrac_low * bangle_ad(l)
        nr_low_ad       = nr_low_ad   &
                          + factor * SQRT(kval(i)) * refrac_low * integral_erf &
                                     * kval(i) * bangle_ad(l)
        refrac_low_ad   = refrac_low_ad   &
                          + factor * SQRT(kval(i)) * integral_erf * bangle_ad(l)
        kval_ad(i)      = kval_ad(i)                                    &
                            + factor * refrac_low * integral_erf        &
                              * (0.5_wp/SQRT(kval(i)) + SQRT(kval(i))   &
                              * (nr_low - impact(l))) * bangle_ad(l)

!       5.2.6 Adjoint integral
!       ----------------------

        erf_low_ad = erf_low_ad - integral_erf_ad
        erf_up_ad = erf_up_ad + integral_erf_ad
        integral_erf_ad = 0.0_wp
        
        t_upper_ad = t_upper_ad + (a-(b-c*zt_up)*zt_up) * zt_up          &
                     * EXP(-(t_upper*t_upper)) * 2.0_wp * t_upper * erf_up_ad
        zt_up_ad = zt_up_ad - (a-(2.0_wp*b-3.0_wp*c*zt_up)*zt_up)        &
                   *EXP(-(t_upper*t_upper)) * erf_up_ad
        erf_up_ad = 0.0_wp
        
        t_lower_ad = t_lower_ad + (a-(b-c*zt_low)*zt_low) * zt_low       &
                      *EXP(-(t_lower*t_lower)) * 2.0_wp * t_lower * erf_low_ad
        zt_low_ad = zt_low_ad - (a-(2.0_wp*b-3.0_wp*c*zt_low)*zt_low)    &
                     * EXP(-(t_lower*t_lower)) * erf_low_ad
        erf_low_ad = 0.0_wp
        
        t_lower_ad = t_lower_ad - 0.47047_wp * zt_low * zt_low * zt_low_ad
        zt_low_ad = 0.0_wp
        
        t_upper_ad = t_upper_ad - 0.47047_wp * zt_up * zt_up * zt_up_ad
        zt_up_ad = 0.0_wp


!       5.2.7 Adjoint upper and lower bounds of the integral
!       ----------------------------------------------------

        IF (i == i_bot) THEN
           t_lower_ad   = 0.0_wp
        ELSE
           nr_ad(i)   = nr_ad(i) + 0.5_wp * kval(i) / t_lower * t_lower_ad
           kval_ad(i) = kval_ad(i) +    &
                          0.5_wp*(nr(i)-impact(l))/t_lower * t_lower_ad
           t_lower_ad = 0.0_wp
        ENDIF

        IF (i == n_lev - 1) THEN
           nr_ad(i+1) = nr_ad(i+1) + 0.5_wp * kval(i) / t_upper * t_upper_ad
           kval_ad(i) = kval_ad(i) +  &
                         0.5_wp*(nr(i+1)+1.0d5-impact(l))/t_upper * t_upper_ad
           t_upper_ad = 0.0_wp
        ELSE
           nr_ad(i+1) = nr_ad(i+1) + 0.5_wp * kval(i) / t_upper * t_upper_ad
           kval_ad(i) = kval_ad(i) +    &
                          0.5_wp * (nr(i+1) - impact(l)) / t_upper * t_upper_ad
           t_upper_ad = 0.0_wp
        ENDIF

!       5.2.8 Adjoint values of refractivity and impact parameter at lower level
!       ------------------------------------------------------------------------

        IF (i == i_bot) THEN
           nr_low_ad    = 0.0_wp
           nr_ad(i)     = nr_ad(i) &
                            + refrac_low * kval(i) * refrac_low_ad
           kval_ad(i)   = kval_ad(i)   &
                            + refrac_low * (nr(i) - impact(l)) * refrac_low_ad
           refrac_ad(i) = refrac_ad(i) + refrac_low / refrac(i) * refrac_low_ad

           refrac_low_ad = 0.0_wp
        ELSE
           nr_ad(i)      = nr_ad(i) + nr_low_ad
           nr_low_ad     = 0.0_wp
           refrac_ad(i)  = refrac_ad(i) + refrac_low_ad
           refrac_low_ad = 0.0_wp
        ENDIF

     ENDDO

     bangle_ad(l) = 0.0_wp

  ENDDO

!-------------------------------------------------------------------------------
! 6. Adjoint code - calculate exponential decay rate between levels
!-------------------------------------------------------------------------------

  DO i = n_lev - 1, 1, -1

     factor = 1.0_wp / MAX(1.0_wp, nr(i+1) - nr(i))

! limit the size of the refractivity gradient
     
     IF (kval_tmp(i) > 0.157_wp/refrac(i)) THEN
     	 
	 refrac_ad(i) = refrac_ad(i) - 0.157_wp/refrac(i)**2*kval_ad(i)
	 
	 kval_ad(i) = 0.0_wp
	 
     ENDIF 	  
         

     refrac_ad(i+1) = refrac_ad(i+1) - factor / refrac(i+1) * kval_ad(i)
     refrac_ad(i)   = refrac_ad(i) + factor / refrac(i)   * kval_ad(i)
     nr_ad(i+1)     = nr_ad(i+1) - factor * kval(i) * kval_ad(i)
     nr_ad(i)       = nr_ad(i) + factor * kval(i) * kval_ad(i)
     kval_ad(i)     = 0.0_wp

  ENDDO

  DEALLOCATE(kval)
  DEALLOCATE(kval_tmp)
  DEALLOCATE(kval_ad)

END SUBROUTINE ropp_fm_abel_ad
