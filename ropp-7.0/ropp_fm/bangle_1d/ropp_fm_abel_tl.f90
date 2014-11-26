! $Id: ropp_fm_abel_tl.f90 3831 2013-08-27 15:22:48Z idculv $

SUBROUTINE ropp_fm_abel_tl(nr, refrac, impact, nr_tl, refrac_tl, bangle_tl)

!****s* BendingAngle/ropp_fm_abel_tl *
!
! NAME
!    ropp_fm_abel_tl - Tangent linear of ropp_fm_abel().
!
! SYNOPSIS
!    call ropp_fm_abel_tl(nr, refrac, impact, nr_tl, refrac_tl, bangle_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_abel.
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x=nr product of state levels
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: impact      ! Observation's impact parameters
!    real(wp), dimension(:) :: nr_tl       ! x=nr perturbations
!    real(wp), dimension(:) :: refrac_tl   ! Refractivity perturbations
!
! OUTPUT
!    real(wp), dimension(:) :: bangle_tl   ! Bending angle perturbations
!
! NOTES
!    The lengths of the arrays nr, nr_tl, refrac and refrac_tl must be
!    equal, as must the lengths of impact and bangle_tl.
!
! SEE ALSO
!    ropp_fm_types
!    rpp_fm_bangle_1d
!    ropp_fm_abel
!    ropp_fm_abel_ad
!
! REFERENCES
!    This routine was created with the help of the 
!
!      Tangent linear and Adjoint Model Compiler,  TAMC 5.3.2.
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

  REAL(wp), DIMENSION(:), INTENT(in)    :: nr          ! x=nr product
  REAL(wp), DIMENSION(:), INTENT(in)    :: refrac      ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)    :: impact      ! Impact parameter 
  REAL(wp), DIMENSION(:), INTENT(inout) :: nr_tl       ! x=nr perturbation
  REAL(wp), DIMENSION(:), INTENT(inout) :: refrac_tl   ! refrac perturbation
  REAL(wp), DIMENSION(:), INTENT(inout) :: bangle_tl   ! bangle perturbation
 
  REAL(wp), DIMENSION(:), ALLOCATABLE :: kval          ! Exponential decay rate
  REAL(wp), DIMENSION(:), ALLOCATABLE :: kval_tl       
  REAL(wp)                            :: t_lower       ! Lower bound of integral
  REAL(wp)                            :: t_lower_tl
  REAL(wp)                            :: t_upper       ! Upper bound of integral
  REAL(wp)                            :: t_upper_tl 
  REAL(wp)                            :: refrac_low    ! refrac at lower level
  REAL(wp)                            :: refrac_low_tl
  REAL(wp)                            :: nr_low        ! x=nr at lower level
  REAL(wp)                            :: nr_low_tl
  REAL(wp)                            :: factor

  REAL(wp)                            :: integral_erf  ! integral approximation
  REAL(wp)                            :: integral_erf_tl
  REAL(wp)                            :: erf_up
  REAL(wp)                            :: erf_low
  REAL(wp)                            :: zt_up
  REAL(wp)                            :: zt_low
  REAL(wp)                            :: erf_up_tl
  REAL(wp)                            :: erf_low_tl
  REAL(wp)                            :: zt_up_tl
  REAL(wp)                            :: zt_low_tl
  REAL(wp), PARAMETER                 :: a=0.3480242_wp
  REAL(wp), PARAMETER                 :: b=0.0958798_wp
  REAL(wp), PARAMETER                 :: c=0.7478556_wp

  INTEGER                               :: n_lev, n_lower, n_impact
  INTEGER                               :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)

  ALLOCATE(kval(n_lev-1))
  ALLOCATE(kval_tl(n_lev-1))

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

     factor = 1.0_wp / MAX(1.0_wp, nr(i+1) - nr(i))

     kval(i)    = LOG(refrac(i)/refrac(i+1)) / MAX(1.0_wp, nr(i+1) - nr(i))
     kval_tl(i) = - nr_tl(i+1) &
                       * (0.5_wp - SIGN(0.5_wp, 1.0_wp - (nr(i+1) - nr(i)))) &
                                   * kval(i) * factor                        &
                  + nr_tl(i) &
                    * (0.5_wp - SIGN(0.5_wp, 1.0_wp - (nr(i+1) - nr(i)))) &
                                * kval(i) * factor      &
                  - refrac_tl(i+1) * factor / refrac(i+1)   &
                  + refrac_tl(i) * factor / refrac(i)

     kval(i)    = MAX(1.0e-6_wp, kval(i))
     kval_tl(i) = kval_tl(i) * (0.5_wp - SIGN(0.5_wp, 1.0e-6_wp - kval(i)))


! limit the size if the refractivity gradient

     IF (kval(i) > 0.157_wp/refrac(i)) THEN
     
         kval(i) = 0.157_wp/refrac(i)

         kval_tl(i) = - 0.157_wp/refrac(i)**2*refrac_tl(i)
	 
     ENDIF 	  

  ENDDO

!-------------------------------------------------------------------------------
! 5. Calculate bending angles for observational heights
!-------------------------------------------------------------------------------

  bangle_tl(:) = 0.0_wp

  DO l = 1, n_impact

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

     bangle_tl(l) = 0.0_wp

     DO i = i_bot, n_lev - 1

!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        IF (i == i_bot) THEN
           refrac_low    = refrac(i_bot)*EXP(-kval(i_bot)*(impact(l)-nr(i_bot)))
           refrac_low_tl = (nr_tl(i_bot) * kval(i_bot)  &
                           - kval_tl(i_bot) * (impact(l) - nr(i_bot)) &
                           + refrac_tl(i_bot) / refrac(i_bot)) * refrac_low

           nr_low    = impact(l)
           nr_low_tl = 0.0_wp
        ELSE
           refrac_low    = refrac(i) 
           refrac_low_tl = refrac_tl(i)
           nr_low    = nr(i) 
           nr_low_tl = nr_tl(i)
        ENDIF

!       5.2.2 Upper (100 km above top end) and lower bounds of the integral
!       -------------------------------------------------------------------

        IF (i == n_lev - 1) THEN
           t_upper    = SQRT(kval(i) * (nr(i+1) + 1.0d5 - impact(l))) 
           t_upper_tl = nr_tl(i+1) * 0.5_wp * kval(i) / t_upper   &
                        + kval_tl(i) * 0.5_wp &
                        * (1.0d5 + nr(i+1) - impact(l)) / t_upper
        ELSE
           t_upper    = SQRT(kval(i) * (nr(i+1) - impact(l)))
           t_upper_tl = nr_tl(i+1) * 0.5_wp * kval(i) / t_upper  &
                        + kval_tl(i) * 0.5_wp * (nr(i+1) - impact(l)) / t_upper
        ENDIF

        IF (i == i_bot) THEN
           t_lower    = 0.0_wp
           t_lower_tl = 0.0_wp
        ELSE
           t_lower    = SQRT(kval(i) * (nr(i) - impact(l)))
           t_lower_tl = nr_tl(i) * 0.5_wp * kval(i) / t_lower   &
                      + kval_tl(i) * 0.5_wp * (nr(i) - impact(l)) / t_lower  
        ENDIF

!       5.2.3 Integral
!       --------------

        ! Approximate error function with polynomial
        zt_low = 1.0_wp / (1.0_wp+0.47047_wp*t_lower)
        zt_low_tl = -(0.47047_wp*t_lower_tl) * (zt_low*zt_low)

        erf_low = 1.0_wp - (a-(b-c*zt_low)*zt_low) * zt_low *     &
                    EXP(-(t_lower*t_lower))
        erf_low_tl = ((a-(b-c*zt_low)*zt_low)*zt_low*2.0_wp*t_lower*t_lower_tl &
                     -(a-(2.0_wp*b-3.0_wp*c*zt_low)*zt_low)*zt_low_tl)         &
                     * EXP(-(t_lower*t_lower))

        zt_up = 1.0_wp / (1.0_wp + 0.47047_wp * t_upper)
        zt_up_tl = -(0.47047_wp * t_upper_tl) * (zt_up*zt_up)


        erf_up = 1.0_wp - (a-(b-c*zt_up)*zt_up) * zt_up *         &
                   EXP(-(t_upper*t_upper))

        IF (i == n_lev - 1) erf_up = 1.0_wp ! To be fully consistent with ROPP documentation


        erf_up_tl = ((a-(b-c*zt_up)*zt_up)*zt_up*2.0_wp*t_upper*t_upper_tl &
                    -(a-(2.0_wp*b-3.0_wp*c*zt_up)*zt_up)*zt_up_tl)         &
                    * EXP(-(t_upper*t_upper))

        IF (i == n_lev - 1) erf_up_tl = 0.0_wp ! To be fully consistent with ROPP documentation


        integral_erf = erf_up - erf_low
        integral_erf_tl = erf_up_tl - erf_low_tl

!       5.2.4 Bending angle value
!       -------------------------

        factor  = 1.0e-6_wp * SQRT(2.0_wp * pi * impact(l)) &
                         * EXP(kval(i) * (nr_low - impact(l)))

        bangle_tl(l) = bangle_tl(l) &
                        + nr_low_tl * factor * SQRT(kval(i)) * refrac_low     &
                           * integral_erf * kval(i)                           &
                       + integral_erf_tl * factor * SQRT(kval(i))* refrac_low &
                       + kval_tl(i) * factor * refrac_low * integral_erf      &
                           * (0.5_wp / SQRT(kval(i)) + SQRT(kval(i)) *        &
                             (nr_low - impact(l)))                            &
                        + refrac_low_tl * factor * SQRT(kval(i)) * integral_erf
     ENDDO
  ENDDO

  DEALLOCATE(kval)
  DEALLOCATE(kval_tl)

END SUBROUTINE ropp_fm_abel_tl
