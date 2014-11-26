! $Id: ropp_fm_refrac_1d_tl.f90 3551 2013-02-25 09:51:28Z idculv $

SUBROUTINE ropp_fm_refrac_1d_tl(x, x_tl, y, y_tl)

!****s* Refractivity/ropp_fm_refrac_1d_tl *
!
! NAME
!    ropp_fm_refrac_1d_tl - Tangent linear of ropp_fm_refrac_1d().
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_tl(x, x_tl, y, y_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_refrac_1d.
!
! INPUTS
!    type(State1dFM)             :: x      ! State vector
!    type(State1dFM)             :: x_tl   ! Perturbation vector
!    type(Obs1dRefrac)           :: y      ! Observation vector
!
! OUTPUT
!    real(wp), dimension(:)      :: y_tl   ! Observation tangent linear
!
! NOTES
!    The obs vector is required only for the observation's geopotential height 
!    levels; no forward simulated refractivity profile is returned.
!
!    The lengths of the arrays x_tl%state and y_tl must agree with the 
!    lengths of the x%state and y%refrac arrays, respectively.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d
!    ropp_fm_refrac_1d_ad
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
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_tl
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),        INTENT(in)  :: x     ! state vector
  TYPE(State1dFM),        INTENT(in)  :: x_tl  ! state perturbation
  TYPE(Obs1dRefrac),      INTENT(in)  :: y     ! obs vector
  REAL(wp), DIMENSION(:), INTENT(out) :: y_tl  ! obs perturbation

  REAL(wp), DIMENSION(x%n_lev)   :: pwvp       ! partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)   :: refrac     ! refractivity
  REAL(wp), DIMENSION(x%n_lev)   :: pwvp_tl    ! pwvp perturbation
  REAL(wp), DIMENSION(x%n_lev)   :: pdry
  REAL(wp), DIMENSION(x%n_lev)   :: pdry_tl


  REAL(wp), DIMENSION(x%n_lev)   :: refrac_tl  ! refractivity perturbation

  REAL(wp), DIMENSION(x%n_lev)     :: z_geop          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: z_geop_tl          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv, zcomp_wet_inv ! compressibilities
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv_tl, zcomp_wet_inv_tl ! compressibilities
  
  REAL(wp)                         :: kap1,kap2,kap3  ! refractivity coefficients used in routine


!-------------------------------------------------------------------------------
! 2. Non ideal gas options 
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:) = 1.0_wp
  zcomp_wet_inv(:) = 1.0_wp
 
  zcomp_dry_inv_tl(:) = 0.0_wp
  zcomp_wet_inv_tl(:) = 0.0_wp
 
   
! initialise geopotential heights
   
  z_geop(:) = x%geop(:)
  z_geop_tl(:) = x_tl%geop(:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp   

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress_tl &
     &(x,x_tl,z_geop,z_geop_tl,zcomp_dry_inv,zcomp_dry_inv_tl,&
     &zcomp_wet_inv,zcomp_wet_inv_tl)  

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3
     
  ENDIF  


!-------------------------------------------------------------------------------
! 2. Calculate water vapor pressure
!-------------------------------------------------------------------------------

  pwvp = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)
    
  pwvp_tl = pwvp * ( x_tl%pres/x%pres + x_tl%shum/x%shum &
              - x_tl%shum * pwvp * (1.0_wp-epsilon_water) / (x%pres*x%shum))

! dry pressure

  pdry = x%pres - pwvp
  pdry_tl =  x_tl%pres - pwvp_tl

!-------------------------------------------------------------------------------
! 3. Calculate refractivity
!-------------------------------------------------------------------------------

	      
  refrac = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

	   
  refrac_tl = kap1 * pdry_tl * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp_tl * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp_tl * zcomp_wet_inv/ x%temp + &
	   kap1 * pdry * zcomp_dry_inv_tl/ x%temp + &
           kap2 * pwvp * zcomp_wet_inv_tl/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv_tl/ x%temp - &
	   (kap1 * pdry * zcomp_dry_inv/ x%temp**2 + &
	   2.0_wp *kap2 * pwvp * zcomp_wet_inv/ x%temp**3 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp**2)* x_tl%temp
	   
	   
!-------------------------------------------------------------------------------
! 4. Interpolate onto measurements geopotential height levels
!-------------------------------------------------------------------------------

  CALL ropp_fm_interpol_log_tl(z_geop,y%geop,refrac, z_geop_tl, refrac_tl, y_tl)

END SUBROUTINE ropp_fm_refrac_1d_tl
