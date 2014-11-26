! $Id: ropp_fm_refrac_1d.f90 3551 2013-02-25 09:51:28Z idculv $

SUBROUTINE ropp_fm_refrac_1d(x, y)

!****s* Refractivity/ropp_fm_refrac_1d *
!
! NAME
!    ropp_fm_refrac_1d - Forward model calculating a one dimensional 
!                        refractivity profile from the state vector.
!
! SYNOPSIS
!    call ropp_fm_refrac_1d(x, y)
! 
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of 
!    refractivity from profiles of temperature, humidity and pressure. 
!    Refractivity values are calculated for the geopotential height levels 
!    given in the observation vector.
!
! INPUTS
!    type(State1dFM)     :: x     ! State vector structure
!    type(Obs1dRefrac)   :: y     ! Observation vector (levels required)
!
! OUTPUT
!    type(Obs1dRefrac)   :: y     ! Obs vector with forward modelled refrac
!
! NOTES
!    The forward model assumes that the state vector structure contains 
!    temperature, humidity and pressure values on common geopotential height 
!    levels, independent of the source of those data. Model-dependent 
!    conversion routines can be used to accomplish this within the 
!    ropp_fm_roprof2state() subroutine.
! 
!    The forward model assumes that the observation vector contains 
!    geopotential height levels onto which the forward simulated
!    observations are interpolated.
!
!    The interpolation of refractivity calculated at the state vector's
!    geopotential height levels to the observation vector's geopotential height
!    levels is carried out assuming that refractivity varies exponentially with
!    geopotential height.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d_ad
!    ropp_fm_refrac_1d_tl
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
  USE ropp_fm,   not_this => ropp_fm_refrac_1d
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),   INTENT(in)    :: x       ! State vector
  TYPE(Obs1dRefrac), INTENT(inout) :: y       ! Observation vector

  REAL(wp), DIMENSION(x%n_lev)     :: pwvp            ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)     :: pdry            ! Partial dry air pressure  
  REAL(wp), DIMENSION(x%n_lev)     :: refrac          ! Refractivity
  REAL(wp), DIMENSION(x%n_lev)     :: z_geop          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv, zcomp_wet_inv ! compressibilities
  
  REAL(wp)                         :: kap1,kap2,kap3  ! refractivity coefficients used in routine


!-------------------------------------------------------------------------------
! 2. Non ideal gas options 
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:) = 1.0_wp
  zcomp_wet_inv(:) = 1.0_wp
   
! initialise geopotential heights
   
  z_geop(:) = x%geop(:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp   

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress(x,z_geop,zcomp_dry_inv,zcomp_wet_inv)  

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3
     
  ENDIF  


!-------------------------------------------------------------------------------
! 3. Calculate water vapor and dry air pressure
!-------------------------------------------------------------------------------

  pwvp = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)
  
  pdry = x%pres - pwvp
  
!-------------------------------------------------------------------------------
! 4. Calculate refractivity
!-------------------------------------------------------------------------------

  refrac = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

!-------------------------------------------------------------------------------
! 5. Interpolate to measurements geopotential height levels
!-------------------------------------------------------------------------------

  CALL ropp_fm_interpol_log(z_geop, y%geop, refrac, y%refrac)
  
END SUBROUTINE ropp_fm_refrac_1d
