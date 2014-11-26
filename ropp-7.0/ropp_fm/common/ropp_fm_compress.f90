! $Id: ropp_fm_compress.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress(x, z_geop, zcomp_dry_inv, zcomp_wet_inv)

!****s* Compressibility/ropp_fm_compress *
!
! NAME
!    ropp_fm_compress - Calculate geopotential height and 
!                       wet and dry compressibilities.
!
! SYNOPSIS
!    call ropp_fm_compresss(x, z_geop, zcomp_dry_inv, zcomp_wet_inv)
! 
! DESCRIPTION
!    This routine calculates the inverses of the wet and dry compressibilities
!    and adjusts the model geopotential height value to account for non-ideal
!    effects.
!
! INPUTS
!    TYPE(State1dFM)       :: x               ! Model state vector.
!
! OUTPUT
!    REAL(wp)               :: z_geop          ! Geopotential heights.
!    REAL(wp)               :: zcomp_dry_inv   ! Inverse of dry compressibility.
!    REAL(wp)               :: zcomp_wet_inv   ! Inverse of wet compressibility.
!
! NOTES
!    Wet and dry compressibilities on model levels calculated using 
!    known constants. The geopotential heights are adjusted accordingly. 
!    Modified GPH and reciprocal wet and dry compressibilities are returned. 
!
! SEE ALSO
!    ropp_fm_compress_ad
!    ropp_fm_compress_tl
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
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),              INTENT(in)  :: x              ! State vector
  REAL(wp), DIMENSION(x%n_lev), INTENT(out) :: z_geop         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev), INTENT(out) :: zcomp_dry_inv  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev), INTENT(out) :: zcomp_wet_inv  ! inverse of wet comp


! local variables

  INTEGER :: i
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix,zcomp1,zcomp2,zcomp3
  REAL(wp) :: zpd,zpwet,ztc,zx,zpot
  REAL(wp) :: zterm1,zterm2,zterm3

! parameters from the Picard et al., 2008, Metrologia, vol 45. 149-155, Append A-2

  REAL(wp), PARAMETER :: za0=1.58123E-6_wp
  REAL(wp), PARAMETER :: za1=-2.91331E-8_wp
  REAL(wp), PARAMETER :: za2=1.1043E-10_wp
  REAL(wp), PARAMETER :: zb0=5.707E-6_wp
  REAL(wp), PARAMETER :: zb1=-2.051E-8_wp
  REAL(wp), PARAMETER :: zc0=1.9898E-4_wp
  REAL(wp), PARAMETER :: zc1=-2.376E-6_wp
  REAL(wp), PARAMETER :: zd=1.83E-11_wp
  REAL(wp), PARAMETER :: ze=-0.765E-8_wp

!-----------------------------------------------------------------
! 1. Calculate the compressibility on the model levels
!-----------------------------------------------------------------

  DO i = 1,x%n_lev

! Temp in celcius
   
     ztc = x%temp(i) - 273.15_wp

! calculate the water vapour and dry pressure

     zpwet = x%pres(i) * x%shum(i) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(i))

! dry pressure

     zpd = x%pres(i) - zpwet

     zterm1 = za0+ztc*(za1+ztc*za2)

     zterm2 = zb0+zb1*ztc
   
     zterm3 = zc0+zc1*ztc
   
! compressibility of moist air
  
     zpot = x%pres(i)/x%temp(i)
  
     zx = zpwet/x%pres(i)
 
     zcomp1(i) = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3)) 
   
     zcomp1(i) = zcomp1(i) + zpot**2*(zd + zx**2*ze)
        
! compressibility for dry air
   
     zpot = zpd/x%temp(i)
   
     zcomp2(i) = 1.0_wp - zpot*(zterm1 - zpot*zd)
   
! compressibility of water vapour

     zpot = zpwet/x%temp(i)
   
     zcomp3(i) = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))
   
  ENDDO   

!------------------------------------------------------------------
! 2. adjust the geopotential heights and invert the wet/dry compress.
!------------------------------------------------------------------
  
  z_geop(:) = 0.0_wp
    
  DO i = 1, x%n_lev

    IF (i == 1) THEN
   
      zcomp_mix(i) = zcomp1(i)
   
      z_geop(i) = x%geop(i)*zcomp_mix(i)
   
    ELSE
  
! use the mean value of the i and (i-1) levels
   
      zcomp_mix(i) = 0.5_wp*(zcomp1(i)+zcomp1(i-1))
      
      z_geop(i) = z_geop(i-1)+zcomp_mix(i)*(x%geop(i)-x%geop(i-1))
            
    ENDIF    
               
! return the inverse of the compressibility for dry air + vapour 
! used to calculate the refractivity.  
   
    zcomp_dry_inv(i) = 1.0_wp/zcomp2(i)
   
    zcomp_wet_inv(i) = 1.0_wp/zcomp3(i)
   
  ENDDO   


END SUBROUTINE ropp_fm_compress


