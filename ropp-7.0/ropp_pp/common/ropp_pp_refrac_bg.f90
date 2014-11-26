! $Id: ropp_pp_refrac_BG.f90 2282 2009-10-22 09:49:29Z frhl $

!****s* ModelRefraction/ropp_pp_refrac_BG *
!
! NAME
!    ropp_pp_refrac_BG - Compute refractivity from BG atmospheric data
!
! SYNOPSIS
!    call ropp_pp_refrac_BG(file, month, lat, lon, alt, refrac)
!
! DESCRIPTION
!    This subroutine calculates a background refractivity profile from a
!    profile of temperature, pressure and humidity data read from a ROPP
!    format netCDF file. Processing is applied according to the ropp_fm module
!    routines.
!
! INPUTS
!    character(len=*) :: file     Background profile filename
!    integer,         :: month    Month of year
!    real(wp),        :: lat      Latitude
!    real(wp),        :: lon      Longitude
!    real(wp), dim(:) :: alt      Altitude levels on which to find N
!
! OUTPUT
!    real(wp), dim(:) :: refrac   Refractivity field
!
! AUTHOR
!   M Gorbunov, Russian Academy of Sciences, Russia.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   Copyright (c) 1998-2010 Michael Gorbunov <michael.gorbunov@zmaw.de>
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

SUBROUTINE ropp_pp_refrac_BG(bfile, month, lat, lon, alt, refrac)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ncdf
  USE ropp_utils
  USE ropp_pp_constants, ONLY: epsilon_water, kappa1, kappa2
  USE ropp_pp, not_this => ropp_pp_refrac_BG

  IMPLICIT NONE

  CHARACTER(len=*),       INTENT(in)  :: bfile    ! Background file
  INTEGER,                INTENT(in)  :: month    ! Month of year
  REAL(wp),               INTENT(in)  :: lat      ! Latitude
  REAL(wp),               INTENT(in)  :: lon      ! Longitude
  REAL(wp), DIMENSION(:), INTENT(in)  :: alt      ! Altitude (m)
  REAL(wp), DIMENSION(:), INTENT(out) :: refrac   ! Refractivity (N-units)

  REAL(wp), DIMENSION(:), ALLOCATABLE :: alt_bg
  REAL(wp), DIMENSION(:), ALLOCATABLE :: geop
  REAL(wp), DIMENSION(:), ALLOCATABLE :: pres
  REAL(wp), DIMENSION(:), ALLOCATABLE :: temp
  REAL(wp), DIMENSION(:), ALLOCATABLE :: shum
  REAL(wp), DIMENSION(:), ALLOCATABLE :: pwvp 
  REAL(wp), DIMENSION(:), ALLOCATABLE :: ref
  INTEGER                             :: nl2
  INTEGER                             :: m1
  REAL(wp)                            :: lat1, lon1
  CHARACTER(len = 256)                :: units
  CHARACTER(len = 256)                :: routine

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_pp_refrac_BG')

!-------------------------------------------------------------------------------
! 2. Read background data
!-------------------------------------------------------------------------------

  ! 2.1 Open file

  CALL ncdf_open(bfile)

  ! 2.2 Check bg position and time
  
  CALL ncdf_getvar('month', m1, rec=1)
  CALL ncdf_getvar('lat', lat1, rec=1)
  CALL ncdf_getvar('lon', lon1, rec=1)

  IF (m1 /= month)     &
     CALL message(msg_diag, "Background file month not equal observation month")
  IF (ABS(lat1-lat) > 30.0_wp .OR. ABS(lon1-lon) > 30.0_wp)   &
     CALL message(msg_diag, "Background file lat/lon not equal observation lat/lon")

  ! 2.3 Obtain dimensions

  CALL ncdf_getsize('geop', nl2, dim=1)
  
  ! 2.4 Initialise and read data
  
  ALLOCATE(alt_bg(nl2))
  ALLOCATE(geop(nl2))
  ALLOCATE(pres(nl2))
  ALLOCATE(temp(nl2))
  ALLOCATE(shum(nl2))
  ALLOCATE(pwvp(nl2))
  ALLOCATE(ref(nl2))

  units="geopotential metres"
  CALL ncdf_getvar('geop',  geop, rec=1, units=units)
  units="Pa"
  CALL ncdf_getvar('press', pres, rec=1, units=units)
  units="kelvin"
  CALL ncdf_getvar('temp',  temp, rec=1, units=units)
  units="kg/kg"
  CALL ncdf_getvar('shum',  shum, rec=1, units=units)
  
  CALL ncdf_close()

  alt_bg = geopotential2geometric(lat1, geop)
  
!-------------------------------------------------------------------------------
! 3. Compute refractivity
!-------------------------------------------------------------------------------
  
  pwvp = pres * shum / ( epsilon_water + (1.0_wp - epsilon_water)*shum)
  ref = kappa1 * pres/temp + kappa2 * pwvp/(temp**2)  

  CALL ropp_pp_interpol_log(alt_bg, alt, ref, refrac)

!-------------------------------------------------------------------------------
! 4. Clean up
!-------------------------------------------------------------------------------

  DEALLOCATE(alt_bg)
  DEALLOCATE(geop)
  DEALLOCATE(pres)
  DEALLOCATE(temp) 
  DEALLOCATE(shum)
  DEALLOCATE(pwvp)
  DEALLOCATE(ref)
 
  CALL message_set_routine(routine)

END SUBROUTINE ropp_pp_refrac_BG

