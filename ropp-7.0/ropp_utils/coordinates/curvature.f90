! $Id: curvature.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/curvature
!
! NAME
!    curvature - Determine the section curvature, its centre for the Earth
!                reference ellipsoid, and the curvature radius
!
! SYNOPSIS
!    call curvature(lat, lon, theta, r_coc, roc)
! 
! DESCRIPTION
!    This subroutine calculates the cartesian coordinates of the local centre
!    of curvature and the radius for the reference ellipsoid.
!
! INPUTS
!    lat           latitude of surface point
!    lon           longitude of surface point
!    theta         azimuth direction of the cross-section
!
! OUTPUT
!    r_coc         cartesian centre of curvature vector (relative to ECF frame)
!    roc           radius of curvature value           
!
! NOTES
!    Uses theorems of Meusnier and Euler
!
! REFERENCES
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

subroutine curvature(lat, lon, theta, r_coc, roc)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: geod2cart, deg2rad, Rp, Re

  implicit none

  real(wp), intent(in)                :: lat     ! Surface point latitude
  real(wp), intent(in)                :: lon     ! Surface point longitude
  real(wp), intent(in)                :: theta   ! Azimuth direction
  real(wp), dimension(3), intent(out) :: r_coc   ! Centre curvature
  real(wp),               intent(out) :: roc     ! Radius curvature

  real(wp), dimension(3)  :: x   ! Cartesian coordinates
  real(wp), dimension(3)  :: n   ! Surface normal
  real(wp)                :: Ak1 ! Main meridional curvature
  real(wp)                :: Ak2 ! Main parallel curvature
  real(wp)                :: rlat, rlon

! 1.2 Initialisation
! ------------------

  rlat = lat * deg2rad
  rlon = lon * deg2rad
  call geod2cart(lat, lon, 0.0_wp, x)

! 1.3 Calculate radius of curvature
! ---------------------------------
     
  Ak1 = Rp*Re**4 / (Sqrt(Re**4 + (Rp**2 - Re**2)*Sum(x(1:2)**2)))**3
  Ak2 = Cos(rlat) / Sqrt(Sum(X(1:2)**2))
  roc  = 1.0_wp / Abs(Ak1*(Cos(Theta))**2 + Ak2*(Sin(Theta))**2)

! 1.4 Calculate curvature centre
! ------------------------------
  
  ! Normal to surface
  
  n(1) = Cos(rlat)*Cos(rlon)
  n(2) = Cos(rlat)*Sin(rlon)
  n(3) = Sin(rlat)

  r_coc(:) = x(:) - roc*n(:)

end subroutine curvature



