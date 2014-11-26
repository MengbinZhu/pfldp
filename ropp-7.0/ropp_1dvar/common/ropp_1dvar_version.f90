! $Id: ropp_1dvar_version.f90 3696 2013-06-17 08:48:37Z idculv $

FUNCTION ropp_1dvar_version() RESULT (version)

!****f* common/ropp_1dvar_version *
!
! NAME
!   ropp_1dvar_version
!
! SYNOPSIS
!   Return ROPP_1DVAR version ID string
!
!   USE ropp_1dvar
!   version = ropp_1dvar_version()
!
! DESCRIPTION
!   This function returns the (common) version string for the ROPP_1DVAR
!   module. By default, this function should be called by all ROPP_1DVAR
!   tools to display a version ID when the '-v' command-line switch is
!   used.
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

  CHARACTER (LEN=40) :: version

! Edit this string when ROPP_1DVAR module version is updated

  version = "v7.0 31-Jul-2013"

END FUNCTION ropp_1dvar_version


