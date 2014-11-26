! $Id: ropp_io_version.f90 3696 2013-06-17 08:48:37Z idculv $

FUNCTION ropp_io_version() RESULT (version)

!****f* common/ropp_io_version *
!
! NAME
!   ropp_io_version
!
! SYNOPSIS
!   Return ROPP_IO version ID string
!
!   USE ropp_io
!   version = ropp_io_version()
!
! DESCRIPTION
!   This function returns the (common) version string for the ROPP_IO
!   module. By default, this function should be called by all ROPP_IO
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

! Edit this string when ROPP_IO module version is updated

  version = "v7.0 31-Jul-2013"

END FUNCTION ropp_io_version


