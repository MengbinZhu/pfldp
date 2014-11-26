! $Id: ropp_io_nrec.f90 3551 2013-02-25 09:51:28Z idculv $

FUNCTION ropp_io_nrec(file) RESULT(n)

!****f* Information/ropp_io_nrec *
!
! NAME
!    ropp_io_nrec - Number of records (e.g., vertical profiles) in an ROPP file.
!
! SYNOPSIS
!    n = ropp_io_nrec(file)
! 
! DESCRIPTION
!    This function returns the number of records (e.g., vertical profiles)
!    in a netCDF based ROPP data file. 
!
! INPUTS
!    char :: file   Name of data file to query.
!
! OUTPUT
!    int  :: n      Number of records; -1 if the data file is not a netCDF.
!
! NOTES
!    No attempt is made to check if the queried data file is indeed an ROPP
!    data file, or simply an ordinary netCDF data file.
!
!    If the quieried netCDF file is an ordinary netCDF file and has no
!    unlimited dimension, an error message might occur.
!
! SEE ALSO
!
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

  USE ncdf

  IMPLICIT NONE

  CHARACTER(len = *)   :: file
  INTEGER              :: n

!-------------------------------------------------------------------------------
! 2. Get the number of records
!-------------------------------------------------------------------------------

  IF (is_netcdf(file)) THEN

     CALL ncdf_open(file)
     n = ncdf_getnrec()
     CALL ncdf_close()

  ELSE

     n = -1

  ENDIF

END FUNCTION ropp_io_nrec
