! $Id: ropp_io_read.f90 3551 2013-02-25 09:51:28Z idculv $

!****s* Reading/ropp_io_read *
!
! NAME
!    ropp_io_read - Read an ROPP data file.
!
! SYNOPSIS
!    use ropp_io
!    type(ROprof) :: ro_data
!      ...
!    call ropp_io_read(data, file [, centre= ..., rec=..., ierr= ..., ranchk=...])
!
! DESCRIPTION
!    This subroutine reads RO event data from a specified file. The file
!    must conform to the ROPP (netCDF) data format
!
! INPUTS
!    data    dtyp   one of the data types defined in ropp_io_types:
!                    e.g.  ROprof
!    file    chr    name of input file
!    centre  string centre identifier (optional), e.g. UCAR
!    rec     int    record number (optional, default 1)
!    ranchk  log    range check flag (optional) Valid values are:
!                    - .T. to perform a range check after input (default) or
!                    - .F. to skip range checking
!                   This flag should normally not be used or set to default .T.;
!                   it is intended for use only when invalid data is explicitly
!                   required to survive from the input netCDF file, such as when
!                   reading invalid data for testing application Q/C.
!
! OUTPUT
!    ierr  int    exit I/O code;
!                    -  0 = ok
!                    - -1 = unexpected EOF
!                    - I/O error (system dependent or as obtained from netCDF)
!
! NOTES
!    If ierr is not given, the routine will abort in case of an error.
!
! SEE ALSO
!    ropp_io
!    ropp_io_types
!    ropp_io_write
!
! CALLS
!    ropp_io_read_ncdf_get
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
! 1. Reading a single profile of ROprof data (netCDF only)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_rodatas(ROdata, file, rec, centre, ierr, ranchk, resolution, getlevel1a, getbufr)

! 1.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_rodatas
  USE ropp_io_types, ONLY: ROprof, &
                           ropp_io_ncopensep

  IMPLICIT NONE

  TYPE(ROprof),       INTENT(inout) :: ROdata
  CHARACTER(len = *), INTENT(in)    :: file
  INTEGER,            OPTIONAL      :: rec
  CHARACTER(len=20),  OPTIONAL      :: centre
  INTEGER,            OPTIONAL      :: ierr
  LOGICAL,            OPTIONAL      :: ranchk
  CHARACTER(len=20),  OPTIONAL      :: resolution
  LOGICAL,            OPTIONAL      :: getlevel1a
  LOGICAL,            OPTIONAL      :: getbufr

  INTEGER                           :: irec
  CHARACTER(len=20)                 :: icentre
  CHARACTER(len = 256)              :: routine
  LOGICAL                           :: iranchk

! 1.2 Error handling
! ------------------

  IF (PRESENT(ierr)) THEN
     ierr = 0
  ELSE
     CALL message_get_routine(routine)
     CALL message_set_routine('ropp_io_read')
  ENDIF

  IF (PRESENT(ranchk)) THEN
    iranchk = ranchk
  ELSE
    iranchk = .TRUE.
  ENDIF

! 1.3 Call appropriate routine for reading
! ----------------------------------------

  IF (is_netcdf(file)) THEN

! 1.3.1 Record number

     IF (PRESENT(rec)) THEN
        irec = rec
     ELSE
        irec = 1
     ENDIF

! 1.3.2 Read data from the netCDF file

     IF (.NOT. ropp_io_ncopensep) THEN
        CALL ncdf_open(file)
     ENDIF

     IF (PRESENT(centre)) THEN
        icentre = TRIM(centre)
     ELSE
        icentre = " "
     END IF
     SELECT CASE (icentre)
       CASE('UCAR')
         CALL ropp_io_read_ncdf_get(ROdata, file, icentre, rec=irec)
       CASE('EUM')
         CALL ropp_io_read_ncdf_get(ROdata, file, icentre, rec=irec, resolution=resolution, getlevel1a=getlevel1a, getbufr=getbufr)
       CASE default
         CALL ropp_io_read_ncdf_get(ROdata, rec=irec)
     END SELECT

     IF (.NOT. ropp_io_ncopensep) THEN
       CALL ncdf_close()
     ENDIF

  ELSE

     CALL message(msg_fatal, &
          "Only netCDF format file read is supported. \n" // &
          "             Check input file.\n             " // &
          "Use conversion tools to convert to ROPP netCDF if required.")
  ENDIF

! 1.4 Range check Q/C
! -------------------

  IF (iranchk) CALL ropp_io_rangecheck ( ROdata )

! 1.5 Clean up
! ------------

  IF (.NOT. PRESENT(ierr)) THEN
     CALL message_set_routine(routine)
  ENDIF

END SUBROUTINE ropp_io_read_rodatas


!-------------------------------------------------------------------------------
! 2. Reading multiple profiles of ROprof data (netCDF only)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_rodatam(ROdata, file, ierr, ranchk)

! 2.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_rodatam
  USE ropp_io_types, ONLY: ROprof

  IMPLICIT NONE

  TYPE(ROprof), DIMENSION(:),   POINTER       :: ROdata
  CHARACTER(len = *),           INTENT(in)    :: file
  INTEGER,            OPTIONAL, INTENT(out)   :: ierr
  LOGICAL,            OPTIONAL, INTENT(in)    :: ranchk

  INTEGER                                     :: i
  INTEGER                                     :: n_profiles
  CHARACTER(len = 256)                        :: routine
  LOGICAL                                     :: iranchk

! 2.2 Error handling
! ------------------

  IF (PRESENT(ierr)) THEN
     ierr = 0
  ELSE
     CALL message_get_routine(routine)
     CALL message_set_routine('ropp_io_read')
  ENDIF

  IF (PRESENT(ranchk)) THEN
    iranchk = ranchk
  ELSE
    iranchk = .TRUE.
  ENDIF

! 2.3 Free already used memory
! ----------------------------

  IF (ASSOCIATED(ROdata)) THEN
     IF (SIZE(ROdata) > 0) THEN
        DO i = 1, SIZE(ROdata)
           CALL ropp_io_free(ROdata(i))
        ENDDO
        DEALLOCATE(ROdata)
     ENDIF
  ENDIF

! 2.4 Read data from netCDF file
! ------------------------------

  IF (is_netcdf(file)) THEN

     ! 2.4.1 Open file

     CALL ncdf_open(file)

     ! 2.4.2 Get number of profiles

     CALL ncdf_getsize('time', n_profiles)

     ! 2.4.3 Allocate and read each profile

     ALLOCATE(ROdata(n_profiles))

     DO i = 1, n_profiles
        CALL ropp_io_read_ncdf_get(ROdata(i), rec = i)
     ENDDO

     ! 2.4.4 Close netCDF

     CALL ncdf_close()

  ELSE

    CALL message(msg_fatal, &
          "Only netCDF format file read is supported. \n" // &
          "             Check input file.\n             " // &
          "Use conversion tools to convert to ROPP netcdf if required.")

  ENDIF

! 2.5 Range check Q/C
! -------------------

  IF (iranchk) THEN
    DO i = 0, SIZE(ROdata)
      CALL ropp_io_rangecheck ( ROdata(i) )
     ENDDO
  ENDIF

! 2.6 Clean up
! ------------

  IF (.NOT. PRESENT(ierr)) THEN
     CALL message_set_routine(routine)
  ENDIF

END SUBROUTINE ropp_io_read_rodatam

!-------------------------------------------------------------------------------
! 3. Reading a single profile of ROprof data (netCDF only) - two dimensional
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_rodata_2d(ROdata, file, rec, ierr, ranchk)

! 3.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_rodata_2d
  USE ropp_io_types, ONLY: ROprof2d, &
                           ropp_io_ncopensep

  IMPLICIT NONE

  TYPE(ROprof2d),     INTENT(inout) :: ROdata
  CHARACTER(len = *), INTENT(in)    :: file
  INTEGER,            OPTIONAL      :: rec
  INTEGER,            OPTIONAL      :: ierr
  LOGICAL,            OPTIONAL      :: ranchk

  INTEGER                           :: irec

  CHARACTER(len =  256)             :: routine
  LOGICAL                           :: iranchk

! 3.2 Error handling
! ------------------

  IF (PRESENT(ierr)) THEN
     ierr = 0
  ELSE
     CALL message_get_routine(routine)
     CALL message_set_routine('ropp_io_read')
  ENDIF

  IF (PRESENT(ranchk)) THEN
    iranchk = ranchk
  ELSE
    iranchk = .TRUE.
  ENDIF

! 3.3 Call appropriate routine for reading
! ----------------------------------------

  IF (is_netcdf(file)) THEN

! 3.3.1 Record number

     IF (PRESENT(rec)) THEN
        irec = rec
     ELSE
        irec = 1
     ENDIF

! 3.3.2 Read data from the netCDF file

     IF (.NOT. ropp_io_ncopensep) THEN
        CALL ncdf_open(file)
     ENDIF

     CALL ropp_io_read_ncdf_get(ROdata, rec=irec)

     IF (.NOT. ropp_io_ncopensep) THEN
        CALL ncdf_close()
     ENDIF

  ELSE

     CALL message(msg_fatal, &
          "Only netCDF format file read is supported. \n" // &
          "             Check input file.\n             " // &
          "Use conversion tools to convert to ROPP netcdf if required.")

  ENDIF

! 3.4 Range check Q/C
! -------------------

  IF (iranchk) call ropp_io_rangecheck ( ROdata )

! 3.5 Clean up
! ------------

  IF (.NOT. PRESENT(ierr)) THEN
     CALL message_set_routine(routine)
  ENDIF

END SUBROUTINE ropp_io_read_rodata_2d

!-------------------------------------------------------------------------------
! 4. Reading error correlation / covariances (netCDF only)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_rocorcov(ROdata, file, ierr)

! 4.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_rocorcov
  USE ropp_io_types, ONLY: ROcorcov

  IMPLICIT NONE

  TYPE(ROcorcov), DIMENSION(:), POINTER       :: ROdata
  CHARACTER(len = *),           INTENT(in)    :: file
  INTEGER,            OPTIONAL, INTENT(out)   :: ierr
  CHARACTER(len = 256)                        :: routine

! 4.2 Error handling
! ------------------

  IF (PRESENT(ierr)) THEN
     ierr = 0
  ELSE
     CALL message_get_routine(routine)
     CALL message_set_routine('ropp_io_read')
  ENDIF

! 4.3 Free already used memory
! ----------------------------

  IF (ASSOCIATED(ROdata)) THEN
     CALL ropp_io_free(ROdata)
  ENDIF

! 4.4 Read data from the netCDF file
! ----------------------------------

  IF (is_netcdf(file)) THEN

     ! 4.4.1 Open netCDF

     CALL ncdf_open(file)

     ! 4.4.2 Read error correlation / covariance data

     CALL ropp_io_read_ncdf_get(ROdata)

     ! 4.4.3 Close netCDF

     CALL ncdf_close()

  ELSE

     CALL message(msg_fatal, &
          "Only netCDF is supported for reading error correlation / covariance data.")

  ENDIF

! 4.5 Clean up
! ------------

  IF (.NOT. PRESENT(ierr)) THEN
     CALL message_set_routine(routine)
  ENDIF

END SUBROUTINE ropp_io_read_rocorcov
