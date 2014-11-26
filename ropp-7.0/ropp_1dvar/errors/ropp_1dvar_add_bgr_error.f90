! $Id: ropp_1dvar_refrac.f90 2379 2009-11-25 14:27:55Z frhl $

PROGRAM ropp_1dvar_add_bgr_error

!****p* Programs/ropp_1dvar_add_bgr_error *
!
! NAME
!    ropp_1dvar_add_bgr_error - Read a ROPP netCDF background file containing
!                               p, T, q and add an error description (sigma
!                               values) to the data. Write resulting file.
!
! SYNOPSIS
!    ropp_1dvar_add_bgr_error <bg_file> -c <cov_file> [-o <out_file>] [-h]
!
! DESCRIPTION
!       This program adds profile-by-profile background error values to a
!       ROPP format file by reading sigma values from the provided error
!       covariance matrix files (in the errors/ directory).
!       Users should modify this program to utilise background error
!       descriptions from other sources.
!
! ARGUMENTS
!    <bg_file>        Name of input background file.
!    -c <cov_file>    Name of background error covariance matrix file
!    -o <out_file>    Name of output file (default for single profile files
!                     to write to input file)
!    -h               Help.

! SEE ALSO
!    ropp_1dvar_add_refrac_error.f90
!    ropp_1dvar_add_bangle_error.f90
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
  USE ncdf
  USE ropp_utils
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof
  USE ropp_fm
  USE ropp_fm_copy

  IMPLICIT NONE

  TYPE(ROprof)          :: ro_data     ! Background data

  INTEGER                :: i, j, k, m, n, iargc, argc, idummy, ilat
  INTEGER                :: n_profiles

  REAL(wp), DIMENSION(:),   POINTER :: lat_min => null()
  REAL(wp), DIMENSION(:),   POINTER :: lat_max => null()
  REAL(wp), DIMENSION(:,:), POINTER :: sigma => null()

  LOGICAL               :: give_help
  LOGICAL               :: ranchk = .FALSE.
  LOGICAL               :: to_input = .TRUE.

  CHARACTER(len = 4096) :: bgr_file
  CHARACTER(len = 4096) :: out_file
  CHARACTER(len = 4096) :: cov_file
  CHARACTER(len = 4096) :: buffer
  CHARACTER(len =  256) :: routine
  CHARACTER(len =   64) :: version
  CHARACTER(len =    6) :: nstr
  CHARACTER(len =    4) :: istr

!-------------------------------------------------------------------------------
! 2. Message handling
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_1dvar_add_bgr_error')

!-------------------------------------------------------------------------------
! 3. Default settings
!-------------------------------------------------------------------------------

  version       = ropp_io_version()
  give_help     = .FALSE.

  cov_file      = "N/A"
  bgr_file      = "N/A"
  out_file      = "ropp_add_bgr.nc"

!-------------------------------------------------------------------------------
! 4. Command line arguments
!-------------------------------------------------------------------------------

   argc = iargc()
   i = 1

   DO WHILE (i <= argc)
     CALL getarg (i, buffer)
     SELECT CASE (buffer)
        CASE('-c')                    ! Error covariance matrix file
          CALL getarg (i+1, buffer)
          cov_file = buffer
          i = i + 1
        CASE('-o')                       ! Output filename
          CALL getarg (i+1, buffer)
          to_input = .FALSE.
          out_file = buffer
          i = i + 1
        CASE('-no_ranchk')               ! No rangecheck on read/write
          ranchk = .FALSE.
        CASE('-h')                       ! Give usage information
          give_help = .TRUE.
        CASE default                     ! Input file name
          bgr_file = buffer
      END SELECT
      i = i+1
    ENDDO

    IF (argc == 0 .OR. give_help) THEN
      CALL usage()
      CALL EXIT(0)
    ENDIF

    CALL message(msg_noin, '')
    CALL message(msg_noin, &
       '---------------------------------------------------------------------')
    CALL message(msg_noin, &
       ' ROPP 1DVar: Add background errors - ' // TRIM(version))
    CALL message(msg_noin, &
       '---------------------------------------------------------------------')
    CALL message(msg_noin, '')

    IF (out_file .ne. bgr_file) CALL file_delete(out_file, idummy)

!-------------------------------------------------------------------------------
! 6. Read error covariance matrix
!-------------------------------------------------------------------------------
    CALL ncdf_open(cov_file)
    IF (ncdf_isvar('lat_min') .AND. ncdf_isvar('lat_max')) THEN
     CALL ncdf_getsize('lat_min', m)
     CALL ncdf_getsize('lat_max', n)
   ELSE
     CALL message(msg_fatal, &
        "NetCDF data file does not seem to contain an error correlation or covariance structure.")
   ENDIF

   CALL ncdf_getvar_alloc('lat_min', lat_min)
   CALL ncdf_getvar_alloc('lat_max', lat_max)

   IF (ncdf_isvar('sigma')) THEN
     CALL ncdf_getvar_alloc('sigma', sigma)
   ELSE
     CALL message(msg_fatal, &
        "NetCDF data file does not seem to contain sigma values.")
   ENDIF

   CALL ncdf_close(cov_file)

!-------------------------------------------------------------------------------
! 5. Read background data
!-------------------------------------------------------------------------------
    CALL message(msg_info, "Reading background file " // TRIM(bgr_file) )

    n_profiles = ropp_io_nrec(bgr_file)

    IF (n_profiles > 1 .AND. to_input) THEN
      CALL message(msg_info, "Reading multiple background file. " // &
         "Writing updated output to " // TRIM(out_file) )
    ENDIF

    DO i = 1, n_profiles

      WRITE(istr, '(i4)') i
      WRITE(nstr, '(i6)') n_profiles
      CALL message(msg_noin, '')
      CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

      CALL ropp_fm_set_units(ro_data)

      CALL ropp_io_read(ro_data, bgr_file, rec = i, ranchk = ranchk)
      CALL message(msg_info, "(" // TRIM(ro_data%occ_id) // ") \n")

!-------------------------------------------------------------------------------
! 6. Add sigma values
!-------------------------------------------------------------------------------
      ilat = 1
      DO j=1,m
        IF (ro_data%GEOref%lat >= lat_min(j) .AND.   &
           ro_data%GEOref%lat <= lat_max(j)) ilat = j
      ENDDO

      ! 6.1 ECMWF-type hybrid level state structure

      IF (INDEX(ro_data%bg%source,'HYBRID') > 0 .OR. &
         INDEX(ro_data%bg%source,'ECMWF') > 0) THEN

        IF ( SIZE(sigma) .ne. 2*ro_data%Lev2b%npoints+1) THEN
          CALL message(msg_fatal, &
           "Number of covariance matrix file sigmas and background state vector data points differ - check input.")
        ENDIF

        DO j=1, ro_data%Lev2b%npoints
          ro_data%Lev2b%temp_sigma(j) = MIN(sigma(j,ilat), 5.0_wp)
        ENDDO
        DO j=1, ro_data%Lev2b%npoints
          k = ro_data%Lev2b%npoints + j
          ro_data%Lev2b%shum_sigma(j) = MIN(sigma(k,ilat), 5.0_wp)
        ENDDO
        ro_data%Lev2c%press_sfc_sigma = MIN(sigma(2*ro_data%Lev2b%npoints + 1,ilat), 100.0_wp)

      ELSE IF (INDEX(ro_data%bg%source,'METOFFICE') > 0) THEN

        IF ( SIZE(sigma) .ne. 2*ro_data%Lev2b%npoints+1) THEN
          CALL message(msg_fatal, &
           "Number of covariance matrix file sigmas and background state vector data points differ - check input.")
        ENDIF

        ro_data%Lev2c%press_sfc_sigma = MIN(sigma(1,ilat), 100.0_wp)
        DO j=2, ro_data%Lev2b%npoints-1
          ro_data%Lev2b%press_sigma(j-1) = MIN(sigma(j,ilat), 100.0_wp)
        ENDDO
        DO j=1, ro_data%Lev2b%npoints
          k = ro_data%Lev2b%npoints + 1 + j
          ro_data%Lev2b%shum_sigma(j) = MIN(sigma(k,ilat), 5.0_wp)
        ENDDO

      ELSE

        CALL message(msg_fatal, "Input background data source " //  &
           ro_data%bg%source // " not recognised.")

      ENDIF

!-------------------------------------------------------------------------------
! 7. Write error profiles to file
!-------------------------------------------------------------------------------

     CALL message(msg_info, "Writing updated bg errors to " // TRIM(out_file) )

     IF (to_input) THEN
       out_file = bgr_file
       CALL ropp_io_write(ro_data, out_file, ranchk=.FALSE.)
     ELSE
       CALL ropp_io_write(ro_data, out_file, append=.TRUE., ranchk=.FALSE.)
     ENDIF

     CALL ropp_io_free(ro_data)

   ENDDO

CONTAINS

!-------------------------------------------------------------------------------
! 9. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()

    PRINT *, 'ropp_1dvar_add_bgr_error - Write background errors to file'
    PRINT *, 'ropp_1dvar_add_bgr_error <bgr_file> -c <cov_file> [-o <out_file>] [-h]'
    PRINT *, 'Valid options are:'
    PRINT *, '  -h               give (this) help.'
    PRINT *, '  -c <cov_file>    name of error covariance file.'
    PRINT *, '  -o <out_file>     name of output file.'
    PRINT *, '               (default for single profile files, write to input)'
    PRINT *, ''

  END SUBROUTINE usage


END PROGRAM ropp_1dvar_add_bgr_error
