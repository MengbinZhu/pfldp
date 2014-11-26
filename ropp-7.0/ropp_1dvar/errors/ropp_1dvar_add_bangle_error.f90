! $Id: ropp_1dvar_add_bangle_error.f90 2379 2009-11-25 14:27:55Z frhl $

PROGRAM ropp_1dvar_add_bangle_error

!****p* Programs/ropp_1dvar_add_bangle_error *
!
! NAME
!    ropp_1dvar_add_bangle_error - Read a ROPP format netCDF radio
!                                  occultation bending angle
!                                  observation data file and add an error
!                                  description (sigma) to the data. Write the
!                                  resulting file
!
! SYNOPSIS
!    ropp_1dvar_add_bangle_error <obs_file> -Omod <Omodel> -o <out_file> [-h]
!
! DESCRIPTION
!
! ARGUMENTS
!    <obs_file>       Name of input observation file.
!    -Omod <Omodel>   Observational error model:
!                        '1%': 1% at 0 km, 0.1% from 12 km,min(std(BA))=6urad
!                        '2%': 2% at 0 km, 0.2% from 12 km,min(std(BA))=6urad
!                        '3%': 3% at 0 km, 0.3% from 12 km,min(std(BA))=6urad
!                        'MO : Met Office operational implementation
!                              latitudinally varying
!                        'EC': ECMWF operational implementation
!    -o <out_file>    Name of output file (default for single profile files
!                     to write to input file)
!    -h               Help.
!
! NOTES
!    Default is for the output file to overwrite the input file.
!
! SEE ALSO
!    ropp_1dvar_add_refrac_error.f90
!    ropp_1dvar_add_bgr_error.f90
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
  USE ropp_utils
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof
  USE ropp_fm
  USE ropp_fm_copy

  IMPLICIT NONE

  TYPE(ROprof)           :: ro_data

  INTEGER                :: i, j, ilev, iargc, argc, idummy
  INTEGER                :: n_profiles

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: height
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: frac

  LOGICAL               :: give_help
  LOGICAL               :: ranchk = .FALSE.
  LOGICAL               :: to_input = .TRUE.

  CHARACTER(len=4)      :: Omodel
  CHARACTER(len = 4096) :: obs_file
  CHARACTER(len = 4096) :: out_file
  CHARACTER(len = 4096) :: buffer
  CHARACTER(len =  256) :: routine
  CHARACTER(len =   64) :: version
  CHARACTER(len =    6) :: nstr
  CHARACTER(len =    4) :: istr

!-------------------------------------------------------------------------------
! 2. Message handling
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_1dvar_add_bangle_error')

!-------------------------------------------------------------------------------
! 3. Default settings
!-------------------------------------------------------------------------------

  version       = ropp_io_version()
  give_help     = .FALSE.

  Omodel        = "N/A"
  obs_file      = "N/A"
  out_file      = "ropp_add_bangle.nc"

!-------------------------------------------------------------------------------
! 4. Command line arguments
!-------------------------------------------------------------------------------

   argc = iargc()
   i = 1

   DO WHILE (i <= argc)
     CALL getarg (i, buffer)
     SELECT CASE (buffer)
        CASE('-Omod')                    ! Obs errors model
          CALL getarg (i+1, buffer)
          Omodel = buffer
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
          obs_file = buffer
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
       ' ROPP 1DVar: Add bending angle observation errors - ' // TRIM(version))
    CALL message(msg_noin, &
       '---------------------------------------------------------------------')
    CALL message(msg_noin, '')

    IF (out_file .ne. obs_file) CALL file_delete(out_file, idummy)

!-------------------------------------------------------------------------------
! 5. Read data
!-------------------------------------------------------------------------------
    CALL message(msg_info, "Reading observation file " // TRIM(obs_file) )

    n_profiles = ropp_io_nrec(obs_file)

    IF (n_profiles > 1 .AND. to_input) THEN
      CALL message(msg_info, "Reading multiple observation file. " // &
         "Writing updated output to " // TRIM(out_file) )
    ENDIF

    DO i = 1, n_profiles

      WRITE(istr, '(i4)') i
      WRITE(nstr, '(i6)') n_profiles
      CALL message(msg_noin, '')
      CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

      CALL ropp_fm_set_units(ro_data)

      CALL ropp_io_read(ro_data, obs_file, rec = i, ranchk = ranchk)
      CALL message(msg_info, "(" // TRIM(ro_data%occ_id) // ") \n")

!-------------------------------------------------------------------------------
! 6. Generate observation error vector
!-------------------------------------------------------------------------------
      SELECT CASE (Omodel)

        ! 6.1 '1%' model - 1% at 0 km, 0.1% from 12 km, min(std(BA))=6urad
        CASE ('1%')
          DO j=1,ro_data%Lev1b%npoints
            ro_data%Lev1b%bangle_sigma(j) = 0.01_wp +   &
               (0.001-0.01)*MIN(ro_data%Lev1b%impact(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.2 '2%' model - 2% at 0 km, 0.2% from 12 km, min(std(BA))=6urad
        CASE ('2%')
          DO j=1,ro_data%Lev1b%npoints
            ro_data%Lev1b%bangle_sigma(j) = 0.02_wp +   &
               (0.002-0.02)*MIN(ro_data%Lev1b%impact(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.3 '3%' model - 3% at 0 km, 0.3% from 12 km, min(std(BA))=6urad
        CASE ('3%')
          DO j=1,ro_data%Lev1b%npoints
            ro_data%Lev1b%bangle_sigma(j) = 0.03_wp +   &
               (0.003-0.03)*MIN(ro_data%Lev1b%impact(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.4 'MetOffice' model - operational error structures used in MetO
        CASE ('MO')

          ALLOCATE(height(5))
          ALLOCATE(frac(5))

          height = (/ -0.01e5_wp, 0.0_wp, 0.025e5_wp, 0.1e5_wp, 0.7e5_wp /)
          IF (ABS(ro_data%GEOref%lat) >= 60.) THEN  ! High latitudes
            frac = (/ 0.2, 0.2, 0.08, 0.02, 0.02 /)
          ELSE IF (ABS(ro_data%GEOref%lat) > 30. .AND.   &
             ABS(ro_data%GEOref%lat) < 60. ) THEN    ! Middle latitudes
            frac = (/ 0.2, 0.2, 0.11, 0.02, 0.02 /)
          ELSE IF (ABS(ro_data%GEOref%lat) <= 30.) THEN  ! Low latitudes
            frac = (/ 0.2, 0.2, 0.15, 0.02, 0.02 /)
          ENDIF

          DO j=1,ro_data%Lev1b%npoints

            ilev = MAXVAL(WHERE(ro_data%Lev1b%impact(j) > height(1:6)))
            ro_data%Lev1b%bangle_sigma(j) = frac(ilev) +                  &
               (frac(ilev+1)-frac(ilev))/(height(ilev+1)-height(ilev)) *  &
               (ro_data%Lev1b%impact(j) - height(ilev))
          ENDDO

          DEALLOCATE(height)
          DEALLOCATE(frac)

        ! 6.5 'ECMWF' model - operational error structures used at ECMWF
        CASE ('EC')

          DO j=1,ro_data%Lev1b%npoints
            ro_data%Lev1b%bangle_sigma(j) = 0.2_wp +   &
               (0.001-0.2)*MIN(ro_data%Lev1b%impact(j)/10000.0_wp,1.0_wp)
          ENDDO

        CASE default
          CALL message(msg_fatal, "Obs error model " //Omodel// "not available.")

     END SELECT

     ! 6.7 Compute sigma values, (limit errors to  0.006 < sigma < 10)

     DO j=1,ro_data%Lev1b%npoints
       ro_data%Lev1b%bangle_sigma(j) = &
          MIN(ro_data%Lev1b%bangle_sigma(j)*ro_data%Lev1b%bangle(j), 10.0_wp)
       ro_data%Lev1b%bangle_sigma(j) = &
          MAX(ro_data%Lev1b%bangle_sigma(j), 0.000006_wp)
     ENDDO

!-------------------------------------------------------------------------------
! 7. Write error profiles to file
!-------------------------------------------------------------------------------

     CALL message(msg_info, "Writing updated ob errors to " // TRIM(out_file) )

     IF (to_input) THEN
       out_file = obs_file
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

    PRINT *, 'ropp_1dvar_add_bangle_error - Write observation errors to file'
    PRINT *, 'ropp_1dvar_add_bangle_error <obs_file> -Omod <Omodel> [-o <out_file>] [-h]'
    PRINT *, 'Valid options are:'
    PRINT *, '  -h               give (this) help.'
    PRINT *, '  -Omod <Omodel>   name of observation error model.'
    PRINT *, '          1%: 1% at 0 km, 0.1% from 12 km, min(std(BA))=6urad'
    PRINT *, '          2%: 2% at 0 km, 0.2% from 12 km, min(std(BA))=6urad'
    PRINT *, '          3%: 3% at 0 km, 0.3% from 12 km, min(std(BA))=6urad'
    PRINT *, '          MO : Met Office operational error model (lat variation)'
    PRINT *, '          EC : ECMWF operational error model'
    PRINT *, '  -o <out_file>     name of output file.'
    PRINT *, '               (default for single profile files, write to input)'
    PRINT *, '  -b <bg_file>      (optional) name of background file'
    PRINT *, '                    (only required for TP model).'
    PRINT *, '  -c <cov_file>     (optional) name of output covariance file.'
    PRINT *, ''

  END SUBROUTINE usage


END PROGRAM ropp_1dvar_add_bangle_error


