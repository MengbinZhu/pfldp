! $Id: ropp_1dvar_add_refrac_error.f90 2379 2009-11-25 14:27:55Z frhl $

PROGRAM ropp_1dvar_add_refrac_error

!****p* Programs/ropp_1dvar_add_refrac_error *
!
! NAME
!    ropp_1dvar_add_refrac_error - Read a ROPP format netCDF radio
!                                  occultation refractivity
!                                  observation data file and add an error
!                                  description (sigma) to the data. Write the
!                                  resulting file
!
! SYNOPSIS
!    ropp_1dvar_add_refrac_error <obs_file> -Omod <Omodel> -o <out_file> [-b <bg_file>] [-c <corr_file>] [-h]
!
! DESCRIPTION
!
! ARGUMENTS
!    <obs_file>       Name of input observation file.
!    -Omod <Omodel>   Observational error model:
!                        '1%': 1% at 0 km, 0.1% from 12 km, min(std(N))=0.02
!                        '2%': 2% at 0 km, 0.2% from 12 km, min(std(N))=0.02
!                        '3%': 3% at 0 km, 0.3% from 12 km, min(std(N))=0.02
!                        'TP : ROM SAF operational implementation. As '2%'
!                              model with dynamic tropopause height (default)
!                              calculation from bg data rather than 12 km.
!                        'MO : Met Office operational implementation
!                              latitudinally varying
!                        'SK': according to Steiner-Kirchengast [2005]
!    -o <out_file>    Name of output file
!    -b <bg_file>     Name of background file (only required for 'TP' model).
!    -c <corr_file>   Name of optional output correlation file
!    -h               Help.
!
! NOTES
!    Default is for the output file to overwrite the input file.
!
! SEE ALSO
!    ropp_1dvar_add_bgr_error.f90

! AUTHOR
!   Met Office, Exeter, UK.
!   DMI, Copenhagen, Denmark.
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
  USE matrix

  IMPLICIT NONE

  TYPE(ROprof)           :: ro_data, BG_data

  INTEGER                :: i, j, k, ilev, iargc, argc, idummy
  INTEGER                :: n_profiles, n, trop_lev
  INTEGER                :: file_id, dim1, dim2, dim3, var

  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: Ocorr, Ocorr_out, sigma_out
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: Ocorr_packed
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: height
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: frac
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: grad_T
  REAL(wp)                              :: trop

  LOGICAL               :: give_help
  LOGICAL               :: ranchk = .FALSE.
  LOGICAL               :: to_input = .TRUE.

  CHARACTER(len=4)      :: Omodel
  CHARACTER(len = 4096) :: obs_file
  CHARACTER(len = 4096) :: bg_file
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
  CALL message_set_routine('ropp_1dvar_add_refrac_error')

!-------------------------------------------------------------------------------
! 3. Default settings
!-------------------------------------------------------------------------------

  version       = ropp_io_version()
  give_help     = .FALSE.

  Omodel        = "N/A"
  obs_file      = "N/A"
  bg_file       = "N/A"
  out_file      = "ropp_add_refrac.nc"
  cov_file      = "N/A"

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
        CASE('-b')                       ! Background filename (if needed)
          CALL getarg (i+1, buffer)
          bg_file = buffer
          i = i + 1
        CASE('-o')                       ! Output filename
          CALL getarg (i+1, buffer)
          to_input = .FALSE.
          out_file = buffer
          i = i + 1
        CASE('-c')                       ! Output covariance matrix filename
          CALL getarg (i+1, buffer)
          cov_file = buffer
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
       ' ROPP 1DVar: Add refractivity observation errors - ' // TRIM(version))
    CALL message(msg_noin, &
       '---------------------------------------------------------------------')
    CALL message(msg_noin, '')

    CALL file_delete(out_file, idummy)

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

        ! 6.1 '1%' model - 1% at 0 km, 0.1% from 12 km, min(std(N))=0.02
        CASE ('1%')
          DO j=1,ro_data%Lev2a%npoints
            ro_data%Lev2a%refrac_sigma(j) = 0.01_wp +   &
               (0.001-0.01)*MIN(ro_data%Lev2a%geop_refrac(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.2 '2%' model - 2% at 0 km, 0.2% from 12 km, min(std(N))=0.02
        CASE ('2%')
          DO j=1,ro_data%Lev2a%npoints
            ro_data%Lev2a%refrac_sigma(j) = 0.02_wp +   &
               (0.002-0.02)*MIN(ro_data%Lev2a%geop_refrac(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.3 '3%' model - 3% at 0 km, 0.3% from 12 km, min(std(N))=0.02
        CASE ('3%')
          DO j=1,ro_data%Lev2a%npoints
            ro_data%Lev2a%refrac_sigma(j) = 0.03_wp +   &
               (0.003-0.03)*MIN(ro_data%Lev2a%geop_refrac(j)/12000.0_wp,1.0_wp)
          ENDDO

        ! 6.4 Steiner-Kirchengast [2002]
        CASE ('SK')
          DO j=1,ro_data%Lev2a%npoints
            ro_data%Lev2a%refrac_sigma(j) = 0.001_wp
            IF (ro_data%Lev2a%geop_refrac(j) > 20000.0_wp)     &
             ro_data%Lev2a%refrac_sigma(j) =                   &
               0.001_wp*exp((ro_data%Lev2a%geop_refrac(j)-20000.0_wp)/11100.0_wp)
            IF (ro_data%Lev2a%geop_refrac(j) < 14000.0_wp)          &
             ro_data%Lev2a%refrac_sigma(j) = 0.001_wp + 0.045_wp*   &
             (1000.0_wp/ro_data%Lev2a%geop_refrac(j)-1000.0_wp/14000.0_wp)
            IF (ro_data%Lev2a%geop_refrac(j) < 2000.0_wp)      &
             ro_data%Lev2a%refrac_sigma(j) = 0.001_wp +        &
               0.045_wp*(1000.0_wp/2000.0_wp - 1000.0_wp/14000.0_wp)
          ENDDO

        ! 6.5 'Tropopause' model - as 2% model, with dynamic tropopause height
        CASE ('TP')

          CALL ropp_fm_set_units(bg_data)
          ! 6.5.1 Read specified bg file temperature profile
          IF (TRIM(bg_file) .ne. "N/A") THEN
            CALL ropp_io_read(bg_data, bg_file, rec=i, ranchk=.FALSE.)
          ! 6.5.2 Alternatively, if present, use input file temperature profile
          ELSE IF (ro_data%lev2b%npoints > 0) THEN
            bg_data%Lev2b = ro_data%Lev2b
          ELSE
            CALL message(msg_fatal, "Valid temperature data required " // &
                 " for 'TP' observation error method. \n " //               &
                 " Use '-b' to specify background data file.")
          ENDIF

          n = bg_data%Lev2b%npoints
          IF (n > 0) THEN
            ! 6.5.3 Compute temperautre gradients
            ALLOCATE(grad_T(n))
            grad_T(1)=(bg_data%Lev2b%temp(2)-bg_data%Lev2b%temp(1))*1000.0 / &
                       (bg_data%Lev2b%geop(2)-bg_data%Lev2b%geop(1))
            DO j=2,n-1
              grad_T(j) =                                                   &
                 (bg_data%Lev2b%temp(j+1)-bg_data%Lev2b%temp(j-1))*1000.0 / &
                       (bg_data%Lev2b%geop(j+1)-bg_data%Lev2b%geop(j-1))
            ENDDO
            grad_T(n)=(bg_data%Lev2b%temp(n)-bg_data%Lev2b%temp(n-1))*1000.0/ &
                       (bg_data%Lev2b%geop(n)-bg_data%Lev2b%geop(n-1))

            ! 6.5.4 Find lowest height with gradient > -2 deg/km (LRT)
            j = 1
            trop_lev = 0
            DO
              IF (grad_T(j) >= -2.0_wp) THEN
                k = j+1
                DO
                  IF(bg_data%Lev2b%geop(k)-bg_data%Lev2b%geop(j) >= 2000.0)THEN
                    trop_lev = j
                    EXIT
                  ELSE IF (grad_T(k) < -2.0) THEN
                    EXIT
                  ENDIF
                  k = k+1
                  IF (k == n) EXIT
                ENDDO
              ENDIF
              IF (trop_lev == j) EXIT
              j = j+1
              IF (j == n) EXIT
            ENDDO
            DEALLOCATE(grad_T)

            trop = 10000.0_wp

            IF (trop_lev > 0) THEN
              trop = bg_data%Lev2b%geop(trop_lev)
              IF (trop < 5000.0 .OR. trop > 20000.) trop=10000.0_wp
            ENDIF

          ELSE
            trop = 10000.0_wp
          ENDIF

          ! 6.5.5 Compute fractional errors
          DO j=1,ro_data%Lev2a%npoints
            ro_data%Lev2a%refrac_sigma(j) = 0.02_wp +   &
               (0.002-0.02)*MIN(ro_data%Lev2a%geop_refrac(j)/trop,1.0_wp)
          ENDDO

        ! 6.6 'MetOffice' model - operational error structures used in MetO
        CASE ('MO')

          ALLOCATE(height(7))
          ALLOCATE(frac(7))

          IF (ABS(ro_data%GEOref%lat) >= 60.) THEN  ! High latitudes
            height = (/ -0.01e5_wp, 0.0_wp, 0.06e5_wp, 0.2e5_wp,   &
                         0.25e5_wp, 0.4e5_wp, 0.5e5_wp /)
            frac = (/ 0.01, 0.01, 0.006, 0.006, 0.01, 0.04, 0.05 /)
          ELSE IF (ABS(ro_data%GEOref%lat) > 30. .AND.   &
             ABS(ro_data%GEOref%lat) < 60. ) THEN    ! Middle latitudes
            height = (/ -0.01e5_wp, 0.0_wp, 0.07e5_wp, 0.25e5_wp,   &
                         0.4e5_wp, 0.5e5_wp, 0.6e5_wp /)
            frac = (/ 0.03, 0.03, 0.007, 0.007, 0.04, 0.05, 0.05 /)
          ELSE IF (ABS(ro_data%GEOref%lat) <= 30.) THEN  ! Low latitudes
            height = (/ -0.01e5_wp, 0.0_wp, 0.04e5_wp, 0.1e5_wp,   &
                         0.25e5_wp, 0.4e5_wp, 0.5e5_wp /)
            frac = (/ 0.05, 0.05, 0.025, 0.005, 0.01, 0.04, 0.05 /)
          ENDIF

          DO j=1,ro_data%Lev2a%npoints

            ilev = MAXVAL(WHERE(ro_data%Lev2a%geop_refrac(j) > height(1:6)))
            ro_data%Lev2a%refrac_sigma(j) = frac(ilev) +                  &
               (frac(ilev+1)-frac(ilev))/(height(ilev+1)-height(ilev)) *  &
               (ro_data%Lev2a%geop_refrac(j) - height(ilev))
          ENDDO

          DEALLOCATE(height)
          DEALLOCATE(frac)

        CASE default
          CALL message(msg_fatal, "Obs error model " //Omodel// "not available.")

     END SELECT

     ! 6.7 Compute sigma values, (limit errors to 0.0003 < sigma < 10)

     DO j=1,ro_data%Lev2a%npoints
       ro_data%Lev2a%refrac_sigma(j) = &
          MIN(ro_data%Lev2a%refrac_sigma(j)*ro_data%Lev2a%refrac(j), 10.0_wp)
       ro_data%Lev2a%refrac_sigma(j) = &
          MAX(ro_data%Lev2a%refrac_sigma(j), 0.02_wp)
     ENDDO

!-------------------------------------------------------------------------------
! 7. Generate observation covariance error matrix file
!-------------------------------------------------------------------------------

     IF (TRIM(cov_file) .ne. "N/A") THEN

       CALL message(msg_info, "Writing observation correlation and sigma values to " // TRIM(cov_file) )

       n = (ro_data%Lev2a%Npoints**2+ro_data%Lev2a%Npoints)/2
       ALLOCATE(Ocorr(ro_data%Lev2a%Npoints,ro_data%Lev2a%Npoints))
       ALLOCATE(Ocorr_out(n,1))
       ALLOCATE(Ocorr_packed(n))
       ALLOCATE(sigma_out(ro_data%Lev2a%Npoints,1))

       DO j=1,ro_data%Lev2a%Npoints
         DO k=1, ro_data%Lev2a%Npoints
           Ocorr(j,k) = EXP(-ABS(RO_data%Lev2a%geop_refrac(j) -    &
              RO_data%Lev2a%geop_refrac(k))/3000.0_wp)
         ENDDO
       ENDDO

       CALL matrix_full2pp(Ocorr, Ocorr_packed)   ! Pack correlation matrix
       Ocorr_out(:,1) = Ocorr_packed(:)
       sigma_out(:,1) = ro_data%Lev2a%refrac_sigma(:)

       CALL ncdf_create(TRIM(cov_file), ncid=file_id)
       CALL ncdf_putatt('Title','Packed observation correlation matrix for ROPP')
       CALL ncdf_putatt('Author','ROM SAF / ROPP v ' // TRIM(version) )
       CALL ncdf_putatt('occ_id', ro_data%occ_id)
       CALL ncdf_putatt('Conventions', 'CF-1.0')

       dim1 = ncdf_defdim('n_bins', 1)
       dim2 = ncdf_defdim('n', n)
       dim3 = ncdf_defdim('n_obs', ro_data%Lev2a%npoints)

       var = ncdf_defvar('lat_min', 'Minimum latitude valid for correllation matrix','Degrees',(/dim1/))
       var = ncdf_defvar('lat_max', 'Maximum latitude valid for correllation matrix','Degrees',(/dim1/))
       var = ncdf_defvar('corr', 'Packed correllation matrix','1',(/dim2,dim1/))
       var = ncdf_defvar('sigma', 'Refractivity standard deviation values','1',(/dim3,dim1/))

       CALL ncdf_datmode(ncid=file_id)
       CALL ncdf_putvar('lat_min', (-90.0))
       CALL ncdf_putvar('lat_max', (90.0))
       CALL ncdf_putvar('corr', Ocorr_out)
       CALL ncdf_putvar('sigma', sigma_out)
       CALL ncdf_close

       DEALLOCATE(Ocorr,Ocorr_packed, Ocorr_out, sigma_out)
     ENDIF

!-------------------------------------------------------------------------------
! 8. Write error profiles to file
!-------------------------------------------------------------------------------

     CALL message(msg_info, "Writing updated ob errors to " // TRIM(out_file) )

     IF (to_input) THEN
       out_file = obs_file
       CALL ropp_io_write(ro_data, out_file, ranchk=.FALSE.)
     ELSE
       CALL ropp_io_write(ro_data, out_file, append=.TRUE., ranchk=.FALSE.)
     ENDIF

     CALL ropp_io_free(ro_data)
     CALL ropp_io_free(bg_data)

   ENDDO

CONTAINS

!-------------------------------------------------------------------------------
! 9. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()

    PRINT *, 'ropp_1dvar_add_refrac_error - Write observation errors to file'
    PRINT *, 'ropp_1dvar_add_refrac_error <obs_file> -Omod <Omodel> [-o <out_file>] [-b <bg_file>] [-c <cov_file>] [-h]'
    PRINT *, 'Valid options are:'
    PRINT *, '  -h               give (this) help.'
    PRINT *, '  -Omod <Omodel>   name of observation error model.'
    PRINT *, '          1%: 1% at 0 km, 0.1% from 12 km, min(std(N))=0.02'
    PRINT *, '          2%: 2% at 0 km, 0.2% from 12 km, min(std(N))=0.02'
    PRINT *, '          3%: 3% at 0 km, 0.3% from 12 km, min(std(N))=0.02'
    PRINT *, '          TP : ROM SAF operational implementation. As 2%'
    PRINT *, '               model with dynamic tropopause height (default)'
    PRINT *, '               calculation from bg data rather than 12 km.'
    PRINT *, '          MO : Met Office operational error model (lat variation)'
    PRINT *, '          SK : according to Steiner-Kirchengast [2002]'
    PRINT *, '  -o <out_file>     name of output file.'
    PRINT *, '  -b <bg_file>      (optional) name of background file'
    PRINT *, '                    (only required for TP model).'
    PRINT *, '  -c <cov_file>     (optional) name of output covariance file.'
    PRINT *, ''

  END SUBROUTINE usage


END PROGRAM ropp_1dvar_add_refrac_error


