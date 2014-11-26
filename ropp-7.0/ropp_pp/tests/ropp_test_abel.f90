! $Id: ropp_test_abel.f90 3696 2013-06-17 08:48:37Z idculv $

PROGRAM ropp_pp_test_abel

!****p* Programs/ropp_pp_tool *
!
! NAME
!    ropp_pp_test_abel - Tool to test Abel transform and inversion algorithms
!
! SYNOPSIS
!    ropp_pp_test_abel <infile(s)> -o <outfile>
!
! DESCRIPTION
!    This program reads RO refractivity data from the input data files and
!    calculates vertical profiles of bending angles using the Abel transform.
!    The inverse algorithm is used to compute a new refractivity profile, which
!    may be compared with the input data. This cycle is repeated to compare
!    bending angle profiles. Depending on input options, the data may be written
!    to an output file or maximum differences computed and output for algorithm
!    testing and validation.
!
! ARGUMENTS
!    <infile(s)>   One (or more) input file names
!                  (optional, default: ../data/ropp_pp_test.nc).
!
!    -o <outfile>  Name of output file (if not specified, output statistics
!                                       to screen or log file).
!    -h            help
!    -v            version information
!
! NOTES
!    If the input file is a multifile, or more than one input files are
!    specified, the output file is a multifile.
!
!    Already existing output files will be overwritten.
!
! EXAMPLE
!    To calculate bending angle and refractivity from one of the example
!    (single-) files in the data directory:
!
!       ropp_pp_test_abel
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
  USE ropp_io_types, ONLY: ROprof, L1btype, L2atype
  USE ropp_pp

  IMPLICIT NONE

  TYPE(ROprof)        :: ro_data             ! RO data structure
  TYPE(ROprof)        :: ro_data2

  TYPE(L1btype)       :: in_bangle           ! Bending angle data structure
  TYPE(L1btype)       :: out_bangle

  TYPE(L2atype)       :: in_refrac           ! Refractivity data structure
  TYPE(L2atype)       :: out_refrac
  TYPE(L2atype)       :: out_refrac2

  REAL(wp), PARAMETER :: limit = 0.01_wp     ! Limit for failure (1.0%)
  REAL(wp)            :: diff

  REAL(wp)            :: scale_ab, scale_in
  INTEGER             :: error
  INTEGER             :: idummy
  INTEGER             :: i, iargc, argc, k
  INTEGER             :: n_l1b, n_l2a
  INTEGER             :: n_files, n_profiles

  LOGICAL             :: output_data

  INTEGER,  DIMENSION(:), POINTER  :: idx => NULL()   ! array indices
  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len = 4096)                            :: ofile = " "
  CHARACTER(len = 4096)                            :: ifile
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =   64)                            :: version
  CHARACTER(len =    4)                            :: istr
  CHARACTER(len =    6)                            :: nstr, nstr1, nstr2

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  output_data = .FALSE.
  ifile     = "../data/ropp_pp_test.nc"
  error     = 0

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, &
       '          ROPP Pre-processor: Test Abel transform routines')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------------------------
! 3. Command line arguments
!-------------------------------------------------------------------------------

  argc = iargc()
  i = 1
  n_files = 0
  ALLOCATE(ifiles(argc))

  DO WHILE(i <= argc)
     CALL getarg(i, buffer)
     SELECT CASE (buffer)
        CASE('-o')                          ! Output file name (netCDF output)
           CALL getarg(i+1, buffer)
           ofile = buffer
           i = i + 1
           output_data = .TRUE.
        CASE('-h', '--help', '?')           ! Give some help
          CALL usage()
           CALL EXIT(0)
        CASE('-v', '-V', '--version')       ! output version info
           CALL version_info()
           CALL EXIT(0)
        CASE default                        ! Input file name
           IF ( buffer(1:1) /= "-" ) THEN
             n_files = n_files + 1
             ifiles(n_files) = buffer
           END IF
     END SELECT
     i = i + 1
  END DO

  IF (n_files == 0) THEN                    ! Set default filename
     n_files = 1
     IF(ALLOCATED(ifiles)) DEALLOCATE(ifiles)
     ALLOCATE(ifiles(n_files))
     ifiles(n_files) = ifile
  ENDIF


!-------------------------------------------------------------------------------
! 4. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(ofile, idummy)
  CALL file_delete("out2.nc", idummy)

!-------------------------------------------------------------------------------
! 5. Loop over all input files
!-------------------------------------------------------------------------------

  DO k = 1, n_files

!-------------------------------------------------------------------------------
! 6. Loop over all profiles
!-------------------------------------------------------------------------------

     n_profiles = ropp_io_nrec(ifiles(k))

     DO i = 1, n_profiles

        WRITE(istr, '(i4)') i
        WRITE(nstr, '(i6)') n_profiles
        CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

!-------------------------------------------------------------------------------
! 7. Read data
!-------------------------------------------------------------------------------

        CALL ropp_io_read(ro_data, ifiles(k), rec = i, ranchk = .TRUE.)
        CALL message(msg_info, "(" // TRIM(ro_data%occ_id) // ") \n")

        IF (ro_data%lev1b%npoints == 0) THEN
          CALL message(msg_fatal, "No Level1b data in file " //    &
             TRIM(ifiles(k)) // ". No data to invert. \n")
        ENDIF

!-------------------------------------------------------------------------------
! 8. Copy data in RO structure to obs vectors
!-------------------------------------------------------------------------------

! 8.1 Check that profiles are increasing in height - 1st element towards surface

        CALL ropp_io_ascend(ro_data)
        ro_data2 = ro_data

! 8.2 Copy valid data by assignment

        in_refrac   = ro_data%lev2a
        out_refrac  = ro_data%lev2a
        out_refrac2 = ro_data%lev2a

        !! Output to higher res profile
!        call set_obs_levels_refrac(ro_data,out_refrac)
!        call set_obs_levels_bangle(ro_data,out_refrac, out_bangle)
!        res_bangle = ro_data%lev1b

!-------------------------------------------------------------------------------
! 9. Define bending angle vectors consistent with input refractivity
!-------------------------------------------------------------------------------

        CALL set_obs_levels_bangle(ro_data, in_refrac, in_bangle)
        CALL set_obs_levels_bangle(ro_data, in_refrac, out_bangle)

        n_l1b = in_bangle%npoints
        n_l2a = in_refrac%npoints

!-------------------------------------------------------------------------------
! 10. Perform Abel transform and inverse
!-------------------------------------------------------------------------------

        ! 10.1 Ensure monotonous increasing impact parameters
        CALL ropp_pp_monotonous(in_bangle%impact, -1)

        ! 10.2 Retrieve bending angle profile
        scale_ab = -in_refrac%refrac(1) /                           &
             ((in_refrac%refrac(5)-in_refrac%refrac(1))/            &
             (in_refrac%alt_refrac(5)-in_refrac%alt_refrac(1)))
        CALL ropp_pp_abel_LIN(in_bangle%impact, in_refrac%refrac,   &
                           in_bangle%impact, in_bangle%bangle, scale=scale_ab)
!        call ropp_pp_abel_EXP(in_bangle%impact, in_refrac%refrac,  &
!                              in_bangle%impact, in_bangle%bangle)

        ! 10.3 Retrieve refractivity profile
        scale_in = -(in_bangle%impact(n_l1b-1) - in_bangle%impact(3*n_l1b/4)) &
             /(LOG(in_bangle%bangle(n_l1b-1))-LOG(in_bangle%bangle(3*n_l1b/4)))
        idx => WHERE(in_bangle%bangle > ropp_MDTV)
        CALL ropp_pp_invert_LIN(in_bangle%impact(idx), in_bangle%bangle(idx),  &
                           in_bangle%impact, out_refrac%refrac, scale=scale_in)
!        call ropp_pp_invert_EXP(in_bangle%impact(idx), in_bangle%bangle(idx), &
!                                 in_bangle%impact, out_refrac%refrac)

        idx => WHERE(out_refrac%refrac > ropp_MDTV)
        CALL ropp_pp_abel_LIN(in_bangle%impact(idx), out_refrac%refrac(idx),  &
                            in_bangle%impact, out_bangle%bangle, scale=scale_ab)
!        call ropp_pp_abel_EXP(in_bangle%impact(idx), out_refrac%refrac(idx), &
!                              in_bangle%impact, out_bangle%bangle)

        ! 10.5 Compute output heights for refractivity
        out_refrac%alt_refrac = (in_bangle%impact /                         &
                                (1.0_wp + 1.e-6_wp*out_refrac%refrac)) -    &
                               ( ro_data%georef%roc + ro_data%georef%undulation)
        out_refrac%geop_refrac =                                            &
               geometric2geopotential(ro_data%georef%lat, out_refrac%alt_refrac)

!-------------------------------------------------------------------------------
! 11. Copy output profiles to RO structure
!-------------------------------------------------------------------------------

        CALL ropp_io_roprof2roprof(in_bangle, ro_data%lev1b)
        CALL ropp_io_roprof2roprof(out_refrac, ro_data%lev2a)

        out_refrac2%alt_refrac = out_refrac%alt_refrac
        out_refrac2%geop_refrac = out_refrac%geop_refrac
        CALL ropp_io_roprof2roprof(out_bangle, ro_data2%lev1b)
        CALL ropp_io_roprof2roprof(out_refrac2, ro_data2%lev2a)

!-------------------------------------------------------------------------------
! 12. Write data
!-------------------------------------------------------------------------------

        IF (output_data) THEN

           CALL ropp_io_write(ro_data, ofile, append = .TRUE., ranchk = .TRUE. )
           CALL ropp_io_write(ro_data2, "out2.nc", append = .TRUE.,   &
                              ranchk = .TRUE. )

!-------------------------------------------------------------------------------
! 13. Output differences statistics
!-------------------------------------------------------------------------------

        ELSE

           WRITE(nstr1, '(f6.2)')  out_refrac%alt_refrac(n_l2a/10)/1000.
           WRITE(nstr2, '(f6.2)')  out_refrac%alt_refrac(n_l2a/2)/1000.
           CALL message(msg_info,   &
                        "Comparing input and computed refractivity profiles " &
                         // "between " // nstr1 // " and " // nstr2 // " km.")

           diff = sngl(MAXVAL( ABS( (out_refrac%refrac(n_l2a/10:n_l2a/2) -   &
                                 in_refrac%refrac(n_l2a/10:n_l2a/2)) /       &
                                 in_refrac%refrac(n_l2a/10:n_l2a/2) )))
           WRITE(nstr, '(f6.3)')  diff*100.0
           IF ( diff > limit ) THEN
              CALL message(msg_info,  &
                           "Refractivity failed, deviation found [%]: " // nstr)
              error = 1
           ELSE
              CALL message(msg_info,  &
                           "Refractivity passed, deviation found [%]: " // nstr)
           ENDIF

           CALL message(msg_info,     &
                       "Comparing input and computed bending angle profiles " &
                       // "between " // nstr1 // " and " // nstr2 // " km.")
           diff = sngl(MAXVAL( ABS( (out_bangle%bangle(n_l1b/10:n_l1b/2) -    &
                                      in_bangle%bangle(n_l1b/10:n_l1b/2)) /   &
                                      in_bangle%bangle(n_l1b/10:n_l1b/2) )))
           WRITE(nstr, '(f6.3)')  diff*100.0
           IF ( diff > limit ) THEN
              CALL message(msg_info,  &
                          "Bending angle failed, deviation found [%]: " // nstr)
              error = 1
           ELSE
              CALL message(msg_info,  &
                          "Bending angle passed, deviation found [%]: " // nstr)
           ENDIF

           CALL message(msg_info,  &
                    "To output data file, re-run with '-o <outputfile>' option")

        ENDIF

!-------------------------------------------------------------------------------
! 14. Clean up
!-------------------------------------------------------------------------------

     CALL ropp_io_free(ro_data)
     CALL ropp_io_free(ro_data2)
     CALL ropp_io_free(in_bangle)
     CALL ropp_io_free(out_bangle)
     CALL ropp_io_free(in_refrac)
     CALL ropp_io_free(out_refrac)
     CALL ropp_io_free(out_refrac2)

  END DO
END DO

IF (error == 1) THEN
     PRINT *,''
     PRINT *,''
     PRINT *,'*********************************'
     PRINT *,'*** ropp_pp (test_abel): FAIL ***'
     PRINT *,'*********************************'
     PRINT *,''
  ELSE
     PRINT *,''
     PRINT *,''
     PRINT *,'*********************************'
     PRINT *,'*** ropp_pp (test_abel): PASS ***'
     PRINT *,'*********************************'
     PRINT *,''
  ENDIF


CONTAINS

!-------------------------------------------------------------------------------
! 15. Calculate observation levels for refractivity
!-------------------------------------------------------------------------------

  SUBROUTINE set_obs_levels_refrac(ro_data, obs_refrac)

!   x.1 Declarations
!   ----------------

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_io_types
    USE geodesy

    IMPLICIT NONE

    TYPE(ROprof)      :: ro_data
    TYPE(L2atype) :: obs_refrac

    INTEGER           :: i, n

!   x.2 Vertical geopotential height levels between 200 and 60000 gpm
!   -----------------------------------------------------------------

    n = INT(80.0_wp / 0.2_wp)

    CALL ropp_io_init(obs_refrac, n)

    obs_refrac%refrac(:)  = 0.0_wp
    obs_refrac%geop_refrac(:) = (/ (i*200.0_wp, i = 1,n) /)
    obs_refrac%alt_refrac(:)  =    &
             geopotential2geometric(ro_data%georef%lat, obs_refrac%geop_refrac)

  END SUBROUTINE set_obs_levels_refrac


!-------------------------------------------------------------------------------
! 16. Calculate observation levels for bending angle
!-------------------------------------------------------------------------------

  SUBROUTINE set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)

!   x.1 Declarations
!   ----------------

    USE typesizes, ONLY: wp => EightByteReal
    USE geodesy
    USE ropp_io

    IMPLICIT NONE

    TYPE(ROprof)      :: ro_data
    TYPE(L2atype) :: obs_refrac
    TYPE(L1btype) :: obs_bangle

    REAL(wp), DIMENSION(:), ALLOCATABLE :: tmp

    INTEGER           :: n

!   x.2 Allocate arrays
!   -------------------

    n = SIZE(obs_refrac%geop_refrac)

    ALLOCATE(tmp(n))

    CALL ropp_io_init(obs_bangle, n)

!   x.4 Calculate levels to coincide with the geopotential levels
!   -------------------------------------------------------------

    tmp = geopotential2geometric(ro_data%GEOref%lat, obs_refrac%geop_refrac) &
            + ro_data%GEOref%roc

    IF (ro_data%GEOref%undulation > ropp_MDTV) THEN
      tmp = tmp + ro_data%GEOref%undulation
    ELSE
      CALL message(msg_warn, "Undulation missing from ROPP structure. " // &
                   "Will assume to be zero when calculating impact parameters.")
    ENDIF

    obs_bangle%impact(:)  = (1.0_wp + obs_refrac%refrac*1.e-6_wp) * tmp

!   x.5 Fill other arrays
!   ---------------------

    obs_bangle%bangle(:)  = 0.0_wp

!   x.6 Clean up
!   ------------

    DEALLOCATE(tmp)

  END SUBROUTINE set_obs_levels_bangle

!-------------------------------------------------------------------------------
! 17. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Test Abel transform and inversion algorithms.'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_test_abel [<options>] <input_file(s)> -o <output_file>'
    PRINT *, 'Options:'
    PRINT *, '  -o <output_file>  name of netCDF/ropp output file'
    PRINT *, '  -h                this help'
    PRINT *, '  -v                version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 18. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_pp_version()
    PRINT *, 'ropp_test_abel - Tool to test Abel transform and inversion algorithms.'
    PRINT *, '                 Map from refractivity to bending angle to refractivity'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (PP) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_pp_test_abel
