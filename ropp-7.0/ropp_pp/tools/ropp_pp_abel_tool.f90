! $Id: ropp_pp_abel_tool.f90 3696 2013-06-17 08:48:37Z idculv $

PROGRAM ropp_pp_abel_tool

!****p* Programs/ropp_pp_abel_tool *
!
! NAME
!   ropp_pp_abel_tool
!
! SYNOPSIS
!   Tool to test Abel transform and inverse Abel transform routines.
!
!   > ropp_pp_abel_tool [<options>] <infile(s)>
!
! ARGUMENTS
!  <infile(s)>   One (or more) input file names.
!
! OPTIONS
!   -o <output_file>  name of ROPP netCDF output file
!   -h                help
!   -v                version information
!
! DESCRIPTION
!   This program reads RO refractivity data from the input data files and 
!   calculates vertical profiles of bending angles using the Abel transform. 
!   The inverse Abel transform is then used to compute a new refractivity 
!   profile, which may be compared with the input data.
!
! NOTES
!   If the input file is a multifile, or more than one input files are
!   specified, the output file is a multifile.
!
!   Already existing output files will be overwritten.
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

  TYPE(ROprof)                                     :: ro_data
  TYPE(ROprof)                                     :: ro_data2

  TYPE(L1btype)                                    :: obs_bangle
  TYPE(L1btype)                                    :: res_bangle
  TYPE(L1btype)                                    :: gor_bangle

  TYPE(L2atype)                                    :: obs_refrac
  TYPE(L2atype)                                    :: res_refrac
  TYPE(L2atype)                                    :: gor_refrac

  REAL(wp), DIMENSION(:),ALLOCATABLE               :: h_out

  INTEGER                                          :: idummy
  INTEGER                                          :: i, iargc, argc, k
  INTEGER                                          :: n_files, n_profiles

  LOGICAL                                          :: give_help

  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len = 4096)                            :: ofile
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =    4)                            :: istr
  CHARACTER(len =    6)                            :: nstr

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  give_help = .FALSE.
  ofile     = "ro2ro.nc"

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, &
       '            ROPP Pre-processor Abel Transform Tool'                    )
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
      CASE('-h', '--help', '?')           ! Give some help
         give_help = .TRUE.
      CASE('-v', '-V', '--version')       ! Output version info
         CALL version_info()
         CALL EXIT(0)
      CASE default                        ! Input file name
         IF ( buffer(1:1) /= '-' ) THEN
           n_files = n_files + 1
           ifiles(n_files) = buffer
         END IF
    END SELECT
    i = i + 1
  END DO

  IF (argc == 0 .OR. n_files == 0 .OR. give_help) THEN
    CALL usage()
    CALL EXIT(0)
  ENDIF

!-------------------------------------------------------------------------------
! 4. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(ofile, idummy)
  CALL file_delete("test2.nc", idummy)

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

        CALL ropp_io_read(ro_data,  ifiles(k), rec=i, ranchk=.TRUE.)
        CALL ropp_io_read(ro_data2, ifiles(k), rec=i, ranchk=.TRUE.)
        CALL message(msg_info, "(" // TRIM(ro_data%occ_id) // ") \n")

        IF (ro_data%lev2a%npoints == 0) THEN
          CALL message(msg_fatal, "No Level2a data in file " //    &
             TRIM(ifiles(k)) // ". Cannot continue. \n")
        ENDIF

!-------------------------------------------------------------------------------
! 8. Copy data in RO structure to obs vectors
!-------------------------------------------------------------------------------

! 8.1 Check that profiles are increasing in height - 1st element towards surface

        CALL ropp_io_ascend(ro_data)
        CALL ropp_io_ascend(ro_data2)

! 8.2 Copy data by assignment

        obs_refrac = ro_data%lev2a
!        res_refrac = ro_data%lev2a
!        gor_refrac = ro_data%lev2a

!-------------------------------------------------------------------------------
! 9. Define bending angle vectors consistent with input refractivity
!-------------------------------------------------------------------------------

        CALL set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)
!        call set_obs_levels_bangle(ro_data, res_refrac, res_bangle)
!        call set_obs_levels_bangle(ro_data, gor_refrac, gor_bangle)
        CALL ropp_pp_abel_LIN(obs_bangle%impact, obs_refrac%refrac, &
                              obs_bangle%impact, obs_bangle%bangle)

        !!!!! TEST 200 levels output....
        CALL set_obs_levels_refrac(ro_data, res_refrac)
        CALL set_obs_levels_refrac(ro_data, gor_refrac)
        CALL set_obs_levels_bangle(ro_data, res_refrac, res_bangle)
        CALL set_obs_levels_bangle(ro_data, gor_refrac, gor_bangle)

!-------------------------------------------------------------------------------
! 10. Perform Abel transform and inverse
!-------------------------------------------------------------------------------

        !! ROPP code
        CALL ropp_pp_invert_EXP(obs_bangle%impact, obs_bangle%bangle, &
                                res_bangle%impact, res_refrac%refrac)

        CALL ropp_pp_abel_EXP(res_bangle%impact, res_refrac%refrac, &
                              res_bangle%impact, res_bangle%bangle)

        !! compute new geopotential height levels
        ALLOCATE(h_out(res_refrac%Npoints))
        h_out = (res_bangle%impact/(1.0_wp + 1.e-6_wp*res_refrac%refrac)) -   &
                    (ro_data%georef%roc + ro_data%georef%undulation)
        res_refrac%geop_refrac =  &
           geometric2geopotential(ro_data%georef%lat, h_out)

        !! Gorbunov code

        CALL ropp_pp_invert_LIN(obs_bangle%impact, obs_bangle%bangle,    &
                                gor_bangle%impact, gor_refrac%refrac)

! NB: Adding the optional scale=1.0e4_wp (or thereabouts) argument can give 
!     closer agreement between ropp_pp_abel_EXP, which accounts for the bending 
!     out to infinity by default, and ropp_pp_abel_LIN, which by default doesn't.

        CALL ropp_pp_abel_LIN(gor_bangle%impact, gor_refrac%refrac,      &
                              gor_bangle%impact, gor_bangle%bangle)

        !! compute new geopotential height levels
        h_out = (gor_bangle%impact/(1.0_wp + 1.e-6_wp*gor_refrac%refrac)) -   &
                    (ro_data%georef%roc + ro_data%georef%undulation)
        gor_refrac%geop_refrac =   &
           geometric2geopotential(ro_data%georef%lat, h_out)

!-------------------------------------------------------------------------------
! 12. Copy simulated observations to RO structure
!-------------------------------------------------------------------------------

        CALL ropp_io_roprof2roprof(res_bangle, ro_data%lev1b)
        CALL ropp_io_roprof2roprof(res_refrac, ro_data%lev2a)

        CALL ropp_io_roprof2roprof(gor_bangle, ro_data2%lev1b)
        CALL ropp_io_roprof2roprof(gor_refrac, ro_data2%lev2a)

!-------------------------------------------------------------------------------
! 14. Write data
!-------------------------------------------------------------------------------

    CALL ropp_io_write(ro_data,   ofile,     append=.TRUE., ranchk=.TRUE. )
    CALL ropp_io_write(ro_data2, "test2.nc", append=.TRUE., ranchk=.TRUE. )

!-------------------------------------------------------------------------------
! 15. Clean up
!-------------------------------------------------------------------------------

    CALL ropp_io_free(ro_data)
    CALL ropp_io_free(ro_data2)
    DEALLOCATE(h_out)

  END DO
END DO

CONTAINS

!-------------------------------------------------------------------------------
! 16. Calculate observation levels for refractivity
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

    n = INT(60.0_wp / 0.2_wp)

    CALL ropp_io_init(obs_refrac, n)

    obs_refrac%refrac(:)  = 0.0_wp
    obs_refrac%geop_refrac(:)    = (/ (i*200.0_wp, i = 1,n) /)
    obs_refrac%alt_refrac(:) =        &
        geopotential2geometric(ro_data%georef%lat, obs_refrac%geop_refrac)

  END SUBROUTINE set_obs_levels_refrac


!-------------------------------------------------------------------------------
! 17. Calculate bending angle observation levels (consistent with obs_refrac)
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
            + (ro_data%GEOref%roc + ro_data%GEOref%undulation)

    obs_bangle%impact(:)  = (1.0_wp + obs_refrac%refrac*1.e-6_wp) * tmp

!   x.5 Fill other arrays
!   ---------------------

    obs_bangle%bangle(:)  = 0.0_wp

!   x.6 Clean up
!   ------------

    DEALLOCATE(tmp)

  END SUBROUTINE set_obs_levels_bangle

!-------------------------------------------------------------------------------
! 18. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Bending angles and refractivity forward model'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_pp_abel_tool [<options>] <input_file(s)>'
    PRINT *, 'Options:'
    PRINT *, '  -o <output_file>  name of ROPP netCDF output file'
    PRINT *, '  -h                this help'
    PRINT *, '  -v                version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 19. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_pp_version()
    PRINT *, 'ropp_pp_abel_tool - Bending angles and refractivity forward model.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (PP) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_pp_abel_tool
