PROGRAM t_fascod

!****p* Programs/t_fascod *
!
! NAME
!    t_fascod - tests the compiled fm model with a precalculated set of FASCOD
!    profiles
!
! SYNOPSIS
!    t_fascod
! 
! DESCRIPTION
!    This program reads the FASCOD input file available in ../data. This file
!    has precalculated refractivity and bending angles. This program then
!    recalculates these profiles and compares it to the precalculated data. It
!    is a modified version of ropp_fm_bg2ro_1d.f90.
!
! NOTES
!
! EXAMPLE
!
! SEE ALSO
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
  USE ropp_fm_types
  USE ropp_fm_copy

  IMPLICIT NONE

  TYPE(ROprof)                                     :: ro_data
  TYPE(ROprof)                                     :: ro_dataorg
  TYPE(State1dFM)                                  :: state
  TYPE(Obs1dRefrac)                                :: obs_refrac
  TYPE(Obs1dBangle)                                :: obs_bangle

  INTEGER                                          :: i, iargc, argc, k
  INTEGER                                          :: n_files, n_profiles
  INTEGER                                          :: error

  LOGICAL                                          :: give_version
  LOGICAL                                          :: give_help
  LOGICAL                                          :: compress

  REAL(wp)                                         :: limit, diff

  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =   64)                            :: version

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  version      = ropp_io_version()
  give_version = .FALSE.
  give_help    = .FALSE.
  compress     = .FALSE.
  error        = 0
! limit for failure, corresponds to 0.01%
  limit        = 0.0001_wp

  ALLOCATE(ifiles(1))
  n_files = 1
  ifiles(n_files) = '../data/FASCOD_Scenarios.nc'

!-------------------------------------------------------------------------------
! 3. Command line arguments
!-------------------------------------------------------------------------------

  argc = iargc()
  i = 1

  DO WHILE(i <= argc)
     CALL getarg(i, buffer)
     SELECT CASE (buffer)
        CASE('-h', '-help', '--help')       ! Give some help
           give_help = .TRUE.
        CASE('-V', '-version', '--version') ! Give some version information
           give_version = .TRUE.
        CASE('-comp')                       ! Non-ideal gas corrections
           compress = .TRUE.
           ifiles(n_files) = '../data/FASCOD_non_ideal.nc' ! pick up the non-ideal gas test data 
        CASE default                        ! Input file name
     END SELECT
     i = i + 1
  END DO

  IF (give_help) THEN
     CALL usage()
  ENDIF

  IF (give_version) THEN
     CALL version_info(version)
  ENDIF

!-------------------------------------------------------------------------------
! 4. Set default units
!-------------------------------------------------------------------------------

  CALL ropp_fm_set_units(ro_data)

!-------------------------------------------------------------------------------
! 5. Loop over all input files
!-------------------------------------------------------------------------------

  DO k = 1, n_files

!-------------------------------------------------------------------------------
! 6. Loop over all profiles
!-------------------------------------------------------------------------------

     n_profiles = ropp_io_nrec(ifiles(k))
     
     IF (compress) state%non_ideal = .TRUE.

     DO i = 1, n_profiles

!-------------------------------------------------------------------------------
! 7. Read data, store the original values in ro_dataorg
!-------------------------------------------------------------------------------
        
       CALL ropp_io_read(ro_data, ifiles(k), rec = i)
       CALL ropp_io_read(ro_dataorg, ifiles(k), rec = i)

!-------------------------------------------------------------------------------
! 8. Copy data in RO structure to state and refrac obs vectors
!-------------------------------------------------------------------------------

        CALL ropp_fm_roprof2state(ro_data, state)
        CALL set_obs_levels_refrac(obs_refrac)
        
!-------------------------------------------------------------------------------
! 9. Calculate refractivity
!-------------------------------------------------------------------------------

        CALL ropp_fm_refrac_1d(state, obs_refrac)
        
!-------------------------------------------------------------------------------
! 10. Copy data in RO and refrac structure to bending angle obs vector
!-------------------------------------------------------------------------------

        CALL set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)

!-------------------------------------------------------------------------------
! 11. Calculate bending angle
!-------------------------------------------------------------------------------
        CALL ropp_fm_bangle_1d(state, obs_bangle)

!-------------------------------------------------------------------------------
! 12. Copy simulated observations to RO structure
!-------------------------------------------------------------------------------

        CALL ropp_fm_obs2roprof(obs_refrac, ro_data)
        CALL ropp_fm_obs2roprof(obs_bangle, ro_data)

!-------------------------------------------------------------------------------
! 13. Check bending angle and refractivity data
!-------------------------------------------------------------------------------

        PRINT *, "Checking FASCOD Scenario: ", TRIM(ro_data%bg%source)
        diff = sngl(MAXVAL( ABS( (ro_dataorg%Lev2a%refrac(:) -               &
                       ro_data%Lev2a%refrac(:)) / ro_dataorg%Lev2a%refrac(:) )))
        IF ( diff > limit ) THEN 
                PRINT *, "   Refractivity failed, deviation found [%]: ",    &
                         diff*100.0
                error = 1
        ELSE 
                PRINT *, "   Refractivity passed"
        ENDIF

        diff = sngl(MAXVAL( ABS( (ro_dataorg%Lev1b%bangle(:) -               &
                       ro_data%Lev1b%bangle(:)) / ro_dataorg%Lev1b%bangle(:) )))
        IF ( diff > limit ) THEN
                PRINT *, "   Bending angle failed, deviation found [%]: ",   &
                         diff*100.0
                error = 1
        ELSE 
                PRINT *, "   Bending angle passed"
        ENDIF
                         
!-------------------------------------------------------------------------------
! 14. Clean up
!-------------------------------------------------------------------------------

        CALL ropp_io_write(ro_data, "test.nc", append=.TRUE.)

        CALL ropp_io_free(ro_data)
        CALL ropp_io_free(ro_dataorg)

     END DO
  END DO

  IF (error == 1) THEN
     PRINT *,''
     PRINT *,''
     PRINT *,'*********************************'
     PRINT *,'*** ropp_fm (t_fascod): FAIL ***'
     PRINT *,'********************************'
     PRINT *,''
  ELSE
     PRINT *,''
     PRINT *,''
     PRINT *,'********************************'
     PRINT *,'*** ropp_fm (t_fascod): PASS ***'
     PRINT *,'********************************'
     PRINT *,''
  ENDIF

CONTAINS

!-------------------------------------------------------------------------------
! 15. Calculate observation levels for refractivity
!-------------------------------------------------------------------------------

  SUBROUTINE set_obs_levels_refrac(obs_refrac)

!   x.1 Declarations
!   ----------------

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_io
    USE ropp_fm

    IMPLICIT NONE

    TYPE(Obs1dRefrac) :: obs_refrac

    INTEGER           :: i, n

!   x.2 Vertical geopotential height levels between 0.2 and 60 gpkm
!   ---------------------------------------------------------------

    n = INT(60.0_wp / 0.2_wp)

    ALLOCATE(obs_refrac%refrac(n))
    ALLOCATE(obs_refrac%geop(n))
    ALLOCATE(obs_refrac%weights(n))

    obs_refrac%refrac(:)  = 0.0_wp
    obs_refrac%geop(:)    = (/ (i*200.0_wp, i = 1,n) /)
    obs_refrac%weights(:) = 1.0_wp

  END SUBROUTINE set_obs_levels_refrac


!-------------------------------------------------------------------------------
! 16. Calculate observation levels for bending angle
!-------------------------------------------------------------------------------

  SUBROUTINE set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)

!   x.1 Declarations
!   ----------------

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_io
    USE ropp_fm
    USE geodesy

    IMPLICIT NONE

    TYPE(ROprof)      :: ro_data
    TYPE(Obs1dRefrac) :: obs_refrac
    TYPE(Obs1dbangle) :: obs_bangle

    REAL(wp), DIMENSION(:), ALLOCATABLE :: tmp

    INTEGER           :: n

!   x.2 Allocate arrays
!   -------------------

    n = SIZE(obs_refrac%geop)

    ALLOCATE(obs_bangle%bangle(n))
    ALLOCATE(obs_bangle%impact(n))
    ALLOCATE(obs_bangle%weights(n))

    ALLOCATE(tmp(n))

!   x.3 Set scalar arguments of the observation vector
!   --------------------------------------------------

    obs_bangle%g_sfc        = gravity(ro_data%GEOref%lat)
    obs_bangle%r_earth      = R_eff(ro_data%GEOref%lat)
    obs_bangle%r_curve      = ro_data%GEOref%roc 
    obs_bangle%undulation   = ro_data%GEOref%undulation

!   x.4 Calculate levels to coincide with the geopotential levels
!   -------------------------------------------------------------

    tmp = geopotential2geometric(ro_data%GEOref%lat, obs_refrac%geop) &
            + obs_bangle%r_curve + obs_bangle%undulation
    obs_bangle%impact(:)  = (1.0_wp + obs_refrac%refrac*1.e-6_wp) * tmp

!   x.5 Fill other arrays
!   ---------------------

    obs_bangle%bangle(:)  = 0.0_wp
    obs_bangle%weights(:) = 1.0_wp

!   x.6 Clean up
!   ------------

    DEALLOCATE(tmp)

  END SUBROUTINE set_obs_levels_bangle

!-------------------------------------------------------------------------------
! 17. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()

    PRINT *, 't_fascod - Test local compilation of ropp_fm.'
    PRINT *, 't_fascod'
    PRINT *, 'Valid options are:'
    PRINT *, '  -h                give (this) help.'
    PRINT *, '  -V                give some version information.'
    PRINT *, '  -comp             use non-ideal gas corrections.'
    PRINT *, ''

  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 18. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info(version)
    CHARACTER(len = *) :: version
    PRINT *, 't_fascod - Test local compilation of ropp_fm.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP version ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM t_fascod
