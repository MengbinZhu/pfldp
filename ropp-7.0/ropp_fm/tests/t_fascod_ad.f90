PROGRAM t_fascod_ad

!****p* Programs/t_fascod_ad *
!
! NAME
!    t_fascod_ad - tests the compiled fm model and adjoint functions
!
! SYNOPSIS
!    t_fascod_ad
! 
! DESCRIPTION
!    This program reads the FASCOD input file available in ../data. This file
!    has precalculated refractivity and bending angles. This program then
!    recalculates these profiles with random perturbations applied. 
!    Correctness of the adjoint routines is checked by testing the definition
!    of the adjoint operator: test inner products of <Ax,y> = <x, A*y> equal
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
  TYPE(State1dFM)                                  :: x
  TYPE(State1dFM)                                  :: x_tl
  TYPE(State1dFM)                                  :: x_ad
  TYPE(Obs1dRefrac)                                :: obs_refrac
  TYPE(Obs1dBangle)                                :: obs_bangle

  INTEGER                                          :: i, iargc, argc, k
  INTEGER                                          :: n_files, n_profiles
  INTEGER                                          :: error

  LOGICAL                                          :: give_version
  LOGICAL                                          :: give_help
  LOGICAL                                          :: compress
  

  REAL(wp)                                         :: norm1, norm2
  REAL(wp), DIMENSION(:), ALLOCATABLE              :: y_tl
  REAL(wp), DIMENSION(:), ALLOCATABLE              :: y_ad

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

     DO i = 1, n_profiles

!-------------------------------------------------------------------------------
! 7. Read data, store the original values in ro_dataorg
!-------------------------------------------------------------------------------

        CALL ropp_io_read(ro_data, ifiles(k), rec = i)
        
!-------------------------------------------------------------------------------
! 8. Copy data in RO structure to state and refrac obs vectors
!-------------------------------------------------------------------------------

        CALL ropp_fm_roprof2state(ro_data, x)
	
        IF (compress) x%non_ideal = .TRUE.

        CALL set_obs_levels_refrac(obs_refrac)
        
!-------------------------------------------------------------------------------
! 9. Calculate refractivity
!-------------------------------------------------------------------------------

        PRINT *, "Checking REFRACTIVITY AD ", TRIM(ro_data%bg%source)

        CALL ropp_fm_refrac_1d(x, obs_refrac)

        ALLOCATE(x_tl%state(SIZE(x%state)))
        ALLOCATE(x_tl%pres(x%n_lev))
        ALLOCATE(x_tl%temp(x%n_lev))
        ALLOCATE(x_tl%shum(x%n_lev))
        ALLOCATE(x_tl%geop(x%n_lev))
        
        ALLOCATE(y_tl(SIZE(obs_refrac%refrac)))
        ALLOCATE(y_ad(SIZE(obs_refrac%refrac)))
        
        !
        ! Use standard random number routine to generate random fields
        !
        
        CALL RANDOM_NUMBER(x_tl%temp(:))
        CALL RANDOM_NUMBER(x_tl%pres(:))
        CALL RANDOM_NUMBER(x_tl%geop(:))
        CALL RANDOM_NUMBER(x_tl%shum(:))

        !  increase the geopotential perturbations

        x_tl%geop(:) = 100.0_wp*x_tl%geop(:)

        ! call tangent linear
	
	write (6,*) 'ad',x%non_ideal
	

        CALL ropp_fm_refrac_1d_tl(x, x_tl, obs_refrac, y_tl)
        
        !
        ! norm1 is calculated from the output of the TL routine
        ! ie, norm1  = (H.dx)^T * (H.dx)
        !
        norm1 = DOT_PRODUCT(y_tl, y_tl)
        
        ! initialise adjoint variables

        ALLOCATE(x_ad%state(SIZE(x%state)))
        ALLOCATE(x_ad%pres(x%n_lev))
        ALLOCATE(x_ad%temp(x%n_lev))
        ALLOCATE(x_ad%shum(x%n_lev))
        ALLOCATE(x_ad%geop(x%n_lev))

        x_ad%temp(:) = 0.0_wp
        x_ad%pres(:) = 0.0_wp
        x_ad%geop(:) = 0.0_wp
        x_ad%shum(:) = 0.0_wp

        ! Set y_ad = (H.dx)

        y_ad = y_tl

        ! call adjoint routine

        CALL ropp_fm_refrac_1d_ad(x, x_ad, obs_refrac, y_ad)
        
        !
        ! norm2 is calculated from the output of the AD routine
        ! and the initial perturbation.
        !

        norm2 = DOT_PRODUCT(RESHAPE(x_ad%temp,(/SIZE(x_ad%temp)/)),   &
             RESHAPE(x_tl%temp,(/SIZE(x_tl%temp)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%pres,(/SIZE(x_ad%pres)/)),      &
             RESHAPE(x_tl%pres,(/SIZE(x_tl%pres)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%geop,(/SIZE(x_ad%geop)/)),      &
             RESHAPE(x_tl%geop,(/SIZE(x_tl%geop)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%shum,(/SIZE(x_ad%shum)/)),      &
             RESHAPE(x_tl%shum,(/SIZE(x_tl%shum)/)))
        
        WRITE(6,'(a,2f15.3,f10.7)') 'Norms ', norm1, norm2, norm1/norm2

        DEALLOCATE(y_tl)
        DEALLOCATE(y_ad)
 
        IF(norm1/norm2 < 0.999_wp .OR. norm1/norm2 > 1.001_wp) error=1
        
!-------------------------------------------------------------------------------
! 10. Copy data in RO and refrac structure to bending angle obs vector
!-------------------------------------------------------------------------------

        CALL set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)

!-------------------------------------------------------------------------------
! 11. Calculate bending angle
!-------------------------------------------------------------------------------
        
        PRINT *, "Checking BENDING ANGLE AD ", TRIM(ro_data%bg%source)

        CALL ropp_fm_bangle_1d(x, obs_bangle)

        ALLOCATE(y_tl(SIZE(obs_bangle%bangle)))
        ALLOCATE(y_ad(SIZE(obs_bangle%bangle)))
        
        !
        ! Use standard random number routine to generate random fields
        !
        
        CALL RANDOM_NUMBER(x_tl%temp(:))
        CALL RANDOM_NUMBER(x_tl%pres(:))
        CALL RANDOM_NUMBER(x_tl%geop(:))
        CALL RANDOM_NUMBER(x_tl%shum(:))

        !  increase the geopotential perturbations

        x_tl%geop(:) = 100.0_wp*x_tl%geop(:)

        ! call tangent linear

        CALL ropp_fm_bangle_1d_tl(x, x_tl, obs_bangle, y_tl)
        
        !
        ! norm1 is calculated from the output of the TL routine
        ! ie, norm1  = (H.dx)^T * (H.dx)
        !
        norm1 = DOT_PRODUCT(y_tl, y_tl)
        
        ! initialise adjoint variables

        ALLOCATE(x_ad%state(SIZE(x%state)))
        ALLOCATE(x_ad%pres(x%n_lev))
        ALLOCATE(x_ad%temp(x%n_lev))
        ALLOCATE(x_ad%shum(x%n_lev))
        ALLOCATE(x_ad%geop(x%n_lev))

        x_ad%temp(:) = 0.0_wp
        x_ad%pres(:) = 0.0_wp
        x_ad%geop(:) = 0.0_wp
        x_ad%shum(:) = 0.0_wp

        ! Set y_ad = (H.dx)

        y_ad = y_tl

        ! call adjoint routine

        CALL ropp_fm_bangle_1d_ad(x, x_ad, obs_bangle, y_ad)
        
        !
        ! norm2 is calculated from the output of the AD routine
        ! and the initial perturbation.
        !

        norm2 = DOT_PRODUCT(RESHAPE(x_ad%temp,(/SIZE(x_ad%temp)/)),   &
             RESHAPE(x_tl%temp,(/SIZE(x_tl%temp)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%pres,(/SIZE(x_ad%pres)/)),   &
             RESHAPE(x_tl%pres,(/SIZE(x_tl%pres)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%geop,(/SIZE(x_ad%geop)/)),   &
             RESHAPE(x_tl%geop,(/SIZE(x_tl%geop)/))) + &
             DOT_PRODUCT(RESHAPE(x_ad%shum,(/SIZE(x_ad%shum)/)),   &
             RESHAPE(x_tl%shum,(/SIZE(x_tl%shum)/)))
        
        WRITE(6,'(a,2f15.5,f10.7)') 'Norms ', norm1, norm2, norm1/norm2

        DEALLOCATE(y_tl)
        DEALLOCATE(y_ad)
        
        IF(norm1/norm2 < 0.999_wp .OR. norm1/norm2 > 1.001_wp) error=1

!-------------------------------------------------------------------------------
! 12. Copy simulated observations to RO structure
!-------------------------------------------------------------------------------

        CALL ropp_fm_obs2roprof(obs_refrac, ro_data)
        CALL ropp_fm_obs2roprof(obs_bangle, ro_data)
                         
!-------------------------------------------------------------------------------
! 13. Clean up
!-------------------------------------------------------------------------------

        CALL ropp_io_free(ro_data)

     END DO
  END DO

  IF (error == 1) THEN
     PRINT *,''
     PRINT *,''
     PRINT *,'***********************************'
     PRINT *,'*** ropp_fm (t_fascod_ad): FAIL ***'
     PRINT *,'***********************************'
     PRINT *,''
  ELSE
     PRINT *,''
     PRINT *,''
     PRINT *,'***********************************'
     PRINT *,'*** ropp_fm (t_fascod_ad): PASS ***'
     PRINT *,'***********************************'
     PRINT *,''
  ENDIF

CONTAINS

!-------------------------------------------------------------------------------
! 14. Calculate observation levels for refractivity
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
! 15. Calculate observation levels for bending angle
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

    n = INT(60.0_wp / 0.2_wp)

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
    obs_bangle%impact(:)  =    &
                   REAL((1.0_wp + obs_refrac%refrac*1.e-6_wp) * tmp, KIND(0.0))

!   x.5 Fill other arrays
!   ---------------------

    obs_bangle%bangle(:)  = 0.0_wp
    obs_bangle%weights(:) = 1.0_wp

!   x.6 Clean up
!   ------------

    DEALLOCATE(tmp)

  END SUBROUTINE set_obs_levels_bangle
  
!-------------------------------------------------------------------------------
! 16. Usage information
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
! 17. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info(version)
    CHARACTER(len = *) :: version
    PRINT *, 't_fascod - Test ropp_fm adjoint routines.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP version ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM t_fascod_ad
