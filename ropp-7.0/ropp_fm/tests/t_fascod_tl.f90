PROGRAM t_fascod_tl

!****p* Programs/t_fascod_tl *
!
! NAME
!    t_fascod_tl - tests the compiled fm model and tangent linear functions
!
! SYNOPSIS
!    t_fascod_tl
! 
! DESCRIPTION
!    This program reads the FASCOD input file available in ../data. This file
!    has precalculated refractivity and bending angles. This program then
!    recalculates these profiles with random perturbations applied. 
!    Correctness of the tangent linear routines is tested by ensuring the 
!    relative error in the tangent linear tends to zero as the angle between
!    vectors tends to zero (cos angle -> 1)
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
  TYPE(State1dFM)                                  :: x_new
  TYPE(State1dFM)                                  :: x_tl
  TYPE(Obs1dRefrac)                                :: obs_refrac
  TYPE(Obs1dRefrac)                                :: obs_refrac_new
  TYPE(Obs1dBangle)                                :: obs_bangle
  TYPE(Obs1dBangle)                                :: obs_bangle_new

  INTEGER                                          :: i, iargc, argc, k, j
  INTEGER                                          :: n_files, n_profiles
  INTEGER                                          :: error

  LOGICAL                                          :: give_version
  LOGICAL                                          :: give_help
  LOGICAL                                          :: compress

  REAL(wp)                                         :: cos_alpha, rel_err
  REAL(wp)                                         :: max_alpha, min_err
  REAL(wp), DIMENSION(:), ALLOCATABLE              :: y_tl
  INTEGER                                          :: jmax, jmin

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

     DO i = 1, 1  !!n_profiles

!-------------------------------------------------------------------------------
! 7. Read data, store the original values in ro_dataorg
!-------------------------------------------------------------------------------

        CALL ropp_io_read(ro_data, ifiles(k), rec = i)
        
!-------------------------------------------------------------------------------
! 8. Copy data in RO structure to state and refrac obs vectors
!-------------------------------------------------------------------------------

        CALL ropp_fm_roprof2state(ro_data, x)
        CALL ropp_fm_roprof2state(ro_data, x_new)

        IF (compress) THEN	
           x%non_ideal = .TRUE.
           x_new%non_ideal = .TRUE.	    
        ENDIF 

        CALL set_obs_levels_refrac(obs_refrac)
        CALL set_obs_levels_refrac(obs_refrac_new)
        
!-------------------------------------------------------------------------------
! 9. Calculate refractivity
!-------------------------------------------------------------------------------

        PRINT *, "Checking REFRACTIVITY TL ", TRIM(ro_data%bg%source)

        CALL ropp_fm_refrac_1d(x, obs_refrac)

        ALLOCATE(x_tl%state(SIZE(x%state)))
        ALLOCATE(x_tl%pres(x%n_lev))
        ALLOCATE(x_tl%temp(x%n_lev))
        ALLOCATE(x_tl%shum(x%n_lev))
        ALLOCATE(x_tl%geop(x%n_lev))
        
        ALLOCATE(y_tl(SIZE(obs_refrac%refrac)))
        
        !
        ! Use standard random number routine to generate random fields
        !
        
        CALL RANDOM_NUMBER(x_tl%temp(:))
        CALL RANDOM_NUMBER(x_tl%pres(:))
        CALL RANDOM_NUMBER(x_tl%geop(:))
        CALL RANDOM_NUMBER(x_tl%shum(:))

        !  increase the geopotential perturbations

        x_tl%geop(:) = 100.0_wp*x_tl%geop(:)

        ! loop through, reducing the perturbation by a factor of 10

        jmax = -10000
        jmin = 10000
        max_alpha = -10000.0_wp
        min_err = 10000.0_wp

        DO j = 1,15
           
           ! update the perturbed variables
           
           x_new%temp(:) = x%temp(:) + x_tl%temp(:)
           x_new%pres(:) = x%pres(:) + x_tl%pres(:)
           x_new%geop(:) = x%geop(:) + x_tl%geop(:)
           x_new%shum(:) = x%shum(:) + x_tl%shum(:)
           
           ! Simulate with perturbed values
           
           CALL ropp_fm_refrac_1d(x_new, obs_refrac_new)
            
           ! Calculate tangent linear

           CALL ropp_fm_refrac_1d_tl(x, x_tl, obs_refrac, y_tl)
           
           ! Angle between the vectors

           cos_alpha = calc_angle(obs_refrac%refrac,obs_refrac_new%refrac,y_tl)
           
           ! Relative error in the tangent linear

           rel_err = calc_err(obs_refrac%refrac, obs_refrac_new%refrac, y_tl)

           WRITE (6,*) j,cos_alpha,100.0*rel_err

           ! look for max/minimum values
           
           IF ( cos_alpha > max_alpha ) THEN
              max_alpha = cos_alpha
              jmax = j
           ENDIF         

           IF ( rel_err < min_err ) THEN
              min_err = rel_err
              jmin = j
           ENDIF
           
           ! reduce the size of the perturbation by a factor of 10

           x_tl%temp = 0.1_wp*x_tl%temp
           x_tl%shum = 0.1_wp*x_tl%shum
           x_tl%pres = 0.1_wp*x_tl%pres
           x_tl%geop = 0.1_wp*x_tl%geop
           
        ENDDO

        DEALLOCATE(y_tl)
        
        ! Check tangent linear
        IF (jmin == jmax) THEN
           PRINT *, "   Refractivity TL check passed"
        ENDIF
        
        IF (ABS(jmin-jmax) > 1) THEN
           error = 1
        ENDIF
        
!-------------------------------------------------------------------------------
! 10. Copy data in RO and refrac structure to bending angle obs vector
!-------------------------------------------------------------------------------

        CALL set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle)
        CALL set_obs_levels_bangle(ro_data, obs_refrac, obs_bangle_new)

!-------------------------------------------------------------------------------
! 11. Calculate bending angle
!-------------------------------------------------------------------------------
        
        PRINT *, "Checking BENDING ANGLE TL ", TRIM(ro_data%bg%source)

        CALL ropp_fm_bangle_1d(x, obs_bangle)

        ALLOCATE(y_tl(SIZE(obs_bangle%bangle)))

        !
        ! Use standard random number routine to generate random fields
        !
        
        CALL RANDOM_NUMBER(x_tl%temp(:))
        CALL RANDOM_NUMBER(x_tl%pres(:))
        CALL RANDOM_NUMBER(x_tl%geop(:))
        CALL RANDOM_NUMBER(x_tl%shum(:))

        !  increase the geopotential perturbations

        x_tl%geop(:) = 100.0_wp*x_tl%geop(:)

        ! loop through, reducing the perturbation by a factor of 10

        jmax = -10000
        jmin = 10000
        max_alpha = -10000.0_wp
        min_err = 10000.0_wp

        DO j = 1,15

           ! update the perturbed variables
           
           x_new%temp(:) = x%temp(:) + x_tl%temp(:)
           x_new%pres(:) = x%pres(:) + x_tl%pres(:)
           x_new%geop(:) = x%geop(:) + x_tl%geop(:)
           x_new%shum(:) = x%shum(:) + x_tl%shum(:)
           
           ! Simulate with perturbed values
           
           CALL ropp_fm_bangle_1d(x_new, obs_bangle_new)

           ! Calaulate tangent linear

           CALL ropp_fm_bangle_1d_tl(x, x_tl, obs_bangle, y_tl)

           ! Angle between the vectors

           cos_alpha = calc_angle(obs_bangle%bangle,obs_bangle_new%bangle,y_tl)

           ! Relative error in the tangent linear

           rel_err = calc_err(obs_bangle%bangle, obs_bangle_new%bangle, y_tl)

           WRITE (6,*) j,cos_alpha,100.0*rel_err

           ! look for max/minimum values
           
           IF ( cos_alpha > max_alpha ) THEN
              max_alpha = cos_alpha
              jmax = j
           ENDIF         

           IF ( rel_err < min_err ) THEN
              min_err = rel_err
              jmin = j
           ENDIF

           ! reduce the size of the perturbation by a factor of 10

           x_tl%temp = 0.1_wp*x_tl%temp
           x_tl%shum = 0.1_wp*x_tl%shum
           x_tl%pres = 0.1_wp*x_tl%pres
           x_tl%geop = 0.1_wp*x_tl%geop
           
        ENDDO

        DEALLOCATE(y_tl)

        ! Check tangent linear
        IF (jmin == jmax) THEN
           PRINT *, "   Bending angle TL check passed"
        ENDIF
        
        IF (ABS(jmin-jmax) > 1) THEN
           error = 1
        ENDIF

!-------------------------------------------------------------------------------
! 12. Copy simulated observations to RO structure
!-------------------------------------------------------------------------------

        CALL ropp_fm_obs2roprof(obs_refrac, ro_data)
        CALL ropp_fm_obs2roprof(obs_bangle, ro_data)
                         
!-------------------------------------------------------------------------------
! 14. Clean up
!-------------------------------------------------------------------------------

        CALL ropp_io_free(ro_data)

     END DO
  END DO

  IF (error == 1) THEN
     PRINT *,''
     PRINT *,''
     PRINT *,'***********************************'
     PRINT *,'*** ropp_fm (t_fascod_tl): FAIL ***'
     PRINT *,'***********************************'
     PRINT *,''
  ELSE
     PRINT *,''
     PRINT *,''
     PRINT *,'***********************************'
     PRINT *,'*** ropp_fm (t_fascod_tl): PASS ***'
     PRINT *,'***********************************'
     PRINT *,''
  ENDIF

CONTAINS

!-------------------------------------------------------------------------------
! 15. Angle between vectors
!-------------------------------------------------------------------------------

  FUNCTION calc_angle(x, x_new, tl) RESULT(angle)

    USE typesizes, ONLY: wp => EightByteReal

    REAL(wp), DIMENSION(:), INTENT(in) :: x
    REAL(wp), DIMENSION(:), INTENT(in) :: x_new
    REAL(wp), DIMENSION(:), INTENT(in) :: tl
    REAL(wp)             :: angle

    angle = DOT_PRODUCT(tl, (x_new-x)) /     &
               SQRT(DOT_PRODUCT(tl, tl)*DOT_PRODUCT((x_new-x),(x_new-x)))
    
  END FUNCTION calc_angle

!-------------------------------------------------------------------------------
! 16. Relative error in the tangent linear
!-------------------------------------------------------------------------------

  FUNCTION calc_err(x, x_new, tl) RESULT(rel_err)

    USE typesizes, ONLY: wp => EightByteReal
    
    REAL(wp), DIMENSION(:), INTENT(in) :: x
    REAL(wp), DIMENSION(:), INTENT(in) :: x_new
    REAL(wp), DIMENSION(:), INTENT(in) :: tl
    REAL(wp)             :: rel_err

    rel_err = SQRT( DOT_PRODUCT( (x_new-x-tl),(x_new-x-tl))) / &
                SQRT( DOT_PRODUCT((x_new-x),(x_new-x)))

  END FUNCTION calc_err

!-------------------------------------------------------------------------------
! 17. Calculate observation levels for refractivity
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
! 18. Calculate observation levels for bending angle
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
    obs_bangle%impact(:) =    &
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
! 19. Usage information
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
! 20. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info(version)
    CHARACTER(len = *) :: version
    PRINT *, 't_fascod_tl - Test FM tangent linear routines.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP version ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM t_fascod_tl
