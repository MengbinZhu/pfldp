! $Id: ropp_1dvar_refrac.f90 3551 2013-02-25 09:51:28Z idculv $

PROGRAM ropp_1dvar_refrac

!****p* Programs/ropp_1dvar_refrac *
!
! NAME
!   ropp_1dvar_refrac
!
! SYNOPSIS
!   Perform a 1DVar retrieval of radio occultation data using refractivity
!   observations and a background
!
!   > ropp_1dvar_refrac [-c <cfg_file>] [-y <obs_file>] [-b <bg_file>]
!                       [-o <out_file>] [-no_ranchk] [-comp] [-check_qsat]
!                       [-d] [-h] [-v]
!
! OPTIONS
!   -c <cfg_file>               name of configuation file
!   -y <obs_file>               name of observation file
!   --obs-corr <obs_corr_file>  name of observation covariance file
!   -b <bg_file>                name of background file
!   --bg-corr <bg_corr_file>    name of background covariance file
!   -o <output_file>            name of output file
!   -no-ranchk                  do not range check input or output
!   -comp                       include non-ideal gas compressibility
!   -check_qsat                 include check against saturation
!   -d                          output additional diagnostics
!   -h                          help
!   -v                          version information
!
! DESCRIPTION
!   Perform a 1DVar retrieval of radio occultation data using refractivity
!   observations and a background.
!
! NOTES
!   Names of input and output files can be specified in both the configuration
!   file and on the command line; command line arguments will overwrite
!   configuration file settings.
!
!   If the input file is a multifile, both input files must be multifiles, and
!   the output file is also a multifile. It this case, it is assumed that
!   observation and background profiles have been arranged in identical order
!   in their respective input files.
!
!   Already existing output files will be overwritten.
!
! SEE ALSO
!   ropp_1dvar_bangle
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
  USE ropp_io_types, ONLY: ROprof, PCD_met
  USE ropp_fm
  USE ropp_fm_copy
  USE ropp_1dvar
  USE ropp_1dvar_types
  USE ropp_1dvar_copy
  USE ropp_qc
  use matrix

  IMPLICIT NONE

  TYPE(ROprof)          :: obs_data
  TYPE(ROprof)          :: bg_data
  TYPE(ROprof)          :: res_data
  TYPE(Obs1dRefrac)     :: obs
  TYPE(State1dFM)       :: bg
  TYPE(State1dFM)       :: state
  TYPE(VarConfig)       :: config
  TYPE(VarDiag)         :: diag

  INTEGER               :: idummy
  INTEGER               :: i, iargc, argc
  INTEGER               :: n_profiles

  LOGICAL               :: give_help
  LOGICAL               :: ranchk = .TRUE.
  LOGICAL               :: compress = .FALSE.
  LOGICAL               :: checkqsat = .FALSE.

  CHARACTER(len = 4096) :: obs_file
  CHARACTER(len = 4096) :: obs_corr_file
  CHARACTER(len = 4096) :: bg_file
  CHARACTER(len = 4096) :: bg_corr_file
  CHARACTER(len = 4096) :: out_file
  CHARACTER(len = 4096) :: cfg_file
  CHARACTER(len = 4096) :: buffer
  CHARACTER(len =  256) :: routine
  CHARACTER(len =    4) :: istr

!-------------------------------------------------------------------------------
! 2. Message handling
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_1dvar_refrac')

!-------------------------------------------------------------------------------
! 3. Default settings
!-------------------------------------------------------------------------------

  give_help     = .FALSE.

  obs_file      = "N/A"
  obs_corr_file = "N/A"
  bg_file       = "N/A"
  bg_corr_file  = "N/A"
  out_file      = "N/A"
  cfg_file      = "N/A"

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                   ROPP Refractivity 1D-Var'                          )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------------------------
! 4. Command line arguments
!-------------------------------------------------------------------------------

  argc = iargc()
  i = 1

! The following configurable options are not available via the command line,
! but via the configuration file:
!    bg_covar_method, obs_covar_method
!    all minimiser options
!    all QC options

  DO WHILE (i <= argc)
     CALL getarg (i, buffer)
     SELECT CASE (buffer)
        CASE('-c')                       ! Configuration file name
           CALL getarg (i+1, buffer)
           cfg_file = buffer
           i = i + 1
        CASE('-y', '-obs', '--obs')      ! Observation data input file name
           CALL getarg (i+1, buffer)
           obs_file = buffer
           i = i + 1
        CASE('--obs-corr')               ! Observation error correlation file
           CALL getarg (i+1, buffer)
           obs_corr_file = buffer
           i = i + 1
        CASE('-b', '-bg', '--bg')        ! Background data input file name
           CALL getarg (i+1, buffer)
           bg_file = buffer
           i = i + 1
        CASE('--bg-corr')                ! Background error correlation file
           CALL getarg (i+1, buffer)
           bg_corr_file = buffer
           i = i + 1
        CASE('-o')                       ! Output file name (netCDF output)
           CALL getarg (i+1, buffer)
           out_file = buffer
           i = i + 1
        CASE ('-no-ranchk')              ! Use no rangecheck on output
           ranchk = .FALSE.
        CASE ('-comp')                   ! Run with compressibilty
           compress = .TRUE.
        CASE ('-check_qsat')             ! Check against saturation
           checkqsat = .TRUE.
        CASE('-d')                       ! Additional diagnostic mode
           msg_MODE = VerboseMode
        CASE('-h', '--help', '?')        ! Help
           give_help = .TRUE.
        CASE('-v', '-V', '--version')    ! Version information
          CALL version_info()
          CALL EXIT(0)
        CASE default
     END SELECT
     i = i + 1
  END DO

  IF (argc == 0 .OR. give_help) THEN
    CALL usage()
    CALL EXIT(0)
  ENDIF

!-------------------------------------------------------------------------------
! 5. Configuration
!-------------------------------------------------------------------------------

! 5.1 Read configuration file
! ---------------------------

  IF (cfg_file /= 'N/A') THEN
    CALL message(msg_info, &
          "Reading configuration file " // TRIM(cfg_file) // "...\n")
    CALL ropp_1dvar_read_config (cfg_file, config)
  ENDIF

! 5.2 Merge configuration file and command line arguments
! -------------------------------------------------------

  IF (obs_file      /= 'N/A') config%obs_file      = obs_file
  IF (obs_corr_file /= 'N/A') config%obs_corr_file = obs_corr_file
  IF (bg_file       /= 'N/A') config%bg_file       = bg_file
  IF (bg_corr_file  /= 'N/A') config%bg_corr_file  = bg_corr_file
  IF (out_file      /= 'N/A') config%out_file      = out_file

  IF (config%use_logp) bg%use_logp = .TRUE.
  IF (config%use_logq) bg%use_logq = .TRUE.
  IF (msg_MODE == VerboseMode) config%extended_1dvar_diag = .TRUE.

!-------------------------------------------------------------------------------
! 6. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(out_file, idummy)

!-------------------------------------------------------------------------------
! 7. Consistency checks for input data
!-------------------------------------------------------------------------------

  n_profiles = ropp_io_nrec (config%obs_file)
  IF (ropp_io_nrec (config%bg_file) /= n_profiles) THEN
    CALL message(msg_fatal, &
     'Observation and background data file must have equal number of profiles.')
  ENDIF

!-------------------------------------------------------------------------------
! 8. Set up units
!-------------------------------------------------------------------------------

  CALL ropp_fm_set_units (obs_data)
  CALL ropp_fm_set_units (bg_data)

!-------------------------------------------------------------------------------
! 9. Loop over all profiles
!-------------------------------------------------------------------------------

  DO i = 1, n_profiles

    WRITE(istr, '(i4)') i
    istr = ADJUSTL(istr)

!-------------------------------------------------------------------------------
! 10. Read observation data and covariance matrix
!-------------------------------------------------------------------------------

    CALL message(msg_info, &
             "Reading observation data for profile " // TRIM(istr) // &
             " from the file\n      " // TRIM(config % obs_file) // ".\n")

    CALL ropp_io_read(obs_data, config%obs_file, rec=i, ranchk=ranchk)
    CALL ropp_fm_roprof2obs(obs_data, obs)

    IF ( obs%obs_ok ) CALL ropp_1dvar_covar (obs, config)

!-------------------------------------------------------------------------------
! 11. Read Background data and covariance matrix
!-------------------------------------------------------------------------------

    CALL message(msg_info, &
             "Reading background data for profile " // TRIM(istr) // &
             " from the file\n      " // TRIM(config % bg_file) // ".\n")

    CALL ropp_io_read(bg_data, config%bg_file, rec=i, ranchk=ranchk)
    CALL ropp_fm_roprof2state(bg_data, bg)
    IF ( bg%state_ok ) CALL ropp_1dvar_covar (bg, config)

!-------------------------------------------------------------------------------
! 12. Quality control
!-------------------------------------------------------------------------------

    diag%ok = .FALSE.
    IF ( obs%obs_ok .AND. bg%state_ok .AND. obs%cov_ok .AND. bg%cov_ok)  &
       diag%ok = .TRUE.
    WHERE(obs%refrac < -900.0_wp)
      obs%weights = 0.0_wp
    END WHERE

    IF (diag % ok) CALL ropp_qc_cutoff(obs, config)
    IF (diag % ok) CALL ropp_qc_genqc(obs, bg, config, diag)
    IF (diag % ok) CALL ropp_qc_OmB(obs, bg, config, diag)
    IF (diag % ok) CALL ropp_qc_bgqc(obs, config, diag)
    IF (diag % ok) CALL ropp_qc_pge(obs, config, diag)

!-------------------------------------------------------------------------------
! 13. 1DVar
!-------------------------------------------------------------------------------

    IF (diag%ok) THEN

      ! 13.1 First guess
      ! ----------------

      IF (compress) bg%non_ideal = .TRUE.

      IF (checkqsat) bg%check_qsat = .TRUE.

      state = bg

      ! 13.2 Retrieval
      ! --------------

      IF (config%minROPP%method == 'LEVMARQ') THEN
        CALL ropp_1dvar_levmarq(obs, bg, state, config, diag)
      ELSE
        CALL ropp_1dvar_solve(obs, bg, state, config, diag)
      ENDIF

      ! 13.3 Copy solution and forward modelled observations
      ! ----------------------------------------------------

      CALL ropp_1dvar_diagnostics(obs, state, config, diag)

      CALL ropp_fm_state2roprof (state, res_data)
      CALL ropp_fm_obs2roprof   (diag%res_refrac, res_data)

      ! 13.4 Copy diagnostic information
      ! ---------------------------------

      CALL ropp_1dvar_diag2roprof (obs, diag, res_data, config)

    ELSE

      res_data%PCD = IBSET(res_data%PCD, PCD_met)

    ENDIF

!-------------------------------------------------------------------------------
! 14. Write result
!-------------------------------------------------------------------------------

    IF (i == 1) THEN
      CALL message(msg_info, &
         "Writing 1DVar retrieval for profile " // TRIM(istr) // &
                " to the file\n      " // TRIM(config % out_file) // ".\n")
    ELSE
      CALL message(msg_info, &
         "Appending 1DVar retrieval for profile " // TRIM(istr) // &
         " to the file\n      " // TRIM(config % out_file) // ".\n")
    ENDIF

    CALL ropp_io_write(res_data, config%out_file, append=.TRUE., ranchk=ranchk)

!-------------------------------------------------------------------------------
! 15. Clean up
!-------------------------------------------------------------------------------

    CALL ropp_io_free (res_data)
    CALL ropp_io_free (bg_data)
    CALL ropp_io_free (obs_data)
    CALL ropp_fm_free (obs)
    CALL ropp_fm_free (state)
    CALL ropp_fm_free (bg)

    IF (ASSOCIATED(diag%OmB)) DEALLOCATE(diag%OmB)
    IF (ASSOCIATED(diag%OmB_sigma)) DEALLOCATE(diag%OmB_sigma)
    IF (ASSOCIATED(diag%pge)) DEALLOCATE(diag%pge)
    IF (ASSOCIATED(diag%pge_weights)) DEALLOCATE(diag%pge_weights)
!    IF (ASSOCIATED(diag%J_bgr)) DEALLOCATE(diag%J_bgr)
!    IF (ASSOCIATED(diag%J_obs)) DEALLOCATE(diag%J_obs)
!    IF (ASSOCIATED(diag%B_sigma)) DEALLOCATE(diag%B_sigma)
    IF (ASSOCIATED(diag%OmA)) DEALLOCATE(diag%OmA)
    IF (ASSOCIATED(diag%OmA_sigma)) DEALLOCATE(diag%OmA_sigma)
    CALL ropp_fm_free (diag%bg_bangle)
    CALL ropp_fm_free (diag%bg_refrac)
    CALL ropp_fm_free (diag%res_bangle)
    CALL ropp_fm_free (diag%res_refrac)

  END DO

CONTAINS


!-------------------------------------------------------------------------------
! 16. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Refractivity 1D-Var retrieval'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_1dvar_refrac [<options>] -o <output_file>'
    PRINT *, 'Options:'
    PRINT *, '  -c <cfg_file>               name of configuation file'
    PRINT *, '  -y <obs_file>               name of observation file'
    PRINT *, '  --obs-corr <obs_corr_file>  name of observation covariance file'
    PRINT *, '  -b <bg_file>                name of background file'
    PRINT *, '  --bg-corr <bg_corr_file>    name of background covariance file'
    PRINT *, '  -o <output_file>            name of ROPP netCDF output file'
    PRINT *, '  -no-ranchk                  do not range check input or output'
    PRINT *, '  -comp                       include non-ideal gas compressibility'
    PRINT *, '  -check_qsat                 include check against saturation'
    PRINT *, '  -d                          output additional diagnostics'
    PRINT *, '  -h                          this help'
    PRINT *, '  -v                          version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 17. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_1dvar_version()
    PRINT *, 'ropp_1dvar_refrac - Refractivity 1D-Var retrieval'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (1DVAR) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_1dvar_refrac
