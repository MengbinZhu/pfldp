! $Id: ropp_pp_spectra_tool.f90 2021 2009-01-16 10:49:04Z frhl $

PROGRAM ropp_pp_spectra_tool

!****p* Programs/ropp_pp_spectra_tool *
!
! NAME
!   ropp_pp_spectra_tool
!
! SYNOPSIS
!   Pre-processing tool to compute local spatial spectra of the
!   complex wave field.
!
!   >  ropp_pp_spectra_tool <infile(s)> [-o <outfile>]
!                           [-c <cnfgfile>] [-m <method>] [-mfile <mfile>]
!                           [-navfile <nfile>]
!                           [-h] [-v]
!
! ARGUMENTS
!   <infile(s)>   One (or more) input file names
!
! OPTIONS
!   -o <outfile>      name of spectra output file
!                     (default: ROanalysis_pp_<type>_<freq>.dat)
!   -c <cnfgfile>     name of configuration file
!   -m <method>       ionospheric correction method
!   -mfile <mfile>    model refractivity file
!   -navfile <nfile>  name of external navigation bit file
!   -h                help
!   -v                version information
!
! DESCRIPTION
!   This program reads RO L1 and L2 excess phase data as a function of time
!   from the input data files and calculates local spatial spectra as a
!   function of doppler frequency shift and time or as a function of bending
!   angle and impact parameter. The results are written to four ASCII files, 
!   whose contents can be plotted by, for instance, 
!   ropp_pp/tests/it_pp_spectra_dt.pro and ropp_pp/tests/it_pp_spectra_dt.pro.
!
! NOTES
!   If the input file is a multifile, or more than one input files are
!   specified, output for all profiles are written out in separate files.
!
!   Already existing output files will be overwritten.
!
! EXAMPLE
!   To calculate spectra for one of the example files in the data directory:
!
!     > ropp_pp_spectra_tool ../data/input.nc
!
! SEE ALSO
!   For an example plotting tool to view the resulting spectra see
!   ropp_pp/tests_plot_spectra.pro
!
! REFERENCES
!  Gorbunov M.E., Lauritsen K.B., Rhodin A., Tomassini M. and
!   Kornblueh L. 2006
!   Radio holographic filtering, error estimation, and quality control of
!   radio occultation data
!   Journal of Geophysical Research (111) D10105
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
  USE ropp_io_types, ONLY: ROprof, L1atype, PCD_rising, PCD_open_loop
  USE ropp_pp
  USE ropp_pp_preproc
  USE ropp_pp_types, ONLY: PPConfig

  IMPLICIT NONE

  TYPE(ROprof)    :: ro_data      ! Input RO data
  TYPE(ppConfig)  :: config       ! Configuration options
  TYPE(L1atype)   :: Lev1a        ! Temporary Level1a structure for storage

  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: A   ! L1 and L2 amplitude
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: S   ! L1 and L2 excess phase
  REAL(wp), DIMENSION(:), POINTER :: phase => null()  ! Work array
  REAL(wp), DIMENSION(:), POINTER :: phase_LM => null() ! Model excess phase (m)
  REAL(wp), DIMENSION(:), POINTER :: impact_LM => null() ! Model impact (m)

  INTEGER,  DIMENSION(:), POINTER :: LCF => null()  ! Lost carrier flag
!  INTEGER,  DIMENSION(:), ALLOCATABLE :: LCF        ! Lost carrier flag
  REAL(wp)                        :: secs_past_hour ! No. of seconds past hour
  REAL(wp)                            :: sb         ! Phase base
  INTEGER                             :: iol        ! Open loop discont index
  INTEGER                             :: ic         ! Index counter
  INTEGER                             :: n          ! Number of data points

  INTEGER         :: idummy
  INTEGER         :: i, iargc, argc, k
  INTEGER         :: n_files, n_profiles
  LOGICAL         :: give_help

  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len = 4096)                            :: ofile
  CHARACTER(len = 4096)                            :: cfg_file = " "
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =   80)                            :: mfile = " "
  CHARACTER(len =   80)                            :: navfile = " "
  CHARACTER(len =   10)                            :: method = " "
  CHARACTER(len =    4)                            :: istr
  CHARACTER(len =    6)                            :: nstr

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  give_help = .FALSE.
  ofile     = "ropp_pp_spectra"

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                 ROPP Pre-processor Spectra Tool'                      )
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
        CASE('-o')                          ! Output file name (ASCII output)
           CALL getarg(i+1, buffer)
           ofile = buffer
           i = i + 1
        CASE('-c')                          ! Configuration file name
           CALL getarg(i+1, buffer)
           cfg_file = buffer
           i = i + 1
        CASE('-m')                          ! Ionospheric correction method
           CALL getarg(i+1, buffer)
           method = buffer
           i = i + 1
        CASE('-mfile')                      ! Model refractivity file
           CALL getarg(i+1, buffer)
           mfile = buffer
           i = i + 1
        CASE('-navfile')                    ! External navigation bit file
           CALL getarg(i+1, buffer)
           navfile = buffer
           i = i + 1
        CASE('-h', '--help', '?')           ! help
           give_help = .TRUE.
        CASE('-v', '-V', '--version')       ! version information
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

  IF (argc == 0 .OR. n_files == 0 .OR. give_help) THEN
    CALL usage()
    CALL EXIT(0)
  ENDIF

  ! 3.1 Read configuration file (if exists)

  IF (method /= " ")    config%method = method
  IF (mfile /= " ")     config%mfile = mfile
  IF (navfile /= " ")   config%navbit_file = navfile

  IF (cfg_file /= " ") THEN
    CALL message(msg_info, &
         "Reading configuration file " // TRIM(cfg_file) // ".\n")
    CALL ropp_pp_read_config(cfg_file, config)
    IF (method /= " ")    config%method = method
    IF (mfile /= " ")     config%mfile = mfile
    IF (navfile /= " ")   config%navbit_file = navfile
  ENDIF

!-------------------------------------------------------------------------------
! 4. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(ofile, idummy)

!-------------------------------------------------------------------------------
! 5. Loop over all input files and profiles
!-------------------------------------------------------------------------------

  DO k = 1, n_files

    n_profiles = ropp_io_nrec(ifiles(k))

    DO i = 1, n_profiles

      WRITE(istr, '(i4)') i
      WRITE(nstr, '(i6)') n_profiles
      CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

!-------------------------------------------------------------------------------
! 6. Read data
!-------------------------------------------------------------------------------

! 6.1 Read input file

      CALL ropp_io_read(ro_data, ifiles(k), rec=i, ranchk=.TRUE.)
      CALL message(msg_info, "(" // TRIM(ro_data%occ_id) // ") \n")

      IF (ro_data%Lev1a%Npoints == 0) THEN
        config%obs_ok = .FALSE.
        CALL message(msg_fatal, "FAILURE: No Level1a data in file " // &
                                 ifiles(k) //  "\n")
      ENDIF

! 6.2 Shrink ro_data to correct size (for multiple profiles)

      CALL ropp_io_init(Lev1a, ro_data%lev1a%npoints)
      Lev1a = ro_data%lev1a
      ro_data%lev1a = Lev1a

!-------------------------------------------------------------------------------
! 7. Check coordinate frames
!-------------------------------------------------------------------------------

      CALL ropp_pp_set_coordinates(ro_data)

!-------------------------------------------------------------------------------
! 8. Determine occultation point georeference information
!-------------------------------------------------------------------------------

      CALL occ_point( ro_data%lev1a%r_leo,  ro_data%lev1a%r_gns,          &
                      ro_data%georef%lat,   ro_data%georef%lon,           &
                      ro_data%georef%r_coc, ro_data%georef%roc,           &
                      ro_data%georef%azimuth, ro_data%georef%undulation , &
                      config%egm96, config%corr_egm96 )

!-------------------------------------------------------------------------------
! 9. Mission-specific pre-processing and input data cut-off, compute spectra
!-------------------------------------------------------------------------------

! 9.0 Set default configuration options

      IF (ANY(ro_data%Lev1a%r_leo == ropp_MDFV) .OR.  &
          ANY(ro_data%Lev1a%r_gns == ropp_MDFV)) THEN
        CALL message(msg_warn, 'Invalid coordinate values. Exiting processing.')
        config%obs_ok = .false.
        EXIT
      ENDIF

      config%opt_spectra = .true.
      config%Acut = 0.0_wp
      config%Pcut = -9999.9_wp
      config%Bcut = 0.0_wp
      config%obs_ok = .TRUE.

      n = ro_data%Lev1a%Npoints

      ALLOCATE(LCF(n))
      LCF(:) = 0

      CALL ropp_pp_cutoff_amplitude(ro_data, LCF, config)

      SELECT CASE (ro_data%leo_id(1:2))

! 9.1 COSMIC

      CASE ('C0','CO')

        CALL message(msg_info, 'COSMIC data preprocessing')
        CALL ropp_pp_preprocess_COSMIC(ro_data, config, LCF)

! 9.2 CHAMP and GRACE

      CASE ('CH','GR')

        CALL message(msg_info, 'CHAMP data preprocessing')

! 9.2.1 Correct L2 amplitude if not defined

      IF ( ANY(ro_data%Lev1a%snr_L2p == 0.0_wp) ) THEN
        ro_data%Lev1a%snr_L2p(:) = ro_data%Lev1a%snr_L1ca(:)
      ENDIF

! 9.3 GRAS

      CASE ('ME')

        CALL message(msg_info, 'GRAS data preprocessing')

        CALL ropp_pp_preprocess_GRASRS(ro_data, config, LCF)

        CALL ropp_pp_cutoff_amplitude(ro_data, LCF, config)

        WRITE(nstr, '(i6)') ro_data%lev1a%npoints
        CALL message(msg_diag, 'Merged RS+CL data size: ' // nstr)

! 9.4 Default

      CASE default

        CALL message(msg_warn, 'Occultation LEO id '// TRIM(ro_data%leo_id) // &
          ' not recognised \n. ' // &
          ' No mission-specific data pre-processing conducted.')

      END SELECT

!-------------------------------------------------------------------------------
! 10. Compute model excess phase
!-------------------------------------------------------------------------------

      IF (ro_data%Lev1a%Npoints .lt. 100) THEN
        config%obs_ok = .FALSE.
        CALL message(msg_fatal, "Error: Too few data")
      ENDIF 

      n = ro_data%Lev1a%Npoints
      ALLOCATE(phase_LM(n))
      ALLOCATE(impact_LM(n))


      CALL ropp_pp_modelphase(ro_data%dtocc%month, ro_data%georef%lat,      &
                              ro_data%georef%lon,                           &
                              ro_data%Lev1a%dtime, ro_data%lev1a%r_leo,     &
                              ro_data%lev1a%r_gns, ro_data%georef%r_coc,    &
                              ro_data%georef%roc, phase_LM, impact_LM, config)

!-------------------------------------------------------------------------------
! 11. Open loop processing (GRAS and COSMIC missions only)
!-------------------------------------------------------------------------------

      SELECT CASE (ro_data%leo_id(1:2))

! 11.1 GRAS

      CASE ('ME')

        CALL message(msg_info,'GRAS data: openloop preprocessing')

        WHERE(MOD(LCF(:),2) /= 0)
          ro_data%Lev1a%phase_L2(:) = phase_LM(:)
        ENDWHERE

        WHERE(ro_data%Lev1a%phase_L2(:) < ropp_MDTV)
          ro_data%Lev1a%phase_L2(:) = -1.0_wp
          ro_data%Lev1a%phase_L2(:) = phase_LM(:)
        ENDWHERE

        secs_past_hour = 0.0_wp
        IF(ro_data%DTocc%Minute .LE.  59) &
          secs_past_hour = secs_past_hour + ro_data%DTocc%Minute*60.0_wp
        IF(ro_data%DTocc%Second .LE.  59) &
          secs_past_hour = secs_past_hour + ro_data%DTocc%Second*1.0_wp
        IF(ro_data%DTocc%Msec   .LT. 999) &  ! Sometimes msec not set, so defaults to 999
          secs_past_hour = secs_past_hour + ro_data%DTocc%Msec*1.0e-3_wp

        CALL ropp_pp_openloop(ro_data%Lev1a%dtime+secs_past_hour,             &
                              ro_data%Lev1a%phase_L1, ro_data%Lev1a%phase_L2, &
                              phase_LM, ro_data%lev1a%r_leo,                  &
                              ro_data%lev1a%r_gns, ro_data%georef%r_coc, LCF)

        ! Set PCD flag
        ro_data%PCD = IBSET(ro_data%PCD, PCD_open_loop)

! 11.2 COSMIC

      CASE ('C0','CO')

        CALL message(msg_info,'COSMIC data: openloop preprocessing')

        CALL ropp_pp_openloop(ro_data%Lev1a%dtime,    ro_data%Lev1a%phase_L1, &
                              ro_data%Lev1a%phase_L2, phase_LM,               &
                              ro_data%lev1a%r_leo,    ro_data%lev1a%r_gns,    &
                              ro_data%georef%r_coc,   LCF)

        ! Set PCD flag
        ro_data%PCD = IBSET(ro_data%PCD, PCD_open_loop)

      END SELECT

!-------------------------------------------------------------------------------
! 12. Data cutoff
!-------------------------------------------------------------------------------

!      CALL ropp_pp_cutoff(ro_data, config, phase_LM, impact_LM)

!-------------------------------------------------------------------------------
! 13. Calculate spectra
!-------------------------------------------------------------------------------

      CALL message(msg_info, 'Compute frequency-time spectra')

      CALL ropp_pp_spectra(ro_data%Lev1a%dtime, ro_data%Lev1a%phase_L1, &
                           ro_data%Lev1a%phase_L2, phase_LM, impact_LM, &
                           config, OutRO=.true., filnam=ofile)

      ALLOCATE(A(2,ro_data%Lev1a%npoints))
      ALLOCATE(S(2,ro_data%Lev1a%npoints))
      A(1,:) = ro_data%Lev1a%snr_L1ca(:)
      A(2,:) = ro_data%Lev1a%snr_L2p(:)
      S(1,:) = ro_data%Lev1a%phase_L1(:)
      S(2,:) = ro_data%Lev1a%phase_L2(:)

      CALL message(msg_info, 'Compute bending-impact spectra')

      CALL ropp_pp_radiooptic_analysis(ro_data%Lev1a%dtime,  &
                                       ro_data%lev1a%r_leo,  &
                                       ro_data%lev1a%r_gns,  &
                                       ro_data%georef%r_coc, &
                                       ro_data%georef%roc,   &
                                       phase_LM, S, A,       &
                                       OutRO=.true.,         &
                                       filnam=ofile)

      DEALLOCATE(A)
      DEALLOCATE(S)

!-------------------------------------------------------------------------------
! 14. Clean up
!-------------------------------------------------------------------------------

      IF (config%obs_ok) THEN

        CALL message(msg_info, "Computed spectra successfully \n")
        CALL message(msg_info, "Output spectra to files " // &
                                TRIM(ro_data%occ_id) // "* \n")
        CALL message(msg_info, "See tests/it_pp_plot_spectra_dt.pro and " // & 
                               "tests/it_pp_plot_spectra_ep.pro for plotting\n")

      ELSE

        CALL message(msg_warn, "Error with input data - " // &
                               "cannot compute spectra. " // &
                                "Program terminated. \n")

      ENDIF

      CALL ropp_io_free(ro_data)

      DEALLOCATE(LCF)
      DEALLOCATE(impact_LM)
      DEALLOCATE(phase_LM)

    END DO  ! loop over profiles

  END DO  ! loop over files

CONTAINS

!-------------------------------------------------------------------------------
! 23. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Pre-processor spectra tool - calculate local spatial spectra'
    PRINT *, '   from L1 and L2 phase and amplitude data'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_pp_spectra_tool [<options>] <input_file(s)>'
    PRINT *, 'Options:'
    PRINT *, '  -c <config_file>  name of configuration file'
    PRINT *, '  -o <output_file>  name of spectra output file'
    PRINT *, '                    (default: ropp_pp_spectra_<type>.dat)'
    PRINT *, '  -h                this help'
    PRINT *, '  -v                version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 24. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_pp_version()
    PRINT *, 'ropp_pp_spectra_tool - Pre-processor spectra tool: '
    PRINT *, '                       Calculate local spatial spectra from'
    PRINT *, '                       L1 and L2 phase and amplitude data'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (PP) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_pp_spectra_tool
