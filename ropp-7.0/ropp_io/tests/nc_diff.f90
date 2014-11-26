! $Id: nc_diff.f90 2197 2009-06-23 09:11:17Z frhl $

PROGRAM nc_diff

!****pi* Tests/nc_diff *
!
! NAME
!    nc_diff - Compares two ROPP nc files for meaningful differences.
!              Needed by t_bgrasc2ropp.sh and t_eum2ropp.sh.
!
! SYNOPSIS
!    nc_diff file1.nc file2.nc "test_name"
! 
! DESCRIPTION
!    Compares two ROPP nc files.
!    Needed by t_bgrasc2ropp.sh and t_eum2ropp.sh.
!
! INPUTS
!    ROPP netcdf files file1.nc and file2.nc.
!
! OUTPUT
!    Reports (to unit 6) whether the elements of the files are "similar".
!
! NOTES
!    The various thresholds for defining differences as significant
!    are subject to change, depending on experience with this routine.
!
! REFERENCES
!
! AUTHOR
!    Met Office, Exeter, UK.
!    Any comments on this software should be given via the ROM SAF
!    Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

  USE typesizes,     wp => EightByteReal
  USE messages
  USE ropp_utils,    ONLY: ropp_MDFV, ropp_MDTV
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_read

  IMPLICIT NONE

! Input variables
  CHARACTER(LEN=256)  :: file_in1, file_in2  ! I/P file names
  CHARACTER(LEN=256)  :: test_name=""        ! Name of test

! Local variables
  TYPE(ROprof)        :: prof1               ! RO profile from file1.nc
  TYPE(ROprof)        :: prof2               ! RO profile from file2.nc

  INTEGER             :: narg                ! No. of command line arguments
  INTEGER             :: iostatus            ! I/O Status
  INTEGER             :: idiff               ! Difference counter
  CHARACTER(LEN=4)    :: sdiff               ! STRING(difference counter)
  LOGICAL             :: exists              ! File exists flag
  LOGICAL             :: bg, GEOref, &       ! The substructures of
                         Lev1a, Lev1b, &     !   ROprof to be compared
                         Lev2a, Lev2b, &     !
                         Lev2c, Lev2d        !

! Some compilers may need the following declaration to be commented out
  INTEGER             :: IARGC


! 0. Read instructions
! --------------------

  narg = IARGC()

  IF (narg < 2) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "nc_diff needs at least two arguments" )
  ENDIF
  
  file_in1 =  " "       ! no default for i/p file name
  CALL GETARG ( 1, file_in1 )

  file_in2 =  " "       ! no default for i/p file name
  CALL GETARG ( 2, file_in2 )

  IF (narg >= 3) CALL GETARG ( 3, test_name )

  CALL message ( msg_info, "Using nc_diff to compare " // &
                           TRIM(ADJUSTL(file_in1)) // " and " // &
                           TRIM(ADJUSTL(file_in2)) )

! Define the ROprof substructures that are to be compared

  SELECT CASE (test_name)
    CASE ("bgrasc2ropp")
      bg     =  .TRUE.  ;  GEOref =  .TRUE.
      Lev1a  = .FALSE.  ;  Lev1b  = .FALSE.
      Lev2a  =  .TRUE.  ;  Lev2b  =  .TRUE.
      Lev2c  =  .TRUE.  ;  Lev2d  =  .TRUE.
    CASE ("eum2ropp")
      bg     = .FALSE.  ;  GEOref = .FALSE.
      Lev1a  =  .TRUE.  ;  Lev1b  =  .TRUE.
      Lev2a  = .FALSE.  ;  Lev2b  = .FALSE.
      Lev2c  = .FALSE.  ;  Lev2d  = .FALSE.
    CASE ("eum2roppb")
      bg     = .FALSE.  ;  GEOref = .FALSE.
      Lev1a  = .FALSE.  ;  Lev1b  =  .TRUE.
      Lev2a  = .FALSE.  ;  Lev2b  = .FALSE.
      Lev2c  = .FALSE.  ;  Lev2d  = .FALSE.
    CASE DEFAULT
      bg     =  .TRUE.  ;  GEOref =  .TRUE.
      Lev1a  =  .TRUE.  ;  Lev1b  =  .TRUE.
      Lev2a  =  .TRUE.  ;  Lev2b  =  .TRUE.
      Lev2c  =  .TRUE.  ;  Lev2d  =  .TRUE.
  END SELECT


! 1. Read 1st prof
! ----------------

  INQUIRE ( FILE=file_in1, EXIST=exists )
  IF ( .NOT. exists ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "Input netCDF file " // TRIM(ADJUSTL(file_in1)) // " not found" )
  ENDIF

  CALL ropp_io_read ( prof1, FILE=file_in1, IERR=iostatus )
  IF ( iostatus > 0 ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "I/O error while reading " // file_in1 )
  ENDIF


! 2. Read 2nd prof
! ----------------

  INQUIRE ( FILE=file_in2, EXIST=exists )
  IF ( .NOT. exists ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_error, "Input netCDF file " // TRIM(ADJUSTL(file_in2)) // " not found" )
  ENDIF

  CALL ropp_io_read ( prof2, FILE=file_in2, IERR=iostatus )
  IF ( iostatus > 0 ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "I/O error while reading " // file_in2 )
  ENDIF


! 3. Compare the two
! ------------------

  idiff = 0  ! Initialise difference counter

  IF (bg) THEN
  
    IF (prof1%bg%source /= prof2%bg%source) THEN
      CALL message ( msg_error, "prof1%bg%source /= prof2%bg%source" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Year - prof2%bg%Year) > 0) THEN
      CALL message ( msg_error, "prof1%bg%Year /= prof2%bg%Year" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Month - prof2%bg%Month) > 0) THEN
      CALL message ( msg_error, "prof1%bg%Month /= prof2%bg%Month" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Day - prof2%bg%Day) > 0) THEN
      CALL message ( msg_error, "prof1%bg%Day /= prof2%bg%Day" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Hour - prof2%bg%Hour) > 0) THEN
      CALL message ( msg_error, "prof1%bg%Hour /= prof2%bg%Hour" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Minute - prof2%bg%Minute) > 0) THEN
      CALL message ( msg_error, "prof1%bg%Minute /= prof2%bg%Minute" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%bg%Fcperiod - prof2%bg%Fcperiod) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%bg%Fcperiod /= prof2%bg%Fcperiod" )
      idiff = idiff + 1
    ENDIF

  ENDIF ! bg


  IF (GEOref) THEN

    IF (ABS(prof1%GEOref%lat - prof2%GEOref%lat) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%GEOref%lat /= prof2%GEOref%lat" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%GEOref%lon - prof2%GEOref%lon) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%GEOref%lon /= prof2%GEOref%lon" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%GEOref%r_coc - prof2%GEOref%r_coc) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%GEOref%r_coc /= prof2%GEOref%r_coc" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%GEOref%roc - prof2%GEOref%roc) > 1.0_wp) THEN
      CALL message ( msg_error, "prof1%GEOref%roc /= prof2%GEOref%roc" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%GEOref%azimuth - prof2%GEOref%azimuth) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%GEOref%azimuth /= prof2%GEOref%azimuth" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%GEOref%undulation - prof2%GEOref%undulation) > 1.0e-6_wp) THEN
      CALL message ( msg_warn, "prof1%GEOref%undulation /= prof2%GEOref%undulation" )
      CALL message ( msg_info, "Suggest checking that GEOPOT_COEF and GEOPOT_CORR " // &
                               "have been set correctly. (See grib2bgrasc man page.) \n")
      idiff = idiff + 0
    ENDIF

  ENDIF ! GEOref


  IF (Lev1a) THEN

    IF (ABS(prof1%Lev1a%Npoints - prof2%Lev1a%Npoints) > 0) THEN
      CALL message ( msg_error, "prof1%Lev1a%Npoints /= prof2%Lev1a%Npoints" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%dtime - prof2%Lev1a%dtime) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%dtime /= prof2%Lev1a%dtime" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%snr_L1ca - prof2%Lev1a%snr_L1ca) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%snr_L1ca /= prof2%Lev1a%snr_L1ca" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%snr_L1p - prof2%Lev1a%snr_L1p) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%snr_L1p /= prof2%Lev1a%snr_L1p" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%snr_L2p - prof2%Lev1a%snr_L2p) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%snr_L2p /= prof2%Lev1a%snr_L2p" )
      idiff = idiff + 1
    ENDIF

! Relax tolerance here to +/- 5*2pi, because different compilers can introduce
! such phase jumps when calculating (eg) MODULO functions in accumulate_phase.
!    IF (ANY(ABS(prof1%Lev1a%phase_L1 - prof2%Lev1a%phase_L1) > 1.0e-6_wp)) THEN
    IF (ANY(ABS(prof1%Lev1a%phase_L1 - prof2%Lev1a%phase_L1) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%phase_L1 /= prof2%Lev1a%phase_L1" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%phase_L2 - prof2%Lev1a%phase_L2) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%phase_L2 /= prof2%Lev1a%phase_L2" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%r_gns - prof2%Lev1a%r_gns) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%r_gns /= prof2%Lev1a%r_gns" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%v_gns - prof2%Lev1a%v_gns) > 1.0e-3_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%v_gns /= prof2%Lev1a%v_gns" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%r_leo - prof2%Lev1a%r_leo) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%r_leo /= prof2%Lev1a%r_leo" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%v_leo - prof2%Lev1a%v_leo) > 1.0e-3_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%v_leo /= prof2%Lev1a%v_leo" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1a%phase_qual - prof2%Lev1a%phase_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1a%phase_qual /= prof2%Lev1a%phase_qual" )
      idiff = idiff + 1
    ENDIF

!  Open_loop_lcf is not (yet) part of standard Lev1a substructure.
!    IF (ANY(ABS(prof1%Lev1a%open_loop_lcf - prof2%Lev1a%open_loop_lcf) > 1.0e-6_wp)) THEN
!      CALL message ( msg_error, "prof1%Lev1a%open_loop_lcf /= prof2%Lev1a%open_loop_lcf" )
!      idiff = idiff + 1
!    ENDIF

!  Open_loop_lcf is not (yet) part of standard Lev1a substructure.
!  Instead, if present, it is probably held in ROprof%vlist%VlistD1d%DATA.
    IF (ASSOCIATED(prof1%vlist%VlistD1d) .AND. ASSOCIATED(prof1%vlist%VlistD1d)) THEN
      IF (prof1%vlist%VlistD1d%name == "open_loop_lcf" .AND. &
          prof2%vlist%VlistD1d%name == "open_loop_lcf") THEN
        IF (ANY(ABS(prof1%vlist%VlistD1d%DATA - prof2%vlist%VlistD1d%DATA) > 1.0e-6_wp)) THEN
          CALL message ( msg_error, "prof1%Lev1a%open_loop_lcf /= prof2%Lev1a%open_loop_lcf" )
          idiff = idiff + 1
        ENDIF
      ENDIF
    ENDIF


  ENDIF ! Lev1a


  IF (Lev1b) THEN

    IF (ABS(prof1%Lev1b%Npoints - prof2%Lev1b%Npoints) > 0) THEN
      CALL message ( msg_error, "prof1%Lev1b%Npoints /= prof2%Lev1b%Npoints" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%lat_tp - prof2%Lev1b%lat_tp) > 1.0e-4_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%lat_tp /= prof2%Lev1b%lat_tp" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%lon_tp - prof2%Lev1b%lon_tp) > 1.0e-4_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%lon_tp /= prof2%Lev1b%lon_tp" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%azimuth_tp - prof2%Lev1b%azimuth_tp) > 1.0e-2_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%azimuth_tp /= prof2%Lev1b%azimuth_tp" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%impact_L1 - prof2%Lev1b%impact_L1) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%impact_L1 /= prof2%Lev1b%impact_L1" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%impact_L2 - prof2%Lev1b%impact_L2) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%impact_L2 /= prof2%Lev1b%impact_L2" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%impact - prof2%Lev1b%impact) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%impact /= prof2%Lev1b%impact" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%impact_opt - prof2%Lev1b%impact_opt) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%impact_opt /= prof2%Lev1b%impact_opt" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L1 - prof2%Lev1b%bangle_L1) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L1 /= prof2%Lev1b%bangle_L1" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L2 - prof2%Lev1b%bangle_L2) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L2 /= prof2%Lev1b%bangle_L2" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle - prof2%Lev1b%bangle) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle /= prof2%Lev1b%bangle" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_opt - prof2%Lev1b%bangle_opt) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_opt /= prof2%Lev1b%bangle_opt" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L1_sigma - prof2%Lev1b%bangle_L1_sigma) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L1_sigma /= prof2%Lev1b%bangle_L1_sigma" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L2_sigma - prof2%Lev1b%bangle_L2_sigma) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L2_sigma /= prof2%Lev1b%bangle_L2_sigma" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_sigma - prof2%Lev1b%bangle_sigma) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_sigma /= prof2%Lev1b%bangle_sigma" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_opt_sigma - prof2%Lev1b%bangle_opt_sigma) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_opt_sigma /= prof2%Lev1b%bangle_opt_sigma" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L1_qual - prof2%Lev1b%bangle_L1_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L1_qual /= prof2%Lev1b%bangle_L1_qual" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_L2_qual - prof2%Lev1b%bangle_L2_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_L2_qual /= prof2%Lev1b%bangle_L2_qual" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_qual - prof2%Lev1b%bangle_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_qual /= prof2%Lev1b%bangle_qual" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev1b%bangle_opt_qual - prof2%Lev1b%bangle_opt_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev1b%bangle_opt_qual /= prof2%Lev1b%bangle_opt_qual" )
      idiff = idiff + 1
    ENDIF

  ENDIF ! Lev1b


  IF (Lev2b) THEN

    IF (ABS(prof1%Lev2b%Npoints - prof2%Lev2b%Npoints) > 0) THEN
      CALL message ( msg_error, "prof1%Lev2b%Npoints /= prof2%Lev2b%Npoints" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2b%press - prof2%Lev2b%press) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2b%press /= prof2%Lev2b%press" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2b%temp - prof2%Lev2b%temp) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2b%temp /= prof2%Lev2b%temp" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2b%shum - prof2%Lev2b%shum) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2b%shum /= prof2%Lev2b%shum" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2b%geop - prof2%Lev2b%geop) > 1.0_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2b%geop /= prof2%Lev2b%geop" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2b%meteo_qual - prof2%Lev2b%meteo_qual) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2b%meteo_qual /= prof2%Lev2b%meteo_qual" )
      idiff = idiff + 1
    ENDIF

  ENDIF ! Lev2b


  IF (Lev2c) THEN

    IF (ABS(prof1%Lev2c%Npoints - prof2%Lev2c%Npoints) > 0) THEN
      CALL message ( msg_error, "prof1%Lev2c%Npoints /= prof2%Lev2c%Npoints" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%Lev2c%geop_sfc - prof2%Lev2c%geop_sfc) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%Lev2c%geop_sfc /= prof2%Lev2c%geop_sfc" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%Lev2c%press_sfc - prof2%Lev2c%press_sfc) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%Lev2c%press_sfc /= prof2%Lev2c%press_sfc" )
      idiff = idiff + 1
    ENDIF

    IF (ABS(prof1%Lev2c%press_sfc_qual - prof2%Lev2c%press_sfc_qual) > 1.0e-6_wp) THEN
      CALL message ( msg_error, "prof1%Lev2c%press_sfc_qual /= prof2%Lev2c%press_sfc_qual" )
      idiff = idiff + 1
    ENDIF

! Don't bother checking the TPHs for now.

  ENDIF ! Lev2c


  IF (Lev2d) THEN

    IF (ABS(prof1%Lev2d%Npoints - prof2%Lev2d%Npoints) > 0) THEN
      CALL message ( msg_error, "prof1%Lev2d%Npoints /= prof2%Lev2d%Npoints" )
      idiff = idiff + 1
    ENDIF

    IF (prof1%Lev2d%level_type /= prof2%Lev2d%level_type) THEN
      CALL message ( msg_error, "prof1%Lev2d%level_type /= prof2%Lev2d%level_type" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2d%level_coeff_a - prof2%Lev2d%level_coeff_a) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2d%level_coeff_a /= prof2%Lev2d%level_coeff_a" )
      idiff = idiff + 1
    ENDIF

    IF (ANY(ABS(prof1%Lev2d%level_coeff_b - prof2%Lev2d%level_coeff_b) > 1.0e-6_wp)) THEN
      CALL message ( msg_error, "prof1%Lev2d%level_coeff_b /= prof2%Lev2d%level_coeff_b" )
      idiff = idiff + 1
    ENDIF

  ENDIF ! Lev2d


! 4. Summarise the results
! ------------------------

  IF (idiff > 0) THEN
    WRITE (sdiff, "(i4)") idiff
    CALL message ( msg_info, sdiff // " elements of " // &
                             TRIM(ADJUSTL(file_in1)) // " and " // &
                             TRIM(ADJUSTL(file_in2)) // " differ significantly" )
                             
    CALL pass_fail(.false., test_name)
  ELSE
    CALL message ( msg_info, "No significant differences between " // &
                             TRIM(ADJUSTL(file_in1)) // " and " // &
                             TRIM(ADJUSTL(file_in2)) )
    CALL pass_fail(.true., test_name)
  ENDIF


CONTAINS


  SUBROUTINE pass_fail(pass, test_name)

    CHARACTER(LEN=256), INTENT(IN)  :: test_name
    LOGICAL,            INTENT(IN)  :: pass
    CHARACTER(LEN=256)              :: test_name2

    test_name2 = ADJUSTL(test_name)
    
    IF (pass) THEN
      PRINT*, ' '
      PRINT*, '******************************'
      PRINT*, '*** ' // test_name2(1:11) // ' test: PASS' // ' ***'
      PRINT*, '******************************'
      PRINT*, ' '
    ELSE
      PRINT*, ' '
      PRINT*, '******************************'
      PRINT*, '*** ' // test_name2(1:11) // ' test: FAIL' // ' ***'
      PRINT*, '******************************'
      PRINT*, ' '
    ENDIF

  END SUBROUTINE pass_fail


END PROGRAM nc_diff
