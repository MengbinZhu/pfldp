! $Id: ropp_io_read_ncdf_get.f90 3831 2013-08-27 15:22:48Z idculv $

!****is* Reading/ropp_io_read_ncdf_get *
!
! NAME
!    ropp_io_read_ncdf_get - Get data from an (already defined) netCDF.
!
! SYNOPSIS
!    call ropp_io_read_ncdf_get(data, rec)
!
! DESCRIPTION
!    This subroutine gets variables from an already openend / netCDF data
!    file.
!    Reads core parameters from V1.0 format version and new parameters
!    added in V1.1. Additional variables contained in the netCDF file are
!    read in and define elements of the Vlist structures in ROprof.
!
! NOTES
!    A netCDF file must have been opened before; this subroutine only works
!    on the current netcdf file. The netCDF data file is left open.
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
! 1. Core RO data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_rodata(DATA, rec)

! 1.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_ncdf_get_rodata
  USE ropp_io_types, ONLY: ROprof, &
                           ThisFmtVer

  IMPLICIT NONE

  TYPE(ROprof), INTENT(inout)  :: DATA
  INTEGER,      OPTIONAL       :: rec

  INTEGER                      :: n, ierr
  INTEGER                      :: irec

  CHARACTER(len =  23)         :: proc_date

  REAL(wp)                     :: time
  REAL                         :: fmtver

  INTEGER :: status, varid, ndim, TYPE
  CHARACTER(len = 256) :: routine

! 1.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_read_ncdf_get')

! 1.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 1.4 (Global) Attributes
! ------------------------

  data%FmtVersion = ' '        ; CALL ncdf_getatt('format_version',    data%FmtVersion)
  READ ( data%FmtVersion(11:), fmt=*, iostat=ierr ) fmtver
  IF ( ierr /= 0 )   data%FmtVersion = ThisFmtVer
  data%processing_centre = ' ' ; CALL ncdf_getatt('processing_centre', data%processing_centre)
  proc_date = ' '              ; CALL ncdf_getatt('processing_date',   proc_date)
  data%pod_method = ' '        ; CALL ncdf_getatt('pod_method',        data%pod_method)
  data%phase_method = ' '      ; CALL ncdf_getatt('phase_method',      data%phase_method)
  data%bangle_method = ' '     ; CALL ncdf_getatt('bangle_method',     data%bangle_method)
  data%refrac_method = ' '     ; CALL ncdf_getatt('refrac_method',     data%refrac_method)
  data%meteo_method = ' '      ; CALL ncdf_getatt('meteo_method',      data%meteo_method)
IF(ncdf_isatt('thin_method'))THEN  ! added at V1.1
  data%thin_method = ' '       ; CALL ncdf_getatt('thin_method',       data%thin_method)
ENDIF
  data%software_version = ' '  ; CALL ncdf_getatt('software_version',  data%software_version)

  IF (proc_date( 1: 4) /= '    ') READ(proc_date( 1: 4), *) data%DTpro%Year
  IF (proc_date( 6: 7) /=   '  ') READ(proc_date( 6: 7), *) data%DTpro%Month
  IF (proc_date( 9:10) /=   '  ') READ(proc_date( 9:10), *) data%DTpro%Day
  IF (proc_date(12:13) /=   '  ') READ(proc_date(12:13), *) data%DTpro%Hour
  IF (proc_date(15:16) /=   '  ') READ(proc_date(15:16), *) data%DTpro%Minute
  IF (proc_date(18:19) /=   '  ') READ(proc_date(18:19), *) data%DTpro%Second
  IF (proc_date(21:23) /=  '   ') READ(proc_date(21:23), *) data%DTpro%Msec

! 1.5 Header variables
! --------------------

  CALL ncdf_getvar('occ_id', data%occ_id, rec = irec)
  CALL ncdf_getvar('gns_id', data%gns_id, rec = irec)
  CALL ncdf_getvar('leo_id', data%leo_id, rec = irec)
  CALL ncdf_getvar('stn_id', data%stn_id, rec = irec)

! 1.6 Date and time
! -----------------

  CALL ncdf_getvar('start_time', time, rec=irec)
  CALL ncdf_getvar('year',   data%DTocc%Year,         &
                     units = data%DTocc%units%Year,   &
                     range = data%DTocc%range%Year,   &
                     rec   = irec)
  CALL ncdf_getvar('month',  data%DTocc%Month,        &
                     units = data%DTocc%units%Month,  &
                     range = data%DTocc%range%Month,  &
                     rec   = irec)
  CALL ncdf_getvar('day',    data%DTocc%Day,          &
                     units = data%DTocc%units%Day,    &
                     range = data%DTocc%range%Day,    &
                     rec   = irec)
  CALL ncdf_getvar('hour',   data%DTocc%Hour,         &
                     units = data%DTocc%units%Hour,   &
                     range = data%DTocc%range%Hour,   &
                     rec   = irec)
  CALL ncdf_getvar('minute', data%DTocc%Minute,       &
                     units = data%DTocc%units%Minute, &
                     range = data%DTocc%range%Minute, &
                     rec   = irec)
  CALL ncdf_getvar('second', data%DTocc%Second,       &
                     units = data%DTocc%units%Second, &
                     range = data%DTocc%range%Second, &
                     rec   = irec)
  CALL ncdf_getvar('msec',   data%DTocc%Msec,         &
                     units = data%DTocc%units%Msec,   &
                     range = data%DTocc%range%Msec,   &
                     rec   = irec)

! 1.7 Overall quality
! -------------------

  CALL ncdf_getvar('pcd',          data%pcd,                &
                           units = data%units%pcd,          &
                           range = data%range%pcd,          &
                           rec   = irec)
  CALL ncdf_getvar('overall_qual', data%overall_qual,       &
                           units = data%units%overall_qual, &
                           range = data%range%overall_qual, &
                           rec   = irec)

! 1.8 Georeferencing
! ------------------

  CALL ncdf_getvar('time', time, rec=irec)
  CALL ncdf_getvar('lat',         data%georef%lat,               &
                          units = data%georef%units%lat,         &
                          range = data%georef%range%lat,         &
                          rec   = irec)
  CALL ncdf_getvar('lon',         data%georef%lon,               &
                          units = data%georef%units%lon,         &
                          range = data%georef%range%lon,         &
                          rec   = irec)
  CALL ncdf_getvar('time_offset', data%georef%time_offset,       &
                          units = data%georef%units%time_offset, &
                          range = data%georef%range%time_offset, &
                          rec   = irec)
  CALL ncdf_getvar('undulation',  data%georef%Undulation,        &
                          units = data%georef%units%Undulation,  &
                          range = data%georef%range%undulation,  &
                          rec = irec)
  CALL ncdf_getvar('roc',         data%georef%roc,               &
                          units = data%georef%units%roc,         &
                          range = data%georef%range%roc,         &
                          rec   = irec)
  CALL ncdf_getvar('r_coc',       data%georef%r_coc,             &
                          units = data%georef%units%r_coc,       &
                          range = data%georef%range%r_coc,       &
                          rec   = irec)
  CALL ncdf_getvar('azimuth',     data%georef%azimuth,           &
                          units = data%georef%units%azimuth,     &
                          range = data%georef%range%azimuth,     &
                          rec   = irec)

! 1.8.1 Other attributes

  CALL ncdf_getatt('reference_frame', data%georef%reference_frame%r_coc, varname= 'r_coc')

! 1.9 Background characterisation (if any)
! ----------------------------------------

  IF (ncdf_isvar('bg_source')) THEN
    data%BG%Source = 'TBD'
  ELSE
    data%BG%Source = 'NONE'
  ENDIF

  IF (data%BG%Source /= 'NONE') THEN
    CALL ncdf_getvar('bg_source',   data%BG%Source, rec = irec)
    CALL ncdf_getvar('bg_year',     data%BG%Year,           &
                            units = data%BG%units%Year,     &
                            range = data%BG%range%Year,     &
                            rec   = irec)
    CALL ncdf_getvar('bg_month',    data%BG%Month,          &
                            units = data%BG%units%Month,    &
                            range = data%BG%range%Month,    &
                            rec   = irec)
    CALL ncdf_getvar('bg_day',      data%BG%Day,            &
                            units = data%BG%units%Day,      &
                            range = data%BG%range%Day,      &
                            rec   = irec)
    CALL ncdf_getvar('bg_hour',     data%BG%Hour,           &
                            units = data%BG%units%Hour,     &
                            range = data%BG%range%Hour,     &
                            rec   = irec)
    CALL ncdf_getvar('bg_minute',   data%BG%Minute,         &
                            units = data%BG%units%Minute,   &
                            range = data%BG%range%Minute,   &
                            rec   = irec)
    CALL ncdf_getvar('bg_fcperiod', data%BG%fcPeriod,       &
                            units = data%BG%units%fcPeriod, &
                            range = data%BG%range%fcPeriod, &
                            rec   = irec)
  ENDIF

! 1.10 Level1a variables (if any)
! ------------------------------

  IF (ncdf_isvar('dtime')) THEN
    CALL ncdf_getsize('dtime', n, dim = 1)
    CALL ropp_io_init(data%Lev1a, n)
  ELSE
    data%Lev1a%Npoints = 0
  ENDIF

  IF (data%Lev1a%Npoints > 0) THEN
    CALL ncdf_getvar('dtime',      data%Lev1a%dtime,            &
                           units = data%Lev1a%units%dtime,      &
                           range = data%Lev1a%range%dtime,      &
                           rec   = irec)
    CALL ncdf_getvar('snr_L1ca',   data%Lev1a%snr_L1ca,         &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                           rec   = irec)
    CALL ncdf_getvar('snr_L1p',    data%Lev1a%snr_L1p,          &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                            rec   = irec)
    CALL ncdf_getvar('snr_L2p',    data%Lev1a%snr_L2p,          &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                           rec   = irec)
    CALL ncdf_getvar('phase_L1',   data%Lev1a%phase_L1,         &
                           units = data%Lev1a%units%phase,      &
                           range = data%Lev1a%range%phase,      &
                           rec   = irec)
    CALL ncdf_getvar('phase_L2',   data%Lev1a%phase_L2,         &
                           units = data%Lev1a%units%phase,      &
                           range = data%Lev1a%range%phase,      &
                           rec   = irec)
    CALL ncdf_getvar('r_gns',      data%Lev1a%r_gns,            &
                           units = data%Lev1a%units%r_gns,      &
                           range = data%Lev1a%range%r_gns,      &
                           rec   = irec)
    CALL ncdf_getvar('v_gns',      data%Lev1a%v_gns,            &
                           units = data%Lev1a%units%v_gns,      &
                           range = data%Lev1a%range%v_gns,      &
                           rec   = irec)
    CALL ncdf_getvar('r_leo',      data%Lev1a%r_leo,            &
                           units = data%Lev1a%units%r_leo,      &
                           range = data%Lev1a%range%r_leo,      &
                           rec   = irec)
    CALL ncdf_getvar('v_leo',      data%Lev1a%v_leo,            &
                           units = data%Lev1a%units%v_leo,      &
                           range = data%Lev1a%range%v_leo,      &
                           rec   = irec)
    CALL ncdf_getvar('phase_qual', data%Lev1a%phase_qual,       &
                           units = data%Lev1a%units%phase_qual, &
                           range = data%Lev1a%range%phase_qual, &
                           rec   = irec)
! 1.10.1 Other attributes

    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%r_gns, varname = 'r_gns')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%v_gns, varname = 'v_gns')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%r_leo, varname = 'r_leo')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%v_leo, varname = 'v_leo')

    data%Lev1a%Missing = .FALSE.
  ENDIF

! 1.11 Level1b variables (if any)
! -------------------------------

  IF (ncdf_isvar('lat_tp')) THEN
    CALL ncdf_getsize('lat_tp', n, dim = 1)
    CALL ropp_io_init(data%Lev1b, n)
  ELSE
    data%Lev1b%Npoints = 0
  ENDIF

  IF (data%Lev1b%Npoints > 0) THEN

    CALL ncdf_getvar('lat_tp',           data%Lev1b%lat_tp,             &
                                 units = data%Lev1b%units%lat_tp,       &
                                 range = data%Lev1b%range%lat_tp,       &
                                 rec   = irec)
    CALL ncdf_getvar('lon_tp',           data%Lev1b%lon_tp,             &
                                 units = data%Lev1b%units%lon_tp,       &
                                 range = data%Lev1b%range%lon_tp,       &
                                 rec   = irec)
    CALL ncdf_getvar('azimuth_tp',       data%Lev1b%azimuth_tp,         &
                                 units = data%Lev1b%units%azimuth_tp,   &
                                 range = data%Lev1b%range%azimuth_tp,   &
                                 rec   = irec)

    CALL ncdf_getvar('impact_L1',        data%Lev1b%impact_L1,          &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
    CALL ncdf_getvar('impact_L2',        data%Lev1b%impact_L2,          &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
    CALL ncdf_getvar('impact',           data%Lev1b%impact,             &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
  IF (ncdf_isvar('impact_opt')) & ! added at v1.1
    CALL ncdf_getvar('impact_opt',       data%Lev1b%impact_opt,         &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1',        data%Lev1b%bangle_L1,          &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2',        data%Lev1b%bangle_L2,          &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
    CALL ncdf_getvar('bangle',           data%Lev1b%bangle,             &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt',       data%Lev1b%bangle_opt,         &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1_sigma',  data%Lev1b%bangle_L1_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2_sigma',  data%Lev1b%bangle_L2_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_sigma',     data%Lev1b%bangle_sigma,       &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt_sigma')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt_sigma', data%Lev1b%bangle_opt_sigma,   &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1_qual',   data%Lev1b%bangle_L1_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2_qual',   data%Lev1b%bangle_L2_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_qual',      data%Lev1b%bangle_qual,        &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt_qual')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt_qual',  data%Lev1b%bangle_opt_qual,    &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
    data%Lev1b%Missing = .FALSE.
  ENDIF

! 1.11 Level2a variables (if any)
! -------------------------------

  IF (ncdf_isvar('alt_refrac')) THEN
    CALL ncdf_getsize('alt_refrac', n, dim = 1)
    CALL ropp_io_init(data%Lev2a, n)
  ELSE
    data%Lev2a%Npoints = 0
  ENDIF

  IF (data%Lev2a%Npoints > 0) THEN

    CALL ncdf_getvar('alt_refrac',   data%Lev2a%alt_refrac,         &
                             units = data%Lev2a%units%alt_refrac,   &
                             range = data%Lev2a%range%alt_refrac,   &
                             rec   = irec)
    CALL ncdf_getvar('geop_refrac',  data%Lev2a%geop_refrac,        &
                             units = data%Lev2a%units%geop_refrac,  &
                             range = data%Lev2a%range%geop_refrac,  &
                             rec   = irec)
    CALL ncdf_getvar('refrac',       data%Lev2a%refrac,             &
                             units = data%Lev2a%units%refrac,       &
                             range = data%Lev2a%range%refrac,       &
                             rec   = irec)
    CALL ncdf_getvar('refrac_sigma', data%Lev2a%refrac_sigma,       &
                             units = data%Lev2a%units%refrac_sigma, &
                             range = data%Lev2a%range%refrac_sigma, &
                             rec   = irec)
    CALL ncdf_getvar('refrac_qual',  data%Lev2a%refrac_qual,        &
                             units = data%Lev2a%units%refrac_qual,  &
                             range = data%Lev2a%range%refrac_qual,  &
                             rec   = irec)
    IF (ncdf_isvar('dry_temp')) THEN  !For backward compatibility
      CALL ncdf_getvar('dry_temp',       data%Lev2a%dry_temp,             &
                                 units = data%Lev2a%units%dry_temp,       &
                                 range = data%Lev2a%range%dry_temp,       &
                                 rec   = irec)
      CALL ncdf_getvar('dry_temp_sigma', data%Lev2a%dry_temp_sigma,       &
                                 units = data%Lev2a%units%dry_temp_sigma, &
                                 range = data%Lev2a%range%dry_temp_sigma, &
                                 rec   = irec)
      CALL ncdf_getvar('dry_temp_qual',  data%Lev2a%dry_temp_qual,        &
                                 units = data%Lev2a%units%dry_temp_qual,  &
                                 range = data%Lev2a%range%dry_temp_qual,  &
                                 rec   = irec)
    ENDIF
    data%Lev2a%Missing = .FALSE.
  ENDIF

! 1.13 Level2b variables (if any)
! -------------------------------

  IF (ncdf_isvar('geop')) THEN
    CALL ncdf_getsize('geop', n, dim = 1)
    CALL ropp_io_init(data%Lev2b, n)
  ELSE
    data%Lev2b%Npoints = 0
  ENDIF

  IF (data%Lev2b%Npoints > 0) THEN

    CALL ncdf_getvar('geop',        data%Lev2b%geop,              &
                            units = data%Lev2b%units%geop,        &
                            range = data%Lev2b%range%geop,        &
                            rec   = irec)
    CALL ncdf_getvar('geop_sigma',  data%Lev2b%geop_sigma,        &
                            units = data%Lev2b%units%geop_sigma,  &
                            range = data%Lev2b%range%geop_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('press',       data%Lev2b%press,             &
                            units = data%Lev2b%units%press,       &
                            range = data%Lev2b%range%press,       &
                            rec   = irec)
    CALL ncdf_getvar('press_sigma', data%Lev2b%press_sigma,       &
                            units = data%Lev2b%units%press_sigma, &
                            range = data%Lev2b%range%press_sigma, &
                            rec   = irec)
    CALL ncdf_getvar('temp',        data%Lev2b%temp,              &
                            units = data%Lev2b%units%temp,        &
                            range = data%Lev2b%range%temp,        &
                            rec   = irec)
    CALL ncdf_getvar('temp_sigma',  data%Lev2b%temp_sigma,        &
                            units = data%Lev2b%units%temp_sigma,  &
                            range = data%Lev2b%range%temp_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('shum',        data%Lev2b%shum,              &
                            units = data%Lev2b%units%shum,        &
                            range = data%Lev2b%range%shum,        &
                            rec   = irec)
    CALL ncdf_getvar('shum_sigma',  data%Lev2b%shum_sigma,        &
                            units = data%Lev2b%units%shum_sigma,  &
                            range = data%Lev2b%range%shum_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('meteo_qual',  data%Lev2b%meteo_qual,        &
                            units = data%Lev2b%units%meteo_qual,  &
                            range = data%Lev2b%range%meteo_qual,  &
                            rec   = irec)

    data%Lev2b%Missing = .FALSE.
  ENDIF

! 1.14 Level2c variables (if any)
! -------------------------------

  IF (ncdf_isvar('geop_sfc')) THEN
    data%Lev2c%Npoints = 1
  ELSE
    data%Lev2c%Npoints = 0
  ENDIF

  IF (data%Lev2c%Npoints > 0) THEN

    CALL ncdf_getvar('geop_sfc',        data%Lev2c%geop_sfc,              &
                                units = data%Lev2c%units%geop_sfc,        &
                                range = data%Lev2c%range%geop_sfc,        &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc',       data%Lev2c%press_sfc,             &
                                units = data%Lev2c%units%press_sfc,       &
                                range = data%Lev2c%range%press_sfc,       &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc_sigma', data%Lev2c%press_sfc_sigma,       &
                                units = data%Lev2c%units%press_sfc_sigma, &
                                range = data%Lev2c%range%press_sfc_sigma, &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc_qual',  data%Lev2c%press_sfc_qual,        &
                                units = data%Lev2c%units%press_sfc_qual,  &
                                range = data%Lev2c%range%press_sfc_qual,  &
                                rec   = irec)

    IF (ncdf_isvar('tph_bangle')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_bangle',      data%Lev2c%tph_bangle,            &
                                  units = data%Lev2c%units%tph_bangle,      &
                                  range = data%Lev2c%range%tph_bangle,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpa_bangle')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpa_bangle',      data%Lev2c%tpa_bangle,            &
                                  units = data%Lev2c%units%tpa_bangle,      &
                                  range = data%Lev2c%range%tpa_bangle,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_bangle_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_bangle_flag', data%Lev2c%tph_bangle_flag,       &
                                  units = data%Lev2c%units%tph_bangle_flag, &
                                  range = data%Lev2c%range%tph_bangle_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('tph_refrac')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_refrac',      data%Lev2c%tph_refrac,            &
                                  units = data%Lev2c%units%tph_refrac,      &
                                  range = data%Lev2c%range%tph_refrac,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpn_refrac')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpn_refrac',      data%Lev2c%tpn_refrac,            &
                                  units = data%Lev2c%units%tpn_refrac,      &
                                  range = data%Lev2c%range%tpn_refrac,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_refrac_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_refrac_flag', data%Lev2c%tph_refrac_flag,       &
                                  units = data%Lev2c%units%tph_refrac_flag, &
                                  range = data%Lev2c%range%tph_refrac_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('tph_tdry_lrt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_tdry_lrt',        data%Lev2c%tph_tdry_lrt,        &
                                  units = data%Lev2c%units%tph_tdry_lrt,      &
                                  range = data%Lev2c%range%tph_tdry_lrt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpt_tdry_lrt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpt_tdry_lrt',        data%Lev2c%tpt_tdry_lrt,        &
                                  units = data%Lev2c%units%tpt_tdry_lrt,      &
                                  range = data%Lev2c%range%tpt_tdry_lrt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_tdry_lrt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_tdry_lrt_flag',   data%Lev2c%tph_tdry_lrt_flag,   &
                                  units = data%Lev2c%units%tph_tdry_lrt_flag, &
                                  range = data%Lev2c%range%tph_tdry_lrt_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('tph_tdry_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_tdry_cpt',        data%Lev2c%tph_tdry_cpt,        &
                                  units = data%Lev2c%units%tph_tdry_cpt,      &
                                  range = data%Lev2c%range%tph_tdry_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpt_tdry_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpt_tdry_cpt',        data%Lev2c%tpt_tdry_cpt,        &
                                  units = data%Lev2c%units%tpt_tdry_cpt,      &
                                  range = data%Lev2c%range%tpt_tdry_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_tdry_cpt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_tdry_cpt_flag',   data%Lev2c%tph_tdry_cpt_flag,   &
                                  units = data%Lev2c%units%tph_tdry_cpt_flag, &
                                  range = data%Lev2c%range%tph_tdry_cpt_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('prh_tdry_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('prh_tdry_cpt',        data%Lev2c%prh_tdry_cpt,        &
                                  units = data%Lev2c%units%prh_tdry_cpt,      &
                                  range = data%Lev2c%range%prh_tdry_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('prt_tdry_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('prt_tdry_cpt',        data%Lev2c%prt_tdry_cpt,        &
                                  units = data%Lev2c%units%prt_tdry_cpt,      &
                                  range = data%Lev2c%range%prt_tdry_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('prh_tdry_cpt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('prh_tdry_cpt_flag',   data%Lev2c%prh_tdry_cpt_flag,   &
                                  units = data%Lev2c%units%prh_tdry_cpt_flag, &
                                  range = data%Lev2c%range%prh_tdry_cpt_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('tph_temp_lrt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_temp_lrt',        data%Lev2c%tph_temp_lrt,        &
                                  units = data%Lev2c%units%tph_temp_lrt,      &
                                  range = data%Lev2c%range%tph_temp_lrt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpt_temp_lrt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpt_temp_lrt',        data%Lev2c%tpt_temp_lrt,        &
                                  units = data%Lev2c%units%tpt_temp_lrt,      &
                                  range = data%Lev2c%range%tpt_temp_lrt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_temp_lrt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_temp_lrt_flag',   data%Lev2c%tph_temp_lrt_flag,   &
                                  units = data%Lev2c%units%tph_temp_lrt_flag, &
                                  range = data%Lev2c%range%tph_temp_lrt_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('tph_temp_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_temp_cpt',        data%Lev2c%tph_temp_cpt,        &
                                  units = data%Lev2c%units%tph_temp_cpt,      &
                                  range = data%Lev2c%range%tph_temp_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tpt_temp_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('tpt_temp_cpt',        data%Lev2c%tpt_temp_cpt,        &
                                  units = data%Lev2c%units%tpt_temp_cpt,      &
                                  range = data%Lev2c%range%tpt_temp_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('tph_temp_cpt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('tph_temp_cpt_flag',   data%Lev2c%tph_temp_cpt_flag,   &
                                  units = data%Lev2c%units%tph_temp_cpt_flag, &
                                  range = data%Lev2c%range%tph_temp_cpt_flag, &
                                  rec   = irec)
    ENDIF

    IF (ncdf_isvar('prh_temp_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('prh_temp_cpt',        data%Lev2c%prh_temp_cpt,        &
                                  units = data%Lev2c%units%prh_temp_cpt,      &
                                  range = data%Lev2c%range%prh_temp_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('prt_temp_cpt')) THEN  !For backward compatibility
      CALL ncdf_getvar('prt_temp_cpt',        data%Lev2c%prt_temp_cpt,        &
                                  units = data%Lev2c%units%prt_temp_cpt,      &
                                  range = data%Lev2c%range%prt_temp_cpt,      &
                                  rec   = irec)
    ENDIF
    IF (ncdf_isvar('prh_temp_cpt_flag')) THEN  !For backward compatibility
      CALL ncdf_getvar('prh_temp_cpt_flag',   data%Lev2c%prh_temp_cpt_flag,   &
                                  units = data%Lev2c%units%prh_temp_cpt_flag, &
                                  range = data%Lev2c%range%prh_temp_cpt_flag, &
                                  rec   = irec)
    ENDIF
    
    data%Lev2c%Missing = .FALSE.

  ENDIF

! 1.15 Level2d variables (if any)
! -------------------------------

  IF (ncdf_isvar('level_coeff_a')) THEN
    CALL ncdf_getsize('level_coeff_a', n, dim = 1)
    CALL ropp_io_init(data%Lev2d, n)
  ELSE
    data%Lev2d%Npoints = 0
  ENDIF

  IF (data%Lev2d%Npoints > 0) THEN

    CALL ncdf_getvar('level_type',    data%Lev2d%level_type,          &
                              rec   = irec)
    CALL ncdf_getvar('level_coeff_a', data%Lev2d%level_coeff_a,       &
                              units = data%Lev2d%units%level_coeff_a, &
                              range = data%Lev2d%range%level_coeff_a, &
                              rec   = irec)
    CALL ncdf_getvar('level_coeff_b', data%Lev2d%level_coeff_b,       &
                              units = data%Lev2d%units%level_coeff_b, &
                              range = data%Lev2d%range%level_coeff_b, &
                              rec   = irec)

    data%Lev2d%Missing = .FALSE.
  ENDIF

! 1.16 Additional variables (if any)
! ----------------------------------

  CALL ropp_io_init(data%vlist)

  DO varid=1,ncdf_nvars

    IF(.NOT. ncdf_read(varid))THEN

       status = nf90_inquire_variable(ncdf_ncid, varid, xtype=TYPE, ndims=ndim)

       IF (TYPE .NE. NF90_CHAR) THEN      ! only read scalar variables

          IF(ndim == 1)THEN
             CALL ropp_io_read_ncdf_get_vlistD0d(varid, data%vlist%VlistD0d, irec)
          ENDIF
          IF(ndim == 2)THEN
             CALL ropp_io_read_ncdf_get_vlistD1d(varid, data%vlist%VlistD1d, irec)
          ENDIF
          IF(ndim == 3)THEN
             CALL ropp_io_read_ncdf_get_vlistD2d(varid, data%vlist%VlistD2d, irec)
          ENDIF
       ENDIF

    ENDIF

    ncdf_read(varid) = .FALSE.           ! reset 'read variable' flag
  ENDDO

! 1.17 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_read_ncdf_get_rodata


!-------------------------------------------------------------------------------
! 2. Core RO data (two-dimensional meteorological data)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_rodata_2d(DATA, rec)

! 2.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_ncdf_get_rodata_2d
  USE ropp_io_types, ONLY: ROprof2d, &
                           ThisFmtVer

  IMPLICIT NONE

  TYPE(ROprof2d), INTENT(inout) :: DATA
  INTEGER,      OPTIONAL       :: rec

  INTEGER                      :: n, ierr
  INTEGER                      :: irec
  INTEGER, DIMENSION(2)        :: n2d

  CHARACTER(len =  23)         :: proc_date
  CHARACTER(len = 256)         :: routine

  REAL(wp)                     :: time
  REAL                         :: fmtver

  INTEGER :: status, varid, ndim, TYPE

! 2.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_read_ncdf_get')

! 2.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 2.4 (Global) Attributes
! ------------------------

  data%FmtVersion = ' '        ; CALL ncdf_getatt('format_version',    data%FmtVersion)
  READ ( data%FmtVersion(11:), fmt=*, iostat=ierr ) fmtver
  IF ( ierr /= 0 )   data%FmtVersion = ThisFmtVer
  data%processing_centre = ' ' ; CALL ncdf_getatt('processing_centre', data%processing_centre)
  proc_date = ' '              ; CALL ncdf_getatt('processing_date',   proc_date)
  data%pod_method = ' '        ; CALL ncdf_getatt('pod_method',        data%pod_method)
  data%phase_method = ' '      ; CALL ncdf_getatt('phase_method',      data%phase_method)
  data%bangle_method = ' '     ; CALL ncdf_getatt('bangle_method',     data%bangle_method)
  data%refrac_method = ' '     ; CALL ncdf_getatt('refrac_method',     data%refrac_method)
  data%meteo_method = ' '      ; CALL ncdf_getatt('meteo_method',      data%meteo_method)
IF(ncdf_isatt('thin_method'))THEN  ! added at V1.1
  data%thin_method = ' '       ; CALL ncdf_getatt('thin_method',       data%thin_method)
ENDIF
  data%software_version = ' '  ; CALL ncdf_getatt('software_version',  data%software_version)

  IF (proc_date( 1: 4) /= '    ') READ(proc_date( 1: 4), *) data%DTpro%Year
  IF (proc_date( 6: 7) /=   '  ') READ(proc_date( 6: 7), *) data%DTpro%Month
  IF (proc_date( 9:10) /=   '  ') READ(proc_date( 9:10), *) data%DTpro%Day
  IF (proc_date(12:13) /=   '  ') READ(proc_date(12:13), *) data%DTpro%Hour
  IF (proc_date(15:16) /=   '  ') READ(proc_date(15:16), *) data%DTpro%Minute
  IF (proc_date(18:19) /=   '  ') READ(proc_date(18:19), *) data%DTpro%Second
  IF (proc_date(21:23) /=  '   ') READ(proc_date(21:23), *) data%DTpro%Msec

! 2.5 Header variables
! --------------------

  CALL ncdf_getvar('occ_id', data%occ_id, rec = irec)
  CALL ncdf_getvar('gns_id', data%gns_id, rec = irec)
  CALL ncdf_getvar('leo_id', data%leo_id, rec = irec)
  CALL ncdf_getvar('stn_id', data%stn_id, rec = irec)

! 2.6 Date and time
! -----------------

  CALL ncdf_getvar('start_time', time, rec=irec)
  CALL ncdf_getvar('year',   data%DTocc%Year,         &
                     units = data%DTocc%units%Year,   &
                     range = data%DTocc%range%Year,   &
                     rec   = irec)
  CALL ncdf_getvar('month',  data%DTocc%Month,        &
                     units = data%DTocc%units%Month,  &
                     range = data%DTocc%range%Month,  &
                     rec   = irec)
  CALL ncdf_getvar('day',    data%DTocc%Day,          &
                     units = data%DTocc%units%Day,    &
                     range = data%DTocc%range%Day,    &
                     rec   = irec)
  CALL ncdf_getvar('hour',   data%DTocc%Hour,         &
                     units = data%DTocc%units%Hour,   &
                     range = data%DTocc%range%Hour,   &
                     rec   = irec)
  CALL ncdf_getvar('minute', data%DTocc%Minute,       &
                     units = data%DTocc%units%Minute, &
                     range = data%DTocc%range%Minute, &
                     rec   = irec)
  CALL ncdf_getvar('second', data%DTocc%Second,       &
                     units = data%DTocc%units%Second, &
                     range = data%DTocc%range%Second, &
                     rec   = irec)
  CALL ncdf_getvar('msec',   data%DTocc%Msec,         &
                     units = data%DTocc%units%Msec,   &
                     range = data%DTocc%range%Msec,   &
                     rec   = irec)

! 2.7 Overall quality
! -------------------

  CALL ncdf_getvar('pcd',          data%pcd,                &
                           units = data%units%pcd,          &
                           range = data%range%pcd,          &
                           rec   = irec)
  CALL ncdf_getvar('overall_qual', data%overall_qual,       &
                           units = data%units%overall_qual, &
                           range = data%range%overall_qual, &
                           rec   = irec)

! 2.8 Georeferencing
! ------------------

  CALL ncdf_getvar('time', time, rec=irec)
  CALL ncdf_getvar('lat',         data%georef%lat,               &
                          units = data%georef%units%lat,         &
                          range = data%georef%range%lat,         &
                          rec   = irec)
  CALL ncdf_getvar('lon',         data%georef%lon,               &
                          units = data%georef%units%lon,         &
                          range = data%georef%range%lon,         &
                          rec   = irec)
  CALL ncdf_getvar('time_offset', data%georef%time_offset,       &
                          units = data%georef%units%time_offset, &
                          range = data%georef%range%time_offset, &
                          rec   = irec)
  CALL ncdf_getvar('undulation',  data%georef%Undulation,        &
                          units = data%georef%units%Undulation,  &
                          range = data%georef%range%undulation,  &
                          rec = irec)
  CALL ncdf_getvar('roc',         data%georef%roc,               &
                          units = data%georef%units%roc,         &
                          range = data%georef%range%roc,         &
                          rec   = irec)
  CALL ncdf_getvar('r_coc',       data%georef%r_coc,             &
                          units = data%georef%units%r_coc,       &
                          range = data%georef%range%r_coc,       &
                          rec   = irec)
  CALL ncdf_getvar('azimuth',     data%georef%azimuth,           &
                          units = data%georef%units%azimuth,     &
                          range = data%georef%range%azimuth,     &
                          rec   = irec)

! 2.8.1 Other attributes

  CALL ncdf_getatt('reference_frame', data%georef%reference_frame%r_coc, varname= 'r_coc')

! 2.9 Background characterisation (if any)
! ----------------------------------------

  IF (ncdf_isvar('bg_source')) THEN
    data%BG%Source = 'TBD'
  ELSE
    data%BG%Source = 'NONE'
  ENDIF

  IF (data%BG%Source /= 'NONE') THEN
    CALL ncdf_getvar('bg_source',   data%BG%Source, rec = irec)
    CALL ncdf_getvar('bg_year',     data%BG%Year,           &
                            units = data%BG%units%Year,     &
                            range = data%BG%range%Year,     &
                            rec   = irec)
    CALL ncdf_getvar('bg_month',    data%BG%Month,          &
                            units = data%BG%units%Month,    &
                            range = data%BG%range%Month,    &
                            rec   = irec)
    CALL ncdf_getvar('bg_day',      data%BG%Day,            &
                            units = data%BG%units%Day,      &
                            range = data%BG%range%Day,      &
                            rec   = irec)
    CALL ncdf_getvar('bg_hour',     data%BG%Hour,           &
                            units = data%BG%units%Hour,     &
                            range = data%BG%range%Hour,     &
                            rec   = irec)
    CALL ncdf_getvar('bg_minute',   data%BG%Minute,         &
                            units = data%BG%units%Minute,   &
                            range = data%BG%range%Minute,   &
                            rec   = irec)
    CALL ncdf_getvar('bg_fcperiod', data%BG%fcPeriod,       &
                            units = data%BG%units%fcPeriod, &
                            range = data%BG%range%fcPeriod, &
                            rec   = irec)
  ENDIF

! 2.10 Level1a variables (if any)
! ------------------------------

  IF (ncdf_isvar('dtime')) THEN
    CALL ncdf_getsize('dtime', n, dim = 1)
    CALL ropp_io_init(data%Lev1a, n)
  ELSE
    data%Lev1a%Npoints = 0
  ENDIF

  IF (data%Lev1a%Npoints > 0) THEN

    CALL ncdf_getvar('dtime',      data%Lev1a%dtime,            &
                           units = data%Lev1a%units%dtime,      &
                           range = data%Lev1a%range%dtime,      &
                           rec   = irec)
    CALL ncdf_getvar('snr_L1ca',   data%Lev1a%snr_L1ca,         &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                           rec   = irec)
    CALL ncdf_getvar('snr_L1p',    data%Lev1a%snr_L1p,          &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                           rec   = irec)
    CALL ncdf_getvar('snr_L2p',    data%Lev1a%snr_L2p,          &
                           units = data%Lev1a%units%snr,        &
                           range = data%Lev1a%range%snr,        &
                           rec   = irec)
    CALL ncdf_getvar('phase_L1',   data%Lev1a%phase_L1,         &
                           units = data%Lev1a%units%phase,      &
                           range = data%Lev1a%range%phase,      &
                           rec   = irec)
    CALL ncdf_getvar('phase_L2',   data%Lev1a%phase_L2,         &
                           units = data%Lev1a%units%phase,      &
                           range = data%Lev1a%range%phase,      &
                           rec   = irec)
    CALL ncdf_getvar('r_gns',      data%Lev1a%r_gns,            &
                           units = data%Lev1a%units%r_gns,      &
                           range = data%Lev1a%range%r_gns,      &
                           rec   = irec)
    CALL ncdf_getvar('v_gns',      data%Lev1a%v_gns,            &
                           units = data%Lev1a%units%v_gns,      &
                           range = data%Lev1a%range%v_gns,      &
                           rec   = irec)
    CALL ncdf_getvar('r_leo',      data%Lev1a%r_leo,            &
                           units = data%Lev1a%units%r_leo,      &
                           range = data%Lev1a%range%r_leo,      &
                           rec   = irec)
    CALL ncdf_getvar('v_leo',      data%Lev1a%v_leo,            &
                           units = data%Lev1a%units%v_leo,      &
                           range = data%Lev1a%range%v_leo,      &
                           rec   = irec)
    CALL ncdf_getvar('phase_qual', data%Lev1a%phase_qual,       &
                           units = data%Lev1a%units%phase_qual, &
                           range = data%Lev1a%range%phase_qual, &
                           rec   = irec)

! 2.10.1 Other attributes

    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%r_gns, varname = 'r_gns')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%v_gns, varname = 'v_gns')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%r_leo, varname = 'r_leo')
    CALL ncdf_getatt('reference_frame', data%Lev1a%reference_frame%v_leo, varname = 'v_leo')

  ENDIF

! 2.11 Level1b variables (if any)
! -------------------------------

  IF (ncdf_isvar('lat_tp')) THEN
    CALL ncdf_getsize('lat_tp', n, dim = 1)
    CALL ropp_io_init(data%Lev1b, n)
  ELSE
    data%Lev1b%Npoints = 0
  ENDIF

  IF (data%Lev1b%Npoints > 0) THEN

    CALL ncdf_getvar('lat_tp',           data%Lev1b%lat_tp,             &
                                 units = data%Lev1b%units%lat_tp,       &
                                 range = data%Lev1b%range%lat_tp,       &
                                 rec   = irec)
    CALL ncdf_getvar('lon_tp',           data%Lev1b%lon_tp,             &
                                 units = data%Lev1b%units%lon_tp,       &
                                 range = data%Lev1b%range%lon_tp,       &
                                 rec   = irec)
    CALL ncdf_getvar('azimuth_tp',       data%Lev1b%azimuth_tp,         &
                                 units = data%Lev1b%units%azimuth_tp,   &
                                 range = data%Lev1b%range%azimuth_tp,   &
                                 rec   = irec)

    CALL ncdf_getvar('impact_L1',        data%Lev1b%impact_L1,          &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
    CALL ncdf_getvar('impact_L2',        data%Lev1b%impact_L2,          &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
    CALL ncdf_getvar('impact',           data%Lev1b%impact,             &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)
  IF (ncdf_isvar('impact_opt')) & ! added at v1.1
    CALL ncdf_getvar('impact_opt',       data%Lev1b%impact_opt,         &
                                 units = data%Lev1b%units%impact,       &
                                 range = data%Lev1b%range%impact,       &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1',        data%Lev1b%bangle_L1,          &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2',        data%Lev1b%bangle_L2,          &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
    CALL ncdf_getvar('bangle',           data%Lev1b%bangle,             &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt',       data%Lev1b%bangle_opt,         &
                                 units = data%Lev1b%units%bangle,       &
                                 range = data%Lev1b%range%bangle,       &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1_sigma',  data%Lev1b%bangle_L1_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2_sigma',  data%Lev1b%bangle_L2_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_sigma',     data%Lev1b%bangle_sigma,       &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt_sigma')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt_sigma', data%Lev1b%bangle_opt_sigma,   &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 range = data%Lev1b%range%bangle_sigma, &
                                 rec   = irec)

    CALL ncdf_getvar('bangle_L1_qual',   data%Lev1b%bangle_L1_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_L2_qual',   data%Lev1b%bangle_L2_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_getvar('bangle_qual',      data%Lev1b%bangle_qual,        &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
  IF (ncdf_isvar('bangle_opt_qual')) & ! added at v1.1
    CALL ncdf_getvar('bangle_opt_qual',  data%Lev1b%bangle_opt_qual,    &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 range = data%Lev1b%range%bangle_qual,  &
                                 rec   = irec)
  ENDIF

! 2.12 Level2a variables (if any)
! -------------------------------

  IF (ncdf_isvar('alt_refrac')) THEN
    CALL ncdf_getsize('alt_refrac', n, dim = 1)
    CALL ropp_io_init(data%Lev2a, n)
  ELSE
    data%Lev2a%Npoints = 0
  ENDIF

  IF (data%Lev2a%Npoints > 0) THEN

    CALL ncdf_getvar('alt_refrac',   data%Lev2a%alt_refrac,         &
                             units = data%Lev2a%units%alt_refrac,   &
                             range = data%Lev2a%range%alt_refrac,   &
                             rec   = irec)
    CALL ncdf_getvar('geop_refrac',  data%Lev2a%geop_refrac,        &
                             units = data%Lev2a%units%geop_refrac,  &
                             range = data%Lev2a%range%geop_refrac,  &
                             rec   = irec)
    CALL ncdf_getvar('refrac',       data%Lev2a%refrac,             &
                             units = data%Lev2a%units%refrac,       &
                             range = data%Lev2a%range%refrac,       &
                             rec   = irec)
    CALL ncdf_getvar('refrac_sigma', data%Lev2a%refrac_sigma,       &
                             units = data%Lev2a%units%refrac_sigma, &
                             range = data%Lev2a%range%refrac_sigma, &
                             rec   = irec)
    CALL ncdf_getvar('refrac_qual',  data%Lev2a%refrac_qual,        &
                             units = data%Lev2a%units%refrac_qual,  &
                             range = data%Lev2a%range%refrac_qual,  &
                             rec   = irec)
    IF (ncdf_isvar('dry_temp')) THEN  !For backward compatibility
      CALL ncdf_getvar('dry_temp',       data%Lev2a%dry_temp,             &
                                 units = data%Lev2a%units%dry_temp,       &
                                 range = data%Lev2a%range%dry_temp,       &
                                 rec   = irec)
      CALL ncdf_getvar('dry_temp_sigma', data%Lev2a%dry_temp_sigma,       &
                                 units = data%Lev2a%units%dry_temp_sigma, &
                                 range = data%Lev2a%range%dry_temp_sigma, &
                                 rec   = irec)
      CALL ncdf_getvar('dry_temp_qual',  data%Lev2a%dry_temp_qual,        &
                                 units = data%Lev2a%units%dry_temp_qual,  &
                                 range = data%Lev2a%range%dry_temp_qual,  &
                                 rec   = irec)
    ENDIF

  ENDIF

! 2.13 Level2b variables (if any)
! -------------------------------

  IF (ncdf_isvar('geop')) THEN
    n2d(1) = 0
    n2d(2) = 0
    CALL ncdf_getsize('geop', n2d)
    CALL ropp_io_init(data%Lev2b, n2d)
  ELSE
    data%Lev2b%Npoints = 0
    data%Lev2b%Nhoriz = 0
  ENDIF

  IF (data%Lev2b%Npoints > 0) THEN

    CALL ncdf_getvar('geop',        data%Lev2b%geop,              &
                            units = data%Lev2b%units%geop,        &
                            range = data%Lev2b%range%geop,        &
                            rec   = irec)
    CALL ncdf_getvar('geop_sigma',  data%Lev2b%geop_sigma,        &
                            units = data%Lev2b%units%geop_sigma,  &
                            range = data%Lev2b%range%geop_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('press',       data%Lev2b%press,             &
                            units = data%Lev2b%units%press,       &
                            range = data%Lev2b%range%press,       &
                            rec   = irec)
    CALL ncdf_getvar('press_sigma', data%Lev2b%press_sigma,       &
                            units = data%Lev2b%units%press_sigma, &
                            range = data%Lev2b%range%press_sigma, &
                            rec   = irec)
    CALL ncdf_getvar('temp',        data%Lev2b%temp,              &
                            units = data%Lev2b%units%temp,        &
                            range = data%Lev2b%range%temp,        &
                            rec   = irec)
    CALL ncdf_getvar('temp_sigma',  data%Lev2b%temp_sigma,        &
                            units = data%Lev2b%units%temp_sigma,  &
                            range = data%Lev2b%range%temp_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('shum',        data%Lev2b%shum,              &
                            units = data%Lev2b%units%shum,        &
                            range = data%Lev2b%range%shum,        &
                            rec   = irec)
    CALL ncdf_getvar('shum_sigma',  data%Lev2b%shum_sigma,        &
                            units = data%Lev2b%units%shum_sigma,  &
                            range = data%Lev2b%range%shum_sigma,  &
                            rec   = irec)
    CALL ncdf_getvar('meteo_qual',  data%Lev2b%meteo_qual,        &
                            units = data%Lev2b%units%meteo_qual,  &
                            range = data%Lev2b%range%meteo_qual,  &
                            rec   = irec)

  ENDIF

! 2.14 Level2c variables (if any)
! -------------------------------

  IF (ncdf_isvar('geop_sfc')) THEN
    n2d(1) = 1
    n2d(2) = 0
    CALL ncdf_getsize('geop_sfc', n2d(2), dim = 1)
    CALL ropp_io_init(data%Lev2c, n2d)
  ELSE
    data%Lev2c%Npoints = 0
    data%Lev2c%Nhoriz = 0
  ENDIF

  IF (data%Lev2c%Npoints > 0) THEN

! new 2d code

    CALL ncdf_getvar('dtheta',        data%Lev2c%dtheta,                &
                                units = data%Lev2c%units%dtheta,        &
                                range = data%Lev2c%range%dtheta,        &
                                rec   = irec)
    CALL ncdf_getvar('lat_2d',        data%Lev2c%lat_2d,                &
                                units = data%Lev2c%units%lat_2d,        &
                                range = data%Lev2c%range%lat_2d,        &
                                rec   = irec)
    CALL ncdf_getvar('lon_2d',        data%Lev2c%lon_2d,                &
                                units = data%Lev2c%units%lon_2d,        &
                                range = data%Lev2c%range%lon_2d,        &
                                rec   = irec)
    CALL ncdf_getvar('geop_sfc',        data%Lev2c%geop_sfc,              &
                                units = data%Lev2c%units%geop_sfc,        &
                                range = data%Lev2c%range%geop_sfc,        &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc',       data%Lev2c%press_sfc,             &
                                units = data%Lev2c%units%press_sfc,       &
                                range = data%Lev2c%range%press_sfc,       &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc_sigma', data%Lev2c%press_sfc_sigma,       &
                                units = data%Lev2c%units%press_sfc_sigma, &
                                range = data%Lev2c%range%press_sfc_sigma, &
                                rec   = irec)
    CALL ncdf_getvar('press_sfc_qual',  data%Lev2c%press_sfc_qual,        &
                                units = data%Lev2c%units%press_sfc_qual,  &
                                range = data%Lev2c%range%press_sfc_qual,  &
                                rec   = irec)
  ENDIF

! 2.15 Level2d variables (if any)
! -------------------------------

  IF (ncdf_isvar('level_coeff_a')) THEN
    CALL ncdf_getsize('level_coeff_a', n, dim = 1)
    CALL ropp_io_init(data%Lev2d, n)
  ELSE
    data%Lev2d%Npoints = 0
  ENDIF

  IF (data%Lev2d%Npoints > 0) THEN

    CALL ncdf_getvar('level_type',    data%Lev2d%level_type,          &
                              rec   = irec)
    CALL ncdf_getvar('level_coeff_a', data%Lev2d%level_coeff_a,       &
                              units = data%Lev2d%units%level_coeff_a, &
                              range = data%Lev2d%range%level_coeff_a, &
                              rec   = irec)
    CALL ncdf_getvar('level_coeff_b', data%Lev2d%level_coeff_b,       &
                              units = data%Lev2d%units%level_coeff_b, &
                              range = data%Lev2d%range%level_coeff_b, &
                              rec   = irec)
  ENDIF

! 2.16 Additional variables (if any)
! ----------------------------------

  CALL ropp_io_init(data%vlist)

  DO varid=1,ncdf_nvars

    IF(.NOT. ncdf_read(varid))THEN

       status = nf90_inquire_variable(ncdf_ncid, varid, xtype=TYPE, ndims=ndim)

       IF (TYPE .NE. NF90_CHAR) THEN      ! only read scalar variables

          IF(ndim == 1)THEN
             CALL ropp_io_read_ncdf_get_vlistD0d(varid, data%vlist%VlistD0d, irec)
          ENDIF
          IF(ndim == 2)THEN
             CALL ropp_io_read_ncdf_get_vlistD1d(varid, data%vlist%VlistD1d, irec)
          ENDIF
          IF(ndim == 3)THEN
             CALL ropp_io_read_ncdf_get_vlistD2d(varid, data%vlist%VlistD2d, irec)
          ENDIF
       ENDIF

    ENDIF

    ncdf_read(varid) = .FALSE.           ! reset 'read variable' flag

  ENDDO

! 2.17 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_read_ncdf_get_rodata_2d


!-------------------------------------------------------------------------------
! 3. Error correlation and covariance matrices
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_rocorcov(DATA)

! 3.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_read_ncdf_get_rocorcov
  USE ropp_io_types, ONLY: ROcorcov

  IMPLICIT NONE

  TYPE(ROcorcov), DIMENSION(:), POINTER :: DATA

  REAL(wp), DIMENSION(:),   POINTER     :: lat_min => null()
  REAL(wp), DIMENSION(:),   POINTER     :: lat_max => null()
  REAL(wp), DIMENSION(:,:), POINTER     :: corr    => null()
  REAL(wp), DIMENSION(:,:), POINTER     :: sigma   => null()

  INTEGER                               :: i, m, n
  CHARACTER(len = 256) :: routine

! 3.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_read_ncdf_get')

! 3.3 Latitude bins
! -----------------

  IF (ncdf_isvar('lat_min') .AND. ncdf_isvar('lat_max')) THEN
    CALL ncdf_getsize('lat_min', m)
    CALL ncdf_getsize('lat_max', n)
  ELSE
    CALL message(msg_fatal, &
         "NetCDF data file does not seem to contain an error correlation or covariance structure.")
  ENDIF

  IF (m /= m) THEN
    CALL message(msg_fatal, &
         "Number of latitude bin boundariess in the netCDF data file is inconsistent.")
  ENDIF

  CALL ncdf_getvar_alloc('lat_min', lat_min)
  CALL ncdf_getvar_alloc('lat_max', lat_max)

! 3.4 Error correlation matrices
! ------------------------------

  IF (ncdf_isvar('corr')) THEN
    CALL ncdf_getvar_alloc('corr', corr)
  ELSE
    CALL message(msg_fatal, &
          "NetCDF data file does not seem to contain an error correlation matrix.")
  ENDIF

! 3.5 Error standard deviations
! -----------------------------

  IF (ncdf_isvar('sigma')) THEN
    CALL ncdf_getvar_alloc('sigma', sigma)
  ENDIF

! 3.6 Allocate and fill ROPP structure
! ------------------------------------

  ALLOCATE(DATA(n))

  DO i = 1, n

    DATA(i)%lat_min = lat_min(i)
    DATA(i)%lat_max = lat_max(i)

    ALLOCATE(DATA(i)%corr(SIZE(corr(:,i), 1)))
    DATA(i)%corr = corr(:,i)

    IF (ASSOCIATED(sigma)) THEN
       ALLOCATE(DATA(i)%sigma(SIZE(sigma(:,i), 1)))
       DATA(i)%sigma = sigma(:, i)
    ENDIF

  ENDDO

! 3.7 Clean up
! ------------

  DEALLOCATE(lat_min)
  DEALLOCATE(lat_max)
  DEALLOCATE(corr)
  IF (ASSOCIATED(sigma)) DEALLOCATE(sigma)

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_read_ncdf_get_rocorcov

!-------------------------------------------------------------------------------
! 4. wrapper for other centres' RO data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_otherdata(DATA, file, centre, rec, resolution, getlevel1a, getbufr)

! 4.1 Declarations
! ----------------

  USE ropp_io,       not_this => ropp_io_read_ncdf_get_otherdata
  USE ropp_io_types, ONLY: ROprof

  IMPLICIT NONE

  TYPE(ROprof),        INTENT(inout)  :: DATA
  CHARACTER (len = *), INTENT(in)     :: file
  CHARACTER (len=20),  INTENT(in)     :: centre
  INTEGER,             OPTIONAL       :: rec
  CHARACTER (len=20),  OPTIONAL       :: resolution
  LOGICAL,             OPTIONAL       :: getlevel1a
  LOGICAL,             OPTIONAL       :: getbufr

  CHARACTER (len=20)                  :: lresolution = 'thinned'
  LOGICAL                             :: lgetlevel1a = .FALSE.
  LOGICAL                             :: lgetbufr    = .FALSE.
  LOGICAL                             :: ldummy      = .FALSE.

! defaults
  IF (PRESENT(resolution)) lresolution=resolution
  IF (PRESENT(getlevel1a)) lgetlevel1a=getlevel1a
  IF (PRESENT(getbufr))    lgetbufr=getbufr

! call the appropriate data handling function, default is ROPP format
  SELECT CASE (centre)
      CASE('UCAR')
         CALL ropp_io_read_ncdf_get_ucardata(DATA, file)
      CASE('EUM')
         CALL ropp_io_read_ncdf_get_eumdata(DATA, file, lresolution, lgetlevel1a, lgetbufr, ldummy)
      CASE default
         CALL ropp_io_read_ncdf_get_rodata(DATA, rec)
  END SELECT

END SUBROUTINE ropp_io_read_ncdf_get_otherdata

!-------------------------------------------------------------------------------
! 5. UCAR RO data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_ucardata(DATA, file)

! 5.1 Declarations
! ----------------

  USE ncdf
  USE ropp_utils
  USE ropp_io_types, ONLY: ROprof

  IMPLICIT NONE

  TYPE(ROprof),      INTENT(inout)  :: DATA
  CHARACTER(len = *), INTENT(in)    :: file
  CHARACTER(len = 256)              :: routine

! 5.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_read_ucardata')

! 5.3 Identify file type
! ----------------------

  IF (ncdf_isvar('Bend_ang') .AND. ncdf_isvar('Impact_parm')) THEN
    CALL ropp_io_read_ucardata_atmPrf(DATA, file)
  ELSE IF (ncdf_isvar('pL1Snr') .AND. ncdf_isvar('pL2Snr')) THEN
    CALL ropp_io_read_ucardata_atmPhs(DATA, file)
  ELSE IF (ncdf_isvar('MSL_alt') .AND. ncdf_isvar ('Pres')) THEN
    CALL ropp_io_read_ucardata_atmPrf(DATA, file)
  ELSE
    CALL message(msg_fatal, &
      "Routine ropp_io_read_ncdf_get_ucardata does not support this" // &
      "file type. Only atmPrf, ecmPrf, gfsPrf, ncpPrf, sonPrf and atmPhs "// &
      "files supported. Check input file type.")
  ENDIF

  CALL message_set_routine(routine)

CONTAINS

!-------------------------------------------------------------------------------
! 6. UCAR RO data - atmPrf files
!-------------------------------------------------------------------------------

  SUBROUTINE ropp_io_read_ucardata_atmPrf(DATA, file)

! NB this routine only supports UCAR 'Prf' format netCDF files
!    (i.e. atmPrf, ecmPrf, gfsPrf, ncpPrf and sonPrf)
! See: http://cosmic-io.cosmic.ucar.edu/cdaac/fileFormats/atmPrf.html

! 6.1 Declarations
! ----------------

  USE DateTimeProgs, ONLY: Date_and_Time_UTC
  USE DateTimeTypes
  USE ropp_utils
  USE ncdf
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof,        &
                           ThisFmtVer,    &
                           PCD_open_loop, &
                           PCD_rising,    &
                           PCD_occultation
  USE geodesy,       ONLY: geometric2geopotential

  IMPLICIT NONE

  TYPE(ROprof),      INTENT(inout)  :: DATA
  CHARACTER(len = *), INTENT(in)    :: file

  INTEGER                      :: n
  INTEGER                      :: readint
  REAL(wp)                     :: readreal
  CHARACTER (len = 256)        :: readstr

  REAL(wp), PARAMETER          :: g_wmo = 9.80665_wp
  REAL(wp), PARAMETER          :: epsilon_water = 0.621971_wp
  INTEGER,  DIMENSION(8)       :: DTnow

! holds where output
!  INTEGER, DIMENSION(:), POINTER :: idx => null()
  INTEGER                      :: nidx

! 6.3 Header variables
! --------------------

  readstr = ' '
  CALL ncdf_getatt('fileStamp', readstr)
  data%leo_id = readstr(1:4)
  CALL ncdf_getatt('occulting_sat_id', readint)
  WRITE(data%gns_id,'(A1,I3.3)') 'G', readint
  readstr = ' '
  CALL ncdf_getatt('fiducial_id', readstr)
  IF(readstr /= " ") data%stn_id = readstr(1:4)

! 6.4 Overall quality
! -------------------

  IF (ncdf_isvar('Bend_ang') .AND. ncdf_isvar('Impact_parm')) THEN
    CALL ncdf_getatt('bad', readstr)
    data%PCD = 0
    IF (TRIM(readstr) == "0") THEN
       data%PCD = 0
       data%overall_qual = 100.0
    END IF
    IF (TRIM(readstr) == "1") THEN
       data%PCD =  1 ! non nominal
       data%overall_qual = 0.0
    END IF
    data%units%overall_qual = "%"
    IF (ncdf_isatt('iol') .AND. ncdf_isatt('irs')) THEN
      CALL ncdf_getatt('iol', readint)
      IF (readint == 1) &
         data%PCD = IBSET(data%PCD, PCD_open_loop)     ! open loop used
      CALL ncdf_getatt('irs', readint)
      IF (readint == -1) &
         data%PCD = IBSET(data%PCD, PCD_rising)       ! rising occultation
    ENDIF
  ELSE
    data%PCD = 0
    data%PCD = IBSET(data%PCD, PCD_occultation)     ! background data
  ENDIF

! 6.5 Date and time
! -----------------

  CALL gettime(data%DTocc)

! 6.6 Georeferencing
! ------------------

  CALL ncdf_getatt('lat',          data%georef%lat)
  data%georef%units%lat = "degrees_north"
  CALL ncdf_getatt('lon',          data%georef%lon)
  data%georef%units%lon = "degrees_east"

  IF (ncdf_isvar('Bend_ang') .AND. ncdf_isvar('Impact_parm')) THEN
    CALL ncdf_getatt('occpt_offset', data%georef%time_offset)
    data%georef%units%time_offset = "seconds"
    CALL ncdf_getatt('rgeoid',  data%georef%Undulation)
    data%georef%Undulation = data%georef%Undulation * 1000.0
    data%georef%units%Undulation = "meters"
    CALL ncdf_getatt('rfict',         data%georef%roc)
    data%georef%roc = data%georef%roc * 1000.0
    data%georef%units%roc = "meters"
    CALL ncdf_getatt('curv',       data%georef%r_coc)
    data%georef%r_coc = data%georef%r_coc * 1000.0
    data%georef%units%r_coc = "meters"
    CALL ncdf_getatt('azim',     data%georef%azimuth)
    IF ((data%georef%azimuth <    0.0) .AND. &
         (data%georef%azimuth > -180.0)) &
         data%georef%azimuth = data%georef%azimuth + 360.0
    data%georef%units%azimuth = "degrees"
  ENDIF

! 6.7 Level1b variables (if any)
! -------------------------------

  IF (ncdf_isvar('Bend_ang') .AND. ncdf_isvar('Impact_parm')) THEN
    CALL ncdf_getsize('Impact_parm', n, dim = 1)
    CALL ropp_io_init(data%Lev1b, n)
  ELSE
    data%Lev1b%Npoints = 0
  ENDIF

  IF (data%Lev1b%Npoints > 0) THEN
    CALL ncdf_getvar('Lat', data%Lev1b%lat_tp)
    data%Lev1b%units%lat_tp = "degrees_north"
    CALL ncdf_getvar('Lon', data%Lev1b%lon_tp)
    data%Lev1b%units%lon_tp = "degrees_east"
    CALL ncdf_getvar('Azim', data%Lev1b%azimuth_tp)
!    idx => WHERE( (data%Lev1b%azimuth_tp <    0.0) .AND. &
!                  (data%Lev1b%azimuth_tp > -180.0), nidx)
!    IF (nidx > 0) data%Lev1b%azimuth_tp(idx) = data%Lev1b%azimuth_tp(idx) + 360.0
    WHERE ( (data%Lev1b%azimuth_tp <    0.0) .AND. &
                  (data%Lev1b%azimuth_tp > -180.0) ) &
      data%Lev1b%azimuth_tp = data%Lev1b%azimuth_tp + 360.0
    data%Lev1b%units%azimuth_tp = "degrees"
    CALL ncdf_getvar('Impact_parm', data%Lev1b%impact,               &
                               units = data%Lev1b%units%impact)
    data%lev1b%impact_opt = data%Lev1b%impact
    CALL ncdf_getvar('Bend_ang', data%Lev1b%bangle)
    CALL ncdf_getvar('Opt_bend_ang', data%Lev1b%bangle_opt)
    data%Lev1b%units%bangle = "radians"
    CALL ncdf_getvar('Bend_ang_stdv', data%Lev1b%bangle_sigma)
    data%Lev1b%bangle_opt_sigma = data%Lev1b%bangle_sigma
    data%Lev1b%units%bangle_sigma = "radians"
!    set the quality for bangle
    CALL ncdf_getatt('_FillValue', readreal, 'Opt_bend_ang')
!    idx => WHERE( data%Lev1b%bangle > readreal, nidx)
!    IF (nidx > 0) data%Lev1b%bangle_qual(idx) = 100.0
    WHERE (data%Lev1b%bangle > readreal) &
      data%Lev1b%bangle_qual = 100.0
    data%Lev1b%bangle_opt_qual = data%Lev1b%bangle_qual
  ENDIF

! 6.8 Level2a variables (if any)
! -------------------------------

  IF (ncdf_isvar('MSL_alt') .AND. ncdf_isvar('Ref')) THEN
    CALL ncdf_getsize('MSL_alt', n, dim = 1)
    CALL ropp_io_init(data%Lev2a, n)
  ELSE
    data%Lev2a%Npoints = 0
  ENDIF

  IF (data%Lev2a%Npoints > 0) THEN

    CALL ncdf_getvar('MSL_alt',   data%Lev2a%alt_refrac,         &
                                  units = data%Lev2a%units%alt_refrac)
! geopotential not in UCAR files - generate from altitude
!    idx => WHERE((data%Lev2a%alt_refrac > -999.0), nidx)
!    IF (nidx > 0 .AND. &
!       data%georef%lat > -999.0) &
!       data%Lev2a%geop_refrac(idx) = geometric2geopotential(data%georef%lat, &
!                                                            data%Lev2a%alt_refrac(idx))
    IF (data%georef%lat > -999.0) THEN
      WHERE(data%Lev2a%alt_refrac > -999.0) &
        data%Lev2a%geop_refrac = geometric2geopotential(data%georef%lat, &
                                                       data%Lev2a%alt_refrac)
    ENDIF

    CALL ncdf_getvar('Ref',       data%Lev2a%refrac)
    data%Lev2a%units%refrac = "1"

    IF (ncdf_isvar('Ref_stdv')) THEN
       CALL ncdf_getvar('Ref_stdv',  data%Lev2a%refrac_sigma)

      data%Lev2a%units%refrac_sigma = "1"
       !    set the quality for ref
       CALL ncdf_getatt('_FillValue', readreal, 'Ref' )
!       idx => WHERE( data%Lev2a%refrac > readreal, nidx)
!       IF (nidx > 0) data%Lev2a%refrac_qual(idx) = 100.0
       WHERE( data%Lev2a%refrac > readreal) &
         data%Lev2a%refrac_qual = 100.0
    ENDIF

!! include dry temperature in Level 2a - if variable 'Bend_ang' exists, the
!! UCAR file is an atmPrf file, and the 'Temp' variable is actually dry temperature.
    IF (ncdf_isvar('Temp') .AND. ncdf_isvar('Bend_ang')) THEN

      CALL ncdf_getvar('Temp', data%Lev2a%dry_temp)

!      idx => WHERE((data%Lev2a%dry_temp > -999.0), nidx)
!      IF (nidx > 0) THEN
!        data%Lev2a%dry_temp(idx) = data%Lev2a%dry_temp(idx) + 273.15_wp
!      ENDIF
      WHERE(data%Lev2a%dry_temp > -999.0) &
        data%Lev2a%dry_temp = data%Lev2a%dry_temp + 273.15_wp
      data%Lev2a%units%dry_temp = "kelvin"

    END IF
     
  ENDIF

! 6.9 Level2b variables (if any)
! -------------------------------

  IF (ncdf_isvar('MSL_alt') .AND. ncdf_isvar('Pres')) THEN
    CALL ncdf_getsize('MSL_alt', n, dim = 1)
    CALL ropp_io_init(data%Lev2b, n)
  ELSE
    data%Lev2b%Npoints = 0
  ENDIF

  IF (data%Lev2b%Npoints > 0) THEN

    CALL ncdf_getvar('Pres',       data%Lev2b%press)
    data%Lev2b%units%press = "hPa"
    CALL ncdf_getvar('Temp',        data%Lev2b%temp)
!    idx => WHERE(data%Lev2b%temp > -999.0, nidx)
!    IF (nidx > 0) data%Lev2b%temp(idx) = data%Lev2b%temp(idx) + 273.15_wp
    WHERE (data%Lev2b%temp > -999.0) &
      data%Lev2b%temp = data%Lev2b%temp + 273.15_wp
    data%Lev2b%units%temp = "kelvin"

    IF (ncdf_isvar('Vp')) THEN
      CALL ncdf_getvar('Vp',        data%Lev2b%shum)
!      idx => WHERE(data%Lev2b%shum > -999.0, nidx)
!      IF (nidx > 0) data%Lev2b%shum(idx) =               &
!         (data%Lev2b%shum(idx)*epsilon_water) /     &
!         (data%Lev2b%press(idx) -                   &
!         (data%Lev2b%shum(idx)*(1.0_wp - epsilon_water)))
      WHERE (data%Lev2b%shum > -999.0) &
        data%Lev2b%shum =               &
         (data%Lev2b%shum*epsilon_water) /     &
         (data%Lev2b%press -                   &
         (data%Lev2b%shum*(1.0_wp - epsilon_water)))
      data%Lev2b%units%shum = "kilogram/kilogram"
    ENDIF

! get the geopotential from L2a
    CALL ncdf_getvar('MSL_alt',   data%Lev2b%geop, units = 'metres')
    data%Lev2b%geop = geometric2geopotential(data%georef%lat,data%Lev2b%geop)

! background time
    data%bg%Year   = data%DTocc%Year
    data%bg%Month  = data%DTocc%Month
    data%bg%Day    = data%DTocc%Day
    data%bg%Hour   = data%DTocc%Hour
    data%bg%Minute = data%DTocc%Minute
    nidx = INDEX(file, 'Prf_', .TRUE.)
    IF ( nidx > 3 ) data%bg%source = file(nidx-3:nidx-1)  ! read from filename

  ENDIF

! 6.10 (Global) Attributes
! ------------------------

  data%FmtVersion = ' '        ; data%FmtVersion       = ThisFmtVer
  data%processing_centre = ' ' ; CALL ncdf_getatt('center', data%processing_centre)
  data%pod_method = ' '        ; data%pod_method       = "UNKNOWN"
  data%phase_method = ' '      ; data%phase_method     = "UNKNOWN"
  data%bangle_method = ' '     ; data%bangle_method    = "UNKNOWN"
  data%refrac_method = ' '     ; data%refrac_method    = "UNKNOWN"
  data%meteo_method = ' '      ; data%meteo_method     = "UNKNOWN"
  data%thin_method = ' '       ; data%thin_method      = "UNKNOWN"
  data%software_version = ' '  ; data%software_version = "UNKNOWN"
  nidx = INDEX(file, 'Prf_', .TRUE.)
  IF ( nidx > 0 ) &                                 ! VNN.nnn from file name
    data%software_version = "V" // file(nidx+38:nidx+39) // &
                            "." // file(nidx+32:nidx+34)

! COSMIC has no processing date, set to current utc date/time
  CALL Date_and_Time_UTC ( Values=DTnow )
  data%DTpro%Year   = DTnow(1)
  data%DTpro%Month  = DTnow(2)
  data%DTpro%Day    = DTnow(3)
  data%DTpro%Hour   = DTnow(5)
  data%DTpro%Minute = DTnow(6)
  data%DTpro%Second = DTnow(7)
  data%DTpro%Msec   = DTnow(8)

! 6.11 Occultation ID
! -------------------

  CALL ropp_io_occid(DATA)

! 6.12 Clean up
! -------------

!  IF (ASSOCIATED(idx)) DEALLOCATE(idx)

END SUBROUTINE ropp_io_read_ucardata_atmPrf

!-------------------------------------------------------------------------------
! 7. UCAR RO data - atmPhs files
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ucardata_atmPhs(DATA, file)

! NB this routine only supports UCAR atmPhs netCDF files,
! See: http://cosmic-io.cosmic.ucar.edu/cdaac/fileFormats/atmPhs.html

! 7.1 Declarations
! ----------------

  USE DateTimeProgs, ONLY: Date_and_Time_UTC
  USE DateTimeTypes
  USE ropp_utils
  USE ncdf
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof, &
                           ThisFmtVer

  IMPLICIT NONE

  TYPE(ROprof),       INTENT(inout) :: DATA
  CHARACTER(len = *), INTENT(in)    :: file

  INTEGER                      :: n
  INTEGER                      :: readint
  REAL(wp)                     :: readreal
  CHARACTER (len = 256)        :: readstr

  REAL(wp), PARAMETER          :: g_wmo = 9.80665
  INTEGER,  DIMENSION(8)       :: DTnow

  REAL(wp), DIMENSION(:), ALLOCATABLE :: workdata

! holds where output
!  INTEGER, DIMENSION(:), POINTER :: idx => null()
  INTEGER                      :: nidx

! 7.3 Header variables
! --------------------

  readstr = ' '
  CALL ncdf_getatt('fileStamp', readstr)
  data%leo_id = readstr(1:4)
  CALL ncdf_getatt('occsatId', readint)
  WRITE(data%gns_id,'(A1,I3.3)') 'G', readint
  readstr = ' '
  data%stn_id = ' '

! 7.4 Date and time
! -----------------

  CALL gettime(data%DTocc)

! 7.5 Overall quality
! -------------------

  CALL ncdf_getatt('bad', readstr)
  data%PCD = 0
  IF (TRIM(readstr) == "0") THEN
    data%PCD = 0
    data%overall_qual = 100.0
  END IF
  IF (TRIM(readstr) == "1") THEN
    data%PCD =  1 ! non nominal
    data%overall_qual = 0.0
  END IF
  data%units%overall_qual = "%"

! 7.6 Level1a variables (if any)
! -------------------------------

  IF (ncdf_isvar('time')) THEN
    CALL ncdf_getsize('time', n, dim = 1)
    CALL ropp_io_init(data%Lev1a, n)
  ELSE
    data%Lev1a%Npoints = 0
  ENDIF

  IF (data%Lev1a%Npoints > 0) THEN

    CALL ncdf_getvar('time', data%Lev1a%dtime)
    CALL ncdf_getvar('caL1Snr', data%Lev1a%snr_L1ca)
    CALL ncdf_getvar('pL1Snr', data%Lev1a%snr_L1p)
    CALL ncdf_getvar('pL2Snr', data%Lev1a%snr_L2p)
    data%Lev1a%units%snr = "volt/volt"

    CALL ncdf_getvar('xLeo', data%Lev1a%r_leo(:,1))
    CALL ncdf_getvar('yLeo', data%Lev1a%r_leo(:,2))
    CALL ncdf_getvar('zLeo', data%Lev1a%r_leo(:,3))
    data%Lev1a%r_leo(:,:)  = data%Lev1a%r_leo(:,:) * 1000.0_wp
    data%Lev1a%units%r_leo = "metres"
    data%Lev1a%reference_frame%r_leo = "ECI"

    CALL ncdf_getvar('xdLeo', data%Lev1a%v_leo(:,1))
    CALL ncdf_getvar('ydLeo', data%Lev1a%v_leo(:,2))
    CALL ncdf_getvar('zdLeo', data%Lev1a%v_leo(:,3))
    data%Lev1a%v_leo(:,:)  = data%Lev1a%v_leo(:,:) * 1000.0_wp
    data%Lev1a%units%v_leo = "metres / seconds"

    CALL ncdf_getvar('xGps', data%Lev1a%r_gns(:,1))
    CALL ncdf_getvar('yGps', data%Lev1a%r_gns(:,2))
    CALL ncdf_getvar('zGps', data%Lev1a%r_gns(:,3))
    data%Lev1a%r_gns(:,:)  = data%Lev1a%r_gns(:,:) * 1000.0_wp
    data%Lev1a%units%r_gns = "metres"
    data%Lev1a%reference_frame%r_gns = "ECI"

    CALL ncdf_getvar('xdGps', data%Lev1a%v_gns(:,1))
    CALL ncdf_getvar('ydGps', data%Lev1a%v_gns(:,2))
    CALL ncdf_getvar('zdGps', data%Lev1a%v_gns(:,3))
    data%Lev1a%v_gns(:,:)  = data%Lev1a%v_gns(:,:) * 1000.0_wp
    data%Lev1a%units%v_gns = "metres / seconds"

    CALL ncdf_getvar('exL1', data%Lev1a%phase_L1)
    CALL ncdf_getvar('exL2', data%Lev1a%phase_L2)
    data%Lev1a%units%phase = "metres"

!    open loop phase model data
    ALLOCATE(workdata(data%Lev1a%Npoints))
    CALL ncdf_getvar('xmdl', workdata)
    CALL ropp_io_addvar_rodataD1d(DATA, name     = "open_loop_lcf",        &
                               long_name= "OpenLoop Phase Model",     &
                               units    = "metres",                   &
                               range    = (/-1000000.0_wp, 1000000.0_wp/),  &
                               DATA     = workdata )
    DEALLOCATE(workdata)

!    set the quality for phase
    CALL ncdf_getatt('_FillValue', readreal, 'exL1')
!    idx => WHERE( data%Lev1a%phase_L1 > readreal, nidx)
!    IF (nidx > 0) data%Lev1a%phase_qual(idx) = 100.0
    WHERE (data%Lev1a%phase_L1 > readreal) & 
      data%Lev1a%phase_qual = 100.0
  ENDIF

! 7.7 (Global) Attributes
! ------------------------

  data%FmtVersion = ' '        ; data%FmtVersion       = ThisFmtVer
  data%processing_centre = ' ' ; CALL ncdf_getatt('center', data%processing_centre)
  data%pod_method = ' '        ; data%pod_method       = "UNKNOWN"
  data%phase_method = ' '      ; data%phase_method     = "UNKNOWN"
  data%bangle_method = ' '     ; data%bangle_method    = "UNKNOWN"
  data%refrac_method = ' '     ; data%refrac_method    = "UNKNOWN"
  data%meteo_method = ' '      ; data%meteo_method     = "UNKNOWN"
  data%thin_method = ' '       ; data%thin_method      = "UNKNOWN"
  data%software_version = ' '  ; data%software_version = "UNKNOWN"
  nidx = INDEX(file, 'atmPhs_', .TRUE.)
  IF ( nidx > 0 ) &                                  ! VNN.nnn from file name
    data%software_version = "V" // file(nidx+38:nidx+39) // &
                            "." // file(nidx+32:nidx+34)

! COSMIC has no processing date, set to current utc date/time
  CALL Date_and_Time_UTC ( Values=DTnow )
  data%DTpro%Year   = DTnow(1)
  data%DTpro%Month  = DTnow(2)
  data%DTpro%Day    = DTnow(3)
  data%DTpro%Hour   = DTnow(5)
  data%DTpro%Minute = DTnow(6)
  data%DTpro%Second = DTnow(7)
  data%DTpro%Msec   = DTnow(8)

! 7.8 Occultation ID
! -------------------

  CALL ropp_io_occid(DATA)

! 7.9 Clean up
! -------------

!  IF (ASSOCIATED(idx)) DEALLOCATE(idx)
  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_read_ucardata_atmPhs

SUBROUTINE gettime(DTocc)

  USE ropp_io_types, ONLY: DT7type
  USE ncdf

  IMPLICIT NONE

  TYPE(DT7type), INTENT(inout)       :: DTocc
  REAL                               :: sec

  CALL ncdf_getatt('year',   DTocc%Year)
  CALL ncdf_getatt('month',  DTocc%Month)
  CALL ncdf_getatt('day',    DTocc%Day)
  CALL ncdf_getatt('hour',   DTocc%Hour)
  CALL ncdf_getatt('minute', DTocc%Minute)
  CALL ncdf_getatt('second', sec)
  DTocc%Second = NINT(sec)
  DTocc%Msec = 0

END SUBROUTINE gettime

END SUBROUTINE ropp_io_read_ncdf_get_ucardata

!-------------------------------------------------------------------------------
! 8. EUM RO data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_read_ncdf_get_eumdata(DATA, file, resolution, getlevel1a, getbufr, ldummy)

! 5.1 Declarations
! ----------------

  USE ncdf
  USE ropp_utils
  USE ropp_io_types, ONLY: ROprof

  IMPLICIT NONE

  TYPE(ROprof),        INTENT(inout) :: DATA
  CHARACTER(len = *),  INTENT(in)    :: file
  CHARACTER(len = 20), INTENT(in)    :: resolution
  LOGICAL,             INTENT(IN)    :: getlevel1a
  LOGICAL,             INTENT(IN)    :: getbufr
  CHARACTER(len = 256)               :: routine
  LOGICAL,             INTENT(IN)    :: ldummy ! needed to differentiate between 
                                               ! ropp_io_read_ncdf_get_eumdata and
                                               ! ropp_io_read_ncdf_get_otherdata

! 5.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_read_eumdata')

  CALL ropp_io_read_eumdata(DATA, file, resolution, getlevel1a, getbufr)

  CALL message_set_routine(routine)

CONTAINS

!-------------------------------------------------------------------------------
! 6. EUMETSAT RO data - level 1B
!-------------------------------------------------------------------------------

  SUBROUTINE ropp_io_read_eumdata(DATA, file, resolution, getlevel1a, getbufr)

! This routine reads the netCDF4 EUMETSAT format into an internal ROPP structure.


! 6.1 Declarations
! ----------------

  USE DateTimeProgs, ONLY: Date_and_Time_UTC
  USE DateTimeTypes
  USE ropp_utils
  USE coordinates
  USE ncdf
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof,        &
                           ThisFmtVer,    &
                           PCD_open_loop, &
                           PCD_rising,    &
                           PCD_occultation
  USE geodesy,       ONLY: geometric2geopotential

  IMPLICIT NONE

  TYPE(ROprof),        INTENT(inout) :: DATA
  TYPE(ROprof)                       :: DATA_CL, DATA_RS
  CHARACTER(len = 20), INTENT(in)    :: resolution
  CHARACTER(len = *),  INTENT(in)    :: file
  LOGICAL,             INTENT(IN)    :: getlevel1a
  LOGICAL,             INTENT(IN)    :: getbufr

  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_navbit_ext    ! RS navbits (external)
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_navbit_int    ! RS navbits (internal)
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: temparray        ! Temporary array
  INTEGER,  DIMENSION(:),   ALLOCATABLE :: rs_lcf, cl_lcf, lcf         !  lost carrier flag

  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: r_gns, v_gns, r_leo,v_leo   !Temporary pos/velocity arrays 

  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_i_ca_uncorr   ! RS I component [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_q_ca_uncorr   ! RS Q component [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_exphase_l1_nco ! RS NCO excess phase [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_phase_l1_iq ! I/Q contribution
  Integer,  DIMENSION(:),   ALLOCATABLE :: tracking_state ! tracking state

  INTEGER                      :: n, n_cl, n_rs, first_valid,j
  INTEGER                      :: readint, readint2
  REAL(EightByteReal)          :: readreal, readreal2, ts, ts1
  CHARACTER (len = 256)        :: readstr

!  BYTE                         :: readbyte1, readbyte2, readbyte3 ! nagfor doesn't like BYTE
  INTEGER(OneByteInt)          :: readbyte1, readbyte2, readbyte3

  CHARACTER (len = 256)        :: dir

  REAL(wp), PARAMETER          :: g_wmo = 9.80665_wp
  REAL(wp), PARAMETER          :: epsilon_water = 0.621971_wp
  REAL(wp), PARAMETER          :: f_L1 = 1.57542e9_wp
  REAL(wp), PARAMETER          :: c_light = 299792458.0_wp
! Pi already defined as parameter in coordinates module. Confuses pgf95.
  REAL(wp), PARAMETER          :: pi1 = 3.141592653589793238_wp

  INTEGER,  DIMENSION(8)       :: DTnow
  REAL,     PARAMETER          :: RMDFV = -9999999.0 ! Real missing data flag value (MetDB)
  INTEGER                      :: have_nb = 0

! holds 'where' output
  INTEGER, DIMENSION(:), POINTER :: idx => null()
  INTEGER                        :: nidx

  REAL(wp) :: lat

! 6.2 group path for level 1B data
! --------------------------------
  dir = '/level_1b/'//TRIM(resolution)//'/'


! 6.3 Header variables
! --------------------

  ! receiver satellite ID in ROPP format
  readstr = ' '
  CALL ncdf_getatt('satellite', readstr)
  IF ( readstr == "Metop-A" ) data%leo_id = "META"
  IF ( readstr == "Metop-B" ) data%leo_id = "METB"
  IF ( readstr == "Metop-C" ) data%leo_id = "METC"

  ! occulting GNSS satellite
  CALL ncdf_getvar('/occultation/prn', readint)
  WRITE(data%gns_id,'(A1,I3.3)') 'G', readint

  ! station ID unused for EUMETSAT data
  ! readstr = ' '
  ! CALL ncdf_getatt('fiducial_id', readstr)
  ! IF(readstr /= " ") data%stn_id = readstr(1:4)


! 6.4 Overall quality
! -------------------

  ! Overall nominal / non-nominal
  CALL ncdf_getvar("/quality_control/fsi_retrieval_available", readbyte1)

  IF (readbyte1 == 0 ) CALL ncdf_getvar("/quality_control/go_retrieval_available", readbyte1)

!  data%PCD = (readbyte1 .ne. 1)
  IF (readbyte1 == 0) THEN
    data%PCD = 1
  ELSE
    data%PCD = 0
  ENDIF

  ! BA nominal (set to same value as overall)
!  IF (.NOT. readbyte1) data%PCD = IBSET(data%PCD, PCD_bangle) ! set to same value as overall
  IF (readbyte1 == 0) data%PCD = IBSET(data%PCD, PCD_bangle) ! set to same value as overall

  ! quality indicator, currently just set to 100% or 0%
  data%overall_qual = 100.0 * readbyte1
  data%units%overall_qual = "%"

  ! open or closed loop data setting
  CALL ncdf_getvar("/quality_control/rs_data_available", readbyte1)
  CALL ncdf_getvar("/quality_control/ol_data_available", readbyte2)
!!  IF ((readbyte1) .OR. (readbyte2)) data%PCD = IBSET(data%PCD, PCD_open_loop)   - right now we only us RS, not OL 
!  IF (readbyte1) data%PCD = IBSET(data%PCD, PCD_open_loop)
  IF (readbyte1 /= 0) data%PCD = IBSET(data%PCD, PCD_open_loop)

  ! setting or rising 
  CALL ncdf_getatt("/occultation/occultation_type", readstr)
  IF (TRIM(readstr) == "rising") data%PCD = IBSET(data%PCD, PCD_rising)

  ! closed loop phase measurements okay?
  CALL ncdf_getvar("/quality_control/cl_snr_ca_ok", readbyte1)
  CALL ncdf_getvar("/quality_control/cl_snr_p1_ok", readbyte2)
  CALL ncdf_getvar("/quality_control/cl_snr_p2_ok", readbyte3)
!  IF ((.NOT. readbyte1) .AND. (.NOT. readbyte2) .AND. (.NOT. readbyte3)) data%PCD = IBSET(data%PCD, PCD_phase) 
  IF ((readbyte1 == 0) .AND. (readbyte2 == 0) .AND. (readbyte3 == 0)) data%PCD = IBSET(data%PCD, PCD_phase) 


! 6.5 Date and time
! -----------------

  ! start time of occultation
  CALL ncdf_getvar('/level_1b/utc_start_absdate', readint)
  CALL ncdf_getvar('/level_1b/utc_start_abstime', readreal)

  ! check that we have the right time
  CALL ncdf_getatt('/level_1b/units', readstr, 'utc_start_absdate')
  IF ( readstr == "days since 2000-01-01 00:00:00.00" ) THEN
    CALL abstimetoDT(readint, readreal, data%DTocc)
  ELSE
    CALL message(msg_fatal, &
      "Time units found for utc_start_absdate are inconsistent.")
  ENDIF

  ! difference between start time to georef time
  CALL ncdf_getvar('/occultation/utc_georef_absdate', readint2)
  CALL ncdf_getvar('/occultation/utc_georef_abstime', readreal2)

  ! FIXME: 1. Is there a simple library call to do this in just one call?
  !        2. This can be negative (georef is determined from orbits, but
  !           occultation must not have started yet, but ROPP format says
  !           its between 0 and 999 s).
  IF ( readint == readint2 ) THEN
    data%georef%time_offset = readreal2 - readreal 
  ELSE 
    IF ( readint <  readint2 ) THEN 
      data%georef%time_offset = 86400.D0 - readreal + readreal2
    ELSE 
      data%georef%time_offset = -1.D0 * ( 86400.D0 - readreal2 + readreal )
    ENDIF
  ENDIF


! 6.6 Georeferencing
! ------------------
   
  CALL ncdf_getvar('/occultation/latitude',             data%georef%lat,        units=data%georef%units%lat)
  CALL ncdf_getvar('/occultation/longitude',            data%georef%lon,        units=data%georef%units%lon)
  CALL ncdf_getvar('/occultation/undulation',           data%georef%Undulation, units=data%georef%units%Undulation)
  CALL ncdf_getvar('/occultation/r_curve',              data%georef%roc,        units=data%georef%units%roc)
  CALL ncdf_getvar('/occultation/r_curve_centre_fixed', data%georef%r_coc,      units=data%georef%units%r_coc)
  CALL ncdf_getvar('/occultation/azimuth_north',        data%georef%azimuth,    units=data%georef%units%azimuth)

  ! FIXME: Do we need this?
  data%georef%reference_frame%r_coc = "ECF"

  ! get the location, velocity of leo and gnss, assure that the getbufr flag
  ! is set, not the getlevel1a. The getbufr only allocates one level 1a for 
  ! this info and reads the value from the correct group occultation
  ! FIXME: Is this always done like this? What about when the level 1a has
  ! a dimension > 1, what is actually chosen for the one bufr value?
  ! It would be better to store the reference values for e.g. the bufr
  ! in a dedicated dimension, not in the level 1a one! This dimension 
  ! would always have the value 1 when used, but allows to separate data
  ! at the reference point from the general level 1a data, which covers the
  ! whole occultation

  IF ((getbufr) .AND. (getlevel1a)) THEN

    CALL message(msg_fatal,"Flags to get bufr and get level 1a data set. This is incompatable.")

  ELSE 

    IF (getbufr) THEN

      CALL ropp_io_init(data%Lev1a, 1)
      CALL ncdf_getvar('/occultation/position_rec_fixed', data%Lev1a%r_leo(1,:), &
                       units=data%Lev1a%units%r_leo, range = data%Lev1a%range%r_leo)
      CALL ncdf_getvar('/occultation/velocity_rec',       data%Lev1a%v_leo(1,:), &
                       units=data%Lev1a%units%v_leo, range = data%Lev1a%range%v_leo)
      CALL ncdf_getvar('/occultation/position_gns_fixed', data%Lev1a%r_gns(1,:), &
                       units=data%Lev1a%units%r_gns, range = data%Lev1a%range%r_gns)
      CALL ncdf_getvar('/occultation/velocity_gns',       data%Lev1a%v_gns(1,:), &
                          units=data%Lev1a%units%v_gns, range = data%Lev1a%range%v_gns)

      ! FIXME: Do we need this?
      data%Lev1a%reference_frame%r_leo = "ECF"
      data%Lev1a%reference_frame%r_gns = "ECF"
      data%Lev1a%reference_frame%v_leo = "ECI"
      data%Lev1a%reference_frame%v_leo = "ECI"

      ! dtime set to zero for only 1 level 1A field
      data%Lev1a%dtime = 0.0

      ! mean SNR values for CA, P1, P2
      ! FIXME: - this is the EUMETSAT mean value for SLTA > 60km, should be mentioned
      !          in ROPP netCDF file if required!
      !        - data appears when doing a bufr2ropp, however seems not to be
      !          part of the bufr content.
      !CALL ncdf_getvar('/level_1a/closed_loop/snr_ca_mean', data%Lev1a%snr_L1ca(1), units=data%Lev1a%units%snr, range = data%Lev1a%range%snr)
      !CALL ncdf_getvar('/level_1a/closed_loop/snr_p1_mean', data%Lev1a%snr_L1p(1),  units=data%Lev1a%units%snr, range = data%Lev1a%range%snr)
      !CALL ncdf_getvar('/level_1a/closed_loop/snr_p2_mean', data%Lev1a%snr_L2p(1),  units=data%Lev1a%units%snr, range = data%Lev1a%range%snr)

      ! data is available
      data%Lev1a%Missing = .FALSE.

    ENDIF ! getbufr

  ENDIF ! getbufr .AND. getlevel1a not both true


! 6.7 Level1a variables (if any)
! ------------------------------

! 6.7.1 Closed Loop Level1a variables (if requested)
! --------------------------------------------------

  IF (getlevel1a) THEN

    ! FIXME: there should be a switch, similar to the level 1b resolution, to determine what lev1a data to read (cl only, cl+rs, cl+ol).
    ! For now the approach is to read and combine closed loop and raw sampling records.
    ! This and related stuff is Kjartan's update

    IF (ncdf_isvar('/level_1a/closed_loop/dtime')) THEN    ! read CL data

      CALL ncdf_getsize('/level_1a/closed_loop/dtime', n_cl, dim = 1)
      CALL ropp_io_init(data_cl%Lev1a, n_cl)

      ! Time

      ! FIXME: ropp_unit_conversion doesn't understand the time units in the EUMETSAT files: "seconds since YYYY-MM-DD HH-MM-SS" 
      ! with changing values. Since default behaviour is to assume seconds the data are actually read correctly but the problem causes
      ! a warning

      CALL ncdf_getvar('/level_1a/closed_loop/dtime', data_cl%Lev1a%dtime) !, units=data_cl%Lev1a%units%dtime)

      ! Position and velocity

      ALLOCATE(r_leo(3,n_cl))
      ALLOCATE(v_leo(3,n_cl))
      ALLOCATE(r_gns(3,n_cl))
      ALLOCATE(v_gns(3,n_cl))

      CALL ncdf_getvar('/level_1a/closed_loop/r_transmitter', r_gns, units=data_cl%Lev1a%units%r_gns)
      CALL ncdf_getvar('/level_1a/closed_loop/v_transmitter', v_gns, units=data_cl%Lev1a%units%v_gns)
      CALL ncdf_getvar('/level_1a/closed_loop/r_receiver',    r_leo, units=data_cl%Lev1a%units%r_leo)
      CALL ncdf_getvar('/level_1a/closed_loop/v_receiver',    v_leo, units=data_cl%Lev1a%units%v_leo)

      DO j=1,n_cl
        data_cl%Lev1a%r_gns(j,:) = r_gns(:,j)
        data_cl%Lev1a%v_gns(j,:) = v_gns(:,j)
        data_cl%Lev1a%r_leo(j,:) = r_leo(:,j)
        data_cl%Lev1a%v_leo(j,:) = v_leo(:,j)
      ENDDO

      DEALLOCATE(r_leo)
      DEALLOCATE(v_leo)
      DEALLOCATE(r_gns)
      DEALLOCATE(v_gns)

      ! Phase and amplitude

      CALL ncdf_getvar('/level_1a/closed_loop/exphase_ca', data_cl%Lev1a%phase_L1, units=data_cl%Lev1a%units%phase)
      CALL ncdf_getvar('/level_1a/closed_loop/exphase_p2', data_cl%Lev1a%phase_L2, units=data_cl%Lev1a%units%phase)
      CALL ncdf_getvar('/level_1a/closed_loop/snr_ca',     data_cl%Lev1a%snr_L1ca, units=data_cl%Lev1a%units%snr)
      CALL ncdf_getvar('/level_1a/closed_loop/snr_p1',     data_cl%Lev1a%snr_L1p,  units=data_cl%Lev1a%units%snr)
      CALL ncdf_getvar('/level_1a/closed_loop/snr_p2',     data_cl%Lev1a%snr_L2p,  units=data_cl%Lev1a%units%snr)

      ! Closed loop lost carrier flag (looking for data gaps)

        ALLOCATE(cl_LCF(n_cl))
        cl_LCF(:) = 0

        ts = MINVAL(ABS(data_cl%Lev1a%dtime(2:n_cl) - data_cl%Lev1a%dtime(1:n_cl-1)))

        IF (BTEST(data_cl%PCD, PCD_rising)) THEN    ! Rising occultation
          DO j=n_cl-1,1,-1
            ts1 = ABS(data_cl%Lev1a%dtime(j) - data_cl%Lev1a%dtime(j+1))
            IF (ts1 > 1.05*ts) THEN
               cl_LCF(j)   = IBSET(cl_LCF(j),  3)
               cl_LCF(j+1) = IBSET(cl_LCF(j+1),3)
            ENDIF
          ENDDO
        ELSE                                        ! Setting occultation
          DO j=2,n_cl
            ts1 = ABS(data_cl%Lev1a%dtime(j) - data_cl%Lev1a%dtime(j-1))
            IF (ts1 > 1.05*ts) THEN
              cl_LCF(j)   = IBSET(cl_LCF(j),  3)
              cl_LCF(j-1) = IBSET(cl_LCF(j-1),3)
            ENDIF
          ENDDO

       ENDIF   ! Rising occultation

     ENDIF  ! /level_1a/closed_loop/dtime = true

! 6.7.2 Raw Sampling Level1a variables (if requested)
! ---------------------------------------------------

    n_rs = 0    ! will be set differently if RS data is available

    CALL ncdf_getvar("/quality_control/rs_data_available", readbyte1)

    IF (readbyte1 == 1) THEN

      IF (ncdf_isvar('/level_1a/raw_sampling/dtime')) THEN    ! read RS data

        data%PCD = IBSET(data%PCD, PCD_open_loop)
        CALL ncdf_getsize('/level_1a/raw_sampling/dtime', n_rs, dim = 1)
        CALL ropp_io_init(data_rs%Lev1a, n_rs)

        ! Time

        ! FIXME: ropp_unit_conversion doesn't understand the time units in the EUMETSAT files: "seconds since YYYY-MM-DD HH-MM-SS" 
        ! with changing values. Since default behaviour is to assume seconds in data are actually read correctly but the problem causes
        ! a warning

        CALL ncdf_getvar('/level_1a/raw_sampling/dtime',  data_rs%Lev1a%dtime) !,  units=data_rs%Lev1a%units%dtime)

        ! Position and velocity

        ALLOCATE(r_leo(3,n_rs))
        ALLOCATE(v_leo(3,n_rs))
        ALLOCATE(r_gns(3,n_rs))
        ALLOCATE(v_gns(3,n_rs))

        CALL ncdf_getvar('/level_1a/raw_sampling/r_transmitter', r_gns, units=data_rs%Lev1a%units%r_gns)
        CALL ncdf_getvar('/level_1a/raw_sampling/v_transmitter', v_gns, units=data_rs%Lev1a%units%v_gns)
        CALL ncdf_getvar('/level_1a/raw_sampling/r_receiver',    r_leo, units=data_rs%Lev1a%units%r_leo)
        CALL ncdf_getvar('/level_1a/raw_sampling/v_receiver',    v_leo, units=data_rs%Lev1a%units%v_leo)

        DO j=1,n_rs
          data_rs%Lev1a%r_gns(j,:) = r_gns(:,j)
          data_rs%Lev1a%v_gns(j,:) = v_gns(:,j)
          data_rs%Lev1a%r_leo(j,:) = r_leo(:,j)
          data_rs%Lev1a%v_leo(j,:) = v_leo(:,j)
        ENDDO

        DEALLOCATE(r_leo)
        DEALLOCATE(v_leo)
        DEALLOCATE(r_gns)
        DEALLOCATE(v_gns)

        ! Phase and amplitude

        ALLOCATE(rs_navbit_int(n_rs))
        ALLOCATE(rs_navbit_ext(n_rs))
        ALLOCATE(tracking_state(n_rs))

        CALL ncdf_getvar('/level_1a/raw_sampling/navbits_internal',      rs_navbit_int)
        CALL ncdf_getvar('/quality_control/rs_external_navbits_applied', have_nb)
        CALL ncdf_getvar('/level_1a/raw_sampling/tracking_state',        tracking_state)

        IF (have_nb > 0) THEN
          CALL ncdf_getvar('/level_1a/raw_sampling/navbits_external', rs_navbit_ext)
        ELSE
          rs_navbit_ext(:) = rs_navbit_int(:)
        ENDIF

        ALLOCATE(rs_phase_l1_iq(n_rs))
        ALLOCATE(rs_i_ca_uncorr(n_rs))
        ALLOCATE(rs_q_ca_uncorr(n_rs))
        ALLOCATE(rs_exphase_l1_nco(n_rs))

        CALL ncdf_getvar('/level_1a/raw_sampling/i_ca_uncorr',    rs_i_ca_uncorr)
        CALL ncdf_getvar('/level_1a/raw_sampling/q_ca_uncorr',    rs_q_ca_uncorr)
        CALL ncdf_getvar('/level_1a/raw_sampling/exphase_l1_nco', rs_exphase_l1_nco, units=data_rs%Lev1a%units%phase)
        CALL ncdf_getvar('/level_1a/raw_sampling/snr_ca',         data_rs%Lev1a%snr_L1ca, units=data_rs%Lev1a%units%snr)

        DO j=1,n_rs
          rs_phase_l1_iq(j) = ATAN2(rs_q_ca_uncorr(j), rs_i_ca_uncorr(j)+TINY(1.0_wp))
        ENDDO

        CALL Accumulate_Phase(rs_phase_l1_iq)

        data_rs%lev1a%phase_l1(:) = rs_exphase_l1_nco(:) + &
          (c_light/(2.0_wp*pi1*f_L1))*rs_phase_l1_iq(:)

!  Comment in the line below if you wish to read the excess phase directly from the EUMETSAT file
!  instead of deriving from I and Q
!       CALL ncdf_getvar('/level_1a/raw_sampling/exphase_ca', data_rs%Lev1a%phase_L1, units=data_rs%Lev1a%units%phase)

! For rising profiles remove all data with tracking state = 2 in the beginning of the record as these are not valid.

        IF (BTEST(data_rs%PCD, PCD_rising)) THEN    ! Rising occultation

          first_valid = -1 ! value to mark that this hasn't been set yet

          DO j=1,n_rs
            IF ((tracking_state(j) /= 2) .and. (first_valid == -1)) THEN
              first_valid = j
            ENDIF
          ENDDO

          CALL ropp_io_shrink(data_rs%Lev1a, first_valid, n_rs, 1)

          ALLOCATE(temparray(data_rs%Lev1a%Npoints))

          temparray = rs_navbit_int(first_valid:n_rs)

          DEALLOCATE(rs_navbit_int)
          ALLOCATE(rs_navbit_int(data_rs%Lev1a%Npoints))
          rs_navbit_int = temparray

          temparray = rs_navbit_ext(first_valid:n_rs)
          DEALLOCATE(rs_navbit_ext)
          ALLOCATE(rs_navbit_ext(data_rs%Lev1a%Npoints))
          rs_navbit_ext = temparray

          DEALLOCATE(temparray)

          n_rs = data_rs%Lev1a%Npoints

        ENDIF  ! Rising occultation

        ! data flag

        ALLOCATE(rs_LCF(n_rs))
        rs_LCF(:) = 0

        ts = MINVAL(ABS(data_rs%Lev1a%dtime(2:n_rs) - data_rs%Lev1a%dtime(1:n_rs-1)))

        IF (BTEST(data_rs%PCD, PCD_rising)) THEN    ! Rising occultation
          DO j=n_rs-1,1,-1
            ts1 = ABS(data_rs%Lev1a%dtime(j) - data_rs%Lev1a%dtime(j+1))
            IF (ts1 > 1.05*ts) THEN
              rs_LCF(j)   = IBSET(rs_LCF(j),  3)
              rs_LCF(j+1) = IBSET(rs_LCF(j+1),3)
            ENDIF
          ENDDO
        ELSE                                        ! Setting occultation
          DO j=2,n_rs
            ts1 = ABS(data_rs%Lev1a%dtime(j) - data_rs%Lev1a%dtime(j-1))
            IF (ts1 > 1.05*ts) THEN
              rs_LCF(j)   = IBSET(rs_LCF(j),  3)
              rs_LCF(j-1) = IBSET(rs_LCF(j-1),3)
            ENDIF
          ENDDO
        ENDIF

        rs_LCF(:) = IBSET(rs_LCF(:), 0)        ! Open loop mode

        WHERE (NINT(rs_navbit_ext(:)) == 1)    ! External navbit
          rs_LCF(:) = IBSET(rs_LCF(:), 1)
        ENDWHERE

        rs_LCF(:) = IBSET(rs_LCF(:), 2)        ! Navbit quality OK

        WHERE (NINT(rs_navbit_int(:)) == 1)    ! Alternative navbit
          rs_LCF(:) = IBSET(rs_LCF(:), 4)
        ENDWHERE

        rs_LCF(:) = IBSET(rs_LCF(:), 5)        ! Alternative navbit quality

        ! Setting flag for duplicated CL record
        WHERE((data_rs%Lev1a%dtime(1) < data_cl%Lev1a%dtime(:)).AND.(data_cl%Lev1a%dtime(:) < data_rs%Lev1a%dtime(n_rs)))
          cl_LCF(:) = IBSET(cl_LCF(:),6)
        ENDWHERE

      ENDIF  ! /level_1a/raw_sampling/dtime exists

    ENDIF  ! /quality_control/rs_data_available is true

! 6.7.3 Combine CL and RS  Level1a variables (if requested)
! ---------------------------------------------------------

    n = n_cl + n_rs

    ! Initialise ro_data structure variables

    CALL ropp_io_init(data%Lev1a, n)
    ALLOCATE(lcf(n))

    ! Set output variables

    IF (BTEST(data%PCD, PCD_rising)) THEN    ! Rising occultation

      DO j=1,n_cl
        data%lev1a%dtime(n_rs+j)    = data_cl%lev1a%dtime(j)
        data%lev1a%r_gns(n_rs+j,:)  = data_cl%lev1a%r_gns(j,:)
        data%lev1a%v_gns(n_rs+j,:)  = data_cl%lev1a%v_gns(j,:)
        data%lev1a%r_leo(n_rs+j,:)  = data_cl%lev1a%r_leo(j,:)
        data%lev1a%v_leo(n_rs+j,:)  = data_cl%lev1a%v_leo(j,:)
        data%lev1a%snr_L1ca(n_rs+j) = data_cl%lev1a%snr_L1ca(j)
        data%lev1a%snr_L1p(n_rs+j)  = data_cl%lev1a%snr_L1p(j)
        data%lev1a%snr_L2p(n_rs+j)  = data_cl%lev1a%snr_L2p(j)
        data%lev1a%phase_L1(n_rs+j) = data_cl%lev1a%phase_L1(j)
        data%lev1a%phase_L2(n_rs+j) = data_cl%lev1a%phase_L2(j)
        lcf(n_rs+j) = cl_LCF(j)
      ENDDO

      IF (n_rs /= 0) THEN

        DO j=1,n_rs
          data%lev1a%dtime(j)    = data_rs%lev1a%dtime(j)
          data%lev1a%r_gns(j,:)  = data_rs%lev1a%r_gns(j,:)
          data%lev1a%v_gns(j,:)  = data_rs%lev1a%v_gns(j,:)
          data%lev1a%r_leo(j,:)  = data_rs%lev1a%r_leo(j,:)
          data%lev1a%v_leo(j,:)  = data_rs%lev1a%v_leo(j,:)
          data%lev1a%snr_L1ca(j) = data_rs%lev1a%snr_L1ca(j)
          data%lev1a%snr_L1p(j)  = ropp_MDFV
          data%lev1a%snr_L2p(j)  = ropp_MDFV
          data%lev1a%phase_L1(j) = data_rs%lev1a%phase_L1(j)
          data%lev1a%phase_L2(j) = ropp_MDFV
          lcf(j) = rs_LCF(j)
        ENDDO

      ! test for gap between cl and rs records
        ts = MINVAL(ABS(data_cl%lev1a%dtime(2:n_cl) - data_cl%lev1a%dtime(1:n_cl-1)))

        IF (data_rs%lev1a%dtime(n_rs) < data_cl%lev1a%dtime(1)-1.5*ts) THEN
          LCF(n_rs:n_rs+1) = IBSET(LCF(n_rs:n_rs+1),3)
        ENDIF

      ENDIF  ! n_rs /= 0

    ELSE                                        ! Setting occultation

      DO j=1,n_cl
        data%lev1a%dtime(j)    = data_cl%lev1a%dtime(j)
        data%lev1a%r_gns(j,:)  = data_cl%lev1a%r_gns(j,:)
        data%lev1a%v_gns(j,:)  = data_cl%lev1a%v_gns(j,:)
        data%lev1a%r_leo(j,:)  = data_cl%lev1a%r_leo(j,:)
        data%lev1a%v_leo(j,:)  = data_cl%lev1a%v_leo(j,:)
        data%lev1a%snr_L1ca(j) = data_cl%lev1a%snr_L1ca(j)
        data%lev1a%snr_L1p(j)  = data_cl%lev1a%snr_L1p(j)
        data%lev1a%snr_L2p(j)  = data_cl%lev1a%snr_L2p(j)
        data%lev1a%phase_L1(j) = data_cl%lev1a%phase_L1(j)
        data%lev1a%phase_L2(j) = data_cl%lev1a%phase_L2(j)
        lcf(j) = cl_LCF(j)
      ENDDO

      IF (n_rs /= 0) THEN

        DO j=1,n_rs
          data%lev1a%dtime(n_cl+j)    = data_rs%lev1a%dtime(j)
          data%lev1a%r_gns(n_cl+j,:)  = data_rs%lev1a%r_gns(j,:)
          data%lev1a%v_gns(n_cl+j,:)  = data_rs%lev1a%v_gns(j,:)
          data%lev1a%r_leo(n_cl+j,:)  = data_rs%lev1a%r_leo(j,:)
          data%lev1a%v_leo(n_cl+j,:)  = data_rs%lev1a%v_leo(j,:)
          data%lev1a%snr_L1ca(n_cl+j) = data_rs%lev1a%snr_L1ca(j)
          data%lev1a%snr_L1p(n_cl+j)  = ropp_MDFV
          data%lev1a%snr_L2p(n_cl+j)  = ropp_MDFV
          data%lev1a%phase_L1(n_cl+j) = data_rs%lev1a%phase_L1(j)
          data%lev1a%phase_L2(n_cl+j) = ropp_MDFV
          lcf(n_cl+j) = rs_LCF(j)
        ENDDO

        ts = MINVAL(ABS(data_cl%lev1a%dtime(2:n_cl) - data_cl%lev1a%dtime(1:n_cl-1)))

        IF (data_cl%lev1a%dtime(n_cl) < data_rs%lev1a%dtime(1)-1.5*ts) THEN
          LCF(n_cl:n_cl+1) = IBSET(LCF(n_cl:n_cl+1),3)
        ENDIF

      ENDIF  !  n_rs /= 0

    ENDIF  ! Rising occultation

! 6.7.4 Missing/invalid data checks
! ---------------------------------

!(idc 24/1/13 pgf95 doesn't like this.
!        DO j=1,n
!          IF (isnan(data%Lev1a%phase_L1(j))) THEN
!            data%Lev1a%phase_L1(j) = ropp_MDFV     ! Check for NaN
!            LCF(j) = IBSET(LCF(j), 3)
!          ENDIF
!          IF (isnan(data%Lev1a%phase_L2(j))) THEN
!            data%Lev1a%phase_L2(j) = ropp_MDFV     ! Check for NaN
!          ENDIF
!          IF (isnan(data%Lev1a%snr_L1ca(j))) THEN
!            data%Lev1a%snr_L1ca(j) = ropp_MDFV     ! Check for NaN
!            LCF(j) = IBSET(LCF(j), 3)
!         ENDIF
!          IF (isnan(data%Lev1a%snr_L1p(j))) THEN
!            data%Lev1a%snr_L1p(j) = ropp_MDFV     ! Check for NaN
!          ENDIF
!          IF (isnan(data%Lev1a%snr_L2p(j))) THEN
!            data%Lev1a%snr_L2p(j) = ropp_MDFV     ! Check for NaN
!          ENDIF
!        ENDDO

    WHERE (ropp_io_isnan(data%Lev1a%phase_L1))
        data%Lev1a%phase_L1 = ropp_MDFV
        LCF = IBSET(LCF, 3)
    ENDWHERE

    WHERE (ropp_io_isnan(data%Lev1a%phase_L2))
        data%Lev1a%phase_L2 = ropp_MDFV
    ENDWHERE

    WHERE (ropp_io_isnan(data%Lev1a%snr_L1ca))
        data%Lev1a%snr_L1ca = ropp_MDFV
        LCF = IBSET(LCF, 3)
    ENDWHERE

    WHERE (ropp_io_isnan(data%Lev1a%snr_L1p))
        data%Lev1a%snr_L1p = ropp_MDFV
    ENDWHERE

    WHERE (ropp_io_isnan(data%Lev1a%snr_L2p))
        data%Lev1a%snr_L2p = ropp_MDFV
    ENDWHERE

!idc 24/1/13 pgf95 doesn't like this.)

    data%Lev1a%reference_frame%r_gns = "ECI"
    data%Lev1a%reference_frame%r_leo = "ECI"

    data%Lev1a%range%phase = (/ MIN(MINVAL(data%Lev1a%phase_L1),   &
      data%Lev1a%range%phase(1)),  &
      MAX(MAXVAL(data%Lev1a%phase_L1),data%Lev1a%range%phase(2)) /)

    ! Add lost carrier information to file

    CALL ropp_io_addvar_rodataD1d(data,   &
                                  name     = "open_loop_lcf",        &
                                  long_name= "OpenLoop Phase Model", &
                                  units    = "",                     &
                                  range    = (/-1000000.0_wp, 1000000.0_wp/),&
                                  DATA     = REAL(LCF,wp) )

    DEALLOCATE(LCF)
    DEALLOCATE(CL_LCF)

    CALL ropp_io_free(data_cl)

    IF (n_rs /= 0) THEN
      DEALLOCATE(RS_LCF)
      CALL ropp_io_free(data_rs)
    ENDIF

  ENDIF !  getlevel1a is true


! 6.8 Level1b variables (if any)
! ------------------------------

  IF (ncdf_isvar(TRIM(dir)//'impact')) THEN
    CALL ncdf_getsize(TRIM(dir)//'impact', n, dim = 1)
    CALL ropp_io_init(data%Lev1b, n)
  ELSE
    data%Lev1b%Npoints = 0
  ENDIF

  IF (data%Lev1b%Npoints > 0) THEN
    CALL ncdf_getvar(TRIM(dir)//'lat_tp',             data%Lev1b%lat_tp,       units=data%Lev1b%units%lat_tp)
    CALL ncdf_getvar(TRIM(dir)//'lon_tp',             data%Lev1b%lon_tp,       units=data%Lev1b%units%lon_tp)
    CALL ncdf_getvar(TRIM(dir)//'azimuth_tp',         data%Lev1b%azimuth_tp,   units=data%Lev1b%units%azimuth_tp)
    CALL ncdf_getvar(TRIM(dir)//'impact',             data%Lev1b%impact,       units=data%Lev1b%units%impact)
    CALL ncdf_getvar(TRIM(dir)//'impact',             data%Lev1b%impact_L1,    units=data%Lev1b%units%impact)
    CALL ncdf_getvar(TRIM(dir)//'impact',             data%Lev1b%impact_L2,    units=data%Lev1b%units%impact)
    CALL ncdf_getvar(TRIM(dir)//'bangle',             data%Lev1b%bangle,       units=data%Lev1b%units%bangle)
    CALL ncdf_getvar(TRIM(dir)//'bangle_ca',          data%Lev1b%bangle_L1,    units=data%Lev1b%units%bangle)
    CALL ncdf_getvar(TRIM(dir)//'bangle_p2',          data%Lev1b%bangle_L2,    units=data%Lev1b%units%bangle)

!(idc 24/1/13 pgf95 doesn't like this.
!    ! remove all NaN from EUM fields
!    ! FIXME: maybe do this in ncdf_getvar?
!    idx => WHERE( isnan(data%Lev1b%lat_tp) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%lat_tp(idx) = RMDFV
!    idx => WHERE( isnan(data%Lev1b%lon_tp) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%lon_tp(idx) = RMDFV
!    idx => WHERE( isnan(data%Lev1b%azimuth_tp) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%azimuth_tp(idx) = RMDFV
!    idx => WHERE( isnan(data%Lev1b%bangle) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%bangle(idx) = RMDFV
!    idx => WHERE( isnan(data%Lev1b%bangle_L1) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%bangle_L1(idx) = RMDFV
!    idx => WHERE( isnan(data%Lev1b%bangle_L2) .eq. .TRUE., nidx)
!    IF (nidx > 0) data%Lev1b%bangle_L2(idx) = RMDFV
!      
!    !    set the quality for bangle, EUMETSAT data uses NaN for missing
!    idx => WHERE( isnan(data%Lev1b%bangle) .eq. .FALSE., nidx)
!    IF (nidx > 0) data%Lev1b%bangle_qual(idx) = 100.0
!    data%Lev1b%Missing = .FALSE.

    ! remove all NaN from EUM fields
    ! FIXME: maybe do this in ncdf_getvar?
    WHERE( ropp_io_isnan(data%Lev1b%lat_tp) ) &
      data%Lev1b%lat_tp = RMDFV
    WHERE( ropp_io_isnan(data%Lev1b%lon_tp) ) &
      data%Lev1b%lon_tp = RMDFV
    WHERE( ropp_io_isnan(data%Lev1b%azimuth_tp) ) &
      data%Lev1b%azimuth_tp = RMDFV
    WHERE( ropp_io_isnan(data%Lev1b%bangle) ) &
      data%Lev1b%bangle = RMDFV
    WHERE( ropp_io_isnan(data%Lev1b%bangle_L1) ) &
      data%Lev1b%bangle_L1 = RMDFV
    WHERE( ropp_io_isnan(data%Lev1b%bangle_L2) ) &
      data%Lev1b%bangle_L2 = RMDFV
      
    ! set the quality for bangle, EUMETSAT data uses NaN for missing
    WHERE( .NOT. ropp_io_isnan(data%Lev1b%bangle) ) &
      data%Lev1b%bangle_qual = 100.0_wp

    data%Lev1b%Missing = .FALSE.

!idc 24/1/13 pgf95 doesn't like this.)

  ENDIF ! data%Lev1b%Npoints > 0


! 6.9 (Global) Attributes
! -----------------------

  data%FmtVersion = ' '        ; data%FmtVersion       = ThisFmtVer
  data%processing_centre = ' ' ; CALL ncdf_getatt('institution', data%processing_centre)
  data%pod_method = ' '        ; CALL ncdf_getatt('/occultation/pod_method', data%pod_method)
  data%phase_method = ' '      ; CALL ncdf_getatt('/occultation/phase_method', data%phase_method)
  data%bangle_method = ' '     ; CALL ncdf_getatt('/occultation/retrieval_method', data%bangle_method)
  data%refrac_method = ' '     ; data%refrac_method    = "UNKNOWN"
  data%meteo_method = ' '      ; data%meteo_method     = "UNKNOWN"
  data%thin_method = ' '       ; data%thin_method      = "UNKNOWN"
  data%software_version = ' '  ; CALL ncdf_getatt('software_version' ,data%software_version)
  
  ! Processing time, split up string
  ! FIXME: Is there a library function for this that does some more format checks?
  CALL ncdf_getatt('processing_time', readstr)
  READ( readstr,   '(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)' ) data%DTpro%Year, data%DTpro%Month, data%DTpro%Day, &
             data%DTpro%Hour, data%DTpro%Minute, data%DTpro%Second, data%DTpro%Msec


! 6.10 Occultation ID
! -------------------

  CALL ropp_io_occid(DATA)


! 6.11 Clean up
! -------------

  IF (ASSOCIATED(idx)) DEALLOCATE(idx)


END SUBROUTINE ropp_io_read_eumdata


!-------------------------------------------------------------------------------
! Accumulate phase 
!-------------------------------------------------------------------------------

SUBROUTINE Accumulate_Phase(Ph, Sign)   ! (Array of (accumulated) phase, dir)

! Method:
!   Sign = 0 or no Sign:
!      Adding +-2*Pi where phase jumps from
!      +-Pi to -+Pi,
!   Sign > 0:
!      Adding +2*Pi where phase jumps from
!      - to +
!   Sign < 0
!      Adding -2*Pi where phase jumps from
!      + to -

  ! 15.1 Declarations

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(inout) :: Ph   ! Phase --> accumulated phase
  INTEGER, OPTIONAL,      INTENT(in)    :: Sign ! Phase change sign
! Pi already defined as parameter in coordinates module. Confuses pgf95.
  REAL(wp), PARAMETER                   :: pi1 = 3.141592653589793238_wp
  INTEGER                               :: i     ! Array index
  INTEGER                               :: PSign ! Phase change sign

  ! 15.2 Determine phase change sign

  IF (.NOT. PRESENT(Sign)) THEN
    PSign = 0
  ELSE
    PSign = Sign
  ENDIF

  ! 15.3 Accumulate phase

  IF (PSign == 0) THEN
    DO i=2,SIZE(Ph)
      Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1)+pi1, 2.0_wp*pi1) - pi1
    ENDDO
  ELSEIF (PSign > 0) THEN
    DO i=2,SIZE(Ph)
      Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1), 2.0_wp*pi1)
    ENDDO
  ELSEIF (PSign < 0) THEN
    DO i=2,SIZE(Ph)
      Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1)+2.0_wp*pi1, 2.0_wp*pi1) - 2.0_wp*pi1
    ENDDO
  ENDIF

END SUBROUTINE Accumulate_Phase


SUBROUTINE abstimetoDT(i, r, DTocc)  ! handle time format of EUM files

! Based on "Practical Ephemeris Calculations" by Oliver Montenbruck
! (Springer-Verlag, 1989). Added handling of hour, seconds as well.
!
  USE ropp_io_types, ONLY: DT7type

  IMPLICIT NONE

  INTEGER,            INTENT(in)     :: i   ! number of days since 2000-01-01 00:00:00
  REAL(KIND(1.0D0)),  INTENT(in)     :: r   ! seconds since start of day
  TYPE(DT7type),      INTENT(inout)  :: DTocc
  REAL(KIND(1.0D0))                  :: h, mi, s, ms
  INTEGER                            :: a, b, c, d, e, f, y, m, dd, jd

  ! calculate JD from input, 2451545 is JD of 2000-01-01
  jd = i + 2451545
  a = INT(jd+0.5D0)

  IF (a < 2299161) THEN
    c = a + 1524
  ELSE
    b = INT( (a - 1867216.25D0) / 36524.25D0 )
    c = a + b - INT(b/4D0) + 1525
  ENDIF

  d  = INT( ( c-122.1D0 ) / 365.25D0 )
  e  = INT( 365.25D0*d)
  f  = INT( (c-e) / 30.6001D0 )
  dd = c - e - INT( 30.6001*f ) + MOD( ( jd+0.5D0 ), DBLE(a) )
  m  = f - 1 - 12*INT( f/14D0 )
  y  = d - 4715 - INT( ( 7+m )/10.D0 )


  DTocc%Year   = y
  DTocc%Month  = m
  DTocc%Day    = dd
  h            = FLOOR(r/3600.D0)
  DTocc%Hour   = INT(h)
  mi           = FLOOR( (r - h*3600.D0) / 60.D0 )
  s            = FLOOR( (r - h*3600.D0 - mi*60.D0 ) )
  ms           = (r - h*3600.D0 - mi*60.D0 - s) * 1000.D0
  DTocc%Hour   = INT(h)
  DTocc%Minute = INT(mi)
  DTocc%Second = INT(s)
  DTocc%Msec   = INT(ms)

END SUBROUTINE abstimetoDT


FUNCTION ropp_io_isnan(x) RESULT(lnan)  ! Checks whether reals are NaNs.

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE

  INTEGER                            :: k
  REAL(wp), DIMENSION(:), INTENT(IN) :: x
  LOGICAL                            :: it_is
  LOGICAL, DIMENSION(:), ALLOCATABLE :: lnan

  ALLOCATE(lnan(SIZE(x)))

  DO k=1,SIZE(x)
  
    it_is = .FALSE.
    
    IF ( x(k) /= x(k) ) it_is = .TRUE.
    IF ( x(k) + 1.0_wp == x(k) ) it_is = .TRUE.
    IF ((x(k) > 0) .EQV. (x(k) <= 0)) it_is = .TRUE.

    lnan(k) = it_is
    
  ENDDO
 
END FUNCTION ropp_io_isnan


END SUBROUTINE ropp_io_read_ncdf_get_eumdata
