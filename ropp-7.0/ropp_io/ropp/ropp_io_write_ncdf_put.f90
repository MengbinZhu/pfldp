! $Id: ropp_io_write_ncdf_put.f90 3831 2013-08-27 15:22:48Z idculv $

!****is* Writing/ropp_io_write_ncdf_put *
!
! NAME
!    ropp_io_write_ncdf_put - Put data into a (already defined) netCDF.
!
! SYNOPSIS
!    call ropp_io_write_ncdf_putvar(data, rec)
!
! DESCRIPTION
!    This subroutine writes data contained in a derived type structure into
!    a netCDF data file. Variables in this data file must have been created
!    before by a call to ropp_io_write_ncdf_def.
!
! INPUTS
!    data  -  ROPP derived type
!    rec   -  file record number (1-)
!
! OUTPUT
!
!
! NOTES
!    A netCDF file must have been created or opened (in an append mode)
!    using the ncdf_create() or ncdf_open(); this subroutine only works
!    on the current netcdf file.
!
! SEE ALSO
!    ropp_io_write_ncdf
!    ropp_io_write_ncdf_def
!    ropp_io_read_ncdf
!    ropp_io_read_ncdf_get
!
! CALLS
!   ncdf_putvar
!   TimeSince
!
! REFERENCES
!
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

SUBROUTINE ropp_io_write_put_rodata(DATA, rec)

! 1.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_put_rodata
  USE ropp_io_types, ONLY: ROprof
  USE DateTimeProgs, ONLY: TimeSince

  IMPLICIT NONE

  TYPE(ROprof), INTENT(in) :: DATA
  INTEGER,      OPTIONAL   :: rec

  REAL(wp)                 :: time ! Time in seconds since 00:00 1-Jan-2000
  INTEGER, DIMENSION(8)    :: DT8  ! Date/time array
  INTEGER                  :: irec
  CHARACTER(len = 256)     :: routine

! 1.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_put')

! 1.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 1.4 Header variables
! --------------------

  CALL ncdf_putvar('occ_id', data%occ_id, rec = irec)
  CALL ncdf_putvar('gns_id', data%gns_id, rec = irec)
  CALL ncdf_putvar('leo_id', data%leo_id, rec = irec)
  CALL ncdf_putvar('stn_id', data%stn_id, rec = irec)

! 1.5 Date and time
! -----------------

! 1.5.1 Derived time for start of occultation

  IF (isroppinrange(data%dtocc)) THEN
    DT8 = (/data%dtocc%year,data%dtocc%month, data%dtocc%day,0,  &
            data%dtocc%hour,data%dtocc%minute,data%dtocc%second, &
            data%dtocc%msec/)
    CALL TimeSince ( DT8, time, 1, Base="JS2000" ) 
  ELSE
    time = 0.0_wp
  ENDIF
  CALL ncdf_putvar('start_time', time, rec = irec)

! 1.5.2 Elements of the data structure

  CALL ncdf_putvar('year',   data%dtocc%year,         &
                     units = data%dtocc%units%year,   &
                     rec   = irec)
  CALL ncdf_putvar('month',  data%dtocc%month,        &
                     units = data%dtocc%units%month,  &
                     rec   = irec)
  CALL ncdf_putvar('day',    data%dtocc%day,          &
                     units = data%dtocc%units%day,    &
                     rec   = irec)
  CALL ncdf_putvar('hour',   data%dtocc%hour,         &
                     units = data%dtocc%units%hour,   &
                     rec   = irec)
  CALL ncdf_putvar('minute', data%dtocc%minute,       &
                     units = data%dtocc%units%minute, &
                     rec   = irec)
  CALL ncdf_putvar('second', data%dtocc%second,       &
                     units = data%dtocc%units%second, &
                     rec   = irec)
  CALL ncdf_putvar('msec',   data%dtocc%msec,         &
                     units = data%dtocc%units%msec,   &
                     rec   = irec)

! 1.6 Overall quality
! -------------------

  CALL ncdf_putvar('pcd',          data%pcd,                &
                           units = data%units%pcd,          &
                           rec   = irec)
  CALL ncdf_putvar('overall_qual', data%overall_qual,       &
                           units = data%units%overall_qual, &
                           rec   = irec)

! 1.7 Georeferencing
! ------------------

! 1.7.1 Derived time for nominal time of georef

  CALL ncdf_putvar('time', time, rec = irec)

! 1.7.2 Elements of the data structure

  CALL ncdf_putvar('time_offset', data%georef%time_offset,       &
                          units = data%georef%units%time_offset, &
                          rec   = irec)
  CALL ncdf_putvar('lat',         data%georef%lat,               &
                          units = data%georef%units%lat,         &
                          rec   = irec)
  CALL ncdf_putvar('lon',         data%georef%lon,               &
                          units = data%georef%units%lon,         &
                          rec   = irec)
  CALL ncdf_putvar('undulation',  data%georef%undulation,        &
                          units = data%georef%units%undulation,  &
                          rec   = irec)
  CALL ncdf_putvar('roc',         data%georef%roc,               &
                          units = data%georef%units%roc,         &
                          rec   = irec)
  CALL ncdf_putvar('r_coc',       data%georef%r_coc,             &
                          units = data%georef%units%r_coc,       &
                          rec   = irec)
  CALL ncdf_putvar('azimuth',     data%georef%azimuth,           &
                          units = data%georef%units%azimuth,     &
                          rec   = irec)

! 1.8 Background characterisation (if any)
! ----------------------------------------

  IF (data%BG%Source /= 'NONE') THEN
    CALL ncdf_putvar('bg_source',   data%BG%Source,         &
                            rec   = irec)
    CALL ncdf_putvar('bg_year',     data%BG%Year,           &
                            units = data%BG%units%Year,     &
                            rec   = irec)
    CALL ncdf_putvar('bg_month',    data%BG%Month,          &
                            units = data%BG%units%Month,    &
                            rec   = irec)
    CALL ncdf_putvar('bg_day',      data%BG%Day,            &
                            units = data%BG%units%Day,      &
                            rec   = irec)
    CALL ncdf_putvar('bg_hour',     data%BG%Hour,           &
                            units = data%BG%units%Hour,     &
                            rec   = irec)
    CALL ncdf_putvar('bg_minute',   data%BG%Minute,         &
                            units = data%BG%units%Minute,   &
                            rec   = irec)
    CALL ncdf_putvar('bg_fcperiod', data%BG%fcPeriod,       &
                            units = data%BG%units%fcPeriod, &
                            rec   = irec)
   ENDIF
 
! 1.9 Level 1a variables (if any)
! -------------------------------

  IF (data%Lev1a%Npoints > 0) THEN

    CALL ncdf_putvar('dtime',      data%Lev1a%dtime,              &
                           units = data%Lev1a%units%dtime,        &
                           rec   = irec)
    CALL ncdf_putvar('snr_L1ca',   data%Lev1a%snr_L1ca,           &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('snr_L1p',    data%Lev1a%snr_L1p,            &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('snr_L2p',    data%Lev1a%snr_L2p,            &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('phase_L1',   data%Lev1a%phase_L1,           &
                           units = data%Lev1a%units%phase,        &
                           rec   = irec)
    CALL ncdf_putvar('phase_L2',   data%Lev1a%phase_L2,           &
                           units = data%Lev1a%units%phase,        &
                           rec   = irec)
    CALL ncdf_putvar('r_gns',      data%Lev1a%r_gns(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%r_gns,        &
                           rec   = irec)
    CALL ncdf_putvar('v_gns',      data%Lev1a%v_gns(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%v_gns,        &
                           rec   = irec)
    CALL ncdf_putvar('r_leo',      data%Lev1a%r_leo(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%r_leo,        &
                           rec   = irec)
    CALL ncdf_putvar('v_leo',      data%Lev1a%v_leo(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%v_leo,        &
                           rec   = irec)
    CALL ncdf_putvar('phase_qual', data%Lev1a%phase_qual,         &
                             units = data%Lev1a%units%phase_qual, &
                             rec   = irec)
  ENDIF

! 1.10 Level 1b variables (if any)
! --------------------------------

  IF (data%Lev1b%Npoints > 0) THEN

    CALL ncdf_putvar('lat_tp',           data%Lev1b%lat_tp,             &
                                 units = data%Lev1b%units%lat_tp,       &
                                 rec   = irec)
    CALL ncdf_putvar('lon_tp',           data%Lev1b%lon_tp,             &
                                 units = data%Lev1b%units%lon_tp,       &
                                 rec   = irec)
    CALL ncdf_putvar('azimuth_tp',       data%Lev1b%azimuth_tp,         &
                                 units = data%Lev1b%units%azimuth_tp,   &
                                 rec   = irec)
    CALL ncdf_putvar('impact_L1',        data%Lev1b%impact_L1,          &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact_L2',        data%Lev1b%impact_L2,          &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact',           data%Lev1b%impact,             &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact_opt',       data%Lev1b%impact_opt,         &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1',        data%Lev1b%bangle_L1,          &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2',        data%Lev1b%bangle_L2,          &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle',           data%Lev1b%bangle,             &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt',       data%Lev1b%bangle_opt,         &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1_sigma',  data%Lev1b%bangle_L1_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2_sigma',  data%Lev1b%bangle_L2_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_sigma',     data%Lev1b%bangle_sigma,       &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt_sigma', data%Lev1b%bangle_opt_sigma,   &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1_qual',   data%Lev1b%bangle_L1_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2_qual',   data%Lev1b%bangle_L2_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_qual',      data%Lev1b%bangle_qual,        &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt_qual',  data%Lev1b%bangle_opt_qual,    &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
  ENDIF

! 1.11 Level 2a variables (if any)
! --------------------------------

  IF (data%Lev2a%Npoints > 0) THEN

    CALL ncdf_putvar('alt_refrac',     data%Lev2a%alt_refrac,           &
                               units = data%Lev2a%units%alt_refrac,     &
                               rec   = irec)
    CALL ncdf_putvar('geop_refrac',    data%Lev2a%geop_refrac,          &
                               units = data%Lev2a%units%geop_refrac,    &
                               rec   = irec)
    CALL ncdf_putvar('refrac',         data%Lev2a%refrac,               &
                               units = data%Lev2a%units%refrac,         &
                               rec   = irec)
    CALL ncdf_putvar('refrac_sigma',   data%Lev2a%refrac_sigma,         &
                               units = data%Lev2a%units%refrac_sigma,   &
                               rec   = irec)
    CALL ncdf_putvar('refrac_qual',    data%Lev2a%refrac_qual,          &
                               units = data%Lev2a%units%refrac_qual,    &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp',       data%Lev2a%dry_temp,             &
                               units = data%Lev2a%units%dry_temp,       &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp_sigma', data%Lev2a%dry_temp_sigma,       &
                               units = data%Lev2a%units%dry_temp_sigma, &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp_qual',  data%Lev2a%dry_temp_qual,        &
                               units = data%Lev2a%units%dry_temp_qual,  &
                               rec   = irec)
  ENDIF

! 1.12 Level 2b variables (if any)
! --------------------------------

  IF (data%Lev2b%Npoints > 0) THEN

    CALL ncdf_putvar('geop',        data%Lev2b%geop,              &
                            units = data%Lev2b%units%geop,        &
                            rec   = irec)
    CALL ncdf_putvar('geop_sigma',  data%Lev2b%geop_sigma,        &
                            units = data%Lev2b%units%geop_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('press',       data%Lev2b%press,             &
                            units = data%Lev2b%units%press,       &
                            rec   = irec)
    CALL ncdf_putvar('press_sigma', data%Lev2b%press_sigma,       &
                            units = data%Lev2b%units%press_sigma, &
                            rec   = irec)
    CALL ncdf_putvar('temp',        data%Lev2b%temp,              &
                            units = data%Lev2b%units%temp,        &
                            rec   = irec)
    CALL ncdf_putvar('temp_sigma',  data%Lev2b%temp_sigma,        &
                            units = data%Lev2b%units%temp_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('shum',        data%Lev2b%shum,              &
                            units = data%Lev2b%units%shum,        &
                            rec   = irec)
    CALL ncdf_putvar('shum_sigma',  data%Lev2b%shum_sigma,        &
                            units = data%Lev2b%units%shum_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('meteo_qual',  data%Lev2b%meteo_qual,        &
                            units = data%Lev2b%units%meteo_qual,  &
                            rec   = irec)
  ENDIF

! 1.13 Level 2c variables (if any)
! --------------------------------

  IF (data%Lev2c%Npoints > 0) THEN

    CALL ncdf_putvar('geop_sfc',        data%Lev2c%geop_sfc,              &
                                units = data%Lev2c%units%geop_sfc,        &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc',       data%Lev2c%press_sfc,             &
                                units = data%Lev2c%units%press_sfc,       &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc_sigma', data%Lev2c%press_sfc_sigma,       &
                                units = data%Lev2c%units%press_sfc_sigma, &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc_qual',  data%Lev2c%press_sfc_qual,        &
                                units = data%Lev2c%units%press_sfc_qual,  &
                                rec   = irec)

    CALL ncdf_putvar('tph_bangle',      data%Lev2c%tph_bangle,            &
                                units = data%Lev2c%units%tph_bangle,      &
                                rec   = irec)
    CALL ncdf_putvar('tpa_bangle',      data%Lev2c%tpa_bangle,            &
                                units = data%Lev2c%units%tpa_bangle,      &
                                rec   = irec)
    CALL ncdf_putvar('tph_bangle_flag', data%Lev2c%tph_bangle_flag,       &
                                units = data%Lev2c%units%tph_bangle_flag, &
                                rec   = irec)

    CALL ncdf_putvar('tph_refrac',        data%Lev2c%tph_refrac,            &
                                  units = data%Lev2c%units%tph_refrac,      &
                                  rec   = irec)
    CALL ncdf_putvar('tpn_refrac',        data%Lev2c%tpn_refrac,            &
                                  units = data%Lev2c%units%tpn_refrac,      &
                                  rec   = irec)
    CALL ncdf_putvar('tph_refrac_flag',   data%Lev2c%tph_refrac_flag,       &
                                  units = data%Lev2c%units%tph_refrac_flag, &
                                  rec   = irec)

    CALL ncdf_putvar('tph_tdry_lrt',      data%Lev2c%tph_tdry_lrt,              &
                                  units = data%Lev2c%units%tph_tdry_lrt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tpt_tdry_lrt',      data%Lev2c%tpt_tdry_lrt,              &
                                  units = data%Lev2c%units%tpt_tdry_lrt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tph_tdry_lrt_flag', data%Lev2c%tph_tdry_lrt_flag,         &
                                  units = data%Lev2c%units%tph_tdry_lrt_flag,   &
                                  rec   = irec)

    CALL ncdf_putvar('tph_tdry_cpt',      data%Lev2c%tph_tdry_cpt,              &
                                  units = data%Lev2c%units%tph_tdry_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tpt_tdry_cpt',      data%Lev2c%tpt_tdry_cpt,              &
                                  units = data%Lev2c%units%tpt_tdry_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tph_tdry_cpt_flag', data%Lev2c%tph_tdry_cpt_flag,         &
                                  units = data%Lev2c%units%tph_tdry_cpt_flag,   &
                                  rec   = irec)

    CALL ncdf_putvar('prh_tdry_cpt',      data%Lev2c%prh_tdry_cpt,              &
                                  units = data%Lev2c%units%prh_tdry_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('prt_tdry_cpt',      data%Lev2c%prt_tdry_cpt,              &
                                  units = data%Lev2c%units%prt_tdry_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('prh_tdry_cpt_flag', data%Lev2c%prh_tdry_cpt_flag,         &
                                  units = data%Lev2c%units%prh_tdry_cpt_flag,   &
                                  rec   = irec)

    CALL ncdf_putvar('tph_temp_lrt',      data%Lev2c%tph_temp_lrt,              &
                                  units = data%Lev2c%units%tph_temp_lrt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tpt_temp_lrt',      data%Lev2c%tpt_temp_lrt,              &
                                  units = data%Lev2c%units%tpt_temp_lrt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tph_temp_lrt_flag', data%Lev2c%tph_temp_lrt_flag,         &
                                  units = data%Lev2c%units%tph_temp_lrt_flag,   &
                                  rec   = irec)

    CALL ncdf_putvar('tph_temp_cpt',      data%Lev2c%tph_temp_cpt,              &
                                  units = data%Lev2c%units%tph_temp_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tpt_temp_cpt',      data%Lev2c%tpt_temp_cpt,              &
                                  units = data%Lev2c%units%tpt_temp_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('tph_temp_cpt_flag', data%Lev2c%tph_temp_cpt_flag,         &
                                  units = data%Lev2c%units%tph_temp_cpt_flag,   &
                                  rec   = irec)

    CALL ncdf_putvar('prh_temp_cpt',      data%Lev2c%prh_temp_cpt,              &
                                  units = data%Lev2c%units%prh_temp_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('prt_temp_cpt',      data%Lev2c%prt_temp_cpt,              &
                                  units = data%Lev2c%units%prt_temp_cpt,        &
                                  rec   = irec)
    CALL ncdf_putvar('prh_temp_cpt_flag', data%Lev2c%prh_temp_cpt_flag,         &
                                  units = data%Lev2c%units%prh_temp_cpt_flag,   &
                                  rec   = irec)

  ENDIF

! 1.14 Level 2d variables (if any)
! --------------------------------

  IF (data%Lev2d%Npoints > 0) THEN

    CALL ncdf_putvar('level_type',    data%Lev2d%level_type,          &
                              rec   = irec)
    CALL ncdf_putvar('level_coeff_a', data%Lev2d%level_coeff_a,       &
                              units = data%Lev2d%units%level_coeff_a, &
                              rec   = irec)
    CALL ncdf_putvar('level_coeff_b', data%Lev2d%level_coeff_b,       &
                              units = data%Lev2d%units%level_coeff_b, &
                              rec   = irec)
  ENDIF

! 1.15 Additional variables (if any)
! ----------------------------------

  IF (SIZE(data%vlist%VlistD0d) > 0) THEN
    CALL ropp_io_write_put_vlistD0d(data%vlist%VlistD0d, rec = irec)
  ENDIF

  IF (SIZE(data%vlist%VlistD1d) > 0) THEN
    CALL ropp_io_write_put_vlistD1d(data%vlist%VlistD1d, rec = irec)
  ENDIF

  IF (SIZE(data%vlist%VlistD2d) > 0) THEN
    CALL ropp_io_write_put_vlistD2d(data%vlist%VlistD2d, rec = irec)
  ENDIF

! 1.16 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_put_rodata

!-------------------------------------------------------------------------------
! 2. Core RO data (two-dimensional meteorological data)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_write_put_rodata_2d(DATA, rec)

! 2.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_put_rodata_2d
  USE ropp_io_types, ONLY: ROprof2d
  USE DateTimeProgs, ONLY: TimeSince

  IMPLICIT NONE

  TYPE(ROprof2d), INTENT(in) :: DATA
  INTEGER,      OPTIONAL   :: rec

  REAL(wp)                 :: time
  INTEGER, DIMENSION(8)    :: DT8

  INTEGER                  :: irec

  CHARACTER(len = 256)     :: routine

! 2.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_put')

! 2.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 2.4 Header variables
! --------------------

  CALL ncdf_putvar('occ_id', data%occ_id, rec = irec)
  CALL ncdf_putvar('gns_id', data%gns_id, rec = irec)
  CALL ncdf_putvar('leo_id', data%leo_id, rec = irec)
  CALL ncdf_putvar('stn_id', data%stn_id, rec = irec)

! 2.5 Date and time
! -----------------

! 2.5.1 Derived time for start of occultation

  IF (isroppinrange(data%dtocc)) THEN
    DT8 = (/data%dtocc%year,data%dtocc%month, data%dtocc%day,0,  &
            data%dtocc%hour,data%dtocc%minute,data%dtocc%second, &
            data%dtocc%msec/)
    CALL TimeSince ( DT8, time, 1, Base="JS2000" ) 
  ELSE
    time = 0.0_wp
  ENDIF
  CALL ncdf_putvar('start_time', time, rec = irec)

! 2.5.2 Elements of the data structure

  CALL ncdf_putvar('year',   data%dtocc%year,         &
                     units = data%dtocc%units%year,   &
                     rec   = irec)
  CALL ncdf_putvar('month',  data%dtocc%month,        &
                     units = data%dtocc%units%month,  &
                     rec   = irec)
  CALL ncdf_putvar('day',    data%dtocc%day,          &
                     units = data%dtocc%units%day,    &
                     rec   = irec)
  CALL ncdf_putvar('hour',   data%dtocc%hour,         &
                     units = data%dtocc%units%hour,   &
                     rec   = irec)
  CALL ncdf_putvar('minute', data%dtocc%minute,       &
                     units = data%dtocc%units%minute, &
                     rec   = irec)
  CALL ncdf_putvar('second', data%dtocc%second,       &
                     units = data%dtocc%units%second, &
                     rec   = irec)
  CALL ncdf_putvar('msec',   data%dtocc%msec,         &
                     units = data%dtocc%units%msec,   &
                     rec   = irec)

! 2.6 Overall quality
! -------------------

  CALL ncdf_putvar('pcd',          data%pcd,                &
                           units = data%units%pcd,          &
                           rec   = irec)
  CALL ncdf_putvar('overall_qual', data%overall_qual,       &
                           units = data%units%overall_qual, &
                           rec   = irec)

! 2.7 Georeferencing
! ------------------

! 2.7.1 Derived time for nominal time of georef

  CALL ncdf_putvar('time', time, rec = irec)

! 2.7.2 Elements of the data structure

  CALL ncdf_putvar('time_offset', data%georef%time_offset,       &
                          units = data%georef%units%time_offset, &
                          rec   = irec)
  CALL ncdf_putvar('lat',         data%georef%lat,               &
                          units = data%georef%units%lat,         &
                          rec   = irec)
  CALL ncdf_putvar('lon',         data%georef%lon,               &
                          units = data%georef%units%lon,         &
                          rec   = irec)
  CALL ncdf_putvar('undulation',  data%georef%undulation,        &
                          units = data%georef%units%undulation,  &
                          rec   = irec)
  CALL ncdf_putvar('roc',         data%georef%roc,               &
                          units = data%georef%units%roc,         &
                          rec   = irec)
  CALL ncdf_putvar('r_coc',       data%georef%r_coc,             &
                          units = data%georef%units%r_coc,       &
                          rec   = irec)
  CALL ncdf_putvar('azimuth',     data%georef%azimuth,           &
                          units = data%georef%units%azimuth,     &
                          rec   = irec)

! 2.8 Background characterisation (if any)
! ----------------------------------------

  IF (data%BG%Source /= 'NONE') THEN
    CALL ncdf_putvar('bg_source',   data%BG%Source,         &
                            rec   = irec)
    CALL ncdf_putvar('bg_year',     data%BG%Year,           &
                            units = data%BG%units%Year,     &
                            rec   = irec)
    CALL ncdf_putvar('bg_month',    data%BG%Month,          &
                            units = data%BG%units%Month,    &
                            rec   = irec)
    CALL ncdf_putvar('bg_day',      data%BG%Day,            &
                            units = data%BG%units%Day,      &
                            rec   = irec)
    CALL ncdf_putvar('bg_hour',     data%BG%Hour,           &
                            units = data%BG%units%Hour,     &
                            rec   = irec)
    CALL ncdf_putvar('bg_minute',   data%BG%Minute,         &
                            units = data%BG%units%Minute,   &
                            rec   = irec)
    CALL ncdf_putvar('bg_fcperiod', data%BG%fcPeriod,       &
                            units = data%BG%units%fcPeriod, &
                            rec   = irec)
  ENDIF

! 2.9 Level 1a variables (if any)
! -------------------------------

  IF (data%Lev1a%Npoints > 0) THEN

    CALL ncdf_putvar('dtime',      data%Lev1a%dtime,              &
                           units = data%Lev1a%units%dtime,        &
                           rec   = irec)
    CALL ncdf_putvar('snr_L1ca',   data%Lev1a%snr_L1ca,           &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('snr_L1p',    data%Lev1a%snr_L1p,            &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('snr_L2p',    data%Lev1a%snr_L2p,            &
                           units = data%Lev1a%units%snr,          &
                           rec   = irec)
    CALL ncdf_putvar('phase_L1',   data%Lev1a%phase_L1,           &
                           units = data%Lev1a%units%phase,        &
                           rec   = irec)
    CALL ncdf_putvar('phase_L2',   data%Lev1a%phase_L2,           &
                           units = data%Lev1a%units%phase,        &
                           rec   = irec)
    CALL ncdf_putvar('r_gns',      data%Lev1a%r_gns(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%r_gns,        &
                           rec   = irec)
    CALL ncdf_putvar('v_gns',      data%Lev1a%v_gns(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%v_gns,        &
                           rec   = irec)
    CALL ncdf_putvar('r_leo',      data%Lev1a%r_leo(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%r_leo,        &
                           rec   = irec)
    CALL ncdf_putvar('v_leo',      data%Lev1a%v_leo(1:data%Lev1a%Npoints,:), &
                           units = data%Lev1a%units%v_leo,        &
                           rec   = irec)
    CALL ncdf_putvar('phase_qual', data%Lev1a%phase_qual,         &
                             units = data%Lev1a%units%phase_qual, &
                             rec   = irec)
  ENDIF

! 2.10 Level 1b variables (if any)
! --------------------------------

  IF (data%Lev1b%Npoints > 0) THEN

    CALL ncdf_putvar('lat_tp',           data%Lev1b%lat_tp,             &
                                 units = data%Lev1b%units%lat_tp,       &
                                 rec   = irec)
    CALL ncdf_putvar('lon_tp',           data%Lev1b%lon_tp,             &
                                 units = data%Lev1b%units%lon_tp,       &
                                 rec   = irec)
    CALL ncdf_putvar('azimuth_tp',       data%Lev1b%azimuth_tp,         &
                                 units = data%Lev1b%units%azimuth_tp,   &
                                 rec   = irec)
    CALL ncdf_putvar('impact_L1',        data%Lev1b%impact_L1,          &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact_L2',        data%Lev1b%impact_L2,          &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact',           data%Lev1b%impact,             &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('impact_opt',       data%Lev1b%impact_opt,         &
                                 units = data%Lev1b%units%impact,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1',        data%Lev1b%bangle_L1,          &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2',        data%Lev1b%bangle_L2,          &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle',           data%Lev1b%bangle,             &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt',       data%Lev1b%bangle_opt,         &
                                 units = data%Lev1b%units%bangle,       &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1_sigma',  data%Lev1b%bangle_L1_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2_sigma',  data%Lev1b%bangle_L2_sigma,    &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_sigma',     data%Lev1b%bangle_sigma,       &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt_sigma', data%Lev1b%bangle_opt_sigma,   &
                                 units = data%Lev1b%units%bangle_sigma, &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L1_qual',   data%Lev1b%bangle_L1_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_L2_qual',   data%Lev1b%bangle_L2_qual,     &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_qual',      data%Lev1b%bangle_qual,        &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
    CALL ncdf_putvar('bangle_opt_qual',  data%Lev1b%bangle_opt_qual,    &
                                 units = data%Lev1b%units%bangle_qual,  &
                                 rec   = irec)
  ENDIF

! 2.11 Level 2a variables (if any)
! --------------------------------

  IF (data%Lev2a%Npoints > 0) THEN

    CALL ncdf_putvar('alt_refrac',     data%Lev2a%alt_refrac,           &
                               units = data%Lev2a%units%alt_refrac,     &
                               rec   = irec)
    CALL ncdf_putvar('geop_refrac',    data%Lev2a%geop_refrac,          &
                               units = data%Lev2a%units%geop_refrac,    &
                               rec   = irec)
    CALL ncdf_putvar('refrac',         data%Lev2a%refrac,               &
                               units = data%Lev2a%units%refrac,         &
                               rec   = irec)
    CALL ncdf_putvar('refrac_sigma',   data%Lev2a%refrac_sigma,         &
                               units = data%Lev2a%units%refrac_sigma,   &
                               rec   = irec)
    CALL ncdf_putvar('refrac_qual',    data%Lev2a%refrac_qual,          &
                               units = data%Lev2a%units%refrac_qual,    &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp',       data%Lev2a%dry_temp,             &
                               units = data%Lev2a%units%dry_temp,       &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp_sigma', data%Lev2a%dry_temp_sigma,       &
                               units = data%Lev2a%units%dry_temp_sigma, &
                               rec   = irec)
    CALL ncdf_putvar('dry_temp_qual',  data%Lev2a%dry_temp_qual,        &
                               units = data%Lev2a%units%dry_temp_qual,  &
                               rec   = irec)
  ENDIF

! 2.12 Level 2b variables (if any)
! --------------------------------

  IF (data%Lev2b%Npoints > 0) THEN

    CALL ncdf_putvar('geop',        data%Lev2b%geop,              &
                            units = data%Lev2b%units%geop,        &
                            rec   = irec)
    CALL ncdf_putvar('geop_sigma',  data%Lev2b%geop_sigma,        &
                            units = data%Lev2b%units%geop_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('press',       data%Lev2b%press,             &
                            units = data%Lev2b%units%press,       &
                            rec   = irec)
    CALL ncdf_putvar('press_sigma', data%Lev2b%press_sigma,       &
                            units = data%Lev2b%units%press_sigma, &
                            rec   = irec)
    CALL ncdf_putvar('temp',        data%Lev2b%temp,              &
                            units = data%Lev2b%units%temp,        &
                            rec   = irec)
    CALL ncdf_putvar('temp_sigma',  data%Lev2b%temp_sigma,        &
                            units = data%Lev2b%units%temp_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('shum',        data%Lev2b%shum,              &
                            units = data%Lev2b%units%shum,        &
                            rec   = irec)
    CALL ncdf_putvar('shum_sigma',  data%Lev2b%shum_sigma,        &
                            units = data%Lev2b%units%shum_sigma,  &
                            rec   = irec)
    CALL ncdf_putvar('meteo_qual',  data%Lev2b%meteo_qual,        &
                            units = data%Lev2b%units%meteo_qual,  &
                            rec   = irec)
  ENDIF

! 2.13 Level 2c variables (if any)
! --------------------------------

  IF (data%Lev2c%Npoints > 0) THEN
  

! new 2d variables (sbh)  
  
    CALL ncdf_putvar('dtheta',        data%Lev2c%dtheta,                &
                                units = data%Lev2c%units%dtheta,        &
                                rec   = irec)
    CALL ncdf_putvar('lat_2d',        data%Lev2c%lat_2d,                &
                                units = data%Lev2c%units%lat_2d,        &
                                rec   = irec)
    CALL ncdf_putvar('lon_2d',        data%Lev2c%lon_2d,                &
                                units = data%Lev2c%units%lon_2d,        &
                                rec   = irec)

    CALL ncdf_putvar('geop_sfc',        data%Lev2c%geop_sfc,              &
                                units = data%Lev2c%units%geop_sfc,        &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc',       data%Lev2c%press_sfc,             &
                                units = data%Lev2c%units%press_sfc,       &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc_sigma', data%Lev2c%press_sfc_sigma,       &
                                units = data%Lev2c%units%press_sfc_sigma, &
                                rec   = irec)
    CALL ncdf_putvar('press_sfc_qual',  data%Lev2c%press_sfc_qual,        &
                                units = data%Lev2c%units%press_sfc_qual,  &
                                rec   = irec)
  ENDIF

! 2.14 Level 2d variables (if any)
! --------------------------------

  IF (data%Lev2d%Npoints > 0) THEN

    CALL ncdf_putvar('level_type',    data%Lev2d%level_type,          &
                              rec   = irec)
    CALL ncdf_putvar('level_coeff_a', data%Lev2d%level_coeff_a,       &
                              units = data%Lev2d%units%level_coeff_a, &
                              rec   = irec)
    CALL ncdf_putvar('level_coeff_b', data%Lev2d%level_coeff_b,       &
                              units = data%Lev2d%units%level_coeff_b, &
                              rec   = irec)
  ENDIF

! 2.15 Additional variables (if any)
! ----------------------------------

  IF (SIZE(data%vlist%VlistD0d) > 0) THEN
    CALL ropp_io_write_put_vlistD0d(data%vlist%VlistD0d, rec = irec)
  ENDIF

  IF (SIZE(data%vlist%VlistD1d) > 0) THEN
    CALL ropp_io_write_put_vlistD1d(data%vlist%VlistD1d, rec = irec)
  ENDIF

  IF (SIZE(data%vlist%VlistD2d) > 0) THEN
    CALL ropp_io_write_put_vlistD2d(data%vlist%VlistD2d, rec = irec)
  ENDIF

! 2.16 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_put_rodata_2d

!-------------------------------------------------------------------------------
! 3. Vlist for scalar variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_put_vlistD0d(vlist, rec)

! 3.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_put_vlistD0d
  USE ropp_io_types, ONLY: VlisttypeD0d

  IMPLICIT NONE

  TYPE(VlisttypeD0d), INTENT(in) :: vlist
  INTEGER,            OPTIONAL   :: rec

  INTEGER                        :: irec
  CHARACTER(len = 256)           :: routine

! 3.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_put')

! 3.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 3.4 Header variables
! --------------------

  CALL ncdf_putvar(vlist%name, vlist%data, rec = irec)

! 3.5 Write next variable
! -----------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_put_vlistD0d(vlist%next, irec)
  ENDIF

! 3.6 Clean up
! ------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_put_vlistD0d


!-------------------------------------------------------------------------------
! 4. Vlist for one dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_put_vlistD1d(vlist, rec)

! 4.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_put_vlistD1d
  USE ropp_io_types, ONLY: VlisttypeD1d

  IMPLICIT NONE

  TYPE(VlisttypeD1d), INTENT(in) :: vlist
  INTEGER,            OPTIONAL   :: rec

  INTEGER                        :: irec
  CHARACTER(len = 256)           :: routine

! 4.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_put')

! 4.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 4.4 Header variables
! --------------------

  CALL ncdf_putvar(vlist%name, vlist%data, rec = irec)

! 4.5 Write next variable
! -----------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_put_vlistD1d(vlist%next, irec)
  ENDIF

! 4.6 Clean up
! ------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_put_vlistD1d


!-------------------------------------------------------------------------------
! 5. Vlist for two dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_put_vlistD2d(vlist, rec)

! 5.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_put_vlistD2d
  USE ropp_io_types, ONLY: VlisttypeD2d

  IMPLICIT NONE

  TYPE(VlisttypeD2d), INTENT(in) :: vlist
  INTEGER,            OPTIONAL   :: rec

  INTEGER                        :: irec
  CHARACTER(len = 256)           :: routine

! 5.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_put')

! 5.3 Default parameters
! ----------------------

  IF (PRESENT(rec)) THEN
    irec = rec
  ELSE
    irec = 1
  ENDIF

! 5.4 Header variables
! --------------------

  CALL ncdf_putvar(vlist%name, vlist%data, rec = irec)

! 5.5 Write next variable
! -----------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_put_vlistD2d(vlist%next, irec)
  ENDIF

! 5.6 Clean up
! ------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_put_vlistD2d
