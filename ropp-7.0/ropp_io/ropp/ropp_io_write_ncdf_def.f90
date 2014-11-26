! $Id: ropp_io_write_ncdf_def.f90 3831 2013-08-27 15:22:48Z idculv $
!
!****s* Writing/ropp_io_write_ncdf_def *
!
! NAME
!    ropp_io_write_ncdf_def - Define variables in a netCDF for the given data.
!
! SYNOPSIS
!    call ropp_io_write_ncdf_def(data, output_precision)
!
! DESCRIPTION
!    This subroutine defines variables in an already created netCDF data file.
!    It defines variables (names, descriptions, units, ranges...) corresponding
!    to the actual data parameters.
!
! INPUTS
!    type(ROprof)          :: data               Data structure to be written.
!    char(len=*), optional :: output_precision   'float' or 'double'.
!
! OUTPUT
!    None.
!
! NOTES
!    For most variables, units as defined in the default declarations for the RO
!    structure) are used as 'units' variable attribute. This ensures that
!    newly created ROPP data files contains data in standard units as defined
!    by the ROPP User Documentation. The drawback is that users *must* ensure
!    that their unit settings in the 'data' structure are correct if they use
!    different units in their own internal processing.
!
!    For all variables, valid ranges as specified in the user-provided data
!    structure are used for the definition of the valid_range attribute of all
!    netCDF variables. This gives the user full control over the acceptable
!    data values. This requires that the valid range values are given in the
!    the same units as the data. Range values are converted to ROPP standard
!    units as necessary before being written to the netCDF file.
!
!    A netCDF file must have been created or opened (in an append mode) before
!    using the ncdf_create() or ncdf_open(). This subroutine only works on the
!    current (as seen by the ncdf library) netcdf file.
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

SUBROUTINE ropp_io_write_def_rodata(DATA, output_precision)

! 1.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_def_rodata
  USE ropp_io_types, ONLY: ROprof,     &
                           ThisFmtVer, &
                           PCD_Occultation

  IMPLICIT NONE

  TYPE(ROprof)                 :: DATA
  CHARACTER(len = *), OPTIONAL :: output_precision

  INTEGER                      :: status
  INTEGER                      :: nf90_type

  INTEGER                      :: varid
  INTEGER, DIMENSION(1)        :: dimidul
  INTEGER, DIMENSION(2)        :: dimid04
  INTEGER, DIMENSION(2)        :: dimid20
  INTEGER, DIMENSION(2)        :: dimid40
  INTEGER, DIMENSION(2)        :: dimid64
  INTEGER, DIMENSION(2)        :: dimid
  INTEGER, DIMENSION(2)        :: dimid_xyz
  INTEGER, DIMENSION(3)        :: dimid2d

  CHARACTER(len =  23)         :: proc_date
  CHARACTER(len =  64)         :: output_prec

  TYPE(ROprof)                 :: ropp    ! ROPP default (standard) attributes
  CHARACTER(len = 256)         :: routine


! 1.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_def')

! 1.3 Default arguments
! ---------------------

  IF (PRESENT(output_precision)) THEN
     output_prec = output_precision
  ELSE
     output_prec = 'float'
  ENDIF

! 1.4 Change into define mode
! ---------------------------

  status = nf90_redef(ncdf_ncid)

! 1.5 Precision of the output
! ---------------------------

  SELECT CASE (TRIM(output_prec))
    CASE('float', 'single')
      nf90_type = nf90_float
    CASE('double')
      nf90_type = nf90_double
    CASE default
      CALL message(msg_warn, &
           'Unknown ncdf_output_precision: ' // TRIM(output_prec) // '\n' // &
           '   Using default (float) precision.')
      nf90_type = nf90_float
  END SELECT

! 1.6 Global attributes
! ---------------------

  IF (BTEST(data%PCD, PCD_occultation)) THEN ! Background data
    CALL ncdf_putatt('title', 'Atmospheric background data for ROPP Radio Occultation data')
    CALL ncdf_putatt('institution', data%bg%source)
  ELSE
    CALL ncdf_putatt('title', 'ROPP Radio Occultation data')
    CALL ncdf_putatt('institution', data%processing_centre)
  ENDIF
  CALL ncdf_putatt('Conventions', 'CF-1.0')

  WRITE(proc_date, "(i4.4,2('-',i2.2),' ',i2.2,2(':',i2.2),'.',i3.3)") &
       data%DTpro%Year,   &
       data%DTpro%Month,  &
       data%DTpro%Day,    &
       data%DTpro%Hour,   &
       data%DTpro%Minute, &
       data%DTpro%Second, &
       data%DTpro%Msec

  CALL ncdf_putatt('format_version',    ThisFmtVer)
  CALL ncdf_putatt('processing_centre', data%processing_centre)
  CALL ncdf_putatt('processing_date',   proc_date)
  CALL ncdf_putatt('pod_method',        data%pod_method)
  CALL ncdf_putatt('phase_method',      data%phase_method)
  CALL ncdf_putatt('bangle_method',     data%bangle_method)
  CALL ncdf_putatt('refrac_method',     data%refrac_method)
  CALL ncdf_putatt('meteo_method',      data%meteo_method)
  CALL ncdf_putatt('thin_method',       data%thin_method)
  CALL ncdf_putatt('software_version',  data%software_version)
  CALL ncdf_putatt('_FillValue',        ropp_MDFV)

! 1.7 Header variables
! --------------------

! 1.7.1 Dimensions

  dimidul = ncdf_defdim('dim_unlim', nf90_unlimited)

  dimid04 = (/ ncdf_defdim('dim_char04',  5), dimidul(1) /)  ! plus char(0)!
  dimid20 = (/ ncdf_defdim('dim_char20', 21), dimidul(1) /)
  dimid40 = (/ ncdf_defdim('dim_char40', 41), dimidul(1) /)
  dimid64 = (/ ncdf_defdim('dim_char64', 65), dimidul(1) /)

! 1.7.2 Variables

  varid = ncdf_defvar('occ_id', 'Occultation ID',    &
                      '', dimid40, TYPE = nf90_char)
  varid = ncdf_defvar('gns_id', 'GNSS satellite ID', &
                      '', dimid04, TYPE = nf90_char)
  varid = ncdf_defvar('leo_id', 'LEO satellite ID',  &
                      '', dimid04, TYPE = nf90_char)
  varid = ncdf_defvar('stn_id', 'Ground station ID', &
                      '', dimid04, TYPE = nf90_char)

! 1.8 Date and time
! -----------------

! 1.8.1 Derived time value

  varid = ncdf_defvar('start_time', 'Starting time for the occultation', &
                      'seconds since 2000-01-01 00:00:00', dimidul, TYPE = nf90_double)

! 1.8.2 Elements of the data structure

  varid = ncdf_defvar('year',   'Year',        ropp%DTocc%units%year,   dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('month',  'Month',       ropp%DTocc%units%month,  dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('day',    'Day',         ropp%DTocc%units%day,    dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('hour',   'Hour',        ropp%DTocc%units%hour,   dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('minute', 'Minute',      ropp%DTocc%units%minute, dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('second', 'Second',      ropp%DTocc%units%second, dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('msec',   'Millisecond', ropp%DTocc%units%msec,   dimidul, TYPE = nf90_int)

! 1.8.3 Valid ranges (converted to ROPP standard units)

  CALL ut_convert(data%DTocc%range%year,   data%DTocc%units%year,   &
                  ropp%DTocc%range%year,   ropp%DTocc%units%year)
  CALL ut_convert(data%DTocc%range%month,  data%DTocc%units%month,  &
                  ropp%DTocc%range%month,  ropp%DTocc%units%month)
  CALL ut_convert(data%DTocc%range%day,    data%DTocc%units%day,    &
                  ropp%DTocc%range%day,    ropp%DTocc%units%day)
  CALL ut_convert(data%DTocc%range%hour,   data%DTocc%units%hour,   &
                  ropp%DTocc%range%hour,   ropp%DTocc%units%hour)
  CALL ut_convert(data%DTocc%range%minute, data%DTocc%units%minute, &
                  ropp%DTocc%range%minute, ropp%DTocc%units%minute)
  CALL ut_convert(data%DTocc%range%second, data%DTocc%units%second, &
                  ropp%DTocc%range%second, ropp%DTocc%units%second)
  CALL ut_convert(data%DTocc%range%msec,   data%DTocc%units%msec,   &
                  ropp%DTocc%range%msec,   ropp%DTocc%units%msec)

  CALL ncdf_putatt('valid_range', ropp%DTocc%range%year,   varname = 'year')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%month,  varname = 'month')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%day,    varname = 'day')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%hour,   varname = 'hour')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%minute, varname = 'minute')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%second, varname = 'second')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%msec,   varname = 'msec')

! 1.9 Overall quality
! -------------------

! 1.9.1 Elements of the data structure

  varid = ncdf_defvar('pcd', 'Product Confidence Data', &
                      ropp%units%pcd,          dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('overall_qual', 'Overall quality',      &
                      ropp%units%overall_qual, dimidul, TYPE = nf90_type)

! 1.9.2 Valid ranges

  CALL ut_convert(data%range%pcd,          data%units%pcd,          &
                  ropp%range%pcd,          ropp%units%pcd)
  CALL ut_convert(data%range%overall_qual, data%units%overall_qual, &
                  ropp%range%overall_qual, ropp%units%overall_qual)

  CALL ncdf_putatt('valid_range', ropp%range%pcd,          varname = 'pcd')
  CALL ncdf_putatt('valid_range', ropp%range%overall_qual, varname = 'overall_qual')

! 1.10 Georeferencing
! ------------------

! 1.10.1 Dimensions

  dimid_xyz = (/ ncdf_defdim('xyz', 3), dimidul(1) /)

! 1.10.2 Derived time

  varid = ncdf_defvar('time', 'Reference time for the occultation',                &
                      'seconds since 2000-01-01 00:00:00', dimidul, TYPE = nf90_double)

! 1.10.3 Elements of the data struture

  varid = ncdf_defvar('time_offset', 'Time offset for georeferencing (since start of occ.)', &
                      ropp%georef%units%time_offset, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('lat', 'Reference latitude for the occultation',                       &
                      ropp%georef%units%lat, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('lon', 'Reference longitude for the occultation',                      &
                      ropp%georef%units%lon, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('undulation', 'Geoid undulation for the reference coordinate',         &
                      ropp%georef%units%Undulation, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('roc', 'Radius of curvature for the reference coordinate',             &
                      ropp%georef%units%roc, dimidul, TYPE = nf90_double)
  varid = ncdf_defvar('r_coc', 'Centre of curvature for the reference coordinate',           &
                      ropp%georef%units%r_coc, dimid_xyz, TYPE = nf90_type)
  varid = ncdf_defvar('azimuth', 'GNSS->LEO line of sight angle (from True North) for the reference coordinate', &
                      ropp%georef%units%azimuth, dimidul, TYPE = nf90_type)

! 1.10.4 Valid ranges (converted to ROPP standard units)

  CALL ut_convert(data%georef%range%time_offset, data%georef%units%time_offset, &
                  ropp%georef%range%time_offset, ropp%georef%units%time_offset)
  CALL ut_convert(data%georef%range%lat,         data%georef%units%lat,         &
                  ropp%georef%range%lat,         ropp%georef%units%lat)
  CALL ut_convert(data%georef%range%lon,         data%georef%units%lon,         &
                  ropp%georef%range%lon,         ropp%georef%units%lon)
  CALL ut_convert(data%georef%range%undulation,  data%georef%units%undulation,  &
                  ropp%georef%range%undulation,  ropp%georef%units%undulation)
  CALL ut_convert(data%georef%range%roc,         data%georef%units%roc,         &
                  ropp%georef%range%roc,         ropp%georef%units%roc)
  CALL ut_convert(data%georef%range%r_coc,       data%georef%units%r_coc,       &
                  ropp%georef%range%r_coc,       ropp%georef%units%r_coc)
  CALL ut_convert(data%georef%range%azimuth,     data%georef%units%azimuth,     &
                  ropp%georef%range%azimuth,     ropp%georef%units%azimuth)

  CALL ncdf_putatt('valid_range', ropp%georef%range%time_offset, varname = 'time_offset')
  CALL ncdf_putatt('valid_range', ropp%georef%range%lat,         varname = 'lat')
  CALL ncdf_putatt('valid_range', ropp%georef%range%lon,         varname = 'lon')
  CALL ncdf_putatt('valid_range', ropp%georef%range%undulation,  varname = 'undulation')
  CALL ncdf_putatt('valid_range', ropp%georef%range%roc,         varname = 'roc')
  CALL ncdf_putatt('valid_range', ropp%georef%range%r_coc,       varname = 'r_coc')
  CALL ncdf_putatt('valid_range', ropp%georef%range%azimuth,     varname = 'azimuth')

! 1.9.5 Other attributes

  CALL ncdf_putatt('reference_frame', data%georef%reference_frame%r_coc, varname = 'r_coc')

! 1.11 Background characterisation (if any)
! -----------------------------------------

  IF (data%BG%Source /= 'NONE') THEN

! 1.11.1 Variables

    varid = ncdf_defvar('bg_source', 'Background data source',  &
                        '', dimid20, TYPE = nf90_char)
    varid = ncdf_defvar('bg_year',      'VT year',              &
                        ropp%BG%units%year,     dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_month',     'VT month',             &
                        ropp%BG%units%month,    dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_day',       'VT day',               &
                        ropp%BG%units%day,      dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_hour',      'VT hour',              &
                        ropp%BG%units%hour,     dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_minute',    'VT minute',            &
                        ropp%BG%units%minute,   dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_fcperiod',  'Forecast period',      &
                        ropp%BG%units%fcPeriod, dimidul, TYPE = nf90_type)

! 1.11.2 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%BG%range%year,      data%BG%units%year,      &
                    ropp%BG%range%year,      ropp%BG%units%year)
    CALL ut_convert(data%BG%range%month,     data%BG%units%month,     &
                    ropp%BG%range%month,     ropp%BG%units%month)
    CALL ut_convert(data%BG%range%day,       data%BG%units%day,       &
                    ropp%BG%range%day,       ropp%BG%units%day)
    CALL ut_convert(data%BG%range%hour,      data%BG%units%hour,      &
                    ropp%BG%range%hour,      ropp%BG%units%hour)
    CALL ut_convert(data%BG%range%minute,    data%BG%units%minute,    &
                    ropp%BG%range%minute,    ropp%BG%units%minute)
    CALL ut_convert(data%BG%range%fcPeriod, data%BG%units%fcPeriod, &
                    ropp%BG%range%fcPeriod, ropp%BG%units%fcPeriod)

    CALL ncdf_putatt('valid_range', ropp%BG%range%year,     varname = 'bg_year')
    CALL ncdf_putatt('valid_range', ropp%BG%range%month,    varname = 'bg_month')
    CALL ncdf_putatt('valid_range', ropp%BG%range%day,      varname = 'bg_day')
    CALL ncdf_putatt('valid_range', ropp%BG%range%hour,     varname = 'bg_hour')
    CALL ncdf_putatt('valid_range', ropp%BG%range%minute,   varname = 'bg_minute')
    CALL ncdf_putatt('valid_range', ropp%BG%range%fcPeriod, varname = 'bg_fcperiod')
  ENDIF

! 1.12 Level 1a variables (if any)
! --------------------------------

  IF (data%Lev1a%Npoints > 0) THEN

!    1.12.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev1a', data%Lev1a%Npoints), dimidul(1) /)
    dimid2d(1) = dimid(1)
    dimid2d(2) = dimid_xyz(1)
    dimid2d(3) = dimidul(1)

!    1.12.2 Variables

    varid = ncdf_defvar('dtime', 'Time since start of occultation',         &
                         ropp%Lev1a%units%dtime, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('snr_L1ca', 'Signal-to-noise ratio (L1, C/A code)', &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('snr_L1p', 'Signal-to-noise ratio (L1, P code)',    &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('snr_L2p', 'Signal-to-noise ratio (L2, P code)',    &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('phase_L1', 'Excess phase (L1)',                    &
                         ropp%Lev1a%units%phase, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('phase_L2', 'Excess phase (L2)',                    &
                         ropp%Lev1a%units%phase, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('r_gns', 'GNSS transmitter position',               &
                         ropp%Lev1a%units%r_gns, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('v_gns', 'GNSS transmitter velocity',               &
                         ropp%Lev1a%units%v_gns, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('r_leo', 'LEO transmitter position',                &
                         ropp%Lev1a%units%r_leo, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('v_leo', 'LEO transmitter velocity',                &
                         ropp%Lev1a%units%v_leo, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('phase_qual', 'Quality value for phase (and SNR)',  &
                          ropp%Lev1a%units%phase_qual, dimid, TYPE = nf90_type)

!    1.12.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev1a%range%dtime,      data%Lev1a%units%dtime,      &
                    ropp%Lev1a%range%dtime,      ropp%Lev1a%units%dtime)
    CALL ut_convert(data%Lev1a%range%snr,        data%Lev1a%units%snr,        &
                    ropp%Lev1a%range%snr,        ropp%Lev1a%units%snr)
    CALL ut_convert(data%Lev1a%range%phase,      data%Lev1a%units%phase,      &
                    ropp%Lev1a%range%phase,      ropp%Lev1a%units%phase)
    CALL ut_convert(data%Lev1a%range%r_gns,      data%Lev1a%units%r_gns,      &
                    ropp%Lev1a%range%r_gns,      ropp%Lev1a%units%r_gns)
    CALL ut_convert(data%Lev1a%range%v_gns,      data%Lev1a%units%v_gns,      &
                    ropp%Lev1a%range%v_gns,      ropp%Lev1a%units%v_gns)
    CALL ut_convert(data%Lev1a%range%r_leo,      data%Lev1a%units%r_leo,      &
                    ropp%Lev1a%range%r_leo,      ropp%Lev1a%units%r_leo)
    CALL ut_convert(data%Lev1a%range%v_leo,      data%Lev1a%units%v_leo,      &
                    ropp%Lev1a%range%v_leo,      ropp%Lev1a%units%v_leo)
    CALL ut_convert(data%Lev1a%range%phase_qual, data%Lev1a%units%phase_qual, &
                    ropp%Lev1a%range%phase_qual, ropp%Lev1a%units%phase_qual)


    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%dtime,      varname = 'dtime')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L1ca')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L1p')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L2p')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase,      varname = 'phase_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase,      varname = 'phase_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%r_gns,      varname = 'r_gns')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%v_gns,      varname = 'v_gns')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%r_leo,      varname = 'r_leo')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%v_leo,      varname = 'v_leo')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase_qual, varname = 'phase_qual')

!    1.11.4 Other attributes

    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%r_gns, varname = 'r_gns')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%v_gns, varname = 'v_gns')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%r_leo, varname = 'r_leo')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%v_leo, varname = 'v_leo')

  ENDIF

! 1.13 Level 1b variables (if any)
! --------------------------------

  IF (data%Lev1b%Npoints > 0) THEN

!    1.13.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev1b', data%Lev1b%Npoints), dimidul(1) /)

!    1.13.2 Variables

    varid = ncdf_defvar('lat_tp', 'Latitudes for tangent points',               &
                         ropp%Lev1b%units%lat_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('lon_tp', 'Longitudes for tangent points',              &
                         ropp%Lev1b%units%lon_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('azimuth_tp', 'GNSS->LEO line of sight angles (from True North) for tangent points', &
                         ropp%Lev1b%units%azimuth_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('impact_L1', 'Impact parameter (L1)',                   &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact_L2', 'Impact parameter (L2)',                   &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact', 'Impact parameter (generic)',                 &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact_opt', 'Impact parameter (optimised)',           &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1', 'Bending angle (L1)',                      &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L2', 'Bending angle (L2)',                      &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle', 'Bending angle (generic)',                    &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_opt', 'Bending angle (optimised)',              &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1_sigma', 'Estimated error (1-sigma) for bending angles (L1)',   &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L2_sigma', 'Estimated error (1-sigma) for bending angles (L2)',   &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_sigma', 'Estimated error (1-sigma) for bending angles (generic)', &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_opt_sigma', 'Estimated error (1-sigma) for bending angles (optimised)', &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1_qual', 'Bending angle quality value (L1)',   &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_L2_qual', 'Bending angle quality value (L2)',   &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_qual', 'Bending angle quality value (generic)', &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_opt_qual', 'Bending angle quality value (optimised)', &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)

!    1.13.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev1b%range%lat_tp,      data%Lev1b%units%lat_tp,      &
                    ropp%Lev1b%range%lat_tp,      ropp%Lev1b%units%lat_tp)
    CALL ut_convert(data%Lev1b%range%lon_tp,      data%Lev1b%units%lon_tp,      &
                    ropp%Lev1b%range%lon_tp,      ropp%Lev1b%units%lon_tp)
    CALL ut_convert(data%Lev1b%range%azimuth_tp,  data%Lev1b%units%azimuth_tp,  &
                    ropp%Lev1b%range%azimuth_tp,  ropp%Lev1b%units%azimuth_tp)
    CALL ut_convert(data%Lev1b%range%impact,      data%Lev1b%units%impact,      &
                    ropp%Lev1b%range%impact,      ropp%Lev1b%units%impact)
    CALL ut_convert(data%Lev1b%range%bangle,      data%Lev1b%units%bangle,      &
                    ropp%Lev1b%range%bangle,      ropp%Lev1b%units%bangle)
    CALL ut_convert(data%Lev1b%range%bangle_sigma, data%Lev1b%units%bangle_sigma, &
                    ropp%Lev1b%range%bangle_sigma, ropp%Lev1b%units%bangle_sigma)
    CALL ut_convert(data%Lev1b%range%bangle_qual,  data%Lev1b%units%bangle_qual,  &
                    ropp%Lev1b%range%bangle_qual,  ropp%Lev1b%units%bangle_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%lat_tp,       varname = 'lat_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%lon_tp,       varname = 'lon_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%azimuth_tp,   varname = 'azimuth_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_opt')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_opt')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_L1_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_L2_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_opt_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_L1_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_L2_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_opt_qual')

  ENDIF

! 1.14 Level 2a variables (if any)
! --------------------------------

  IF (data%Lev2a%Npoints > 0) THEN

!    1.14.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2a', data%Lev2a%Npoints), dimidul(1) /)

!    1.14.2 Variables

    varid = ncdf_defvar('alt_refrac', 'Geometric height above geoid for refractivity',     &
                         ropp%Lev2a%units%alt_refrac, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('geop_refrac', 'Geopotential height above geoid for refractivity', &
                         ropp%Lev2a%units%geop_refrac, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('refrac', 'Refractivity',                                          &
                         ropp%Lev2a%units%refrac, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('refrac_sigma', 'Estimated error (1-sigma) for refractivity',      &
                         ropp%Lev2a%units%refrac_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('refrac_qual', 'Quality value for refractivity',                   &
                          ropp%Lev2a%units%refrac_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('dry_temp', 'Dry temperature',                                     &
                         ropp%Lev2a%units%dry_temp, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('dry_temp_sigma', 'Estimated error (1-sigma) for dry temperature', &
                         ropp%Lev2a%units%dry_temp_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('dry_temp_qual', 'Quality value for dry temperature',              &
                          ropp%Lev2a%units%dry_temp_qual, dimid, TYPE = nf90_type)

!    1.13.3 Valid ranges

    CALL ut_convert(data%Lev2a%range%alt_refrac,     data%Lev2a%units%alt_refrac,     &
                    ropp%Lev2a%range%alt_refrac,     ropp%Lev2a%units%alt_refrac)
    CALL ut_convert(data%Lev2a%range%geop_refrac,    data%Lev2a%units%geop_refrac,    &
                    ropp%Lev2a%range%geop_refrac,    ropp%Lev2a%units%geop_refrac)
    CALL ut_convert(data%Lev2a%range%refrac,         data%Lev2a%units%refrac,         &
                    ropp%Lev2a%range%refrac,         ropp%Lev2a%units%refrac)
    CALL ut_convert(data%Lev2a%range%refrac_sigma,   data%Lev2a%units%refrac_sigma,   &
                    ropp%Lev2a%range%refrac_sigma,   ropp%Lev2a%units%refrac_sigma)
    CALL ut_convert(data%Lev2a%range%refrac_qual,    data%Lev2a%units%refrac_qual,    &
                    ropp%Lev2a%range%refrac_qual,    ropp%Lev2a%units%refrac_qual)
    CALL ut_convert(data%Lev2a%range%dry_temp,       data%Lev2a%units%dry_temp,       &
                    ropp%Lev2a%range%dry_temp,       ropp%Lev2a%units%dry_temp)
    CALL ut_convert(data%Lev2a%range%dry_temp_sigma, data%Lev2a%units%dry_temp_sigma, &
                    ropp%Lev2a%range%dry_temp_sigma, ropp%Lev2a%units%dry_temp_sigma)
    CALL ut_convert(data%Lev2a%range%dry_temp_qual,  data%Lev2a%units%dry_temp_qual,  &
                    ropp%Lev2a%range%dry_temp_qual,  ropp%Lev2a%units%dry_temp_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%alt_refrac,     varname = 'alt_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%geop_refrac,    varname = 'geop_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac,         varname = 'refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac_sigma,   varname = 'refrac_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac_qual,    varname = 'refrac_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp,       varname = 'dry_temp')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp_sigma, varname = 'dry_temp_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp_qual,  varname = 'dry_temp_qual')

  ENDIF

! 1.15 Level 2b variables (if any)
! --------------------------------

  IF (data%Lev2b%Npoints > 0) THEN

!    1.15.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2b', data%Lev2b%Npoints), dimidul(1) /)

!    1.15.2 Variables

    varid = ncdf_defvar('geop', 'Geopotential height above geoid for P,T,H',      &
                         ropp%Lev2b%units%geop, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('geop_sigma', 'Estimated error (1-sigma) for geopotential height', &
                         ropp%Lev2b%units%geop_sigma, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('press', 'Pressure',                                       &
                         ropp%Lev2b%units%press, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('press_sigma', 'Estimated error (1-sigma) for pressure',   &
                         ropp%Lev2b%units%press_sigma, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('temp', 'Temperature',                                     &
                         ropp%Lev2b%units%temp, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('temp_sigma', 'Estimated error (1-sigma) for temperature', &
                         ropp%Lev2b%units%temp_sigma, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('shum', 'Specific humidity',                               &
                         ropp%Lev2b%units%shum, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('shum_sigma', 'Estimated  error (1-sigma) in specific humidity', &
                         ropp%Lev2b%units%shum_sigma, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('meteo_qual', 'Quality value for meteorological data',     &
                          ropp%Lev2b%units%meteo_qual, dimid, TYPE = nf90_type)

!    1.14.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev2b%range%geop,        data%Lev2b%units%geop,        &
                    ropp%Lev2b%range%geop,        ropp%Lev2b%units%geop)
    CALL ut_convert(data%Lev2b%range%geop_sigma,  data%Lev2b%units%geop_sigma,  &
                    ropp%Lev2b%range%geop_sigma,  ropp%Lev2b%units%geop_sigma)
    CALL ut_convert(data%Lev2b%range%press,       data%Lev2b%units%press,       &
                    ropp%Lev2b%range%press,       ropp%Lev2b%units%press)
    CALL ut_convert(data%Lev2b%range%press_sigma, data%Lev2b%units%press_sigma, &
                    ropp%Lev2b%range%press_sigma, ropp%Lev2b%units%press_sigma)
    CALL ut_convert(data%Lev2b%range%temp,        data%Lev2b%units%temp,        &
                    ropp%Lev2b%range%temp,        ropp%Lev2b%units%temp)
    CALL ut_convert(data%Lev2b%range%temp_sigma,  data%Lev2b%units%temp_sigma,  &
                    ropp%Lev2b%range%temp_sigma,  ropp%Lev2b%units%temp_sigma )
    CALL ut_convert(data%Lev2b%range%shum,        data%Lev2b%units%shum,        &
                    ropp%Lev2b%range%shum,        ropp%Lev2b%units%shum)
    CALL ut_convert(data%Lev2b%range%shum_sigma,  data%Lev2b%units%shum_sigma,  &
                    ropp%Lev2b%range%shum_sigma,  ropp%Lev2b%units%shum_sigma)
    CALL ut_convert(data%Lev2b%range%meteo_qual,  data%Lev2b%units%meteo_qual,  &
                    ropp%Lev2b%range%meteo_qual,  ropp%Lev2b%units%meteo_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%geop,        varname = 'geop')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%geop_sigma,  varname = 'geop_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%press,       varname = 'press')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%press_sigma, varname = 'press_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%temp,        varname = 'temp')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%temp_sigma,  varname = 'temp_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%shum,        varname = 'shum')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%shum_sigma,  varname = 'shum_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%meteo_qual,  varname = 'meteo_qual')

  ENDIF

! 1.16 Level 2c variables (if any)
! --------------------------------

  IF (data%Lev2c%Npoints > 0) THEN

!    1.16.1 Variables

    varid = ncdf_defvar('geop_sfc', 'Surface geopotential height',                             &
                         ropp%Lev2c%units%geop_sfc, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc', 'Surface pressure',                                       &
                         ropp%Lev2c%units%press_sfc, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc_sigma', 'Estimated error (1-sigma) for surface pressure',   &
                         ropp%Lev2c%units%press_sfc_sigma, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc_qual', 'Surface pressure quality value',                    &
                         ropp%Lev2c%units%press_sfc_qual, dimidul, TYPE = nf90_type)

    varid = ncdf_defvar('tph_bangle', 'Bending angle-based TPH',                               &
                         ropp%Lev2c%units%tph_bangle, dimidul, TYPE = nf90_double)
    varid = ncdf_defvar('tpa_bangle', 'Bending angle-based TPA',                               &
                         ropp%Lev2c%units%tpa_bangle, dimidul, TYPE = nf90_double)
    varid = ncdf_defvar('tph_bangle_flag', 'Bending angle-based TPH QC flag',                  &
                         ropp%Lev2c%units%tph_bangle_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('tph_refrac', 'Refractivity-based TPH',                                &
                         ropp%Lev2c%units%tph_refrac, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tpn_refrac', 'Refractivity-based TPN',                                &
                         ropp%Lev2c%units%tpn_refrac, dimidul, TYPE = nf90_double)
    varid = ncdf_defvar('tph_refrac_flag', 'Refractivity-based TPH QC flag',                   &
                         ropp%Lev2c%units%tph_refrac_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('tph_tdry_lrt', 'Dry temperature-based TPH (lapse rate)',              &
                         ropp%Lev2c%units%tph_tdry_lrt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tpt_tdry_lrt', 'Dry temperature-based TPT (lapse rate)',              &
                         ropp%Lev2c%units%tpt_tdry_lrt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tph_tdry_lrt_flag', 'Dry temperature-based TPH QC flag (lapse rate)', &
                         ropp%Lev2c%units%tph_tdry_lrt_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('tph_tdry_cpt', 'Dry temperature-based TPH (cold point)',              &
                         ropp%Lev2c%units%tph_tdry_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tpt_tdry_cpt', 'Dry temperature-based TPT (cold point)',              &
                         ropp%Lev2c%units%tpt_tdry_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tph_tdry_cpt_flag', 'Dry temperature-based TPH QC flag (cold point)', &
                         ropp%Lev2c%units%tph_tdry_cpt_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('prh_tdry_cpt', 'Dry temperature-based PRH (cold point)',              &
                         ropp%Lev2c%units%prh_tdry_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('prt_tdry_cpt', 'Dry temperature-based PRT (cold point)',              &
                         ropp%Lev2c%units%prt_tdry_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('prh_tdry_cpt_flag', 'Dry temperature-based PRH QC flag (cold point)', &
                         ropp%Lev2c%units%prh_tdry_cpt_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('tph_temp_lrt', 'Temperature-based TPH (lapse rate)',                  &
                         ropp%Lev2c%units%tph_temp_lrt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tpt_temp_lrt', 'Temperature-based TPT (lapse rate)',                  &
                         ropp%Lev2c%units%tpt_temp_lrt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tph_temp_lrt_flag', 'Temperature-based TPH QC flag (lapse rate)',     &
                         ropp%Lev2c%units%tph_temp_lrt_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('tph_temp_cpt', 'Temperature-based TPH (cold point)',                  &
                         ropp%Lev2c%units%tph_temp_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tpt_temp_cpt', 'Temperature-based TPT (cold point)',                  &
                         ropp%Lev2c%units%tpt_temp_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('tph_temp_cpt_flag', 'Temperature-based TPH QC flag (cold point)',     &
                         ropp%Lev2c%units%tph_temp_cpt_flag, dimidul, TYPE = nf90_int)

    varid = ncdf_defvar('prh_temp_cpt', 'Temperature-based PRH (cold point)',                  &
                         ropp%Lev2c%units%prh_temp_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('prt_temp_cpt', 'Temperature-based PRT (cold point)',                  &
                         ropp%Lev2c%units%prt_temp_cpt, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('prh_temp_cpt_flag', 'Temperature-based PRH QC flag (cold point)',     &
                         ropp%Lev2c%units%prh_temp_cpt_flag, dimidul, TYPE = nf90_int)

!    1.16.2 Units

    CALL ut_convert(data%Lev2c%range%geop_sfc,        data%Lev2c%units%geop_sfc,        &
                    ropp%Lev2c%range%geop_sfc,        ropp%Lev2c%units%geop_sfc)
    CALL ut_convert(data%Lev2c%range%press_sfc,       data%Lev2c%units%press_sfc,       &
                    ropp%Lev2c%range%press_sfc,       ropp%Lev2c%units%press_sfc)
    CALL ut_convert(data%Lev2c%range%press_sfc_sigma, data%Lev2c%units%press_sfc_sigma, &
                    ropp%Lev2c%range%press_sfc_sigma, ropp%Lev2c%units%press_sfc_sigma)
    CALL ut_convert(data%Lev2c%range%press_sfc_qual,  data%Lev2c%units%press_sfc_qual,  &
                    ropp%Lev2c%range%press_sfc_qual,  ropp%Lev2c%units%press_sfc_qual)

    CALL ut_convert(data%Lev2c%range%tph_bangle, data%Lev2c%units%tph_bangle,           &
                    ropp%Lev2c%range%tph_bangle, ropp%Lev2c%units%tph_bangle)
    CALL ut_convert(data%Lev2c%range%tpa_bangle, data%Lev2c%units%tpa_bangle,           &
                    ropp%Lev2c%range%tpa_bangle, ropp%Lev2c%units%tpa_bangle)
    CALL ut_convert(data%Lev2c%range%tph_bangle_flag, data%Lev2c%units%tph_bangle_flag, &
                    ropp%Lev2c%range%tph_bangle_flag, ropp%Lev2c%units%tph_bangle_flag)

    CALL ut_convert(data%Lev2c%range%tph_refrac, data%Lev2c%units%tph_refrac,           &
                    ropp%Lev2c%range%tph_refrac, ropp%Lev2c%units%tph_refrac)
    CALL ut_convert(data%Lev2c%range%tpn_refrac, data%Lev2c%units%tpn_refrac,           &
                    ropp%Lev2c%range%tpn_refrac, ropp%Lev2c%units%tpn_refrac)
    CALL ut_convert(data%Lev2c%range%tph_refrac_flag, data%Lev2c%units%tph_refrac_flag, &
                    ropp%Lev2c%range%tph_refrac_flag, ropp%Lev2c%units%tph_refrac_flag)

    CALL ut_convert(data%Lev2c%range%tph_tdry_lrt, data%Lev2c%units%tph_tdry_lrt,           &
                    ropp%Lev2c%range%tph_tdry_lrt, ropp%Lev2c%units%tph_tdry_lrt)
    CALL ut_convert(data%Lev2c%range%tpt_tdry_lrt, data%Lev2c%units%tpt_tdry_lrt,           &
                    ropp%Lev2c%range%tpt_tdry_lrt, ropp%Lev2c%units%tpt_tdry_lrt)
    CALL ut_convert(data%Lev2c%range%tph_tdry_lrt_flag, data%Lev2c%units%tph_tdry_lrt_flag, &
                    ropp%Lev2c%range%tph_tdry_lrt_flag, ropp%Lev2c%units%tph_tdry_lrt_flag)

    CALL ut_convert(data%Lev2c%range%tph_tdry_cpt, data%Lev2c%units%tph_tdry_cpt,           &
                    ropp%Lev2c%range%tph_tdry_cpt, ropp%Lev2c%units%tph_tdry_cpt)
    CALL ut_convert(data%Lev2c%range%tpt_tdry_cpt, data%Lev2c%units%tpt_tdry_cpt,           &
                    ropp%Lev2c%range%tpt_tdry_cpt, ropp%Lev2c%units%tpt_tdry_cpt)
    CALL ut_convert(data%Lev2c%range%tph_tdry_cpt_flag, data%Lev2c%units%tph_tdry_cpt_flag, &
                    ropp%Lev2c%range%tph_tdry_cpt_flag, ropp%Lev2c%units%tph_tdry_cpt_flag)

    CALL ut_convert(data%Lev2c%range%prh_tdry_cpt, data%Lev2c%units%prh_tdry_cpt,           &
                    ropp%Lev2c%range%prh_tdry_cpt, ropp%Lev2c%units%prh_tdry_cpt)
    CALL ut_convert(data%Lev2c%range%prt_tdry_cpt, data%Lev2c%units%prt_tdry_cpt,           &
                    ropp%Lev2c%range%prt_tdry_cpt, ropp%Lev2c%units%prt_tdry_cpt)
    CALL ut_convert(data%Lev2c%range%prh_tdry_cpt_flag, data%Lev2c%units%prh_tdry_cpt_flag, &
                    ropp%Lev2c%range%prh_tdry_cpt_flag, ropp%Lev2c%units%prh_tdry_cpt_flag)

    CALL ut_convert(data%Lev2c%range%tph_temp_lrt, data%Lev2c%units%tph_temp_lrt,           &
                    ropp%Lev2c%range%tph_temp_lrt, ropp%Lev2c%units%tph_temp_lrt)
    CALL ut_convert(data%Lev2c%range%tpt_temp_lrt, data%Lev2c%units%tpt_temp_lrt,           &
                    ropp%Lev2c%range%tpt_temp_lrt, ropp%Lev2c%units%tpt_temp_lrt)
    CALL ut_convert(data%Lev2c%range%tph_temp_lrt_flag, data%Lev2c%units%tph_temp_lrt_flag, &
                    ropp%Lev2c%range%tph_temp_lrt_flag, ropp%Lev2c%units%tph_temp_lrt_flag)

    CALL ut_convert(data%Lev2c%range%tph_temp_cpt, data%Lev2c%units%tph_temp_cpt,           &
                    ropp%Lev2c%range%tph_temp_cpt, ropp%Lev2c%units%tph_temp_cpt)
    CALL ut_convert(data%Lev2c%range%tpt_temp_cpt, data%Lev2c%units%tpt_temp_cpt,           &
                    ropp%Lev2c%range%tpt_temp_cpt, ropp%Lev2c%units%tpt_temp_cpt)
    CALL ut_convert(data%Lev2c%range%tph_temp_cpt_flag, data%Lev2c%units%tph_temp_cpt_flag, &
                    ropp%Lev2c%range%tph_temp_cpt_flag, ropp%Lev2c%units%tph_temp_cpt_flag)

    CALL ut_convert(data%Lev2c%range%prh_temp_cpt, data%Lev2c%units%prh_temp_cpt,           &
                    ropp%Lev2c%range%prh_temp_cpt, ropp%Lev2c%units%prh_temp_cpt)
    CALL ut_convert(data%Lev2c%range%prt_temp_cpt, data%Lev2c%units%prt_temp_cpt,           &
                    ropp%Lev2c%range%prt_temp_cpt, ropp%Lev2c%units%prt_temp_cpt)
    CALL ut_convert(data%Lev2c%range%prh_temp_cpt_flag, data%Lev2c%units%prh_temp_cpt_flag, &
                    ropp%Lev2c%range%prh_temp_cpt_flag, ropp%Lev2c%units%prh_temp_cpt_flag)

!    1.16.3 Valid ranges

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%geop_sfc,          varname = 'geop_sfc')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc,         varname = 'press_sfc')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc_sigma,   varname = 'press_sfc_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc_qual,    varname = 'press_sfc_qual')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_bangle,        varname = 'tph_bangle')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpa_bangle,        varname = 'tpa_bangle')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_bangle_flag,   varname = 'tph_bangle_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_refrac,        varname = 'tph_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpn_refrac,        varname = 'tpn_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_refrac_flag,   varname = 'tph_refrac_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_tdry_lrt,      varname = 'tph_tdry_lrt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpt_tdry_lrt,      varname = 'tpt_tdry_lrt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_tdry_lrt_flag, varname = 'tph_tdry_lrt_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_tdry_cpt,      varname = 'tph_tdry_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpt_tdry_cpt,      varname = 'tpt_tdry_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_tdry_cpt_flag, varname = 'tph_tdry_cpt_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prh_tdry_cpt,      varname = 'tph_tdry_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prt_tdry_cpt,      varname = 'tpt_tdry_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prh_tdry_cpt_flag, varname = 'tph_tdry_cpt_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_temp_lrt,      varname = 'tph_temp_lrt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpt_temp_lrt,      varname = 'tpt_temp_lrt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_temp_lrt_flag, varname = 'tph_temp_lrt_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_temp_cpt,      varname = 'tph_temp_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tpt_temp_cpt,      varname = 'tpt_temp_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%tph_temp_cpt_flag, varname = 'tph_temp_cpt_flag')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prh_temp_cpt,      varname = 'tph_temp_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prt_temp_cpt,      varname = 'tpt_temp_cpt')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%prh_temp_cpt_flag, varname = 'tph_temp_cpt_flag')

  ENDIF


! 1.17 Level 2d variables (if any)
! --------------------------------

  IF (data%Lev2d%Npoints > 0) THEN

!    1.17.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2d', data%Lev2d%Npoints), dimidul(1) /)

!    1.17.2 Variables

    varid = ncdf_defvar('level_type', 'Vertical level type', &
                        '', dimid64, TYPE = nf90_char)
    varid = ncdf_defvar('level_coeff_a', 'Hybrid / Eta level coefficient (a or eta)', &
                        ropp%Lev2d%units%level_coeff_a, dimid, nf90_type)
    varid = ncdf_defvar('level_coeff_b', 'Hybrid / Eta level coefficient (b or tau)', &
                        ropp%Lev2d%units%level_coeff_b, dimid, nf90_type)

!    1.17.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev2d%range%level_coeff_a, data%Lev2d%units%level_coeff_a, &
                    ropp%Lev2d%range%level_coeff_a, ropp%Lev2d%units%level_coeff_a)
    CALL ut_convert(data%Lev2d%range%level_coeff_b, data%Lev2d%units%level_coeff_b, &
                    ropp%Lev2d%range%level_coeff_b, ropp%Lev2d%units%level_coeff_b)

    CALL ncdf_putatt('valid_range', ropp%Lev2d%range%level_coeff_a, varname = 'level_coeff_a')
    CALL ncdf_putatt('valid_range', ropp%Lev2d%range%level_coeff_b, varname = 'level_coeff_b')

  ENDIF

! 1.18 Additional variables
! -------------------------

  IF (SIZE(data%vlist%VlistD0d) > 0) THEN
    CALL ropp_io_write_def_vlistD0d(data%vlist%VlistD0d, output_prec)
  ENDIF

  IF (SIZE(data%vlist%VlistD1d) > 0) THEN
    CALL ropp_io_write_def_vlistD1d(data%vlist%VlistD1d, output_prec)
  ENDIF

  IF (SIZE(data%vlist%VlistD2d) > 0) THEN
    CALL ropp_io_write_def_vlistD2d(data%vlist%VlistD2d, output_prec)
  ENDIF


! 1.19 Switch into data mode
! --------------------------

  status = nf90_enddef(ncdf_ncid)

! 1.19 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_def_rodata


!-------------------------------------------------------------------------------
! 2. Core RO data (two-dimensional data)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_io_write_def_rodata_2d(DATA, output_precision)

! 2.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_def_rodata_2d
  USE ropp_io_types, ONLY: ROprof,     &
                           ROprof2d,   &
                           ThisFmtVer, &
                           PCD_Occultation

  IMPLICIT NONE
 
  TYPE(ROprof2d)               :: DATA
  CHARACTER(len = *), OPTIONAL :: output_precision

  INTEGER                      :: status
  INTEGER                      :: nf90_type

  INTEGER                      :: varid
  INTEGER, DIMENSION(1)        :: dimidul
  INTEGER, DIMENSION(2)        :: dimid04
  INTEGER, DIMENSION(2)        :: dimid20
  INTEGER, DIMENSION(2)        :: dimid40
  INTEGER, DIMENSION(2)        :: dimid64
  INTEGER, DIMENSION(2)        :: dimid
  INTEGER, DIMENSION(2)        :: dimid_xyz
  INTEGER, DIMENSION(2)        :: dimid_horiz
  INTEGER, DIMENSION(3)        :: dimid2d

  CHARACTER(len =  23)         :: proc_date
  CHARACTER(len =  64)         :: output_prec
  CHARACTER(len = 256)         :: routine

  TYPE(ROprof)                 :: ropp    ! ROPP default (standard) attributes


! 2.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_def')

! 2.3 Default arguments
! ---------------------

  IF (PRESENT(output_precision)) THEN
     output_prec = output_precision
  ELSE
     output_prec = 'float'
  ENDIF

! 2.4 Change into define mode
! ---------------------------

  status = nf90_redef(ncdf_ncid)

! 2.5 Precision of the output
! ---------------------------

  SELECT CASE (TRIM(output_prec))
    CASE('float', 'single')
      nf90_type = nf90_float
    CASE('double')
      nf90_type = nf90_double
    CASE default
      CALL message(msg_warn, &
           'Unknown ncdf_output_precision: ' // TRIM(output_prec) // '\n' // &
           '   Using default (float) precision.')
      nf90_type = nf90_float
  END SELECT

! 2.6 Global attributes
! ---------------------

  IF (BTEST(data%PCD, PCD_occultation)) THEN ! Background data
    CALL ncdf_putatt('title', 'Atmospheric background data for ROPP Radio Occultation data')
    CALL ncdf_putatt('institution', data%bg%source)
  ELSE
    CALL ncdf_putatt('title', 'ROPP Radio Occultation data')
    CALL ncdf_putatt('institution', data%processing_centre)
  ENDIF
  CALL ncdf_putatt('Conventions', 'CF-1.0')

  WRITE(proc_date, "(i4.4,2('-',i2.2),' ',i2.2,2(':',i2.2),'.',i3.3)") &
       data%DTpro%Year,   &
       data%DTpro%Month,  &
       data%DTpro%Day,    &
       data%DTpro%Hour,   &
       data%DTpro%Minute, &
       data%DTpro%Second, &
       data%DTpro%Msec

  CALL ncdf_putatt('format_version',    ThisFmtVer)
  CALL ncdf_putatt('processing_centre', data%processing_centre)
  CALL ncdf_putatt('processing_date',   proc_date)
  CALL ncdf_putatt('pod_method',        data%pod_method)
  CALL ncdf_putatt('phase_method',      data%phase_method)
  CALL ncdf_putatt('bangle_method',     data%bangle_method)
  CALL ncdf_putatt('refrac_method',     data%refrac_method)
  CALL ncdf_putatt('meteo_method',      data%meteo_method)
  CALL ncdf_putatt('thin_method',       data%thin_method)
  CALL ncdf_putatt('software_version',  data%software_version)

! 2.7 Header variables
! --------------------

! 2.7.1 Dimensions

  dimidul = ncdf_defdim('dim_unlim', nf90_unlimited)

  dimid04 = (/ ncdf_defdim('dim_char04',  5), dimidul(1) /)  ! plus char(0)!
  dimid20 = (/ ncdf_defdim('dim_char20', 21), dimidul(1) /)
  dimid40 = (/ ncdf_defdim('dim_char40', 41), dimidul(1) /)
  dimid64 = (/ ncdf_defdim('dim_char64', 65), dimidul(1) /)

! 2.7.2 Variables

  varid = ncdf_defvar('occ_id', 'Occultation ID',    &
                      '', dimid40, TYPE = nf90_char)
  varid = ncdf_defvar('gns_id', 'GNSS satellite ID', &
                      '', dimid04, TYPE = nf90_char)
  varid = ncdf_defvar('leo_id', 'LEO satellite ID',  &
                      '', dimid04, TYPE = nf90_char)
  varid = ncdf_defvar('stn_id', 'Ground station ID', &
                      '', dimid04, TYPE = nf90_char)

! 2.8 Date and time
! -----------------

! 2.8.1 Derived time value

  varid = ncdf_defvar('start_time', 'Starting time for the occultation', &
                      'seconds since 2000-01-01 00:00:00', dimidul, TYPE = nf90_double)

! 2.8.2 Elements of the data structure

  varid = ncdf_defvar('year',   'Year',        ropp%DTocc%units%year,   dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('month',  'Month',       ropp%DTocc%units%month,  dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('day',    'Day',         ropp%DTocc%units%day,    dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('hour',   'Hour',        ropp%DTocc%units%hour,   dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('minute', 'Minute',      ropp%DTocc%units%minute, dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('second', 'Second',      ropp%DTocc%units%second, dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('msec',   'Millisecond', ropp%DTocc%units%msec,   dimidul, TYPE = nf90_int)

! 2.8.3 Valid ranges (converted to ROPP standard units)

  CALL ut_convert(data%DTocc%range%year,   data%DTocc%units%year,   &
                  ropp%DTocc%range%year,   ropp%DTocc%units%year)
  CALL ut_convert(data%DTocc%range%month,  data%DTocc%units%month,  &
                  ropp%DTocc%range%month,  ropp%DTocc%units%month)
  CALL ut_convert(data%DTocc%range%day,    data%DTocc%units%day,    &
                  ropp%DTocc%range%day,    ropp%DTocc%units%day)
  CALL ut_convert(data%DTocc%range%hour,   data%DTocc%units%hour,   &
                  ropp%DTocc%range%hour,   ropp%DTocc%units%hour)
  CALL ut_convert(data%DTocc%range%minute, data%DTocc%units%minute, &
                  ropp%DTocc%range%minute, ropp%DTocc%units%minute)
  CALL ut_convert(data%DTocc%range%second, data%DTocc%units%second, &
                  ropp%DTocc%range%second, ropp%DTocc%units%second)
  CALL ut_convert(data%DTocc%range%msec,   data%DTocc%units%msec,   &
                  ropp%DTocc%range%msec,   ropp%DTocc%units%msec)

  CALL ncdf_putatt('valid_range', ropp%DTocc%range%year,   varname = 'year')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%month,  varname = 'month')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%day,    varname = 'day')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%hour,   varname = 'hour')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%minute, varname = 'minute')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%second, varname = 'second')
  CALL ncdf_putatt('valid_range', ropp%DTocc%range%msec,   varname = 'msec')

! 2.9 Overall quality
! -------------------

! 2.9.1 Elements of the data structure

  varid = ncdf_defvar('pcd', 'Product Confidence Data', &
                      ropp%units%pcd,          dimidul, TYPE = nf90_int)
  varid = ncdf_defvar('overall_qual', 'Overall quality',      &
                      ropp%units%overall_qual, dimidul, TYPE = nf90_type)

! 2.9.2 Valid ranges

  CALL ut_convert(data%range%pcd,          data%units%pcd,          &
                  ropp%range%pcd,          ropp%units%pcd)
  CALL ut_convert(data%range%overall_qual, data%units%overall_qual, &
                  ropp%range%overall_qual, ropp%units%overall_qual)

  CALL ncdf_putatt('valid_range', ropp%range%pcd,          varname = 'pcd')
  CALL ncdf_putatt('valid_range', ropp%range%overall_qual, varname = 'overall_qual')

! 2.10 Georeferencing
! ------------------

! 2.10.1 Dimensions

  dimid_xyz = (/ ncdf_defdim('xyz', 3), dimidul(1) /)

! 2.10.2 Derived time

  varid = ncdf_defvar('time', 'Reference time for the occultation',                &
                      'seconds since 2000-01-01 00:00:00', dimidul, TYPE = nf90_double)

! 2.10.3 Elements of the data struture

  varid = ncdf_defvar('time_offset', 'Time offset for georeferencing (since start of occ.)', &
                      ropp%georef%units%time_offset, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('lat', 'Reference latitude for the occultation',                       &
                      ropp%georef%units%lat, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('lon', 'Reference longitude for the occultation',                      &
                      ropp%georef%units%lon, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('undulation', 'Geoid undulation for the reference coordinate',         &
                      ropp%georef%units%Undulation, dimidul, TYPE = nf90_type)
  varid = ncdf_defvar('roc', 'Radius of curvature for the reference coordinate',             &
                      ropp%georef%units%roc, dimidul, TYPE = nf90_double)
  varid = ncdf_defvar('r_coc', 'Centre of curvature for the reference coordinate',           &
                      ropp%georef%units%r_coc, dimid_xyz, TYPE = nf90_type)
  varid = ncdf_defvar('azimuth', 'GNSS->LEO line of sight angle (from True North) for the reference coordinate', &
                      ropp%georef%units%azimuth, dimidul, TYPE = nf90_type)

! 2.10.4 Valid ranges (converted to ROPP standard units)

  CALL ut_convert(data%georef%range%time_offset, data%georef%units%time_offset, &
                  ropp%georef%range%time_offset, ropp%georef%units%time_offset)
  CALL ut_convert(data%georef%range%lat,         data%georef%units%lat,         &
                  ropp%georef%range%lat,         ropp%georef%units%lat)
  CALL ut_convert(data%georef%range%lon,         data%georef%units%lon,         &
                  ropp%georef%range%lon,         ropp%georef%units%lon)
  CALL ut_convert(data%georef%range%undulation,  data%georef%units%undulation,  &
                  ropp%georef%range%undulation,  ropp%georef%units%undulation)
  CALL ut_convert(data%georef%range%roc,         data%georef%units%roc,         &
                  ropp%georef%range%roc,         ropp%georef%units%roc)
  CALL ut_convert(data%georef%range%r_coc,       data%georef%units%r_coc,       &
                  ropp%georef%range%r_coc,       ropp%georef%units%r_coc)
  CALL ut_convert(data%georef%range%azimuth,     data%georef%units%azimuth,     &
                  ropp%georef%range%azimuth,     ropp%georef%units%azimuth)

  CALL ncdf_putatt('valid_range', ropp%georef%range%time_offset, varname = 'time_offset')
  CALL ncdf_putatt('valid_range', ropp%georef%range%lat,         varname = 'lat')
  CALL ncdf_putatt('valid_range', ropp%georef%range%lon,         varname = 'lon')
  CALL ncdf_putatt('valid_range', ropp%georef%range%undulation,  varname = 'undulation')
  CALL ncdf_putatt('valid_range', ropp%georef%range%roc,         varname = 'roc')
  CALL ncdf_putatt('valid_range', ropp%georef%range%r_coc,       varname = 'r_coc')
  CALL ncdf_putatt('valid_range', ropp%georef%range%azimuth,     varname = 'azimuth')

! 2.10.5 Other attributes

  CALL ncdf_putatt('reference_frame', data%georef%reference_frame%r_coc, varname = 'r_coc')

! 2.11 Background characterisation (if any)
! -----------------------------------------

  IF (data%BG%Source /= 'NONE') THEN

! 2.11.1 Variables

    varid = ncdf_defvar('bg_source', 'Background data source',  &
                        '', dimid20, TYPE = nf90_char)
    varid = ncdf_defvar('bg_year',      'VT year',              &
                        ropp%BG%units%year,     dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_month',     'VT month',             &
                        ropp%BG%units%month,    dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_day',       'VT day',               &
                        ropp%BG%units%day,      dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_hour',      'VT hour',              &
                        ropp%BG%units%hour,     dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_minute',    'VT minute',            &
                        ropp%BG%units%minute,   dimidul, TYPE = nf90_int)
    varid = ncdf_defvar('bg_fcperiod',  'Forecast period',      &
                        ropp%BG%units%fcPeriod, dimidul, TYPE = nf90_type)

! 2.11.2 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%BG%range%year,      data%BG%units%year,      &
                    ropp%BG%range%year,      ropp%BG%units%year)
    CALL ut_convert(data%BG%range%month,     data%BG%units%month,     &
                    ropp%BG%range%month,     ropp%BG%units%month)
    CALL ut_convert(data%BG%range%day,       data%BG%units%day,       &
                    ropp%BG%range%day,       ropp%BG%units%day)
    CALL ut_convert(data%BG%range%hour,      data%BG%units%hour,      &
                    ropp%BG%range%hour,      ropp%BG%units%hour)
    CALL ut_convert(data%BG%range%minute,    data%BG%units%minute,    &
                    ropp%BG%range%minute,    ropp%BG%units%minute)
    CALL ut_convert(data%BG%range%fcPeriod, data%BG%units%fcPeriod, &
                    ropp%BG%range%fcPeriod, ropp%BG%units%fcPeriod)

    CALL ncdf_putatt('valid_range', ropp%BG%range%year,     varname = 'bg_year')
    CALL ncdf_putatt('valid_range', ropp%BG%range%month,    varname = 'bg_month')
    CALL ncdf_putatt('valid_range', ropp%BG%range%day,      varname = 'bg_day')
    CALL ncdf_putatt('valid_range', ropp%BG%range%hour,     varname = 'bg_hour')
    CALL ncdf_putatt('valid_range', ropp%BG%range%minute,   varname = 'bg_minute')
    CALL ncdf_putatt('valid_range', ropp%BG%range%fcPeriod, varname = 'bg_fcperiod')
  ENDIF

! 2.12 Level 1a variables (if any)
! --------------------------------

  IF (data%Lev1a%Npoints > 0) THEN

!    2.12.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev1a', data%Lev1a%Npoints), dimidul(1) /)
    dimid2d(1) = dimid(1)
    dimid2d(2) = dimid_xyz(1)
    dimid2d(3) = dimidul(1)

!    2.12.2 Variables

    varid = ncdf_defvar('dtime', 'Time since start of occultation',         &
                         ropp%Lev1a%units%dtime, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('snr_L1ca', 'Signal-to-noise ratio (L1, C/A code)', &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('snr_L1p', 'Signal-to-noise ratio (L1, P code)',    &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('snr_L2p', 'Signal-to-noise ratio (L2, P code)',    &
                         ropp%Lev1a%units%snr, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('phase_L1', 'Excess phase (L1)',                    &
                         ropp%Lev1a%units%phase, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('phase_L2', 'Excess phase (L2)',                    &
                         ropp%Lev1a%units%phase, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('r_gns', 'GNSS transmitter position',               &
                         ropp%Lev1a%units%r_gns, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('v_gns', 'GNSS transmitter velocity',               &
                         ropp%Lev1a%units%v_gns, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('r_leo', 'LEO transmitter position',                &
                         ropp%Lev1a%units%r_leo, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('v_leo', 'LEO transmitter velocity',                &
                         ropp%Lev1a%units%v_leo, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('phase_qual', 'Quality value for phase (and SNR)',  &
                          ropp%Lev1a%units%phase_qual, dimid, TYPE = nf90_type)

!    2.12.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev1a%range%dtime,      data%Lev1a%units%dtime,      &
                    ropp%Lev1a%range%dtime,      ropp%Lev1a%units%dtime)
    CALL ut_convert(data%Lev1a%range%snr,        data%Lev1a%units%snr,        &
                    ropp%Lev1a%range%snr,        ropp%Lev1a%units%snr)
    CALL ut_convert(data%Lev1a%range%phase,      data%Lev1a%units%phase,      &
                    ropp%Lev1a%range%phase,      ropp%Lev1a%units%phase)
    CALL ut_convert(data%Lev1a%range%r_gns,      data%Lev1a%units%r_gns,      &
                    ropp%Lev1a%range%r_gns,      ropp%Lev1a%units%r_gns)
    CALL ut_convert(data%Lev1a%range%v_gns,      data%Lev1a%units%v_gns,      &
                    ropp%Lev1a%range%v_gns,      ropp%Lev1a%units%v_gns)
    CALL ut_convert(data%Lev1a%range%r_leo,      data%Lev1a%units%r_leo,      &
                    ropp%Lev1a%range%r_leo,      ropp%Lev1a%units%r_leo)
    CALL ut_convert(data%Lev1a%range%v_leo,      data%Lev1a%units%v_leo,      &
                    ropp%Lev1a%range%v_leo,      ropp%Lev1a%units%v_leo)
    CALL ut_convert(data%Lev1a%range%phase_qual, data%Lev1a%units%phase_qual, &
                    ropp%Lev1a%range%phase_qual, ropp%Lev1a%units%phase_qual)


    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%dtime,      varname = 'dtime')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L1ca')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L1p')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%snr,        varname = 'snr_L2p')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase,      varname = 'phase_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase,      varname = 'phase_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%r_gns,      varname = 'r_gns')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%v_gns,      varname = 'v_gns')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%r_leo,      varname = 'r_leo')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%v_leo,      varname = 'v_leo')
    CALL ncdf_putatt('valid_range', ropp%Lev1a%range%phase_qual, varname = 'phase_qual')

!    2.12.4 Other attributes

    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%r_gns, varname = 'r_gns')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%v_gns, varname = 'v_gns')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%r_leo, varname = 'r_leo')
    CALL ncdf_putatt('reference_frame', data%Lev1a%reference_frame%v_leo, varname = 'v_leo')

  ENDIF

! 2.13 Level 1b variables (if any)
! --------------------------------

  IF (data%Lev1b%Npoints > 0) THEN

!    2.13.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev1b', data%Lev1b%Npoints), dimidul(1) /)

!    2.13.2 Variables

    varid = ncdf_defvar('lat_tp', 'Latitudes for tangent points',               &
                         ropp%Lev1b%units%lat_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('lon_tp', 'Longitudes for tangent points',              &
                         ropp%Lev1b%units%lon_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('azimuth_tp', 'GNSS->LEO line of sight angles (from True North) for tangent points', &
                         ropp%Lev1b%units%azimuth_tp, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('impact_L1', 'Impact parameter (L1)',                   &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact_L2', 'Impact parameter (L2)',                   &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact', 'Impact parameter (generic)',                 &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('impact_opt', 'Impact parameter (optimised)',           &
                         ropp%Lev1b%units%impact, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1', 'Bending angle (L1)',                      &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L2', 'Bending angle (L2)',                      &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle', 'Bending angle (generic)',                    &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_opt', 'Bending angle (optimised)',              &
                         ropp%Lev1b%units%bangle, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1_sigma', 'Estimated error (1-sigma) for bending angles (L1)',   &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L2_sigma', 'Estimated error (1-sigma) for bending angles (L2)',   &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_sigma', 'Estimated error (1-sigma) for bending angles (generic)', &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_opt_sigma', 'Estimated error (1-sigma) for bending angles (optimised)', &
                         ropp%Lev1b%units%bangle_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('bangle_L1_qual', 'Bending angle quality value (L1)',   &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_L2_qual', 'Bending angle quality value (L2)',   &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_qual', 'Bending angle quality value (generic)', &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('bangle_opt_qual', 'Bending angle quality value (optimised)', &
                         ropp%Lev1b%units%bangle_qual, dimid, TYPE = nf90_type)

!    2.13.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev1b%range%lat_tp,      data%Lev1b%units%lat_tp,      &
                    ropp%Lev1b%range%lat_tp,      ropp%Lev1b%units%lat_tp)
    CALL ut_convert(data%Lev1b%range%lon_tp,      data%Lev1b%units%lon_tp,      &
                    ropp%Lev1b%range%lon_tp,      ropp%Lev1b%units%lon_tp)
    CALL ut_convert(data%Lev1b%range%azimuth_tp,  data%Lev1b%units%azimuth_tp,  &
                    ropp%Lev1b%range%azimuth_tp,  ropp%Lev1b%units%azimuth_tp)
    CALL ut_convert(data%Lev1b%range%impact,      data%Lev1b%units%impact,      &
                    ropp%Lev1b%range%impact,      ropp%Lev1b%units%impact)
    CALL ut_convert(data%Lev1b%range%bangle,      data%Lev1b%units%bangle,      &
                    ropp%Lev1b%range%bangle,      ropp%Lev1b%units%bangle)
    CALL ut_convert(data%Lev1b%range%bangle_sigma, data%Lev1b%units%bangle_sigma, &
                    ropp%Lev1b%range%bangle_sigma, ropp%Lev1b%units%bangle_sigma)
    CALL ut_convert(data%Lev1b%range%bangle_qual,  data%Lev1b%units%bangle_qual,  &
                    ropp%Lev1b%range%bangle_qual,  ropp%Lev1b%units%bangle_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%lat_tp,       varname = 'lat_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%lon_tp,       varname = 'lon_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%azimuth_tp,   varname = 'azimuth_tp')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%impact,       varname = 'impact_opt')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_L1')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_L2')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle,       varname = 'bangle_opt')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_L1_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_L2_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_sigma, varname = 'bangle_opt_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_L1_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_L2_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev1b%range%bangle_qual,  varname = 'bangle_opt_qual')

  ENDIF

! 2.14 Level 2a variables (if any)
! --------------------------------

  IF (data%Lev2a%Npoints > 0) THEN

!    2.14.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2a', data%Lev2a%Npoints), dimidul(1) /)

!    2.14.2 Variables

    varid = ncdf_defvar('alt_refrac', 'Geometric height above geoid for refractivity',     &
                         ropp%Lev2a%units%alt_refrac, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('geop_refrac', 'Geopotential height above geoid for refractivity', &
                         ropp%Lev2a%units%geop_refrac, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('refrac', 'Refractivity',                                          &
                         ropp%Lev2a%units%refrac, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('refrac_sigma', 'Estimated error (1-sigma) for refractivity',      &
                         ropp%Lev2a%units%refrac_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('refrac_qual', 'Quality value for refractivity',                   &
                          ropp%Lev2a%units%refrac_qual, dimid, TYPE = nf90_type)
    varid = ncdf_defvar('dry_temp', 'Dry temperature',                                     &
                         ropp%Lev2a%units%dry_temp, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('dry_temp_sigma', 'Estimated error (1-sigma) for dry temperature', &
                         ropp%Lev2a%units%dry_temp_sigma, dimid, TYPE = nf90_double)
    varid = ncdf_defvar('dry_temp_qual', 'Quality value for dry temperature',              &
                          ropp%Lev2a%units%dry_temp_qual, dimid, TYPE = nf90_type)

!    2.14.3 Valid ranges

    CALL ut_convert(data%Lev2a%range%alt_refrac,     data%Lev2a%units%alt_refrac,     &
                    ropp%Lev2a%range%alt_refrac,     ropp%Lev2a%units%alt_refrac)
    CALL ut_convert(data%Lev2a%range%geop_refrac,    data%Lev2a%units%geop_refrac,    &
                    ropp%Lev2a%range%geop_refrac,    ropp%Lev2a%units%geop_refrac)
    CALL ut_convert(data%Lev2a%range%refrac,         data%Lev2a%units%refrac,         &
                    ropp%Lev2a%range%refrac,         ropp%Lev2a%units%refrac)
    CALL ut_convert(data%Lev2a%range%refrac_sigma,   data%Lev2a%units%refrac_sigma,   &
                    ropp%Lev2a%range%refrac_sigma,   ropp%Lev2a%units%refrac_sigma)
    CALL ut_convert(data%Lev2a%range%refrac_qual,    data%Lev2a%units%refrac_qual,    &
                    ropp%Lev2a%range%refrac_qual,    ropp%Lev2a%units%refrac_qual)
    CALL ut_convert(data%Lev2a%range%dry_temp,       data%Lev2a%units%dry_temp,       &
                    ropp%Lev2a%range%dry_temp,       ropp%Lev2a%units%dry_temp)
    CALL ut_convert(data%Lev2a%range%dry_temp_sigma, data%Lev2a%units%dry_temp_sigma, &
                    ropp%Lev2a%range%dry_temp_sigma, ropp%Lev2a%units%dry_temp_sigma)
    CALL ut_convert(data%Lev2a%range%dry_temp_qual,  data%Lev2a%units%dry_temp_qual,  &
                    ropp%Lev2a%range%dry_temp_qual,  ropp%Lev2a%units%dry_temp_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%alt_refrac,     varname = 'alt_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%geop_refrac,    varname = 'geop_refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac,         varname = 'refrac')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac_sigma,   varname = 'refrac_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%refrac_qual,    varname = 'refrac_qual')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp,       varname = 'dry_temp')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp_sigma, varname = 'dry_temp_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2a%range%dry_temp_qual,  varname = 'dry_temp_qual')

  ENDIF

! 2.15 Level 2b variables (if any)
! --------------------------------

  IF (data%Lev2b%Npoints > 0) THEN

!    2.15.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2b', data%Lev2b%Npoints), dimidul(1) /)
    dimid_horiz = (/ ncdf_defdim('dim_horiz', data%Lev2b%Nhoriz), dimidul(1) /)
    dimid2d(1) = dimid(1)
    dimid2d(2) = dimid_horiz(1)
    dimid2d(3) = dimidul(1)


!    2.15.2 Variables

    varid = ncdf_defvar('geop', 'Geopotential height above geoid for P,T,H',      &
                         ropp%Lev2b%units%geop, dimid2d, TYPE = nf90_type)
    varid = ncdf_defvar('geop_sigma', 'Estimated error (1-sigma) for geopotential height', &
                         ropp%Lev2b%units%geop_sigma, dimid2d, TYPE = nf90_type)
    varid = ncdf_defvar('press', 'Pressure',                                       &
                         ropp%Lev2b%units%press, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('press_sigma', 'Estimated error (1-sigma) for pressure',   &
                         ropp%Lev2b%units%press_sigma, dimid2d, TYPE = nf90_type)
    varid = ncdf_defvar('temp', 'Temperature',                                     &
                         ropp%Lev2b%units%temp, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('temp_sigma', 'Estimated error (1-sigma) for temperature', &
                         ropp%Lev2b%units%temp_sigma, dimid2d, TYPE = nf90_type)
    varid = ncdf_defvar('shum', 'Specific humidity',                               &
                         ropp%Lev2b%units%shum, dimid2d, TYPE = nf90_double)
    varid = ncdf_defvar('shum_sigma', 'Estimated  error (1-sigma) in specific humidity', &
                         ropp%Lev2b%units%shum_sigma, dimid2d, TYPE = nf90_type)
    varid = ncdf_defvar('meteo_qual', 'Quality value for meteorological data',     &
                          ropp%Lev2b%units%meteo_qual, dimid2d, TYPE = nf90_type)

!    2.15.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev2b%range%geop,        data%Lev2b%units%geop,        &
                    ropp%Lev2b%range%geop,        ropp%Lev2b%units%geop)
    CALL ut_convert(data%Lev2b%range%geop_sigma,  data%Lev2b%units%geop_sigma,  &
                    ropp%Lev2b%range%geop_sigma,  ropp%Lev2b%units%geop_sigma)
    CALL ut_convert(data%Lev2b%range%press,       data%Lev2b%units%press,       &
                    ropp%Lev2b%range%press,       ropp%Lev2b%units%press)
    CALL ut_convert(data%Lev2b%range%press_sigma, data%Lev2b%units%press_sigma, &
                    ropp%Lev2b%range%press_sigma, ropp%Lev2b%units%press_sigma)
    CALL ut_convert(data%Lev2b%range%temp,        data%Lev2b%units%temp,        &
                    ropp%Lev2b%range%temp,        ropp%Lev2b%units%temp)
    CALL ut_convert(data%Lev2b%range%temp_sigma,  data%Lev2b%units%temp_sigma,  &
                    ropp%Lev2b%range%temp_sigma,  ropp%Lev2b%units%temp_sigma )
    CALL ut_convert(data%Lev2b%range%shum,        data%Lev2b%units%shum,        &
                    ropp%Lev2b%range%shum,        ropp%Lev2b%units%shum)
    CALL ut_convert(data%Lev2b%range%shum_sigma,  data%Lev2b%units%shum_sigma,  &
                    ropp%Lev2b%range%shum_sigma,  ropp%Lev2b%units%shum_sigma)
    CALL ut_convert(data%Lev2b%range%meteo_qual,  data%Lev2b%units%meteo_qual,  &
                    ropp%Lev2b%range%meteo_qual,  ropp%Lev2b%units%meteo_qual)

    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%geop,        varname = 'geop')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%geop_sigma,  varname = 'geop_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%press,       varname = 'press')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%press_sigma, varname = 'press_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%temp,        varname = 'temp')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%temp_sigma,  varname = 'temp_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%shum,        varname = 'shum')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%shum_sigma,  varname = 'shum_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2b%range%meteo_qual,  varname = 'meteo_qual')

  ENDIF

! 2.16 Level 2c variables (if any)
! --------------------------------

  IF (data%Lev2c%Npoints > 0) THEN

!    2.16.1 Variables

! new 2d variables

    varid = ncdf_defvar('dtheta', 'Separation of points',                           &
                         ropp%Lev2c%units%dtheta, dimidul, TYPE = nf90_type)
    varid = ncdf_defvar('lat_2d', 'Latitude of points',                           &
                         ropp%Lev2c%units%lat_2d, dimid_horiz, TYPE = nf90_type)
    varid = ncdf_defvar('lon_2d', 'Longitude of points',                           &
                         ropp%Lev2c%units%lon_2d, dimid_horiz, TYPE = nf90_type)

    varid = ncdf_defvar('geop_sfc', 'Surface geopotential height',                           &
                         ropp%Lev2c%units%geop_sfc, dimid_horiz, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc', 'Surface pressure',                                     &
                         ropp%Lev2c%units%press_sfc, dimid_horiz, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc_sigma', 'Estimated error (1-sigma) for surface pressure', &
                         ropp%Lev2c%units%press_sfc_sigma, dimid_horiz, TYPE = nf90_type)
    varid = ncdf_defvar('press_sfc_qual', 'Surface pressure quality value',                  &
                         ropp%Lev2c%units%press_sfc_qual, dimid_horiz, TYPE = nf90_type)

!    2.16.2 Valid ranges

! new 2d variables

    CALL ut_convert(data%Lev2c%range%dtheta,        data%Lev2c%units%dtheta,        &
                    ropp%Lev2c%range%dtheta,        ropp%Lev2c%units%dtheta)
    CALL ut_convert(data%Lev2c%range%lat_2d,        data%Lev2c%units%lat_2d,        &
                    ropp%Lev2c%range%lat_2d,        ropp%Lev2c%units%lat_2d)
    CALL ut_convert(data%Lev2c%range%lon_2d,        data%Lev2c%units%lon_2d,        &
                    ropp%Lev2c%range%lon_2d,        ropp%Lev2c%units%lon_2d)

    CALL ut_convert(data%Lev2c%range%geop_sfc,        data%Lev2c%units%geop_sfc,        &
                    ropp%Lev2c%range%geop_sfc,        ropp%Lev2c%units%geop_sfc)
    CALL ut_convert(data%Lev2c%range%press_sfc,       data%Lev2c%units%press_sfc,       &
                    ropp%Lev2c%range%press_sfc,       ropp%Lev2c%units%press_sfc)
    CALL ut_convert(data%Lev2c%range%press_sfc_sigma, data%Lev2c%units%press_sfc_sigma, &
                    ropp%Lev2c%range%press_sfc_sigma, ropp%Lev2c%units%press_sfc_sigma)
    CALL ut_convert(data%Lev2c%range%press_sfc_qual,  data%Lev2c%units%press_sfc_qual,  &
                    ropp%Lev2c%range%press_sfc_qual,  ropp%Lev2c%units%press_sfc_qual)

! new 2d variables

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%dtheta,        varname = 'dtheta')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%lat_2d,        varname = 'lat_2d')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%lon_2d,        varname = 'lon_2d')

    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%geop_sfc,        varname = 'geop_sfc')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc,       varname = 'press_sfc')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc_sigma, varname = 'press_sfc_sigma')
    CALL ncdf_putatt('valid_range', ropp%Lev2c%range%press_sfc_qual,  varname = 'press_sfc_qual')

  ENDIF

! 2.17 Level 2d variables (if any)
! --------------------------------

  IF (data%Lev2d%Npoints > 0) THEN

!    2.17.1 Dimensions

    dimid = (/ ncdf_defdim('dim_lev2d', data%Lev2d%Npoints), dimidul(1) /)

!    2.17.2 Variables

    varid = ncdf_defvar('level_type', 'Vertical level type', &
                        '', dimid64, TYPE = nf90_char)
    varid = ncdf_defvar('level_coeff_a', 'Hybrid / Eta level coefficient (a or eta)', &
                        ropp%Lev2d%units%level_coeff_a, dimid, nf90_type)
    varid = ncdf_defvar('level_coeff_b', 'Hybrid / Eta level coefficient (b or tau)', &
                        ropp%Lev2d%units%level_coeff_b, dimid, nf90_type)

!    2.17.3 Valid ranges (converted to ROPP standard units)

    CALL ut_convert(data%Lev2d%range%level_coeff_a, data%Lev2d%units%level_coeff_a, &
                    ropp%Lev2d%range%level_coeff_a, ropp%Lev2d%units%level_coeff_a)
    CALL ut_convert(data%Lev2d%range%level_coeff_b, data%Lev2d%units%level_coeff_b, &
                    ropp%Lev2d%range%level_coeff_b, ropp%Lev2d%units%level_coeff_b)

    CALL ncdf_putatt('valid_range', ropp%Lev2d%range%level_coeff_a, varname = 'level_coeff_a')
    CALL ncdf_putatt('valid_range', ropp%Lev2d%range%level_coeff_b, varname = 'level_coeff_b')

  ENDIF

! 2.18 Additional variables
! -------------------------

  IF (SIZE(data%vlist%VlistD0d) > 0) THEN
    CALL ropp_io_write_def_vlistD0d(data%vlist%VlistD0d, output_prec)
  ENDIF

  IF (SIZE(data%vlist%VlistD1d) > 0) THEN
    CALL ropp_io_write_def_vlistD1d(data%vlist%VlistD1d, output_prec)
  ENDIF

  IF (SIZE(data%vlist%VlistD2d) > 0) THEN
    CALL ropp_io_write_def_vlistD2d(data%vlist%VlistD2d, output_prec)
  ENDIF


! 2.19 Switch into data mode
! --------------------------

  status = nf90_enddef(ncdf_ncid)

! 2.20 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_def_rodata_2d

!-------------------------------------------------------------------------------
! 3. Vlist for scalar variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_def_vlistD0d(vlist, output_precision)

! 3.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_def_vlistD0d
  USE ropp_io_types, ONLY: VlisttypeD0d

  IMPLICIT NONE

  TYPE(VlisttypeD0d)           :: vlist
  CHARACTER(len = *), OPTIONAL :: output_precision

  INTEGER                      :: status
  INTEGER                      :: nf90_type

  INTEGER                      :: varid
  INTEGER, DIMENSION(1)        :: dimid

  CHARACTER(len =  64)         :: output_prec
  CHARACTER(len = 256)         :: routine


! 3.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_def')

! 3.3 Default arguments
! ---------------------

  IF (PRESENT(output_precision)) THEN
    output_prec = output_precision
  ELSE
    output_prec = 'float'
  ENDIF

! 3.4 Precision of the output
! ---------------------------

  SELECT CASE (TRIM(output_prec))
    CASE('float', 'single')
      nf90_type = nf90_float
    CASE('double')
      nf90_type = nf90_double
    CASE default
      CALL message(msg_warn, &
           'Unknown ncdf_output_precision: ' // TRIM(output_prec) // '\n' // &
           '   Using default (float) precision.')
      nf90_type = nf90_float
  END SELECT

! 3.5 Check (and create) dimensions
! ---------------------------------

  status = nf90_inquire(ncdf_ncid, unlimitedDimId = dimid(1))
  IF (status /= nf90_noerr) THEN
    CALL message(msg_fatal, &
         "Cannot detect unlimited dimension when defining additional netCDF variables.")
  ENDIF

! 3.6 Define variable
! -------------------

  varid = ncdf_defvar(vlist%name, vlist%long_name, vlist%units, dimid, TYPE = nf90_type)

! 3.7 Add valid_range attribute
! -----------------------------

  CALL ncdf_putatt('valid_range', vlist%range, varname = vlist%name)

! 3.8 Write next list element
! ---------------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_def_vlistD0d(vlist%next, output_prec)
  ENDIF

! 3.9 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_def_vlistD0d


!-------------------------------------------------------------------------------
! 4. Vlist for one dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_def_vlistD1d(vlist, output_precision)

! 4.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_def_vlistD1d
  USE ropp_io_types, ONLY: VlisttypeD1d

  IMPLICIT NONE

  TYPE(VlisttypeD1d)             :: vlist
  CHARACTER(len = *), OPTIONAL   :: output_precision

  INTEGER                        :: status
  INTEGER                        :: nf90_type

  INTEGER                        :: i, j
  INTEGER                        :: ndims
  INTEGER                        :: ndims_already
  INTEGER                        :: dimsize
  INTEGER                        :: varid
  INTEGER, DIMENSION(2)          :: dimids
  INTEGER, DIMENSION(1)          :: cnts

  LOGICAL                        :: use_as_dim

  CHARACTER(len = nf90_max_name) :: dimnam
  CHARACTER(len = nf90_max_name) :: dim_name
  CHARACTER(len =  64)           :: output_prec
  CHARACTER(len = 256)           :: routine

! 4.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_def')

! 4.3 Default arguments
! ---------------------

  IF (PRESENT(output_precision)) THEN
    output_prec = output_precision
  ELSE
    output_prec = 'float'
  ENDIF

! 4.4 Precision of the output
! ---------------------------

  SELECT CASE (TRIM(output_prec))
    CASE('float', 'single')
      nf90_type = nf90_float
    CASE('double')
      nf90_type = nf90_double
    CASE default
      CALL message(msg_warn, &
           'Unknown ncdf_output_precision: ' // TRIM(output_prec) // '\n' // &
           '   Using default (float) precision.')
      nf90_type = nf90_float
  END SELECT

! 4.5 Check (and create) dimensions
! ---------------------------------

! 4.5.1 Variable's size

  ndims         = 1
  cnts(1:ndims) = SIZE(vlist%data)
  use_as_dim    = .FALSE.

! 4.5.2 Check for (and create) dimensions

  IF (ndims == 1 .AND. use_as_dim) THEN
    status = nf90_def_dim(ncdf_ncid, vlist%name, cnts(1), dimids(1))
    IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
  ELSE
    status = nf90_inquire(ncdf_ncid, nDimensions = ndims_already)
    IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
    DO i = 1, ndims
      dim_name = 'Not found'
      DO j = 1, ndims_already
        status = nf90_inquire_dimension(ncdf_ncid, j, dimnam, dimsize)
        IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
        IF (dimsize == cnts(i)) THEN
          dim_name = dimnam
          dimids(i) = j
          EXIT
        ENDIF
      ENDDO
      IF (dim_name == 'Not found') THEN
        ndims_already = ndims_already + 1
        WRITE(dim_name, "('dim_',i3.3)") ndims_already
        status = nf90_def_dim(ncdf_ncid, dim_name, cnts(i), dimids(i))
      ENDIF
   ENDDO
  ENDIF

! 4.5.3 Unlimited dimension

  status = nf90_inquire(ncdf_ncid, unlimitedDimId = dimids(2))
  IF (status /= nf90_noerr) THEN
    CALL message(msg_fatal, &
         "Cannot detect unlimited dimension when defining additional netCDF variables.")
  ENDIF

! 4.6 Define variable
! -------------------

  varid = ncdf_defvar(vlist%name, vlist%long_name, vlist%units, dimids, TYPE = nf90_type)

! 4.7 Add valid_range attribute
! -----------------------------

  CALL ncdf_putatt('valid_range', vlist%range, varname = vlist%name)

! 4.8 Write next list element
! ---------------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_def_vlistD1d(vlist%next, output_prec)
  ENDIF

! 4.9 Clean up
! -------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_io_write_def_vlistD1d


!-------------------------------------------------------------------------------
! 5. Vlist for two dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_write_def_vlistD2d(vlist, output_precision)

! 5.1 Declarations
! ----------------

  USE ropp_utils
  USE ncdf
  USE ropp_io,       not_this => ropp_io_write_def_vlistD2d
  USE ropp_io_types, ONLY: VlisttypeD2d

  IMPLICIT NONE

  TYPE(VlisttypeD2d)             :: vlist
  CHARACTER(len = *), OPTIONAL   :: output_precision

  INTEGER                        :: status
  INTEGER                        :: nf90_type

  INTEGER                        :: i, j
  INTEGER                        :: ndims
  INTEGER                        :: ndims_already
  INTEGER                        :: dimsize
  INTEGER                        :: varid
  INTEGER, DIMENSION(3)          :: dimids
  INTEGER, DIMENSION(2)          :: cnts

  LOGICAL                        :: use_as_dim

  CHARACTER(len = nf90_max_name) :: dimnam
  CHARACTER(len = nf90_max_name) :: dim_name
  CHARACTER(len =  64)           :: output_prec
  CHARACTER(len = 256)           :: routine

! 5.2 Error handling
! ------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_io_write_ncdf_def')

! 5.3 Default arguments
! ---------------------

  IF (PRESENT(output_precision)) THEN
    output_prec = output_precision
  ELSE
    output_prec = 'float'
  ENDIF

! 5.4 Precision of the output
! ---------------------------

  SELECT CASE (TRIM(output_prec))
    CASE('float', 'single')
      nf90_type = nf90_float
    CASE('double')
      nf90_type = nf90_double
    CASE default
      CALL message(msg_warn, &
           'Unknown ncdf_output_precision: ' // TRIM(output_prec) // '\n' // &
           '   Using default (float) precision.')
      nf90_type = nf90_float
  END SELECT

! 5.5 Check (and create) dimensions
! ---------------------------------

! 5.5.1 Variable's size

  ndims      = 2
  cnts       = SHAPE(vlist%data)
  use_as_dim = .FALSE.

! 5.5.2 Check for (and create) dimensions

  IF (ndims == 1 .AND. use_as_dim) THEN
    status = nf90_def_dim(ncdf_ncid, vlist%name, cnts(1), dimids(1))
    IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
  ELSE
    status = nf90_inquire(ncdf_ncid, nDimensions = ndims_already)
    IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
    DO i = 1, ndims
      dim_name = 'Not found'
      DO j = 1, ndims_already
        status = nf90_inquire_dimension(ncdf_ncid, j, dimnam, dimsize)
        IF (status /= nf90_noerr) CALL ncdf_error_handler(status)
        IF (dimsize == cnts(i)) THEN
           dim_name = dimnam
           dimids(i) = j
           EXIT
        ENDIF
      ENDDO
      IF (dim_name == 'Not found') THEN
        ndims_already = ndims_already + 1
        WRITE(dim_name, "('dim_',i3.3)") ndims_already
        status = nf90_def_dim(ncdf_ncid, dim_name, cnts(i), dimids(i))
      ENDIF
    ENDDO
  ENDIF

! 5.5.3 Unlimited dimension

  status = nf90_inquire(ncdf_ncid, unlimitedDimId = dimids(3))
  IF (status /= nf90_noerr) THEN
    CALL message(msg_fatal, &
         "Cannot detect unlimited dimension when defining additional netCDF variables.")
  ENDIF

! 5.6 Define variable
! -------------------

  varid = ncdf_defvar(vlist%name, vlist%long_name, vlist%units, dimids, TYPE = nf90_type)

! 5.7 Add valid_range attribute
! -----------------------------

  CALL ncdf_putatt('valid_range', vlist%range, varname = vlist%name)

! 5.8 Write next list element
! ---------------------------

  IF (ASSOCIATED(vlist%next)) THEN
    CALL ropp_io_write_def_vlistD2d(vlist%next, output_prec)
  ENDIF

! 5.9 Clean up
! -------------

  CALL message_set_routine(routine)
  
END SUBROUTINE ropp_io_write_def_vlistD2d
