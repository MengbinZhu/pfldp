! $Id: bgrasc2ropp.f90 3551 2013-02-25 09:51:28Z idculv $

PROGRAM bgrasc2ropp

!****x* Programs/bgrasc2ropp *
!
! NAME
!   bgrasc2ropp
!
! SYNOPSIS
!   Convert ASCII file containing background profile data to ROPP netCDF
!
!   > bgrasc2ropp file_in [-o <file_out>] [-n] [-d] [-h] [-v]
!
! ARGUMENTS
!   file_in - ascii file containing Level 2b background data
!             in a fortran namelist.
!             There is no default for this argument.
!
! OPTIONS
!  -o <file_out> - is the output netCDF file name.
!  -n - switches off range checking before writing ROPP file
!  -d - writes additional diagnostic information to stdout
!  -h - help
!  -v - version information
!
! INPUTS
!   Ascii file holding a fortran namelist "bgr_profile" containing those
!   elements of the {bg, GEOref, Lev2b, Lev2c and Lev2d} substructures 
!   of the ROprof structure needed by the ropp forward model. 
!   This namelist could be generated from ECMWF grib data by the sister tool
!   ropp_io/tools/grib2bgrasc.
!
! OUTPUTS
!   ROPP netCDF file, suitable for use by ropp_fm/tools/ropp_fm_bg2ro_1d
!
! MODULES
!   typesizes
!   messages
!   ropp_utils
!   ropp_io_types
!   ropp_io
!
! CALLS
!   usage
!   ropp_io_init
!   ropp_io_write
!   ropp_io_free
!   ropp_io_version
!   message
!   message_set_routine
!
! DESCRIPTION
!   Conversion of level bg/GEOref/2b/2c/2d background profile data in fortran namelist 
!   format to ROPP netCDF format. The input file is in the same format as that 
!   produced by the sister program grib2bgrasc, which extracts a profile from a GRIB2 format model dump. 
!   The data is then written out to a ROPP-standard netCDF file in PWD.
!
! RESTRICTIONS
!   1) Assumes ECMWF-like model level structures.
!   2) Users needing to read in more than 200 levels of data will have to
!      increase Nlevs_max and recompile.
!
! REFERENCES
!   1. ROPP User Guide - Part I: I/O module.
!      SAF/ROM/METO/UG/ROPP/002
!!
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

  USE typesizes,     wp => EightByteReal
  USE messages
  USE DateTimeProgs, ONLY: CalToJul
  USE ropp_utils,    ONLY: WHERE, ropp_MDFV, ropp_MDTV
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_free,    &
                           ropp_io_init,    &
                           ropp_io_write,   &
                           ropp_io_addvar,  &
                           ropp_io_version

  IMPLICIT NONE

! Fixed parameters

  INTEGER,           PARAMETER :: funit        = 11

! Local variables

  TYPE(ROprof) :: BGRprof   ! BG profile structure

  CHARACTER (LEN=256) :: arg                          ! Command line argument
  CHARACTER (LEN=256) :: file_in, file_out            ! I/P & O/P file names
  CHARACTER (LEN=80)  :: outstr                       ! Formatted output string
  CHARACTER (LEN=10)  :: number                       ! Number as string
  INTEGER             :: SWrev=0                      ! TEXT revision number
  LOGICAL             :: exists                       ! File exists flag
  LOGICAL             :: iranchk=.TRUE.               ! Range checking flag
  REAL(wp)            :: SWver=0.0                    ! TEXT POCS software version number

  INTEGER             :: i, j, ih                     ! Loop counters / indices
  INTEGER             :: iarg                         ! Command line argument index
  INTEGER             :: narg                         ! No. of command line arguments
  INTEGER             :: iostatus                     ! I/O Status
  REAL(wp)               :: jul_day                   ! Julian day of data and validity time
  INTEGER, DIMENSION(8)  :: CDT                       ! Date/time combo for cal2jul

! Holds WHERE output (NB this is NOT the F90 intrinsic)
  INTEGER, DIMENSION(:), POINTER :: idx  => NULL()
  INTEGER                        :: nidx = 0

  INTEGER, PARAMETER  :: Nlevs_max=200                ! Maximum number of model levels

  INTEGER             :: NLevs                        ! Number of full model levels
  INTEGER             :: year, mon, day               ! Background validity date
  INTEGER             :: hour, min, sec               ! Background validity time
  REAL(wp)            :: tlead=ropp_MDFV              ! Background forecast range (hr)
  REAL(wp)            :: lon=ropp_MDFV, lat=ropp_MDFV ! Tangent point lat, lon (deg)
  REAL(wp)            :: und=ropp_MDFV                ! Tangent point undulation (m)
  REAL(wp)            :: roc=ropp_MDFV                ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc=(/ropp_MDFV, ropp_MDFV, ropp_MDFV/)  ! Tangent point centre of curvature (m)
  REAL(wp)            :: azi=ropp_MDFV                ! GNSS-->LEO line of sight angle (degT)

  REAL(wp)                         :: Z0=ropp_MDFV    ! Surface geopotential height (m)
  REAL(wp)                         :: P0=ropp_MDFV    ! Surface pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: P=ropp_MDFV     ! Pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: T=ropp_MDFV     ! Temperature (K)
  REAL(wp), DIMENSION(Nlevs_max)   :: Q=ropp_MDFV     ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(Nlevs_max)   :: Z=ropp_MDFV     ! Geopotential height (m)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Ak=ropp_MDFV    ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Bk=ropp_MDFV    ! Hybrid/Eta level B-coefficient

  REAL(wp), DIMENSION(Nlevs_max)   :: Tvirt=ropp_MDFV ! Virtual temperature (K)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Ph=ropp_MDFV    ! Pressure on half levs (hPa)
  REAL(wp), DIMENSION(Nlevs_max)   :: deltaP=ropp_MDFV! Change in pressure between half levs (hPa)
  REAL(wp), DIMENSION(Nlevs_max)   :: lnP=ropp_MDFV   ! Log of pressure ratio between half levs
  REAL(wp), DIMENSION(Nlevs_max)   :: alpha=ropp_MDFV ! Verical interpolation coefficient
  REAL(wp), DIMENSION(Nlevs_max)   :: dZ=ropp_MDFV    ! Function to be interpolated
  REAL(wp), DIMENSION(Nlevs_max+1) :: Zh=ropp_MDFV    ! Geopotential height on half levs (m)

  REAL(wp), PARAMETER              :: g_wmo=9.80665   ! Standard gravity (m/s2)
  REAL(wp), PARAMETER              :: R_dry=287.0597  ! Dry gas constant (K/kg/K)

! Input to this program
  NAMELIST / bgr_profile / year, mon, day, &
                           hour, min, sec, &
                           tlead, &
                           lon, lat, und, roc, coc, azi, &
                           Z0, P0, &
                           Nlevs, &
                           P, T, Q, Z, Ak, Bk

!NB: If Z is not in namelist it can be calculated from {Z0, P0, T, Q, Ak, Bk}

! Some compilers may need the following declaration to be commented out
  INTEGER             :: IARGC

!-------------------------------------------------------------
! 1. Initialise
!-------------------------------------------------------------

  CALL message_set_routine ( "bgrasc2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                     bgrasc to ROPP converter'                           )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------
! 2. Parse command line options
!-------------------------------------------------------------

  narg = IARGC()

  file_in   = " "       ! no default for i/p file name
  file_out  = " "       ! assume a default generated from i/p file name

  iarg = 1
  DO WHILE ( iarg <= narg )
    CALL GETARG ( iarg, arg )

    SELECT CASE (arg)
      CASE ("-d","-D","--debug")
        msg_MODE = VerboseMode

      CASE ("-h","-H","--help","?")
        narg = 0
        file_in = "dummy"

      CASE ("-o","-O","--output")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        file_out = arg

      CASE ("-n","-N","--no-ranchk")
        iranchk = .FALSE.

      CASE ("-v","-V","--version")
        CALL version_info()
        CALL EXIT(0)

      CASE DEFAULT
         IF ( arg(1:1) /= '-' ) THEN
           file_in = arg
         END IF
    END SELECT

    iarg = iarg + 1
  END DO

  IF ( file_in == " " ) THEN
    CALL message ( msg_error, "No input file(s) specified" )
    narg = 0
  END IF

  IF ( narg == 0 ) THEN
    CALL Usage
    CALL EXIT(0)
  END IF

!-------------------------------------------------------------
! 3. Check text input file exists;
!    make output file name if not given on command line
!-------------------------------------------------------------

  INQUIRE ( FILE=file_in, EXIST=exists )
  IF ( .NOT. exists ) &
    CALL message ( msg_fatal, "Input namelist file " // TRIM(file_in) // &
                              " not found" )

  IF ( file_out == " " ) THEN
    j = INDEX(file_in, "/", BACK=.TRUE.)
    file_out = TRIM(ADJUSTL(file_in(j+1:))) // ".nc"
  END IF

  IF (msg_MODE  == VerboseMode) THEN
    CALL message ( msg_diag, "file_in = " // file_in )
    CALL message ( msg_diag, "file_out = " // file_out )
  ENDIF

!-------------------------------------------------------------
! 4. Read namelist
!-------------------------------------------------------------

  CALL message ( msg_info, "Reading file " // TRIM(file_in) )

  OPEN ( UNIT=funit, FILE=file_in, STATUS="OLD", ACTION="READ" )

  READ ( UNIT=funit, NML=bgr_profile, iostat=iostatus)

  IF ( iostatus > 0 ) THEN
    CALL message ( msg_fatal, "I/O error while reading text file" )
  ELSE
    IF (msg_MODE == VerboseMode) THEN
      CALL message( msg_diag, "Contents of bgr_profile namelist:" )
      WRITE (*, NML=bgr_profile)  ! For diagnostic purposes
    ENDIF
  ENDIF

  CLOSE ( UNIT=funit )

!-------------------------------------------------------------
! 4.1 Check namelist
!-------------------------------------------------------------

  IF (Nlevs > Nlevs_max) CALL message ( msg_fatal, "Too many levels. Increase Nlevs_max and recompile" )

  IF (Nlevs <= 0) CALL message ( msg_fatal, "Nlevs missing" )

  IF (P0 < ropp_MDTV) CALL message ( msg_fatal, "Surface pressure missing" )

  IF (Z0 < ropp_MDTV) CALL message ( msg_fatal, "Surface GPH missing" )

  IF (ALL(P < ropp_MDTV)) CALL message ( msg_fatal, "Pressure profile missing" )
  idx => WHERE(P >= ropp_MDTV, nidx)
  IF (nidx /= Nlevs) CALL message ( msg_fatal, "Wrong number of pressures" )

  IF (ALL(T < ropp_MDTV)) CALL message ( msg_fatal, "Temperature profile missing" )
  idx => WHERE(T >= ropp_MDTV, nidx)
  IF (nidx /= Nlevs) CALL message ( msg_fatal, "Wrong number of temperatures" )

  IF (ALL(Q < ropp_MDTV)) CALL message ( msg_fatal, "Specific humidity profile missing" )
  idx => WHERE(Q >= ropp_MDTV, nidx)
  IF (nidx /= Nlevs) CALL message ( msg_fatal, "Wrong number of specific humidities" )

! If possible, set Ak and Bk to precise ECMWF values.
! (The values in the grib file, used to calculate P on full model levels, 
!  are derived from the single precision variable pv.)
! Maybe read these in from a namelist?
  IF (Nlevs == 60) THEN 
    Ak(1:Nlevs+1) = Ak_60()
    Bk(1:Nlevs+1) = Bk_60()
  ELSE IF (Nlevs == 91) THEN
    Ak(1:Nlevs+1) = Ak_91()
    Bk(1:Nlevs+1) = Bk_91()
  ELSE IF (Nlevs == 137) THEN
    Ak(1:Nlevs+1) = Ak_137()
    Bk(1:Nlevs+1) = Bk_137()
  ELSE ! Use the values from the grib file - if the right number are present
    idx => WHERE(Ak >= ropp_MDTV, nidx)
    IF (nidx /= Nlevs+1) THEN
      CALL message ( msg_fatal, "Unable to find Aks" )
    ENDIF
  ENDIF

! If necessary, recalculate geopotential here, or rangechecking will remove the whole lev1b structure.
! Calculate using Z(p)-Z(p*) = int_from(p*)_to_(p) (-RTv(p')/g0) dlogp'.

  IF (ANY(Z(1:Nlevs) < ropp_MDTV)) THEN 

    !--- Virtual temperatures
    Tvirt  = (1.0_wp + 0.61_wp * Q/1000.0_wp) * T

    !--- Pressure differences
    Ph = Ak + Bk*P0
    deltaP = Ph(1:Nlevs) - Ph(2:Nlevs+1)

    !--- Log of pressure ratio
    lnP = log(Ph(1:Nlevs) / Ph(2:Nlevs+1))

    !--- Interpolation coefficients
    alpha = 1.0_wp - Ph(2:Nlevs+1)/deltaP * lnP
    alpha(Nlevs) = log(2.0_wp)

    !--- Function to be integrated
    dZ = R_dry * Tvirt * lnP / g_wmo

    !--- Calculate geopotential height integral
    Zh(1) = 0.0_wp
    DO ih = 2,Nlevs+1
      Zh(ih) = SUM(dZ(1:ih-1))
    ENDDO
    IF (Z0 > ropp_MDTV) Zh(:) = Zh(:) + Z0

    !--- Interpolate onto full levels
    Z(1:Nlevs) = Zh(1:Nlevs) + alpha(1:Nlevs) * R_dry * Tvirt(1:Nlevs) / g_wmo

  ENDIF

  idx => WHERE(Z >= ropp_MDTV, nidx)
  IF (nidx /= Nlevs) CALL message ( msg_fatal, "Wrong number of GPHs" )

! Checks on ranges could be put in here, although the whole BGprof structure
! will be range-checked in the call to ropp_io_write below. 

!-------------------------------------------------------------
! 5.0 Initialise ROPP structures & fill in static header data
!     if not in text files
!-------------------------------------------------------------

  CALL ropp_io_init ( BGRprof, 0, 0, 0, NLevs, 1, NLevs+1 )

!-------------------------------------------------------------
! 5.1 Populate ROPP structure with data from namelist
!-------------------------------------------------------------

  BGRprof%bg%source   = "ECMWF"
  BGRprof%bg%Year     = year
  BGRprof%bg%Month    = mon
  BGRprof%bg%Day      = day
  BGRprof%bg%Hour     = hour
  BGRprof%bg%Minute   = min
!  BGRprof%bg%Second   = sec  ! Not currently part of the bg substructure
  BGRprof%bg%Fcperiod = tlead

  BGRprof%GEOref%lat        = lat
  BGRprof%GEOref%lon        = lon
  BGRprof%GEOref%r_coc      = coc ! depends on azimuth
  BGRprof%GEOref%roc        = roc ! depends on azimuth
  BGRprof%GEOref%azimuth    = azi
  BGRprof%GEOref%undulation = und

  BGRprof%Lev2b%Npoints        = Nlevs
  BGRprof%Lev2b%press          = P(1:Nlevs)*1.0e-2_wp ! Pa to hPa for ROPP
  BGRprof%Lev2b%temp           = T(1:Nlevs)
  BGRprof%Lev2b%shum           = Q(1:Nlevs)
  BGRprof%Lev2b%geop           = Z(1:Nlevs)
  BGRprof%Lev2b%meteo_qual     = 100.0_wp

  BGRprof%Lev2c%Npoints        = 1
  BGRprof%Lev2c%geop_sfc       = Z0
  BGRprof%Lev2c%press_sfc      = P0*1.0e-2_wp ! Pa to hPa for ROPP
  BGRprof%Lev2c%press_sfc_qual = 100.0_wp

  BGRprof%Lev2d%Npoints        = Nlevs+1
  BGRprof%Lev2d%level_type     = "ECMWF"
  BGRprof%Lev2d%level_coeff_a  = ak(1:Nlevs+1)*1.0e-2_wp ! Pa to hPa for ROPP
  BGRprof%Lev2d%level_coeff_b  = bk(1:Nlevs+1)

!-------------------------------------------------------------
! 5.2 Convert text parameters to ROPP standard
!-------------------------------------------------------------

  WRITE ( BGRprof%Software_Version, FMT="(F6.3)" ) SWver + SWrev * 0.001_wp
  IF ( SWver < 10.0 ) THEN
    BGRprof%Software_Version = "V0"//ADJUSTL(BGRprof%Software_Version)
  ELSE
    BGRprof%Software_Version = "V" //ADJUSTL(BGRprof%Software_Version)
  END IF
  WRITE ( outstr, FMT="(I4,'/',I2,'/',I2,' ',I4,'Z')" ) BGRprof%bg%Year, BGRprof%bg%Month, BGRprof%bg%Day, &
         100*BGRprof%bg%Hour+BGRprof%bg%Minute
  CALL message ( msg_diag, "   Data time                    : "//TRIM(outstr) )
  CDT = (/ BGRprof%bg%Year, BGRprof%bg%Month, BGRprof%bg%Day, 0, BGRprof%bg%Hour, BGRprof%bg%Minute, 0, 0 /)
  CALL CalToJul( CDT,  jul_day, 1 )
  jul_day = jul_day + (BGRprof%bg%Fcperiod/24.0_wp)
  CALL CalToJul( CDT,  jul_day, -1 )
  WRITE ( outstr, FMT="(I4,'/',I2,'/',I2,' ',I4,'Z')" ) CDT(1), CDT(2), CDT(3), 100*CDT(5)+CDT(6)
  CALL message ( msg_diag, "   Validity time                : "//TRIM(outstr) )
  WRITE ( outstr, FMT="(F6.2,',',F7.2)" ) BGRprof%GeoRef%Lat, BGRprof%GeoRef%Lon
  CALL message ( msg_diag, "   Latitude,Longitude           : "//TRIM(outstr) )
  WRITE ( number, FMT="(I6)" ) BGRprof%Lev2a%Npoints
  CALL message ( msg_diag, "   No. of refractivity samples  : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) BGRprof%Lev2b%Npoints
  CALL message ( msg_diag, "   No. of geophysical samples   : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) BGRprof%Lev2c%Npoints
  CALL message ( msg_diag, "   No. of surface geo. samples  : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) BGRprof%Lev2d%Npoints
  CALL message ( msg_diag, "   No. of model coeff. levels   : "//TRIM(number) )

!-------------------------------------------------------------
! 6. Write ROPP netCDF file
!-------------------------------------------------------------

  CALL message ( msg_info, "Writing " // TRIM(file_out) )

  CALL ropp_io_write ( BGRprof, file=file_out, ierr=iostatus , ranchk=iranchk)

  IF ( iostatus > 0 ) THEN
    CALL message ( msg_fatal, "Error while writing output file" )
  END IF

!-------------------------------------------------------------
! 7. Tidy up - deallocate structures & free memory
!-------------------------------------------------------------

  CALL ropp_io_free ( BGRprof )
  CALL message ( msg_noin, " " )

CONTAINS

!-------------------------------------------------------------------------------
! 8. Usage (help) information
!-------------------------------------------------------------------------------

  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Convert an ascii file containing BGR model data, '
    PRINT *, '  in fortran namelist format to a ROPP netCDF file'
    PRINT *, 'Usage:'
    PRINT *, '  > bgrasc2ropp file_in [-o <file_out>] [-n] [-d] [-h] [-v]'
    PRINT *, '  where:'
    PRINT *, '    file_in is an ascii file holding the fortran namelist.'
    PRINT *, 'Options:'
    PRINT *, '  -o <file_out> is the output (netCDF) file name'
    PRINT *, '  -n switches off range checking before writing ROPP file'
    PRINT *, '  -d prints out some additional diagnostics to stdout'
    PRINT *, '  -h this help'
    PRINT *, '  -v version information'
    PRINT *, 'Defaults:'
    PRINT *, '  Input  file name : required'
    PRINT *, '  Output file name : $PWD/file_in.nc'
    PRINT *, 'See bgrasc2ropp(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage

!-------------------------------------------------------------------------------
! 9. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'bgrasc2ropp - convert ascii files to ROPP netCDF'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

!-------------------------------------------------------------------------------
! 10. Standard level data
!-------------------------------------------------------------------------------

  FUNCTION Ak_60() RESULT (Ak)
! Standard 60 level Ak (Pa), from
! http://www.ecmwf.int/products/data/technical/model_levels/model_def_60.html
    REAL(wp), DIMENSION(61)          :: Ak
    REAL(wp), DIMENSION(61)          :: Ak60=(/&
    0.000000,0.000000,7.367743,65.889244,210.393890,467.333588,855.361755,1385.912598,2063.779785,&
    2887.696533,3850.913330,4941.778320,6144.314941,7438.803223,8802.356445,10209.500977,11632.758789,&
    13043.218750,14411.124023,15706.447266,16899.468750,17961.357422,18864.750000,19584.330078,&
    20097.402344,20384.480469,20429.863281,20222.205078,19755.109375,19027.695313,18045.183594,&
    16819.474609,15379.805664,13775.325195,12077.446289,10376.126953,8765.053711,7306.631348,&
    6018.019531,4906.708496,3960.291504,3196.421631,2579.888672,2082.273926,1680.640259,1356.474609,&
    1094.834717,883.660522,713.218079,575.651001,464.618134,373.971924,298.495789,234.779053,&
    180.584351,134.483307,95.636963,63.647804,38.425343,20.000000,0.000000 /)

    Ak = Ak60

    RETURN
  END FUNCTION Ak_60
  
  FUNCTION Bk_60() RESULT (Bk)
! Standard 60 level Bk (no unit), from
! http://www.ecmwf.int/products/data/technical/model_levels/model_def_60.html
    REAL(wp), DIMENSION(61)          :: Bk
    REAL(wp), DIMENSION(61)          :: Bk60=(/&
    1.00000000,0.99763012,0.99401945,0.98827010,0.97966272,0.96764523,0.95182151,0.93194032,0.90788388,&
    0.87965691,0.84737492,0.81125343,0.77159661,0.72878581,0.68326861,0.63554746,0.58616841,0.53570992,&
    0.48477158,0.43396294,0.38389215,0.33515489,0.28832296,0.24393314,0.20247594,0.16438432,0.13002251,&
    0.09967469,0.07353383,0.05169041,0.03412116,0.02067788,0.01114291,0.00508112,0.00181516,0.00046139,&
    0.00007582,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,&
    0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,&
    0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000 /)

    Bk = Bk60

    RETURN
  END FUNCTION Bk_60
  
  FUNCTION Ak_91() RESULT (Ak)
! Standard 91 level Ak (Pa), from
! http://www.ecmwf.int/products/data/technical/model_levels/model_def_91.html
    REAL(wp), DIMENSION(92)          :: Ak
    REAL(wp), DIMENSION(92)          :: Ak91=(/&
    0.000000,0.003160,6.575628,54.208336,162.043427,336.772369,576.314148,895.193542,&
    1297.656128,1784.854614,2356.202637,3010.146973,3743.464355,4550.215820,5422.802734,&
    6353.920898,7335.164551,8356.252930,9405.222656,10471.310547,11543.166992,12608.383789,&
    13653.219727,14665.645508,15633.566406,16544.585938,17385.595703,18141.296875,18798.822266,&
    19348.775391,19785.357422,20107.031250,20319.011719,20426.218750,20434.158203,20348.916016,&
    20175.394531,19919.796875,19587.513672,19184.544922,18716.968750,18191.029297,17613.281250,&
    16990.623047,16329.560547,15638.053711,14922.685547,14192.009766,13453.225586,12713.897461,&
    11982.662109,11262.484375,10558.881836,9873.560547,9208.305664,8564.624023,7942.926270,&
    7341.469727,6759.727051,6199.839355,5663.156250,5150.859863,4663.776367,4202.416504,&
    3767.196045,3358.425781,2976.302246,2620.898438,2292.155518,1989.874390,1713.709595,&
    1463.163940,1237.585449,1036.166504,857.945801,701.813354,566.519226,450.685791,&
    352.824493,271.356506,204.637451,150.986023,108.715561,76.167656,51.746601,33.952858,&
    21.413612,12.908319,7.387186,3.980832,2.000040,0.000000 /)

    Ak = Ak91

    RETURN
  END FUNCTION Ak_91
  
  FUNCTION Bk_91() RESULT (Bk)
! Standard 91 level Bk (no unit), from
! http://www.ecmwf.int/products/data/technical/model_levels/model_def_91.html
    REAL(wp), DIMENSION(92)          :: Bk
    REAL(wp), DIMENSION(92)          :: Bk91=(/&
    1.000000,0.997630,0.994204,0.989153,0.982238,0.973466,0.963007,0.950274,0.935157,0.917651,&
    0.897767,0.875518,0.850950,0.824185,0.795385,0.764679,0.732224,0.698224,0.662934,0.626559,&
    0.589317,0.551458,0.513280,0.475016,0.436906,0.399205,0.362203,0.326329,0.291993,0.259554,&
    0.229315,0.201520,0.176091,0.152934,0.131935,0.112979,0.095964,0.080777,0.067316,0.055474,&
    0.045146,0.036227,0.028610,0.022189,0.016860,0.012508,0.009035,0.006322,0.004267,0.002765,&
    0.001701,0.001000,0.000548,0.000279,0.000131,0.000055,0.000014,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000 /)

    Bk = Bk91

    RETURN
  END FUNCTION Bk_91
  
  FUNCTION Ak_137() RESULT (Ak)
! Standard 137 level Ak (Pa), from
! http://www.ecmwf.int/products/changes/ifs_cycle_38r2/L137.html
    REAL(wp), DIMENSION(138)         :: Ak
    REAL(wp), DIMENSION(138)         :: Ak137=(/&
    0.000000,0.000000,3.757813,22.835938,62.781250,122.101562,202.484375,&
    302.476562,424.414062,568.062500,734.992188,926.507812,1143.250000,&
    1387.546875,1659.476562,1961.500000,2294.242188,2659.140625,3057.265625,&
    3489.234375,3955.960938,4457.375000,4993.796875,5564.382812,6168.531250,&
    6804.421875,7470.343750,8163.375000,8880.453125,9617.515625,10370.175781,&
    11133.304688,11901.339844,12668.257812,13427.769531,14173.324219,&
    14898.453125,15596.695312,16262.046875,16888.687500,17471.839844,&
    18006.925781,18489.707031,18917.460938,19290.226562,19608.572266,&
    19874.025391,20087.085938,20249.511719,20361.816406,20425.718750,&
    20442.078125,20412.308594,20337.863281,20219.664062,20059.931641,&
    19859.390625,19620.042969,19343.511719,19031.289062,18685.718750,&
    18308.433594,17901.621094,17467.613281,17008.789062,16527.322266,&
    16026.115234,15508.256836,14975.615234,14432.139648,13881.331055,&
    13324.668945,12766.873047,12211.547852,11660.067383,11116.662109,&
    10584.631836,10065.978516,9562.682617,9076.400391,8608.525391,8159.354004,&
    7727.412109,7311.869141,6911.870605,6526.946777,6156.074219,5798.344727,&
    5452.990723,5119.895020,4799.149414,4490.817383,4194.930664,3911.490479,&
    3640.468262,3381.743652,3135.119385,2900.391357,2677.348145,2465.770508,&
    2265.431641,2076.095947,1897.519287,1729.448975,1571.622925,1423.770142,&
    1285.610352,1156.853638,1037.201172,926.344910,823.967834,729.744141,&
    643.339905,564.413452,492.616028,427.592499,368.982361,316.420746,&
    269.539581,227.968948,191.338562,159.279404,131.425507,107.415741,&
    86.895882,69.520576,54.955463,42.879242,32.985710,24.985718,18.608931,&
    13.605424,9.746966,6.827977,4.666084,3.102241,2.000365,0.000000 /)

    Ak = Ak137

    RETURN
  END FUNCTION Ak_137
  
  FUNCTION Bk_137() RESULT (Bk)
! Standard 137 level Bk (no unit), from
! http://www.ecmwf.int/products/changes/ifs_cycle_38r2/L137.html
    REAL(wp), DIMENSION(138)         :: Bk
    REAL(wp), DIMENSION(138)         :: Bk137=(/&
    1.000000,0.997630,0.995003,0.991984,0.988500,0.984542,0.980072,0.975078,&
    0.969513,0.963352,0.956550,0.949064,0.940860,0.931881,0.922096,0.911448,&
    0.899900,0.887408,0.873929,0.859432,0.843881,0.827256,0.809536,0.790717,&
    0.770798,0.749797,0.727739,0.704669,0.680643,0.655736,0.630036,0.603648,&
    0.576692,0.549301,0.521619,0.493800,0.466003,0.438391,0.411125,0.384363,&
    0.358254,0.332939,0.308598,0.285354,0.263242,0.242244,0.222333,0.203491,&
    0.185689,0.168910,0.153125,0.138313,0.124448,0.111505,0.099462,0.088286,&
    0.077958,0.068448,0.059728,0.051773,0.044548,0.038026,0.032176,0.026964,&
    0.022355,0.018318,0.014816,0.011806,0.009261,0.007133,0.005378,0.003971,&
    0.002857,0.001992,0.001353,0.000890,0.000562,0.000340,0.000199,0.000112,&
    0.000059,0.000024,0.000007,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,&
    0.000000,0.000000 /)

    Bk = Bk137

    RETURN
  END FUNCTION Bk_137
  
END Program bgrasc2ropp
