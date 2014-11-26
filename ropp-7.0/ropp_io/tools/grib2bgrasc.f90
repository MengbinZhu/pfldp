! $Id: grib2bgrasc.f90 2738 2011-03-22 15:34:18Z frdo $

PROGRAM grib2bgrasc

!****x* Programs/grib2bgrasc *
!
! NAME
!   grib2bgrasc    (grib2bgrasc.f90)
!
! SYNOPSIS
!   Extract RO background data from GRIB (ed 1 or 2) file(s) 
!   and output in fortran namelist format.
!
!   > grib2bgrasc grib_file -lat <lat> -lon <lon>
!                 [-date <date>] [-time <time>] 
!                 [-g grib_file2]
!                 [-z grib_filez]
!                 [-o <namelist_file>]
!                 [-r <radius_of_curvature>]
!                 [-u <undulation>]
!                 [-d] [-h] [-v]
!
! ARGUMENTS
!   grib_file  - GRIB file containing background fields.
!   lat        - latitude (deg)
!   lon        - longitude (deg)
!
! OPTIONS
!   Option switches can be in any order and are case-insensitive;
!   any space(s) between a switch and its (madatory) argument is
!   optional.
!     -g grib_file2.  Second GRIB file containing background fields, if time interpolation needed.
!     -date <date> (yyyymmdd).  Date, if time interpolation needed.
!     -time <time> (HHMM or HHMMSS).  Time, if time interpolation needed.
!     -z grib_filez.  GRIB file containing surface geopotential, aka g_wmo*orography, if needed.
!     -r <roc>. radius of curvature of tangent plane (m).
!     -u <und>. Undulation at tangent point (m).
!     -o <namelist_file> output file name
!     -d to output additional diagnostics
!     -h help
!     -v version information
!   Defaults:
!     Input file name : required
!     Output file name : $PWD/input_file.nml
! NOTE
!   The environment variables GEOPOT_COEF and GEOPOT_CORR must be
!   set appropriately if the user wants the undulation to be calculated.
!
! INPUTS
!   grib_files containing background fields in GRIB (ed 1 or 2) format.
!     Each input file must contain {p*, phi*, T, q, Ak, Bk}.
!
! OUTPUTS
!   Ascii file holding fortran namelist bgr_profile. 
!     The output file name is optional.
!
! CALLS
!   IARGC
!   message
!   message_set_routine
!   extract_prof_from_GRIB
!   generate_GPH
!
! MODULES
!   grib_api
!   typesizes
!   messages
!   coordinates
!   DateTimeProgs
!   ropp_utils
!   ropp_io
!
! DEPENDENCIES
!   ECMWF GRIB package  - GRIB kernel routines
!   ROPP Utils library  - ROPP utility routines
!   The environment variables GEOPOT_COEF and GEOPOT_CORR must be 
!     set appropriately if the user wants the undulation to be calculated.
!
! DESCRIPTION
!   A GRIB background data translator for Radio Occultation data.
!   Generates one profile, in Fortran namelist format, from one GRIB file. 
!
! REFERENCES
!   1) GRIB API
!      https://software.ecmwf.int/wiki/display/GRIB/Home
!   2) ROPP User Guide - Part 1: IO module
!      SAF/ROM/METO/UG/ROPP/002
!
! SEE ALSO
!   grib2bgrasc(1)
!
! AUTHOR
!   Met Office, Exeter, UK and DMI, Copenhagen, Denmark.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

! Modules

  USE typesizes,     wp => EightByteReal
  USE messages
  USE DateTimeProgs, ONLY: CalToJul
  USE ropp_utils,    ONLY: ropp_MDFV,     &
                           ropp_MDTV
  USE ropp_io,       ONLY: ropp_io_version

  IMPLICIT NONE

  CHARACTER(LEN=256)     :: file_in, file_out            ! I/P & O/P file names
  CHARACTER(LEN=256)     :: file_in2=" ", file_inz=" "   ! Optional I/P file names
  CHARACTER(LEN=8)       :: datestring=" "               ! I/P date
  CHARACTER(LEN=8)       :: timestring=" "               ! I/P time
  INTEGER                :: year_in, mon_in, day_in      ! Input date
  INTEGER                :: hour_in, min_in, sec_in      ! Input time
  REAL(wp)               :: lon_in, lat_in               ! Input tangent point lon, lat (deg)

! Local variables
  CHARACTER(LEN=10)      :: slon_in, slat_in             ! Input tangent point lon, lat (deg)
  CHARACTER(LEN=256)     :: arg                          ! Command line argument
  CHARACTER(LEN=256)     :: GEOPOT_FILE                  ! EGM96 coeff or corrn file name
  LOGICAL                :: exists                       ! File exists flag
  INTEGER                :: funit = 9                    ! ascii file lun
  INTEGER                :: i, j, ih, ii                 ! Loop counters / indices
  INTEGER                :: iarg                         ! Command line argument index
  INTEGER                :: narg                         ! No. of command line arguments
  INTEGER                :: iostatus                     ! I/O Status
  INTEGER, DIMENSION(8)  :: CDT                          ! Date/time combo for cal2jul
  REAL(wp)               :: tt                           ! Time interpolation coefficient
  REAL(wp)               :: jul_day1, jul_day2, jul_day_in ! Julian days of file_in, file_in2 and requested date/time
  REAL(wp)               :: dtor, delta_lon              ! Used to compare longitudes

! Namelist variables
  INTEGER, PARAMETER     :: Nlevs_max=200                ! Maximum number of model levels

  INTEGER                :: NLevs                        ! Number of full model levels
  INTEGER                :: year, mon, day               ! Background validity date
  INTEGER                :: hour, min, sec               ! Background validity time
  REAL(wp)               :: tlead=ropp_MDFV              ! Background forecast range (hr)
  REAL(wp)               :: lon=ropp_MDFV, lat=ropp_MDFV ! Tangent point lat, lon (deg)
  REAL(wp)               :: und=ropp_MDFV                ! Tangent point undulation (m)
  REAL(wp)               :: roc=ropp_MDFV                ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc=ropp_MDFV                ! Tangent point centre of curvature (m)
  REAL(wp)               :: azi=ropp_MDFV                ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                            :: Z0=ropp_MDFV    ! Surface geopotential height (m)
  REAL(wp)                            :: P0=ropp_MDFV    ! Surface pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)      :: P=ropp_MDFV     ! Pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)      :: T=ropp_MDFV     ! Temperature (K)
  REAL(wp), DIMENSION(Nlevs_max)      :: Q=ropp_MDFV     ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(Nlevs_max)      :: Z=ropp_MDFV     ! Geopotential height (m)
  REAL(wp), DIMENSION(Nlevs_max+1)    :: Ak=ropp_MDFV    ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(Nlevs_max+1)    :: Bk=ropp_MDFV    ! Hybrid/Eta level B-coefficient
  NAMELIST / bgr_profile / year, mon, day, &
                           hour, min, sec, &
                           tlead, &
                           lon, lat, und, roc, coc, azi, &
                           Z0, P0, &
                           Nlevs, &
                           P, T, Q, Z, Ak, Bk

! First input grib file
  INTEGER                :: NLevs1                       ! Number of full model levels
  INTEGER                :: year1, mon1, day1            ! Background validity date
  INTEGER                :: hour1, min1, sec1            ! Background validity time
  REAL(wp)               :: tlead1                       ! Background forecast range (hr)
  REAL(wp)               :: lon1, lat1                   ! Tangent point lat, lon (deg)
  REAL(wp)               :: und1                         ! Tangent point undulation (m)
  REAL(wp)               :: roc1                         ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc1                         ! Tangent point centre of curvature (m)
  REAL(wp)               :: azi1                         ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                            :: Z01             ! Surface geopotential height (m)
  REAL(wp)                            :: P01             ! Surface pressure (Pa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: P1              ! Pressure (Pa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: T1              ! Temperature (K)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Q1              ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Z1              ! Geopotential height (m)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Ak1             ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Bk1             ! Hybrid/Eta level B-coefficient

! Second or surface geopotential ("orography") input file
  INTEGER                :: NLevs2                       ! Number of full model levels
  INTEGER                :: year2, mon2, day2            ! Background validity date
  INTEGER                :: hour2, min2, sec2            ! Background validity time
  REAL(wp)               :: tlead2                       ! Background forecast range (hr)
  REAL(wp)               :: lon2, lat2                   ! Tangent point lat, lon (deg)
  REAL(wp)               :: und2                         ! Tangent point undulation (m)
  REAL(wp)               :: roc2                         ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc2                         ! Tangent point centre of curvature (m)
  REAL(wp)               :: azi2                         ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                            :: Z02             ! Surface geopotential height (m)
  REAL(wp)                            :: P02             ! Surface pressure (hPa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: P2              ! Pressure (hPa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: T2              ! Temperature (K)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Q2              ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Z2              ! Geopotential height (m)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Ak2             ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(:), ALLOCATABLE :: Bk2             ! Hybrid/Eta level B-coefficient

! Functions

  ! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

! Constants

  dtor = ATAN(1.0_wp) / 45.0_wp

!-------------------------------------------------------------
! 1. Initialise
!-------------------------------------------------------------

  CALL message_set_routine ( "grib2bgrasc" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                     GRIB to background ascii converter'              )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')


!-------------------------------------------------------------
! 2. Parse command line options
!-------------------------------------------------------------

  narg = IARGC()

  file_in = " "       ! no default for i/p file name
  file_out = " "       ! assume a default generated from i/p file name
  
  lon_in = ropp_MDFV
  lat_in = ropp_MDFV

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

      CASE ("-lon", "-Lon", "-LON")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        READ (arg, *) lon_in
        WRITE(slon_in, "(f10.5)") lon_in

      CASE ("-lat", "-Lat", "-LAT")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        READ (arg, *) lat_in
        WRITE(slat_in, "(f10.5)") lat_in

      CASE ("-date")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        datestring = arg

      CASE ("-time")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        timestring = arg

      CASE ("-v","-V","--version")
        CALL version_info()
        CALL EXIT(0)

      CASE ("-g", "-G", "--grib")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        file_in2 = arg

      CASE ("-z", "-Z")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        file_inz = arg

      CASE ("-r", "-R", "-roc", "-RoC", "-ROC")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        READ (arg, *) roc

      CASE ("-u", "-U", "-und", "-UND")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        READ (arg, *) und

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
  ENDIF

  IF ( (file_in2 /= " ") .AND. (datestring == " ") .AND. (timestring == " ") ) THEN
    CALL message ( msg_error, "Date and time needed if 2nd input file specified.\n" // &
                              "Will use fields from first file only.")
    file_in2 = " "
  ENDIF

  IF ( narg == 0 ) THEN
    CALL Usage
    CALL EXIT(0)
  ENDIF

!-------------------------------------------------------------
! 3. Check existence of input file(s);
!    generate output file name if not given on command line;
!    check valid lats, lons, dates and times. 
!    Also read and display the environment variables 
!    GEOPOT_COEF, which is the full pathname of the file containing the egm96 geoid coefficients (eg ...ropp_pp/data/egm96.dat), and
!    GEOPOT_CORR, which is the full pathname of the file containing the correction coefficients (eg ...ropp_pp/data/corrcoef.dat).
!    If either is null, issue a warning that the undulation will not be calculable.
!-------------------------------------------------------------

  INQUIRE ( FILE=file_in, EXIST=exists )
  IF ( .NOT. exists ) &
    CALL message ( msg_fatal, "GRIB input file " // TRIM(file_in) // &
                              " not found" )

  IF (file_in2 /= " ") THEN
    INQUIRE ( FILE=file_in2, EXIST=exists )
    IF ( .NOT. exists ) &
      CALL message ( msg_fatal, "GRIB input file " // TRIM(file_in2) // &
                              " not found" )
  ENDIF

  IF (file_inz /= " ") THEN
    INQUIRE ( FILE=file_inz, EXIST=exists )
    IF ( .NOT. exists ) &
      CALL message ( msg_fatal, "GRIB input file " // TRIM(file_inz) // &
                              " not found" )
  ENDIF

  IF ( (lon_in < ropp_MDTV) .OR. (lon_in < -360.0_wp) .OR. (lon_in > 360.0_wp) ) THEN
    CALL message ( msg_fatal, "Unset or invalid input longitude " // TRIM(slon_in) )
  ENDIF

  IF ( (lat_in < ropp_MDTV) .OR. (lat_in <  -90.0_wp) .OR. (lat_in >  90.0_wp) ) THEN
    CALL message ( msg_fatal, "Unset or invalid input latitude " // TRIM(slat_in) )
  ENDIF

  IF ( file_out == " " ) THEN
    j = INDEX(file_in, "/", BACK=.TRUE.)
    file_out = TRIM(ADJUSTL(file_in(j+1:))) // ".nml"
  ENDIF

  IF (LEN(TRIM(datestring)) == 8) THEN
    READ(datestring,*) ii
    year_in = ii/10000
    mon_in  = (ii - year_in*10000)/100
    day_in  = ii - year_in*10000 - mon_in*100
  ELSE
    IF (datestring /= " ") &
      CALL message ( msg_fatal, "Input date " // TRIM(datestring) // " not valid.\n" )
  ENDIF

  IF (LEN(TRIM(timestring)) == 6) THEN
    READ(timestring,*) ii
    hour_in = ii/10000
    min_in  = (ii - hour_in*10000)/100
    sec_in  = ii - hour_in*10000 - min_in*100
  ELSE IF (LEN(TRIM(timestring)) == 4) THEN
    READ(timestring,*) ii
    hour_in   = ii/100
    min_in  = ii - hour_in*100
    sec_in  = 0
  ELSE
    IF (timestring /= " ") &
      CALL message ( msg_fatal, "Input time " // TRIM(timestring) // &
                                " not valid.\n" )
  ENDIF

  GEOPOT_FILE = " "
  CALL getenv ( "GEOPOT_COEF", GEOPOT_FILE )
  IF (GEOPOT_FILE == " ") &
    CALL message(msg_warn, "$GEOPOT_COEF environment file not set; " // &
                           "undulation will not be calculable. \n")

  GEOPOT_FILE = " "
  CALL getenv ( "GEOPOT_CORR", GEOPOT_FILE )
  IF (GEOPOT_FILE == " ") &
    CALL message(msg_warn, "$GEOPOT_CORR environment file not set; " // &
                           "undulation will not be calculable. \n")

  IF (msg_MODE  == VerboseMode) THEN
    CALL message ( msg_diag, "file_in = " // file_in )
    CALL message ( msg_diag, "lon_in = " // slon_in )
    CALL message ( msg_diag, "lat_in = " // slat_in )
    IF (file_in2 /= " ")   CALL message ( msg_diag, "file_in2 = " // file_in2 )
    IF (datestring /= " ") CALL message ( msg_diag, "datestring = " // datestring )
    IF (timestring /= " ") CALL message ( msg_diag, "timestring = " // timestring )
    IF (file_inz /= " ")   CALL message ( msg_diag, "file_inz = " // file_inz )
    CALL message ( msg_diag, "file_out = " // file_out )
  ENDIF


!-------------------------------------------------------------
! 4. Read GRIB file(s)
!-------------------------------------------------------------

! 4.1  Read variables from file_in
! --------------------------------

  CALL message ( msg_info, "Reading file " // TRIM(file_in) )

  CALL extract_prof_from_GRIB( &
                              file_in, &
                              lon_in, lat_in, &
                              year1, mon1, day1, &
                              hour1, min1, sec1, &
                              tlead1, &
                              lon1, lat1, und1, roc1, coc1, azi1, &
                              Nlevs1, &
                              Z01, P01, &
                              P1, T1, Q1, Ak1, Bk1)


! Check consistency

  IF (Nlevs1 > Nlevs_max) CALL message ( msg_fatal, "Too many levels. " // &
                                         "Increase Nlevs_max and recompile. \n" )

  delta_lon = ATAN2(SIN(dtor*(lon_in-lon1)), COS(dtor*(lon_in-lon1))) / dtor ! True whether or not lon_in or lon1 jumps a 2pi "cut"
  IF (ABS(delta_lon) >= 1.0e-6_wp) &
    CALL message ( msg_fatal, "Output and input longitudes do not match. \n" )

  IF (ABS(lat_in-lat1) >= 1.0e-6_wp) &
    CALL message ( msg_fatal, "Output and input latitudes do not match. \n" )

  CDT = (/ year1,  mon1,  day1,  0,  hour1,  min1,  sec1,  0/)
  CALL CalToJul( CDT,  jul_day1, 1 )
  jul_day1 = jul_day1 + (tlead1/24.0_wp)   ! Validity time of fields in file_in

  IF ((datestring /= " ") .AND. (timestring /= " ")) THEN ! Compare input (if specified) and bgr times
    CDT = (/ year_in,  mon_in,  day_in,  0,  hour_in,  min_in,  sec_in,  0/)
    CALL CalToJul( CDT,  jul_day_in, 1 )
    IF (ABS(jul_day1-jul_day_in) > 0.25_wp) &
      CALL message ( msg_warn, "Validity time of background data in " // &
                               TRIM(file_in) // " differs by " // &
                               "more than 6 hours from input time. \n" )
  ENDIF

! Populate the namelist

  year = year1  ;  mon = mon1  ;  day = day1
  hour = hour1  ;  min = min1  ;  sec = sec1
  tlead = tlead1
  lon = lon1  ;  lat = lat1
  coc = coc1  ;  azi = azi1
  Z0 = Z01  ;  P0 = P01
  Nlevs = Nlevs1
  P(1:Nlevs) = P1  ;  T(1:Nlevs) = T1  ;  Q(1:Nlevs) = Q1
  Ak(1:Nlevs+1) = Ak1  ;  Bk(1:Nlevs+1) = Bk1
  IF (roc < ropp_MDTV) roc = roc1  ! Use the user-specified value by default
  IF (und < ropp_MDTV) und = und1  ! Use the user-specified value by default


! 4.2  Read variables from file_in2
! ---------------------------------

  IF (file_in2 /= " ") THEN
  
    CALL message ( msg_info, "Reading file " // TRIM(file_in2) )

    CALL extract_prof_from_GRIB( &
                                file_in2, &
                                lon_in, lat_in, &
                                year2, mon2, day2, &
                                hour2, min2, sec2, &
                                tlead2, &
                                lon2, lat2, und2, roc2, coc2, azi2, &
                                Nlevs2, &
                                Z02, P02, &
                                P2, T2, Q2, Ak2, Bk2)

! Check consistency

    IF (Nlevs2 > Nlevs_max) CALL message ( msg_fatal, "Too many levels. " // &
                                        "Increase Nlevs_max and recompile. \n" )

    delta_lon = ATAN2(SIN(dtor*(lon_in-lon2)), COS(dtor*(lon_in-lon2))) / dtor ! True whether or not lon_in or lon2 jumps a 2pi "cut"
    IF (ABS(delta_lon) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Output and input longitudes do not match. \n" )

    IF (ABS(lat2-lat_in) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Output latitude does not match the input. \n" )

    IF (Nlevs2 /= Nlevs1) &
      CALL message ( msg_fatal, "Different number of levels in the two grib files. \n" )

    IF (ANY(ABS(Ak2-Ak1) >= 1.0e-6_wp)) &
      CALL message ( msg_fatal, "Different Aks in the two grib files. \n" )

    IF (ANY(ABS(Bk2-Bk1) >= 1.0e-6_wp)) &
      CALL message ( msg_fatal, "Different Bks in the two grib files. \n" )

    delta_lon = ATAN2(SIN(dtor*(lon2-lon1)), COS(dtor*(lon2-lon1))) / dtor ! True whether or not lon2 or lon1 jumps a 2pi "cut"
    IF (ABS(delta_lon) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Different longitudes in the two grib files. \n" )

    IF (ABS(lat2-lat1) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Different latitudes in the two grib files. \n" )

    IF (ABS(und2-und1) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Different undulations in the two grib files. \n" )

    IF (ABS(roc2-roc1) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Different radii of curvature in the two grib files. \n" )

    IF (ANY(ABS(coc2-coc1) >= 1.0e-6_wp)) &
      CALL message ( msg_fatal, "Different centres of curvature in the two grib files. \n" )

    IF (ABS(azi2-azi1) >= 1.0e-6_wp) &
      CALL message ( msg_fatal, "Different azimuths in the two grib files. \n" )

    IF ((Z01 > ropp_MDTV) .AND. (Z02 > ropp_MDTV) .AND. (ABS(Z02-Z01) >= 1.0e-6_wp)) &
      CALL message ( msg_fatal, "Different surface geopotentials in the two grib files. \n" )

! Interpolate in time
  
    CDT = (/year2, mon2, day2,  0, hour2, min2, sec2,  0/)
    CALL CalToJul( CDT,  jul_day2, 1 )
    jul_day2 = jul_day2 + (tlead2/24.0_wp)   ! Validity time of fields in file_in2
   
    IF (ABS(jul_day2 - jul_day1) <= 1.0e-6_wp) THEN
      CALL message ( msg_warn, "Same background times in the two grib files. \n"// &
                                "Using 50-50 mixture of fields from each. \n" )
      tt = 0.5_wp
    ELSE
      tt = (jul_day_in - jul_day1) / (jul_day2 - jul_day1)  ! time interpolation coefficient
    ENDIF
    
    P0          = tt*P02 + (1 - tt)*P01
    P(1:Nlevs)  = tt*P2  + (1 - tt)*P1
    T(1:Nlevs)  = tt*T2  + (1 - tt)*T1
    Q(1:Nlevs)  = tt*Q2  + (1 - tt)*Q1
    IF ((Z01 > ropp_MDTV) .AND. (Z02 > ropp_MDTV)) THEN
      Z0        = tt*Z02 + (1 - tt)*Z01  ! Should return Z0 anyway
    ELSE
      IF (Z02 > ropp_MDTV) Z0 = Z02  ! Take Z0 from the second file
    ENDIF

! Set hour etc to be the requested value.  (This also avoids problems interpolating integers.)

    year = year_in  ;  mon = mon_in  ;  day = day_in
    hour = hour_in  ;  min = min_in  ;  sec = sec_in

    tlead = 0.0_wp  ! Since the interpolated fields are valid at the same time as year_in etc.

  ENDIF  ! Interpolating in time

  
! 4.3  Read orography Z0 (only) from file_inz if necessary
! --------------------------------------------------------

  IF (Z0 < ropp_MDTV) THEN

    IF (file_inz == " ") THEN 
      CALL message ( msg_fatal, "Surface geopotential not present in input file(s), " // &
                                "and an auxiliary file containing it is not specified.\n" )
    ENDIF
  
! Read Z0 (only) from file_inz

    CALL message ( msg_info, "Reading file " // TRIM(file_inz) )

    CALL extract_prof_from_GRIB( &
                                file_inz, &
                                lon_in, lat_in, &
                                year2, mon2, day2, &
                                hour2, min2, sec2, &
                                tlead2, &
                                lon2, lat2, und2, roc2, coc2, azi2, &
                                Nlevs2, &
                                Z02, P02, &
                                P2, T2, Q2, Ak2, Bk2)

    IF (Z02 < ropp_MDTV) &
      CALL message ( msg_fatal, "Surface geopotential not present in auxiliary file " // &
                                "that is required to contain it.\n" )

    Z0 = Z02  ! Ignore the other "2" variables.
  
  ENDIF


  IF (Z0 < ropp_MDTV) &   ! Shouldn't ever reach here
    CALL message ( msg_fatal, "Surface geopotential not present in any input file. \n" )


! 4.4 Generate Z at the profile location
! --------------------------------------

  CALL generate_GPH(Z0, P0, T(1:Nlevs), Q(1:Nlevs), Ak(1:Nlevs+1), Bk(1:Nlevs+1), Z1)

  Z(1:Nlevs) = Z1


! 4.5 Write out namelist
! ----------------------

  IF (msg_MODE  == VerboseMode) THEN
    CALL message( msg_diag, "Contents of bgr_profile namelist:" )
    WRITE (*, NML=bgr_profile)  ! For diagnostic purposes
  ENDIF

!-------------------------------------------------------------
! 5. Define and write bgr_profile namelist
!-------------------------------------------------------------

  CALL message ( msg_info, "Writing file " // TRIM(file_out) )

  OPEN (unit=funit, file=TRIM(file_out), status='replace', action='write', iostat=iostatus)

  IF (iostatus > 0) &
    CALL message ( msg_fatal, "I/O error when opening namelist file" )

  WRITE (funit, NML=bgr_profile, iostat=iostatus) 

  IF (iostatus > 0) &
    CALL message ( msg_fatal, "I/O error while writing namelist file" )

  CLOSE (UNIT=funit)



CONTAINS


!-------------------------------------------------------------------------------
! 6. Extract profile from GRIB file
!-------------------------------------------------------------------------------

  SUBROUTINE extract_prof_from_GRIB( &
                        gribfile, &
                        lon_in, lat_in, &
                        year_prf, mon_prf, day_prf, &
                        hour_prf, min_prf, sec_prf, &
                        tlead_prf, &
                        lon_prf, lat_prf, und_prf, roc_prf, coc_prf, azi_prf, &
                        Nlevs, &
                        Z0_prf, P0_prf, &
                        P_prf, T_prf, Q_prf, Ak_prf, Bk_prf)

    USE grib_api
    USE EarthMod, ONLY: Datum_HMSL

    IMPLICIT NONE

    CHARACTER(LEN=256), INTENT(IN)  :: gribfile           ! Input grib filename

    REAL(wp), INTENT(IN)   :: lon_in, lat_in              ! Tangent point lat, lon (deg)

    INTEGER,  INTENT(OUT)  :: year_prf, mon_prf, day_prf  ! Background validity date
    INTEGER,  INTENT(OUT)  :: hour_prf, min_prf, sec_prf  ! Background validity time
    REAL(wp), INTENT(OUT)  :: tlead_prf                   ! Background forecast range (hr)
    REAL(wp), INTENT(OUT)  :: lon_prf, lat_prf            ! Tangent point location (deg)
    REAL(wp), INTENT(OUT)  :: und_prf                     ! Tangent point undulation (m)
    REAL(wp), INTENT(OUT)  :: roc_prf                     ! Tangent point radius of curvature (m)
    REAL(wp), DIMENSION(3), INTENT(OUT)  :: coc_prf       ! Tangent point centre of curvature (m)
    REAL(wp), INTENT(OUT)  :: azi_prf                     ! GNSS->LEO line of sight angle (degT)

    INTEGER,                             INTENT(OUT)  :: Nlevs
    REAL(wp),                            INTENT(OUT)  :: Z0_prf, P0_prf
    REAL(wp), DIMENSION(:), ALLOCATABLE, INTENT(OUT)  :: P_prf, T_prf, Q_prf, Ak_prf, Bk_prf

!   Local variables: GRIB work space, etc.
    INTEGER                :: Nlat, Nlon, Nlev
    INTEGER                :: date, time
    INTEGER                :: ifile=5, iret, imess, Nmess, ilev, from, to
    INTEGER                :: ilon, ilonp1, ilat, ilatp1
    INTEGER                :: centre, gridsize, pvsize
    CHARACTER(LEN=8)       :: shortname
    CHARACTER(LEN=256)     :: sret, routine
    INTEGER                :: numberOfPoints
    REAL(KIND=8)           :: missingValue=9999
    REAL(wp)               :: dlon, dlat, tt, uu
    REAL(KIND=4), DIMENSION(:),     ALLOCATABLE :: pv
    REAL(KIND=8), DIMENSION(:),     ALLOCATABLE :: lats, lons, values
    REAL(KIND=8), DIMENSION(:),     ALLOCATABLE :: lon_grb, lat_grb
    REAL(KIND=8), DIMENSION(:),     ALLOCATABLE :: Ak_grb, Bk_grb
    REAL(KIND=8), DIMENSION(:,:),   ALLOCATABLE :: Z0_grb, P0_grb
    REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: P_grb, T_grb, Q_grb

    REAL(wp), PARAMETER    :: g_wmo=9.80665   ! Standard gravity (m/s2)


! Set subroutine name for messages

    CALL message_get_routine(routine)
    CALL message_set_routine('extract_prof_from_GRIB')

! 0.0 Set defaults
! ----------------

    lon_prf = ropp_MDFV  ;  lat_prf=ropp_MDFV
    und_prf = ropp_MDFV
    roc_prf = ropp_MDFV
    coc_prf = (/ropp_MDFV, ropp_MDFV, ropp_MDFV/)
    azi_prf = ropp_MDFV


! 1.0 Open file for reading metadata from first message;
!     check that it is readable as a GRIB file.
! ------------------------------------------------------

    CALL grib_open_file(ifile, TRIM(gribfile), 'R', iret)
    IF (iret /= GRIB_SUCCESS) THEN
      IF (msg_MODE  == VerboseMode) THEN
        CALL grib_get_error_string (iret, sret)
        CALL message( msg_diag, "GRIB error message: " // TRIM(sret) )
      ENDIF
      CALL message ( msg_fatal, "Error opening " // TRIM(gribfile) // &
                                " - check file existence and format" )
    ENDIF
    
    CALL grib_new_from_file(ifile, imess, iret)
    IF (iret /= GRIB_SUCCESS) THEN
      IF (msg_MODE  == VerboseMode) THEN
        CALL grib_get_error_string (iret, sret)
        CALL message( msg_diag, "GRIB error message: " // TRIM(sret) )
      ENDIF
      CALL message ( msg_fatal, "Error reading " // TRIM(gribfile) // &
                                " - check file existence and format" )
    ENDIF


! 1.1  Read header info from first message in GRIB file
! -----------------------------------------------------

    CALL grib_get(imess, 'centre',         centre)
    CALL grib_get(imess, 'numberOfPoints', gridsize)
    CALL grib_get_size(imess, 'pv',        pvsize)
    CALL grib_get(imess, 'numberOfVerticalCoordinateValues', Nlev)
    CALL grib_get(imess, 'Ni',                               Nlon)
    CALL grib_get(imess, 'Nj',                               Nlat)
    IF (centre/=98) then
      CALL message ( msg_fatal, "Error: invalid centre (not 98 = ECMWF)" )
    ENDIF

    IF (Nlat<1 .or. Nlon<1 .or. Nlev<1 .or. gridsize/=(Nlat*Nlon) .or. pvsize/=Nlev) THEN
      CALL message ( msg_fatal, "Error: invalid or inconsistent Nlat, Nlon, Nlev, gridsize or pvsize" )
    ENDIF

    dlon = 360.0_wp / Nlon
    dlat = 180.0_wp / (Nlat-1)
    Nlev = (Nlev - 2) / 2   ! Number of full model levels

    ALLOCATE(lats(gridsize))  ! workspace
    ALLOCATE(lons(gridsize))  ! workspace
    ALLOCATE(values(gridsize))  ! workspace


! 1.2  Read date/time info from GRIB file
! ---------------------------------------

    CALL grib_get(imess, 'dataDate',  date)
    CALL grib_get(imess, 'dataTime',  time)
    CALL grib_get(imess, 'stepRange', tlead_prf)

    year_prf  = date / 10000
    mon_prf = (date - year_prf*10000) / 100
    day_prf   = date - year_prf*10000 - mon_prf*100
    hour_prf  = time / 100
    min_prf   = time - hour_prf*100
    sec_prf   = 0


! 1.3 Read pv and extract Ak and Bk from it
! -----------------------------------------

    ALLOCATE(ak_grb(Nlev+1), bk_grb(Nlev+1))
    ALLOCATE(pv(pvsize))

    CALL grib_get(imess, 'pv', pv)

    DO ilev=1,Nlev+1
      ak_grb(ilev) = DBLE(pv(  Nlev+2-ilev))
      bk_grb(ilev) = DBLE(pv(2*Nlev+3-ilev))
    ENDDO
    ak_grb(Nlev+1) = 1.0d-32

    DEALLOCATE(pv)


! 1.4 Read lat and lon
! --------------------

    ALLOCATE(lat_grb(Nlat), lon_grb(Nlon))

    CALL grib_get_data(imess, lats, lons, values)

    DO ilat=Nlat,1,-1
      from = (Nlat-ilat)*Nlon + 1
      lat_grb(ilat) = lats(from)
    ENDDO

    lon_grb(:) = lons(1:Nlon)

    CALL grib_close_file(ifile)

! 1.5 Set number of levels
! ------------------------

    Nlevs = Nlev


! 2.0 Reopen file and read all required grib messages
! ---------------------------------------------------

    ALLOCATE(Z0_grb(Nlat,Nlon), P0_grb(Nlat,Nlon))
    Z0_grb(:, :) = ropp_MDFV  ! In case it isn't read in (eg it's a fcast file)

    ALLOCATE(P_grb(Nlat,Nlon,Nlev), T_grb(Nlat,Nlon,Nlev), Q_grb(Nlat,Nlon,Nlev))

    Nmess = 0

    CALL grib_open_file(ifile, TRIM(gribfile), 'R', iret)

    CALL grib_new_from_file(ifile, imess, iret)

    DO WHILE (iret /= GRIB_END_OF_FILE)

      Nmess = Nmess + 1

      CALL grib_get(imess, 'level', ilev)

      ilev = Nlev - ilev + 1

      CALL grib_set(imess, 'missingValue', missingValue)

      CALL grib_get_data(imess, lats, lons, values)

      CALL grib_get(imess, 'shortName', shortname)

      IF (TRIM(shortName)=='t') THEN  ! Temperature in K
        DO ilat=Nlat,1,-1
          from = (Nlat-ilat)*Nlon + 1
          to   = (Nlat-ilat)*Nlon + Nlon
          T_grb(ilat,:,ilev) = values(from:to)
        ENDDO
      ELSEIF (TRIM(shortName)=='q') THEN  ! Specific humidity in kg/kg
        DO ilat=Nlat,1,-1
          from = (Nlat-ilat)*Nlon + 1
          to   = (Nlat-ilat)*Nlon + Nlon
          Q_grb(ilat,:,ilev) = values(from:to)*1000.0d0  ! convert to g/kg
        ENDDO
      ELSEIF ((TRIM(shortName)=='z') .AND. (ilev==Nlev)) THEN  ! Surface geopotential in m2/s2
        DO ilat=Nlat,1,-1
          from = (Nlat-ilat)*Nlon + 1
          to   = (Nlat-ilat)*Nlon + Nlon
          Z0_grb(ilat,:) = values(from:to)/g_wmo  ! convert to GPH in m
        ENDDO
      ELSEIF ((TRIM(shortName)=='lnsp') .AND. (ilev==Nlev)) THEN  ! Log(surface pressure in Pa)
        DO ilat=Nlat,1,-1
          from = (Nlat-ilat)*Nlon + 1
          to   = (Nlat-ilat)*Nlon + Nlon
          P0_grb(ilat,:) = EXP(values(from:to))  ! convert to P* in Pa
        ENDDO
      ENDIF

      CALL grib_release(imess)

      CALL grib_new_from_file(ifile, imess, iret)

    ENDDO

    CALL grib_close_file(ifile)

    DEALLOCATE(lats, lons, values)


! 3.0 Calculate P on full pressure levels
! ---------------------------------------

    DO ilev=1,Nlev
      P_grb(:,:,ilev) = 0.5_wp*(ak_grb(ilev  ) + bk_grb(ilev  )*P0_grb(:,:)) + &
                        0.5_wp*(ak_grb(ilev+1) + bk_grb(ilev+1)*P0_grb(:,:))
    ENDDO


! 3.1 Interpolate bilinearly in (lon,lat) to tangent point location,
!     to generate a profile.  This will go bad near the poles,
!     where (eg) a polar stereographic projection would be better
! ------------------------------------------------------------------

    ALLOCATE(P_prf(Nlevs), T_prf(Nlevs), Q_prf(Nlevs), Ak_prf(Nlevs+1), Bk_prf(Nlevs+1))

    ilon = MOD(Nlon + FLOOR((lon_in-lon_grb(1))/dlon), Nlon) + 1 
    ilat = MOD(Nlat + FLOOR((lat_in-lat_grb(1))/dlat), Nlat) + 1 

    ilonp1 = MOD(ilon,Nlon) + 1
    ilatp1 = ilat + 1

!    tt = MOD((360.0d0 + lon_in - lon_grb(ilon)), 360.0d0) / dlon  ! Interpolation coefficient
    tt = ASIN(SIN((lon_in - lon_grb(ilon))*dtor)) / dtor / dlon    ! Safe interpolation coefficient
    uu = (lat_in - lat_grb(ilat)) / dlat                          ! Interpolation coefficient

!    lon_prf = (1.0d0-tt)*lon_grb(ilon) + tt*lon_grb(ilonp1)  ! Should be the same as lon_in
    lon_prf = lon_grb(ilon) + tt*dlon  ! Should be the same as lon_in

    lat_prf = (1.0d0-uu)*lat_grb(ilat) + uu*lat_grb(ilatp1)  ! Should be the same as lat_in

    Z0_prf  = (1.0d0-tt)*(1.0d0-uu)*Z0_grb(  ilat,  ilon)  +         tt*(1.0d0-uu)*Z0_grb(  ilat,ilonp1)  + &
                     tt*        uu *Z0_grb(ilatp1,ilonp1)  + (1.0d0-tt)*       uu *Z0_grb(ilatp1,  ilon)

    P0_prf  = (1.0d0-tt)*(1.0d0-uu)*P0_grb(  ilat,  ilon)  +         tt*(1.0d0-uu)*P0_grb(  ilat,ilonp1)  + &
                     tt*        uu *P0_grb(ilatp1,ilonp1)  + (1.0d0-tt)*       uu *P0_grb(ilatp1,  ilon)

    P_prf   = (1.0d0-tt)*(1.0d0-uu)*P_grb(  ilat,  ilon,:) +         tt*(1.0d0-uu)*P_grb(  ilat,ilonp1,:) + &
                     tt*        uu *P_grb(ilatp1,ilonp1,:) + (1.0d0-tt)*       uu *P_grb(ilatp1,  ilon,:)

    T_prf   = (1.0d0-tt)*(1.0d0-uu)*T_grb(  ilat,  ilon,:) +         tt*(1.0d0-uu)*T_grb(  ilat,ilonp1,:) + &
                     tt*        uu *T_grb(ilatp1,ilonp1,:) + (1.0d0-tt)*       uu *T_grb(ilatp1,  ilon,:)

    Q_prf   = (1.0d0-tt)*(1.0d0-uu)*Q_grb(  ilat,  ilon,:) +         tt*(1.0d0-uu)*Q_grb(  ilat,ilonp1,:) + &
                     tt*        uu *Q_grb(ilatp1,ilonp1,:) + (1.0d0-tt)*       uu *Q_grb(ilatp1,  ilon,:)

    Ak_prf = Ak_grb

    Bk_prf = Bk_grb

    DEALLOCATE(lat_grb, lon_grb, Z0_grb, P0_grb, P_grb, T_grb, Q_grb, Ak_grb, Bk_grb)


! 3.2 Generate undulation at the TP
!     This routine requires the user to have exported the environment variables:
! (1) GEOPOT_COEF, which is the full pathname of the file containing the egm96 geoid coefficients
!     (eg ...ropp_pp/data/egm96.dat); and
! (2) GEOPOT_CORR, which is the full pathname of the file containing the correction coefficients
!     (eg ...ropp_pp/data/corrcoef.dat)
! -----------------------------------------------------------------------------------------------

    CALL datum_HMSL("WGS84", (/lat_in, lon_in, 0.0_wp/), und_prf)

    IF (und_prf > ropp_MDTV) und_prf = -und_prf ! Because datum_HMSL actually returns height of ellipsoid (0 here) wrt MSL.


! 4.0 Clean up
! ------------

    CALL message_set_routine(routine)


  END SUBROUTINE extract_prof_from_GRIB


!-------------------------------------------------------------------------------
! 7. Generate geopotential height at the interpolated point by integrating density
!-------------------------------------------------------------------------------

  SUBROUTINE generate_GPH(Z0, P0, T, Q, Ak, Bk, Z)
  
    IMPLICIT NONE

    REAL(wp),               INTENT(IN)                :: Z0, P0
    REAL(wp), DIMENSION(:), INTENT(IN)                :: T, Q
    REAL(wp), DIMENSION(:), INTENT(IN)                :: Ak, Bk
    REAL(wp), DIMENSION(:), INTENT(OUT), ALLOCATABLE  :: Z

!   Local
    REAL(wp), DIMENSION(:), ALLOCATABLE               :: Tvirt, Phlv, deltaP, lnP, Zhlv, dZ, alpha
    INTEGER                                           :: ihlev, Nz

!   Constants
    REAL(wp), PARAMETER                               :: g_wmo=9.80665   ! Standard gravity (m/s2)
    REAL(wp), PARAMETER                               :: R_dry=287.0597  ! Dry gas constant (K/kg/K)

    Nz = SIZE(ak) - 1
    ALLOCATE(Z(Nz))
    ALLOCATE(Tvirt(Nz), Phlv(Nz+1), deltaP(Nz), lnP(Nz), Zhlv(Nz+1), dZ(Nz), alpha(Nz))

    !--- Virtual temperatures
    Tvirt = (1.0_wp + 0.61_wp * Q/1000.0_wp) * T

    !--- Pressure differences
    Phlv = ak + bk*P0
    deltaP = Phlv(1:Nz) - Phlv(2:Nz+1)

    !--- Log of pressure ratio
    lnP = LOG(Phlv(1:Nz)/Phlv(2:Nz+1))

    !--- Interpolation coefficients 
    alpha = 1.0d0 - Phlv(2:Nz+1)/deltaP * lnP
    alpha(Nz) = LOG(2.0d0)

    !--- Function to be integrated 
    dZ = R_dry * Tvirt * lnP / g_wmo

    !--- Calculate geopotential height integral
    Zhlv(1) = 0.0_wp
    DO ihlev = 2,Nz+1
      Zhlv(ihlev) = SUM(dZ(1:ihlev-1))
    ENDDO
    IF (Z0 > ropp_MDTV) Zhlv(:) = Zhlv(:) + Z0

    !--- Interpolate onto full levels 
    Z = Zhlv(1:Nz) + alpha * R_dry * Tvirt / g_wmo

    DEALLOCATE(Tvirt, Phlv, deltaP, lnP, Zhlv, dZ, alpha)
  
  END SUBROUTINE generate_GPH

!-------------------------------------------------------------------------------
! 8. Usage (help) information
!-------------------------------------------------------------------------------

  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Extract a background fields profile, in fortran namelist format,'
    PRINT *, '  from a GRIB (edition 1 or 2) file containing background model data.'
    PRINT *, 'Usage:'
    PRINT *, '  > grib2bgrasc grib_file -lat <lat> -lon <lon>'
    PRINT *, '                [-date <date> ] [-time <time>]'
    PRINT *, '                [-g <grib_file2>]'
    PRINT *, '                [-z <orog_file>]'
    PRINT *, '                [-o <namelist_file>]'
    PRINT *, '                [-d] [-h] [-v]'
    PRINT *, '  where:'
    PRINT *, '    grib_file is a GRIB file holding {p*, phi*, T, q};'
    PRINT *, '    -lat <lat> is the latitude of the required profile (deg);'
    PRINT *, '    -lon <lon> is the longitude of the required profile (deg).'
    PRINT *, 'Options:'
    PRINT *, '  -date <date> is the date required (yyyymmdd)'
    PRINT *, '  -time <time> is the time required (HHMM or HHMMSS)'
    PRINT *, '  -g <grib_file2> is a second GRIB file, if time interpolation is desired'
    PRINT *, '  -z <grib_filez> is an additional GRIB file containing surface geopotential,'
    PRINT *, '                  if it is not in either grib_file or grib_file2'
    PRINT *, '  -o <namelist_file> is the output file name'
    PRINT *, '  -r <radius_of_curvature> is the radius of curvature (in m)'
    PRINT *, '  -u <undulation> is the undulation (in m)'
    PRINT *, '  -d prints out some additional diagnostics to stdout'
    PRINT *, '  -h this help'
    PRINT *, '  -v version information'
    PRINT *, 'Defaults:'
    PRINT *, '  Input  file name : required'
    PRINT *, '  Output file name : $PWD/input_file.nml'
    PRINT *, 'Note that the environment variables GEOPOT_COEF and GEOPOT_CORR must be'
    PRINT *, '  set appropriately if the user wants the undulation to be calculated.'
    PRINT *, 'See grib2bgrasc(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage


!-------------------------------------------------------------------------------
! 9. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    USE ropp_io, ONLY : ropp_io_version
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'grib2bgrasc -  GRIB to background ASCII decoder [ECMWF library]'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info


END PROGRAM grib2bgrasc
