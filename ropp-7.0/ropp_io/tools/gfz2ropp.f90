! $Id: gfz2ropp.f90 3831 2013-08-27 15:22:48Z idculv $

PROGRAM gfz2ropp

!****x* Programs/gfz2ropp *
!
! NAME
!   gfz2ropp
!
! SYNOPSIS
!   Convert GFZ NRT DAT/DSC files to ROPP netCDF
!
!   > gfz2ropp <gfzfile.dat> [-o opfile] [-d] [-h] [-v]
!
! ARGUMENTS
!   gfzfile.dat - GFZ 'PD' or 'NRT' DAT (.dat) file containing Level 1b,2a,2b
!                 RO profiles. May include a path. The companion DSC file
!                 containing meta-data is assumed to be in the same path and
!                 have the same name at the DAT file, but with a type of .dsc.
!                 There is no default for this argument.
!
! OPTIONS
!  -o  - specifies the output netCDF file name. The default name
!        is the same as the DAT/DSC file name, but with a type of
!        .nc and in the current working directory.
!  -d - writes additional diagnostic information to stdout
!  -h - help
!  -v - version information
!
! INPUTS
!   Pair of GFZ NRT .dat (profile) and .dsc (meta information)
!
! OUTPUTS
!   ROPP netCDF file - As given with the -o option, or defaults to the
!                      same path and file name as for input DAT file,
!                      but with type .nc
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
!   ropp_io_occid
!   ropp_io_ascend
!   ropp_io_write
!   ropp_io_free
!   ropp_io_version
!   message
!   message_set_routine
!
! DESCRIPTION
!   Conversion from GFZ native-format GPSRO 'PD' (ROPP Level1a) or 'NRT'
!   (ROPP Levels 1b,2a,2b) data to ROPP netCDF format. Input is the pair of
!   DAT (data) & DCS (description) files; the profiles are taken from the
!   former, and meta-data (non-profile header info) from the latter.
!   The data is then written out to a ROPP-standard netCDF file in PWD.
!   Only the name of the DAT file is given on the command line; the companion
!   DSC file is assumed to be in the same path and have the same name at the
!   DAT file, but with a type of .dsc.
!
! REFERENCES
!   1. ROPP User Guide - Part I.
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

  USE typesizes,     dp => EightByteReal
  USE messages
  USE ropp_utils,    ONLY: To_Upper
  USE ropp_io_types, ONLY: ROprof,          &
                           PCD_rising
  USE ropp_io,       ONLY: ropp_io_free,    &
                           ropp_io_init,    &
                           ropp_io_occid,   &
                           ropp_io_write,   &
                           ropp_io_addvar,  &
                           ropp_io_version

  IMPLICIT NONE

! Fixed parameters

  INTEGER,           PARAMETER :: funit        = 11
  REAL(dp),          PARAMETER :: CtoK         = 273.15_dp
  REAL(dp),          PARAMETER :: KMtoM        = 1000.0_dp
  REAL(dp),          PARAMETER :: SecsPerDay   = 86400.0_dp

  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                     "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"

! Local variables

  TYPE(ROprof) :: ROdata ! RO main profile structure

  CHARACTER (LEN=256) :: arg                          ! Command line argument
  CHARACTER (LEN=256) :: datfile, dscfile, opfile     ! I/P & O/P file names
  CHARACTER (LEN=200) :: line                         ! Line read from I/P file
  CHARACTER (LEN=80)  :: key                          ! key word(s) in DSC file
  INTEGER             :: i, j                         ! Loop counters / indices
  INTEGER             :: iarg                         ! Command line argument index
  INTEGER             :: narg                         ! No. of command line arguments
  INTEGER             :: iostatus                     ! I/O Status
  INTEGER             :: NLevs                        ! Number of samples in DAT file
  INTEGER             :: SatID                        ! Satellite ID
  INTEGER             :: Year, Month, Day             ! Processing date from DSC file
  INTEGER             :: Hour, Minute, Second         ! Occultation end time from DSC file
  INTEGER             :: SWrev = 0                    ! GFZ revision number
  LOGICAL             :: exists                       ! File exists flag
  LOGICAL             :: setting_occ                  ! .T. if a setting occultation
  REAL(dp)            :: SWver = 0.0_dp               ! GFZ POCS software version number
  REAL(dp)            :: sTime, eTime                 ! Start & end time of occultation (secs)

  INTEGER             :: qflag                        ! parameters read but not used in ROPP
  REAL(dp)            :: snr1, snr2                   ! parameters read but not used in ROPP
  REAL(dp)            :: density, pressure, basmth    ! parameters read but not used in ROPP
  REAL(dp)            :: alpha, beta, gamma, phaseLC  ! parameters read but not used in ROPP

  LOGICAL             :: lev1a_exists = .FALSE.       ! Reading Level 1a data flag
  LOGICAL             :: no_productid = .TRUE.        ! No Product_ID present flag
  REAL                :: Start_Lat, Start_Lon         ! Nominal start lat/lon (for Level 1a data)
  REAL(dp), DIMENSION(:), ALLOCATABLE :: LCF          ! Lost carrier flag from Level 1a file
  CHARACTER (LEN=80)  :: outstr                       ! Formatted output string
  CHARACTER (LEN=10)  :: number                       ! Number as string

! Some compilers may need the following declaration to be commented out
  INTEGER             :: IARGC

!-------------------------------------------------------------
! 1. Initialise
!-------------------------------------------------------------

  CALL message_set_routine ( "gfz2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                     GFZ to ROPP Converter'                           )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------
! 2. Parse command line options
!-------------------------------------------------------------

  narg = IARGC()

  datfile = " "       ! no default for i/p file name
  opfile  = " "       ! assume a default generated from i/p file name

  iarg = 1
  DO WHILE ( iarg <= narg )
    CALL GETARG ( iarg, arg )

    SELECT CASE (arg)
      CASE ("-d","-D","--debug")
        msg_MODE = VerboseMode

      CASE ("-h","-H","--help","?")
        narg = 0
        datfile = "dummy"

      CASE ("-o","-O","--output")
        iarg = iarg + 1
        CALL GETARG ( iarg, arg )
        opfile = arg

      CASE ("-v","-V","--version")
        CALL version_info()
        CALL EXIT(0)

      CASE DEFAULT
         IF ( arg(1:1) /= '-' ) THEN
           datfile = arg
         END IF
    END SELECT

    iarg = iarg + 1
  END DO

  IF ( datfile == " " ) THEN
    CALL message ( msg_error, "No input file(s) specified" )
    narg = 0
  END IF

  IF ( narg == 0 ) THEN
    CALL Usage
    CALL EXIT(0)
  END IF

!-------------------------------------------------------------
! 3. Check GFZ .dat and .dsc input files exist;
!    make output file name if not given on command line
!-------------------------------------------------------------

  INQUIRE ( FILE=datfile, EXIST=exists )
  IF ( .NOT. exists ) &
    CALL message ( msg_fatal, "GFZ input file " // TRIM(datfile) // &
                              " not found" )

  i = INDEX ( datfile, ".dat" )
  IF ( i == 0 ) &
    CALL message ( msg_fatal, "GFZ input file " // TRIM(datfile) //  &
                              " not of type '.dat'.\n" //            &
                              "Check input file type." )

  dscfile = datfile(1:i-1)//".dsc"
  INQUIRE ( FILE=dscfile, EXIST=exists )
  IF ( .NOT. exists ) &
    CALL message ( msg_fatal, "GFZ input file " // TRIM(dscfile) //  &
                              " not found" )

  IF ( opfile == " " ) THEN
    j = INDEX(datfile, "/", BACK=.TRUE.)
    opfile = datfile(j+1:i-1)//".nc"
  END IF

!-------------------------------------------------------------
! 4. Read data (.dat) file
!-------------------------------------------------------------

  CALL message ( msg_info, "Reading file " // TRIM(datfile) )

  OPEN ( UNIT=funit,   &
         FILE=datfile, &
       STATUS="OLD",   &
       ACTION="READ" )

!-------------------------------------------------------------
! 4.1 Read DAT file header data & extract no. of points in
!     data section plus other meta-info.
!-------------------------------------------------------------

  DO
    READ ( UNIT=funit, FMT="(A)", IOSTAT=iostatus ) line
    IF ( iostatus  /= 0 .OR. &
         line(1:1) /= "#" ) EXIT

    CALL To_Upper ( Line )
    key = line(2:30)
    arg = ADJUSTL(line(31:))

    SELECT CASE (key)
      CASE ("NUMBER OF DATA   LINES")
        READ ( arg, FMT=* ) NLevs

      CASE ("STARTTIME(UTC)", "STARTTIME (UTC)") ! Date/Time of start of occultation (& secs since midnight)
        READ ( arg, FMT="(I4,5(1X,I2))" ) ROdata%DTocc%Year,   &
                                          ROdata%DTocc%Month,  &
                                          ROdata%DTocc%Day,    &
                                          ROdata%DTocc%Hour,   &
                                          ROdata%DTocc%Minute, &
                                          ROdata%DTocc%Second
        ROdata%DTocc%Msec = 000
        sTime = ((ROdata%DTocc%Hour*60.0_dp)   &
              +   ROdata%DTocc%Minute)*60.0_dp &
              +   ROdata%DTocc%Second

      CASE ("ENDTIME(UTC)","ENDTIME (UTC)") ! Date/Time of end of occultation (secs since midnight)
        READ ( arg, FMT="(10X,3(1X,I2))" ) Hour, Minute, Second
        eTime = ((Hour*60.0_dp)   &
              +   Minute)*60.0_dp &
              +   Second

      CASE ("OCCSAT(PRN)") ! GNSS (GPS) satellite ID
        READ  ( arg, FMT=* ) SatID
        WRITE ( ROdata%GNS_ID, FMT="(A1,I3.3)" ) "G", SatID

      CASE ("OCCULTATION DIRECTION") ! Azimuth angle GNSS-->LEO (deg) (if not in DSC)
        READ ( arg, FMT=* ) ROdata%GeoRef%Azimuth

      CASE ("ALT_MSL(KM)|LATITUDE(DEG)|LON")
         lev1a_exists = .FALSE.
         CALL message ( msg_diag, "File contains Level 1b, 2a, 2b data" )

      CASE(" T|SNR_CA|SNR_P1|SNR_P2|X_LEO")
         lev1a_exists = .TRUE.
         CALL message ( msg_diag, "File contains Level 1a data" )
         NLevs = -1      ! Find number of entries in file
         DO
            READ ( UNIT=funit, FMT="(A)", IOSTAT=iostatus ) line
            IF (iostatus /= 0) THEN
               EXIT
            ENDIF
            NLevs = NLevs + 1
         END DO
         DO i=1,Nlevs+1
            BACKSPACE (UNIT=funit)
         END DO

      CASE DEFAULT
    END SELECT
  END DO
  IF ( iostatus == 0 ) BACKSPACE ( UNIT=funit )

!-------------------------------------------------------------
! 4.2 Initialise ROPP structures & fill in static header data
!     if not in DAT or DSC files
!-------------------------------------------------------------

  IF ( lev1a_exists ) THEN
    CALL ropp_io_init ( ROdata, NLevs, 0, 0, 0, 0, 0 )
    ALLOCATE ( lcf(NLevs) )
  ELSE
    CALL ropp_io_init ( ROdata, 0, NLevs, NLevs, 0, 0, 0 )
  END IF

  ROdata%Processing_Centre = "GFZ  Helmholtz Centre, Potsdam"
  ROdata%Overall_Qual      = 100.0_dp

!-------------------------------------------------------------
! 4.3 Read levels data
!-------------------------------------------------------------

  DO i = 1, NLevs

    IF ( lev1a_exists ) THEN
      READ ( UNIT=funit,                                 &
              FMT=*,                                     &
           IOSTAT=iostatus ) ROdata%Lev1a%dtime(i),      &
                             ROdata%Lev1a%snr_L1ca(i),   &
                             ROdata%Lev1a%snr_L1p(i),    &
                             ROdata%Lev1a%snr_L2p(i),    &
                             ROdata%Lev1a%r_leo(i,:),    &
                             ROdata%Lev1a%v_leo(i,:),    &
                             ROdata%Lev1a%r_gns(i,:),    &
                             ROdata%Lev1a%v_gns(i,:),    &
                             phaseLC,                    &
                             ROdata%Lev1a%phase_L1(i),   &
                             ROdata%Lev1a%phase_L2(i),   &
                             lcf(i)
    ELSE
      READ ( UNIT=funit, &
             FMT=*, &
          IOSTAT=iostatus ) ROdata%Lev2a%Alt_Refrac(i),  &
                            ROdata%Lev1b%Lat_tp(i),      &
                            ROdata%Lev1b%Lon_tp(i),      &
                            ROdata%Lev2a%Refrac(i),      &
                            density,                     &
                            pressure,                    &
                            ROdata%Lev2a%Dry_Temp(i),    &
                            basmth,                      &
                            ROdata%Lev1b%Impact(i),      &
                            alpha, beta, gamma,          &
                            snr1,  snr2, qflag,          &
                            ROdata%Lev2a%Geop_Refrac(i), &
                            ROdata%Lev1b%Bangle(i)
    END IF

    IF ( iostatus /= 0 ) EXIT
  END DO

  CLOSE ( UNIT=funit )

  IF ( iostatus > 0 ) THEN
    CALL message ( msg_fatal, "I/O error while reading DAT file" )
  END IF

!-------------------------------------------------------------
! 4.4 Copy/convert units to ROPP standard
!-------------------------------------------------------------

  IF ( lev1a_exists ) THEN
    ROdata%Lev1a%r_leo(:,:)     = ROdata%Lev1a%r_leo(:,:) * KMtoM
    ROdata%Lev1a%v_leo(:,:)     = ROdata%Lev1a%v_leo(:,:) * KMtoM
    ROdata%Lev1a%r_gns(:,:)     = ROdata%Lev1a%r_gns(:,:) * KMtoM
    ROdata%Lev1a%v_gns(:,:)     = ROdata%Lev1a%v_gns(:,:) * KMtoM
    ROdata%Lev1a%phase_qual(:)  = 100.0_dp
    ROdata%Lev1a%reference_frame%r_leo = "ECI"
    ROdata%Lev1a%reference_frame%r_gns = "ECI"
  ELSE
    ROdata%Lev1b%Impact(:)      = ROdata%Lev1b%Impact(:)     * KMtoM
    ROdata%Lev1b%Bangle_Qual(:) = 100.0_dp
    ROdata%Lev2a%Alt_Refrac(:)  = ROdata%Lev2a%Alt_Refrac(:) * KMtoM
    ROdata%Lev2a%Refrac_Qual(:) = 100.0_dp
    ROdata%Lev2a%Dry_Temp(:)    = ROdata%Lev2a%Dry_Temp(:)   + CtoK
  END IF

!-------------------------------------------------------------
! 4.5 Add lost carrier flag data (Level 1a)
!-------------------------------------------------------------

  IF ( lev1a_exists ) THEN
    CALL ropp_io_addvar ( ROdata, name = "lcf",             &
                          long_name= "Lost carrier flag",   &
                          units    = " ",                   &
                          range    = (/0.0_dp, 1.0_dp/),    &
                          data     = lcf )
    DEALLOCATE ( lcf )
  END IF

!-------------------------------------------------------------
! 5. Read DSC file
!-------------------------------------------------------------

  CALL message ( msg_info, "Reading file " // TRIM(dscfile) )
  OPEN ( UNIT=funit,   &
         FILE=dscfile, &
       STATUS="OLD",   &
       ACTION="READ" )

!-------------------------------------------------------------
! 5.1 Parse lines for key words and extract values for those
!     meta-data that are not in the DAT file
!-------------------------------------------------------------

  setting_occ = .TRUE.

  DO
    READ ( UNIT=funit, FMT="(A)", IOSTAT=iostatus ) line
    IF ( iostatus /= 0 ) EXIT

    CALL To_Upper ( Line )
    i = INDEX ( line, "=" ) + 1
    IF ( i < 3 ) CYCLE
    key = line(1:i-2)

    j = INDEX ( line, ';' ) - 1
    IF ( j < i ) j = LEN_TRIM ( line )
    arg = ADJUSTL(line(i:j))

    SELECT CASE (key)

! DSC file has Processing Date but not the Time. ropp_io_init will have
! intialised Processing Date/Time to current time; if Date in DSC file
! is not today, use it & set Time to the end of the day, else leave
! current time as processing time.

      CASE ("GENERATION DATE") ! Date/Time of processing
        READ ( arg, FMT="(I4,2(1X,I2))" ) Year, Month, Day
        IF ( Year  /= ROdata%DTpro%Year  .OR. &
             Month /= ROdata%DTpro%Month .OR. &
             Day   /= ROdata%DTpro%Day ) THEN
          ROdata%DTpro%Year   = Year
          ROdata%DTpro%Month  = Month
          ROdata%DTpro%Day    = Day
          ROdata%DTpro%Hour   = 23
          ROdata%DTpro%Minute = 59
          ROdata%DTpro%Second = 59
          ROdata%DTpro%Msec   = 999
        ELSE
          ROdata%DTpro%Msec   = 000
        END IF

      CASE ("LOCAL RADIUS OF CURVATURE") ! Earth's radius of curvature at occ. point (m)
        READ ( arg, FMT=* ) ROdata%GeoRef%RoC
        ROdata%GeoRef%RoC = ROdata%GeoRef%RoC * KMtoM

      CASE ("GEOID UNDULATION EGM96")    ! Geoid undulation (EGM96-WGS-84) at occ. point (m)
        READ ( arg, FMT=* ) ROdata%GeoRef%Undulation

      CASE ("MISSION")  ! LEO Satellite IDx

! Older GFZ DSC files did not contain the PRODUCT_ID keyword, so we
! attempt to extact it here if PRODUCT_ID is not present (if it is,
! it will be at the top of the file and processed before this line).
! Unfortunately, older DSC files always had "mission=CHAMP;" even for
! GRACE-A & TerraSAR-X, so MISSION wasn't very useful either. [Even today,
! mission=GRACE doesn't distinguish the (potential) -A from -B, but we
! can assume GRACE-A.] Instead we detect the satellite ID from the DAT
! file basename. Normally this is the same as the PRODUCT_ID. NB: This
! only works if the file has not been renamed!

        IF ( no_productid ) THEN       !  if no PRODUCT_ID key present (thus far)
          SELECT CASE (arg)
            CASE ( "CHAMP" )           ! don't trust this! Use the file name
              i = LEN_TRIM(datfile)
              DO WHILE ( i > 0 .AND. &
                         datfile(i:i) /= "/" )
                i = i - 1
              END DO
              arg = datfile(i+1:)
              CALL To_Upper ( arg )
              SELECT CASE (arg(1:2))
                CASE ("CH")
                  ROdata%LEO_ID = "CHMP"
                CASE ("GA")
                  ROdata%LEO_ID = "GRAA"
                CASE ("GB")
                  ROdata%LEO_ID = "GRAB"
                CASE ("TS")
                  ROdata%LEO_ID = "TSRX"
                CASE ("TD")
                  ROdata%LEO_ID = "TDMX"
                CASE DEFAULT
                  ROdata%LEO_ID = "UNKN"
              END SELECT
            CASE ("GRACE")              ! potentially ambiguous but assume -A
               ROdata%LEO_ID = "GRAA"
            CASE ("TERRASAR-X")
              ROdata%LEO_ID = "TSRX"
            CASE ("TANDEM-X")
              ROdata%LEO_ID = "TDMX"
            CASE DEFAULT
              ROdata%LEO_ID = "UNKN"
          END SELECT
        END IF

      CASE ("OCCULTATION DIRECTION") ! Azimuth angle GNSS-->LEO (deg) (if not in DAT)
        READ ( arg, FMT=* ) ROdata%GeoRef%Azimuth

      CASE ("START LATITUDE") ! Start latitude
        READ ( arg, FMT=* ) Start_Lat

      CASE ("START LONGITUDE") ! Start longitude
        READ ( arg, FMT=* ) Start_Lon

      CASE ("PRODUCT_ID")  ! LEO satellite ID
        SELECT CASE (arg(1:2))
          CASE ("CH")
            ROdata%LEO_ID = "CHMP"
          CASE ("GA")
            ROdata%LEO_ID = "GRAA"
          CASE ("GB")
            ROdata%LEO_ID = "GRAB"
          CASE ("TS")
            ROdata%LEO_ID = "TSRX"
          CASE ("TD")
            ROdata%LEO_ID = "TDMX"
          CASE DEFAULT
            ROdata%LEO_ID = "UNKN"
        END SELECT
        no_productid = .FALSE.

      CASE ("PROCESSING FACILITY")
        ROdata%Processing_Centre = arg

      CASE ("SETTING(1)/RISING OCCULTATION") ! rising or setting occultation
        IF ( arg /= "1" ) setting_occ = .FALSE.

      CASE ("SOFTWARE PACKAGE") ! Software ID (major) "POCS-n.n"
        READ ( arg(6:), FMT=*, IOSTAT=iostatus ) SWver
        IF ( iostatus /= 0 ) SWver = 0.0

      CASE ("REVISION")         ! Software ID (minor)
        READ ( arg, FMT=* ) SWrev

      CASE DEFAULT

    END SELECT

  END DO

  CLOSE ( UNIT=funit )

  IF ( iostatus > 0 ) THEN
    CALL message ( msg_fatal, "I/O error while reading DSC file" )
  END IF

!-------------------------------------------------------------
! 5.2 Convert GFZ parameters to ROPP standard
!-------------------------------------------------------------

  WRITE ( ROdata%Software_Version, FMT="(F6.3)" ) SWver + SWrev * 0.001_dp
  IF ( SWver < 10.0 ) THEN
    ROdata%Software_Version = "V0"//ADJUSTL(ROdata%Software_Version)
  ELSE
    ROdata%Software_Version = "V" //ADJUSTL(ROdata%Software_Version)
  END IF

! If Lev1a data, take nominal lat/lon from occulation start lat/lon.
! Otherwise, take nominal lat/lon items from lowest point in (Lev1b) profile
! with time offset of zero for rising occultations or end-start time for
! setting. Set PCD rising bit accordingly.

  IF ( lev1a_exists ) THEN
      ROdata%GeoRef%Lat = Start_Lat
      ROdata%GeoRef%Lon = Start_Lon
  ELSE
    IF ( ROdata%Lev2a%Alt_Refrac(1) < &
         ROdata%Lev2a%Alt_Refrac(NLevs) ) THEN
      ROdata%GeoRef%Lat = ROdata%Lev1b%Lat_tp(1)
      ROdata%GeoRef%Lon = ROdata%Lev1b%Lon_tp(1)
    ELSE
      ROdata%GeoRef%Lat = ROdata%Lev1b%Lat_tp(Nlevs)
      ROdata%GeoRef%Lon = ROdata%Lev1b%Lon_tp(Nlevs)
    END IF
  END IF

  ROdata%PCD = 0
  IF ( setting_occ ) THEN
    IF ( eTime < sTime ) eTime = eTime + SecsPerDay  ! in case event spans midnight
    ROdata%GeoRef%Time_Offset  = eTime - sTime
  ELSE
    ROdata%GeoRef%Time_Offset  = 0.0
    ROdata%PCD                 = IBSET(ROdata%PCD,PCD_rising)
  END IF

  CALL ropp_io_ascend ( ROdata )

!-------------------------------------------------------------
! 6. Generate ROPP Occultation ID; optionally dump some key data
!-------------------------------------------------------------

  CALL ropp_io_occid ( ROdata )
  CALL message ( msg_info, "Profile    1: "//ROdata%occ_id )
  WRITE ( outstr, FMT="(F6.2,',',F7.2)" ) ROdata%GeoRef%Lat, ROdata%GeoRef%Lon
  CALL message ( msg_diag, "   Latitude/Longitude           : "//TRIM(outstr) )
  WRITE ( number, FMT="(I6)") ROdata%Lev1a%Npoints
  CALL message ( msg_diag, "   No. of phase/SNR samples     : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) ROdata%Lev1b%Npoints
  CALL message ( msg_diag, "   No. of bending angle samples : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) ROdata%Lev2a%Npoints
  CALL message ( msg_diag, "   No. of refractivity samples  : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) ROdata%Lev2b%Npoints
  CALL message ( msg_diag, "   No. of geophysical samples   : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) ROdata%Lev2c%Npoints
  CALL message ( msg_diag, "   No. of surface geo. samples  : "//TRIM(number) )
  WRITE ( number, FMT="(I6)" ) ROdata%Lev2d%Npoints
  CALL message ( msg_diag, "   No. of model coeff. levels   : "//TRIM(number) )

!-------------------------------------------------------------
! 7. Write ROPP netCDF file
!-------------------------------------------------------------

  CALL message ( msg_info, "Writing " // TRIM(opfile) )
  CALL ropp_io_write ( ROdata, file=opfile, ierr=iostatus )
  IF ( iostatus > 0 ) THEN
    CALL message ( msg_warn, "I/O error while writing output file" )
  END IF

!-------------------------------------------------------------
! 8. Tidy up - deallocate structures & free memory
!-------------------------------------------------------------

  CALL ropp_io_free ( ROdata )
  CALL message ( msg_noin, " " )

CONTAINS

!-------------------------------------------------------------------------------
! 9. Usage (help) information
!-------------------------------------------------------------------------------

  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Convert a GFZ PD or NRT DAT/DSC pair of RO files to a'
    PRINT *, '  ROPP netCDF file'
    PRINT *, 'Usage:'
    PRINT *, '  > gfz2ropp ip_file [-o op_file] [-d] [-h] [-v]'
    PRINT *, '  where ip_file is a GFZ formated PD or NRT .dat file name with an'
    PRINT *, '  assumed companion .dsc file with the same name and in the same'
    PRINT *, '  directory.'
    PRINT *, 'Options:'
    PRINT *, '  -o specifies the output (netCDF) file name'
    PRINT *, '  -d prints out some additional diagnostics to stdout'
    PRINT *, '  -h this help'
    PRINT *, '  -v version information'
    PRINT *, 'Defaults:'
    PRINT *, '  Input  file name : required'
    PRINT *, '  Output file name : from ip_file but .nc and in PWD'
    PRINT *, 'See gfz2ropp(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage

!-------------------------------------------------------------------------------
! 10. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'gfz2ropp - convert GFZ RO data files to ROPP netCDF'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END Program gfz2ropp
