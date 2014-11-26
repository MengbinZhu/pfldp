! $Id: test2ropp.f90 3831 2013-08-27 15:22:48Z idculv $

PROGRAM test2ropp

!****x* Programs/test2ropp *
!
! NAME
!   test2ropp
!
! SYNOPSIS
!   Tool to generate (non-scientific) test data in ROPP netCDF format
!
!   > test2ropp test [-o opfile] [-n nsamp] [-d] [-h] [-v]
!
! ARGUMENTS
!   test - one of the supported test data types MISSING, VALID, INVALID,
!          BADPROF or MISPROF
!
! OPTIONS
!   -n sets the number of samples in the profile (default: 247)
!   -o specifies an output file name - mandatory argument if used
!     (default: name generated from the occultation ID. NB this
!      will always be a valid string for a file name even if the
!      individual header elements are invalid)
!   -d outputs a sampled dump of the file header to stdout
!      plus other diagnostics
!   -h help
!   -v version information
!
! DESCRIPTION
!   test2ropp is a tool to generate (non-scientific) test data in ROPP format.
!   The supported test data types are:
!    MISSING : generate a set of test data in which all parameters are
!              set to missing data values.
!    VALID   : generate a set of test data in which all parameters are
!              set to random, but valid (within range), data values.
!    INVALID : generate a set of test data in which all parameters are
!              set to random, out-of-range, data values
!    BADPROF : generate a set of test data in which all time and vertical
!              coordinates are increasing within valid ranges, but all
!              observed profile data is randomly invalid and all sigmas &
!              quality values are set missing. Header data is randomly valid.
!    MISPROF : As BADPROF except a range check is performed to output observed
!              profiles as missing.
!    The test type is case-insensitive.
!
! CALLS
!   test2ropp_random
!   ropp_io_init
!   ropp_io_write
!   ropp_io_occid
!   ropp_io_free
!   ropp_io_version
!   MonthOfYear
!   messages
!
! DESCRIPTION
!   test2ropp is a tool to generate (non-scientific) test data
!   in ROPP format.
!
! REFERENCES
!   1. ROPP User Guide - Part I
!      SAF/ROM/METO/UG/ROPP/002
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

! Modules

  USE messages
  USE ropp_utils,    ONLY: To_Upper,          &
                           ropp_MDTV
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_write,     &
                           ropp_io_occid,     &
                           ropp_io_free,      &
                           ropp_io_init,      &
                           ropp_io_version
  USE DateTimeProgs, ONLY: MonthOfYear
  USE DateTimeTypes

  IMPLICIT NONE

  CHARACTER (LEN=*), PARAMETER :: chfmt = "(1X,2A)"
  CHARACTER (LEN=*), PARAMETER :: gefmt1 = "(A,F10.3,2F8.1)"
  CHARACTER (LEN=*), PARAMETER :: gefmt2 = "(A,F10.1,5F8.1)"
  CHARACTER (LEN=*), PARAMETER :: bgfmt  = &
                      "(1X,2A,2X,I2.2,':',I2.2,1X,I2.2,'-',A,'-',I4,F9.1)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                      "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt2 = &
                      "(A,2(I2.2,':'),I2.2,'.',I3.3,1X,I2.2,'-',A3,'-',I4.4)"

  TYPE(ROprof) :: ROdata

  CHARACTER (LEN=20)    :: test
  CHARACTER (LEN=256)   :: opfile, arg
  CHARACTER (LEN=64)    :: pcd
  CHARACTER (LEN=80)    :: outstr            ! Output text string
  CHARACTER (LEN=10)    :: MonthName         ! Month name

  INTEGER :: i, ierr, narg, iarg
  INTEGER :: nsamp
  REAL    :: lat, lon, toff, roc, coc(3), azim, undu
  LOGICAL :: dump   = .FALSE.
  LOGICAL :: ranchk = .FALSE.

! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

!--------------------------------------------------------------
!  1. Initalise
!--------------------------------------------------------------

  test   = "VALID"    ! default test type
  opfile = " "        ! default name will be generated from Occ ID
  nsamp  = 247        ! default no. of vertical samples in profile

  CALL message_set_routine ( "test2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                     ROPP Test Data Generator'                        )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')


!-------------------------------------------------------------
! 2. Parse command line options
!-------------------------------------------------------------

  narg = IARGC()
  iarg = 1
  DO WHILE ( iarg <= narg )
    CALL GETARG ( iarg, arg )
    SELECT CASE (arg)
      CASE ("-d","-D")
        msg_MODE = VerboseMode
        dump    = .true.

      CASE ("-h","-H","--help","?")
        CALL Usage
        CALL EXIT(0)

      CASE ("-n","-N")
        CALL GETARG ( iarg+1, arg )
        READ ( arg, FMT=*, iostat=ierr ) i
        IF ( ierr == 0 ) nsamp = i
        nsamp = MAX(nsamp,0)
        iarg  = iarg + 1

      CASE ("-o","-O")
        CALL GETARG ( iarg+1, arg )
        opfile = arg
        iarg   = iarg + 1

      CASE ("-v", "-V", "--version")
        CALL version_info()
        CALL EXIT(0)

      CASE DEFAULT
         IF ( arg(1:1) /= "-" ) THEN
           test = arg(1:20)
           CALL To_Upper ( test )
         END IF
    END SELECT
    iarg = iarg + 1
  END DO

!--------------------------------------------------------------------
! 3. Generate a basic RO structure will all elements set 'missing'
!--------------------------------------------------------------------

  CALL ropp_io_init ( ROdata, &
                      nsamp, nsamp, &
                      nsamp, nsamp, nsamp, nsamp )

!--------------------------------------------------------------------
! 4. Modify elements according to test
!--------------------------------------------------------------------

  SELECT CASE (test)
    CASE ( "MISSING" )
    ROdata%BG%Source = "UNKNOWN"  ! ropp_io_init created (almost) all MISSING

    CASE ( "VALID" )
      CALL test2ropp_valid   ( ROdata )

    CASE ( "INVALID" )
      CALL test2ropp_invalid ( ROdata )

    CASE ( "BADPROF" )
      CALL test2ropp_badprof ( ROdata )

    CASE ( "MISPROF" )
      CALL test2ropp_badprof  ( ROdata )
      ranchk = .TRUE.

    CASE DEFAULT
      CALL message ( msg_fatal, "Test "//TRIM(test)//" not supported" )
  END SELECT

!--------------------------------------------------------------------
! 5. Check/create output file name. If the file exists, it will
!    be overwritten
!--------------------------------------------------------------------

  CALL ropp_io_occid ( ROdata )
  IF ( opfile == " " ) THEN
    opfile = TRIM(ROdata%occ_id) // '.nc'
    CALL To_Lower(opfile)
  END IF

!--------------------------------------------------------------------
! 5. Optional dump/list of some header parameters
!--------------------------------------------------------------------

! change some potential missing data values for consistent printing widths

  lat  = ROdata%georef%lat
  IF ( lat < ropp_MDTV ) lat = -99.9
  lon  = ROdata%georef%lon
  IF ( lon < ropp_MDTV ) lon = -999.9
  toff = ROdata%georef%time_offset
  IF ( toff < ropp_MDTV ) toff = -9999.999
  roc = ROdata%georef%roc
  IF ( roc < ropp_MDTV )  roc = -999999.9
  coc(:) = ROdata%georef%r_coc(:)
  WHERE ( coc < ropp_MDTV ) coc = -9999.9
  azim = ROdata%georef%azimuth
  IF ( azim < ropp_MDTV ) azim = -9999.9
  undu = ROdata%georef%undulation
  IF ( undu < ropp_MDTV ) undu = -9999.9

  CALL message ( msg_info, "Profile    1 : "//TRIM(ROdata%occ_id)// &
                           "  ("//TRIM(test)//")" )

  CALL message ( msg_diag, "Test profile "//TRIM(test) )
  CALL message ( msg_diag, "OccID :  "//ROdata%occ_id )
  CALL message ( msg_diag, "LEOID :  "//ROdata%leo_id )
  CALL message ( msg_diag, "GNSID :  "//ROdata%gns_id )
  CALL message ( msg_diag, "STNID :  "//ROdata%stn_id )
  CALL message ( msg_diag, "ProCen:  "//TRIM(ROdata%processing_centre) )
  IF ( ROdata%dtocc%month >=  1 .AND. &
       ROdata%dtocc%month <= 12 ) THEN
    CALL MonthOfYear ( ROdata%dtocc%month, MonthName, 1 )
  ELSE
    MonthName = "XXX"
  END IF
  WRITE ( outstr, FMT=DTfmt2 ) "DTocc :  ",            &
                               ROdata%dtocc%hour,      &
                               ROdata%dtocc%minute,    &
                               ROdata%dtocc%second,    &
                           MIN(ROdata%dtocc%msec,999), &
                               ROdata%dtocc%day,       &
                               MonthName(1:3),         &
                               ROdata%dtocc%year
  CALL message ( msg_diag, TRIM(outstr) )
  IF ( ROdata%dtpro%month >=  1 .AND. &
       ROdata%dtpro%month <= 12 ) THEN
    CALL MonthOfYear ( ROdata%dtpro%month, MonthName, 1 )
  ELSE
    MonthName = "XXX"
  END IF
  WRITE ( outstr, FMT=DTfmt2 ) "DTpro :  ",            &
                               ROdata%dtpro%hour,      &
                               ROdata%dtpro%minute,    &
                               ROdata%dtpro%second,    &
                           MIN(ROdata%dtpro%msec,999), &
                               ROdata%dtpro%day,       &
                               MonthName(1:3),         &
                               ROdata%dtpro%year
  CALL message ( msg_diag, TRIM(outstr) )
  WRITE ( outstr, FMT=gefmt1 ) "GEOref: ", toff, lat, lon
  CALL message ( msg_diag, TRIM(outstr) )
  WRITE ( outstr, FMT=gefmt2 ) "        ", roc, coc, azim, undu
  CALL message ( msg_diag, TRIM(outstr) )
  WRITE ( pcd, FMT="(B64.64)" ) ROdata%pcd
  CALL message ( msg_diag, "PCD   :  "//pcd(49:64)) ! mask off all but 1st 16 bits
  IF (  ROdata%overall_qual >= 0.0 ) THEN
    WRITE ( outstr, FMT="(F7.1)" ) ROdata%overall_qual
  ELSE
    outstr = "MISSING"
  END IF
  CALL message ( msg_diag, "Qual  :  "//TRIM(outstr) )
  CALL message ( msg_diag, "PODmth:  "//TRIM(ROdata%pod_method) )
  CALL message ( msg_diag, "LVLmth:  "//TRIM(ROdata%bangle_method)// &
                           " "//TRIM(ROdata%refrac_method)//         &
                           " "//TRIM(ROdata%meteo_method) )
  CALL message ( msg_diag, "THNmth:  "//TRIM(ROdata%thin_method) )
  CALL message ( msg_diag, "SW-ID :  "//TRIM(ROdata%software_version) )

  IF ( ROdata%bg%year < 1999 .OR. &
       ROdata%bg%year > 2099 ) THEN
    WRITE ( outstr, FMT=bgfmt ) "B/G   :  ", TRIM(ROdata%bg%source)
  ELSE
    CALL MonthOfYear ( ROdata%dtocc%month, MonthName, 1 )
    WRITE ( outstr, FMT=bgfmt ) "B/G   :  ",        &
                                TRIM(ROdata%bg%source), &
                                ROdata%bg%hour,         &
                                ROdata%bg%minute,       &
                                ROdata%bg%day,          &
                                MonthName(1:3),         &
                                ROdata%bg%year
  END IF
  CALL message ( msg_diag, TRIM(outstr) )

  IF ( ROdata%Lev2d%npoints > 0 ) THEN
    CALL message ( msg_diag, "ModLev:  "//TRIM(ROdata%Lev2d%level_type) )
  ELSE
     CALL message ( msg_diag, "ModTyp:  NONE" )
  END IF

  WRITE ( outstr, FMT="(A,6I5)" ) "Npts  :  ",    &
                                  ROdata%lev1a%npoints, &
                                  ROdata%lev1b%npoints, &
                                  ROdata%lev2a%npoints, &
                                  ROdata%lev2b%npoints, &
                                  ROdata%lev2c%npoints, &
                                  ROdata%lev2d%npoints
  CALL message ( msg_diag, TRIM(outstr) )

!--------------------------------------------------------------------
! 6. Write output file with no range checking
!--------------------------------------------------------------------

  CALL message ( msg_info, "Writing "//TRIM(opfile) )
  CALL ropp_io_write ( ROdata, file=opfile, ranchk=ranchk )

!--------------------------------------------------------------------
! 7. Tidy up
!--------------------------------------------------------------------

  CALL ropp_io_free ( ROdata )

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

SUBROUTINE test2ropp_valid ( ROdata ) ! (inout)

!****x* test2ropp/test2ropp_valid *
!
! NAME
!   test2ropp_valid - generate random, valid, test data
!
! SYNOPSIS
!   USE ropp_io_types
!   TYPE(ROprof) :: ROdata
!   CALL test2ropp_valid(ROdata)
!
! INPUT
!   ROdata  dtyp  ROPP structure
!
! OUTPUT
!   ROdata  dtyp  ROPP structure
!
! DESCRIPTION
!   Fills a ROPP structure with random values with a linear PDF.
!   All parameters will be within their valid ranges; string
!   parameters will have a randomly chosen sensible option taken
!   from a list.
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

! Modules

  USE typesizes,     dp => EightByteReal
  USE DateTimeProgs, ONLY: DateTimeOffset
  USE ropp_io_types

  IMPLICIT NONE

! Fixed parameters

  INTEGER, PARAMETER :: mn = 1     ! Index to minimum range value
  INTEGER, PARAMETER :: mx = 2     ! Index to maximum range value
  INTEGER, PARAMETER :: mr = 10    ! No. of random number sequences

  INTEGER, PARAMETER :: ngid = 3
  INTEGER, PARAMETER :: nlid = 14
  INTEGER, PARAMETER :: nsid = 26
  INTEGER, PARAMETER :: npid = 5
  INTEGER, PARAMETER :: nbid = 4
  INTEGER, PARAMETER :: npod = 6
  INTEGER, PARAMETER :: nphs = 6
  INTEGER, PARAMETER :: nban = 6
  INTEGER, PARAMETER :: nref = 6
  INTEGER, PARAMETER :: nmet = 2
  INTEGER, PARAMETER :: nthn = 8

  CHARACTER (LEN=*), PARAMETER :: validgid(ngid) = (/ "G", "R", "E" /)
  CHARACTER (LEN=*), PARAMETER :: validlid(nlid) = (/ "META", "METB", "METC", &
                                                      "OERS", "CHMP",         &
                                                      "GRAA", "GRAB", "TSRX", &
                                                      "CO01", "CO02", "CO03", &
                                                      "CO04", "CO05", "CO06" /)
  CHARACTER (LEN=*), PARAMETER :: validsid(nsid) = (/ "ACOR", "BOGO", "CANT", &
                                                      "DENT", "EIJS", "FRAG", &
                                                      "GOPE", "HERS", "INVE", &
                                                      "JOEN", "KIRU", "LAMP", &
                                                      "MALL", "NEWL", "ONSA", &
                                                      "PADO", "QUAL", "ROVE", &
                                                      "SFER", "TERS", "UNPG", &
                                                      "VILL", "WTZR", "XFER", &
                                                      "YEBE", "ZIMM" /)
  CHARACTER (LEN=*), PARAMETER :: validpid(npid) = (/ "DMI     ", "GFZ     ", "UCAR    ", &
                                                      "NESDIS  ", "EUMETSAT" /)
  CHARACTER (LEN=*), PARAMETER :: validbid(nbid) = (/ "ECMWF ","METO  ", "HIRLAM", "NCEP  " /)

  CHARACTER (LEN=*), PARAMETER :: validpod(npod) = (/ "POD_1",    "POD_2",    "POD_3",    &
                                                      "POD_4",    "POD_5",    "POD_6"    /)
  CHARACTER (LEN=*), PARAMETER :: validphs(nphs) = (/ "PHASE_1",  "PHASE_2",  "PHASE_3",  &
                                                      "PHASE_4",  "PHASE_5",  "PHASE_6"  /)
  CHARACTER (LEN=*), PARAMETER :: validban(nban) = (/ "BANGLE_1", "BANGLE_2", "BANGLE_3", &
                                                      "BANGLE_4", "BANGLE_5", "BANGLE_6" /)
  CHARACTER (LEN=*), PARAMETER :: validref(nref) = (/ "REFRAC_1", "REFRAC_2", "REFRAC_3", &
                                                      "REFRAC_4", "REFRAC_5", "REFRAC_6" /)
  CHARACTER (LEN=*), PARAMETER :: validmet(nmet) = (/ "T-DRY ",   "1D-VAR" /)
  CHARACTER (LEN=*), PARAMETER :: validthn(nthn) = (/ "NONE  ",   "SAMPLE", &
                                                      "LIN   ",   "LOG   ", &
                                                      "SGLIN ",   "SGLOG ", &
                                                      "ASGLIN",   "ASGLOG" /)

! Argument list parameters

  TYPE (ROprof), INTENT(INOUT) :: ROdata

! Local variables

  CHARACTER (LEN=10)    :: number        ! number as string
  INTEGER               :: i, np         ! loop/index numbers
  INTEGER               :: SeedArray(1000), MaxElem, Value
  REAL(dp)              :: v             ! some d.p. value

  INTEGER               :: nr            ! random number sequence length
  INTEGER, DIMENSION(8) :: DT1, DT2      ! Date/time arrays
  INTEGER               :: OffDay        ! random offset days in the past
  REAL(dp), ALLOCATABLE :: randf(:,:)    ! random number (0-1)
  REAL(dp), ALLOCATABLE :: lats(:), lons(:), rads(:)  ! For generating sensible satellite radii and speeds.
  REAL(dp)              :: pi

!----------------------------------------------------------------
! 0. Initialise
!----------------------------------------------------------------

!   Calling RANDOM_SEED with no arguments is supposed to
!   set the internal random number seed to a value based on
!   the system clock date/time, thus giving a new pseudo-random
!   number sequence. The HP version fails to do this - YMMV.
!   This code explicitly modifies the current seed
!   values using the system clock.

  CALL RANDOM_SEED  ( SIZE=MaxElem )

  CALL RANDOM_SEED  ( GET=SeedArray(1:MaxElem) )

  CALL SYSTEM_CLOCK ( COUNT=Value )

  DO i = 1, MaxElem
    SeedArray(i) = SeedArray(i) + MOD ( i+Value, 17 )
  END DO
  CALL RANDOM_SEED  ( PUT=SeedArray(1:MaxElem) )

  nr = MAX ( ROdata%Lev1a%Npoints, &
             ROdata%Lev1b%Npoints, &
             ROdata%Lev2a%Npoints, &
             ROdata%Lev2b%Npoints, &
             ROdata%Lev2d%Npoints, &
             247 )
  ALLOCATE ( randf(nr,mr) )

  pi = 4.0_dp*ATAN(1.0_dp)

!----------------------------------------------------------------
! 1. Header parameters
!----------------------------------------------------------------

! 1.1 Character-valued parameters - choose from pre-set lists

  CALL RANDOM_NUMBER ( randf )

  i = 1 + NINT(randf(1,1)*(ngid-1))
  WRITE ( ROdata%GNS_ID, FMT="(A,I3.3)" ) validgid(i), &
                                          1+NINT(randf(1,1)*23.0) ! 1-24
  i = 1 + NINT(randf(2,1)*(nlid-1))
  ROdata%LEO_ID = validlid(i)

  i = 1 + NINT(randf(3,1)*(nsid-1))
  ROdata%STN_ID = validsid(i)

  i = 1 + NINT(randf(4,1)*(npid-1))
  ROdata%Processing_centre = validpid(i)

  i = 1 + NINT(randf(5,1)*(npod-1))
  ROdata%POD_Method = validpod(i)

  i = 1 + NINT(randf(6,1)*(nphs-1))
  ROdata%Phase_Method = validphs(i)

  i = 1 + NINT(randf(7,1)*(nban-1))
  ROdata%Bangle_Method = validban(i)

  i = 1 + NINT(randf(8,1)*(nref-1))
  ROdata%Refrac_Method = validref(i)

  i = 1 + NINT(randf(9,1)*(nmet-1))
  ROdata%Meteo_Method = validmet(i)

  i = 1 + NINT(randf(10,1)*(nthn-1))
  ROdata%Thin_Method = validthn(i)

  v = randf(12,1) * 99.999
  WRITE ( number, FMT="(F6.3)" ) v
  IF ( v < 10.0_dp ) THEN
    ROdata%Software_Version = "V0"//ADJUSTL(number)
  ELSE
    ROdata%Software_Version = "V" //ADJUSTL(number)
  END IF

! 1.2 Occultation date/time - some time in the past 30 days.

  DT1 = (/ROdata%DTpro%Year,ROdata%DTpro%Month,ROdata%DTpro%Day, &
          0,0,0,0,0/)
  OffDay = NINT ( randf(13,1) * 30.0 )
  DT2 = (/0,0,OffDay,0,0,0,0,0/)
  CALL DateTimeOffset ( DT1, "-", DT2 )
  ROdata%DTocc%Year  = DT1(1)
  ROdata%DTocc%Month = DT1(2)
  ROdata%DTocc%Day   = DT1(3)

  ROdata%DTocc%Hour   = ROdata%DTocc%Range%Hour(mn)         &
                      + NINT((ROdata%DTocc%Range%Hour(mx)   &
                      - ROdata%DTocc%Range%Hour(mn))        &
                      * randf(14,1))

  ROdata%DTocc%Minute = ROdata%DTocc%Range%Minute(mn)       &
                      + NINT((ROdata%DTocc%Range%Minute(mx) &
                      - ROdata%DTocc%Range%Minute(mn)) &
                      * randf(15,1))

  ROdata%DTocc%Second = ROdata%DTocc%Range%Second(mn)        &
                      + NINT((ROdata%DTocc%Range%Second(mx)  &
                      - ROdata%DTocc%Range%Second(mn))       &
                      * randf(16,1))

  ROdata%DTocc%Msec   = ROdata%DTocc%Range%Msec(mn)          &
                      + NINT((ROdata%DTocc%Range%Msec(mx)    &
                      - ROdata%DTocc%Range%Msec(mn))         &
                      * randf(17,1))

! 1.3 Processing date/time left as-is

! 1.4 Quality parameters

  ROdata%PCD =  ROdata%Range%PCD(mn)       &
             + NINT((ROdata%Range%PCD(mx)  &
                   - ROdata%Range%PCD(mn)) &
             * randf(18,1))

  IF ( BTEST ( ROdata%PCD, PCD_phase   ) .OR. &
       BTEST ( ROdata%PCD, PCD_bangle  ) .OR. &
       BTEST ( ROdata%PCD, PCD_refrac  ) .OR. &
       BTEST ( ROdata%PCD, PCD_met     ) .OR. &
       BTEST ( ROdata%PCD, PCD_bg      ) .OR. &
       BTEST ( ROdata%PCD, PCD_missing ) )    &
               ROdata%PCD = IBSET ( ROdata%PCD, PCD_summary )

  ROdata%Overall_Qual =  ROdata%Range%Overall_Qual(mn) &
                      + (ROdata%Range%Overall_Qual(mx) - &
                         ROdata%Range%Overall_Qual(mn)) * randf(19,1)
  ROdata%Overall_Qual = MIN(MAX(ROdata%Overall_Qual,-99.9_dp),999.9_dp)

!----------------------------------------------------------------
! 2. Geo-referencing parameters
!----------------------------------------------------------------

  ROdata%GeoRef%Time_Offset =  ROdata%GeoRef%Range%Time_Offset(mn)  &
                            + (ROdata%GeoRef%Range%Time_Offset(mx)  &
                             - ROdata%GeoRef%Range%Time_Offset(mn)) &
                            * randf(20,1)

  ROdata%GeoRef%Lat         =  ROdata%GeoRef%Range%Lat(mn)  &
                            + (ROdata%GeoRef%Range%Lat(mx)  &
                             - ROdata%GeoRef%Range%Lat(mn)) &
                            * randf(21,1)

  ROdata%GeoRef%Lon         =  ROdata%GeoRef%Range%Lon(mn)  &
                            + (ROdata%GeoRef%Range%Lon(mx)  &
                             - ROdata%GeoRef%Range%Lon(mn)) &
                            * randf(22,1)

  ROdata%GeoRef%RoC         =  ROdata%GeoRef%Range%RoC(mn)  &
                            + (ROdata%GeoRef%Range%RoC(mx)  &
                             - ROdata%GeoRef%Range%RoC(mn)) &
                            * randf(23,1)

  ROdata%GeoRef%R_CoC(1:3)  =  ROdata%GeoRef%Range%R_CoC(mn)  &
                            + (ROdata%GeoRef%Range%R_CoC(mx)  &
                             - ROdata%GeoRef%Range%R_CoC(mn)) &
                            * randf(24:26,1)

  ROdata%GeoRef%Azimuth     =  ROdata%GeoRef%Range%Azimuth(mn)  &
                            + (ROdata%GeoRef%Range%Azimuth(mx)  &
                             - ROdata%GeoRef%Range%Azimuth(mn)) &
                            * randf(27,1)

  ROdata%GeoRef%Undulation  =  ROdata%GeoRef%Range%Undulation(mn)  &
                            + (ROdata%GeoRef%Range%Undulation(mx)  &
                             - ROdata%GeoRef%Range%Undulation(mn)) &
                            * randf(28,1)

!----------------------------------------------------------------
! 3. Meteo background parameters
!----------------------------------------------------------------

  i = 1 + NINT(randf(29,1)*(nbid-1))
  ROdata%BG%Source = validbid(i)

! Date same as that of occulation, random hour & f/c period

  ROdata%BG%Year      = ROdata%DTocc%Year
  ROdata%BG%Month     = ROdata%DTocc%Month
  ROdata%BG%Day       = ROdata%DTocc%Day

  ROdata%BG%Hour      =  ROdata%BG%Range%Hour(mn)  &
                      + NINT((ROdata%BG%Range%Hour(mx)  &
                       - ROdata%BG%Range%Hour(mn)) &
                      * randf(30,1))
  ROdata%BG%Minute    = 0

  ROdata%BG%FCperiod  =  FLOOR ( ROdata%BG%Range%FCperiod(mn) &
                      + (ROdata%BG%Range%FCperiod(mx)         &
                       - ROdata%BG%Range%FCperiod(mn))        &
                      * randf(31,1) )

!----------------------------------------------------------------
! 4. Level 1a profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev1a%Npoints

  ROdata%Lev1a%Dtime      =  ROdata%Lev1a%Range%Dtime(mn)  &
                          + (ROdata%Lev1a%Range%Dtime(mx)  &
                           - ROdata%Lev1a%Range%Dtime(mn)) &
                          * randf(1:np,1)

  ROdata%Lev1a%SNR_L1ca   =  ROdata%Lev1a%Range%SNR(mn)  &
                          + (ROdata%Lev1a%Range%SNR(mx)  &
                           - ROdata%Lev1a%Range%SNR(mn)) &
                          * randf(1:np,2)

  ROdata%Lev1a%SNR_L1p    =  ROdata%Lev1a%Range%SNR(mn)  &
                          + (ROdata%Lev1a%Range%SNR(mx)  &
                           - ROdata%Lev1a%Range%SNR(mn)) &
                          * randf(1:np,3)

  ROdata%Lev1a%SNR_L2p    =  ROdata%Lev1a%Range%SNR(mn)  &
                          + (ROdata%Lev1a%Range%SNR(mx)  &
                           - ROdata%Lev1a%Range%SNR(mn)) &
                          * randf(1:np,4)

  ROdata%Lev1a%Phase_L1   =  ROdata%Lev1a%Range%Phase(mn)  &
                          + (ROdata%Lev1a%Range%Phase(mx)  &
                           - ROdata%Lev1a%Range%Phase(mn)) &
                          * randf(1:np,5)

  ROdata%Lev1a%Phase_L2   =  ROdata%Lev1a%Range%Phase(mn)  &
                          + (ROdata%Lev1a%Range%Phase(mx)  &
                           - ROdata%Lev1a%Range%Phase(mn)) &
                          * randf(1:np,6)

! For r and v, generate random values within a spherical annulus 
! bounded by radii (min, max) as in ropp_io_rangecheck.
! For r, min = 6.2e6 and max = 0.9*Range%r_???(mx).
! For v, min = 0.0   and max = 0.9*Range%v_???(mx).

  ALLOCATE (lons(np), lats(np), rads(np))


  lons = (2.0_dp*randf(1:np, 7) - 1.0_dp)*pi
  lats = (2.0_dp*randf(1:np, 8) - 1.0_dp)*pi/2.0_dp
  rads = 6.2e6_dp + randf(1:np, 9)*(ROdata%Lev1a%Range%r_GNS(mx)-6.2e6_dp)

  ROdata%Lev1a%r_GNS(:,1) = rads*COS(lats)*COS(lons)
  ROdata%Lev1a%r_GNS(:,2) = rads*COS(lats)*SIN(lons)
  ROdata%Lev1a%r_GNS(:,3) = rads*SIN(lats)


  lons = (2.0_dp*randf(1:np,10) - 1.0_dp)*pi
  lats = (2.0_dp*randf(1:np, 1) - 1.0_dp)*pi/2.0_dp
  rads = randf(1:np, 2)*ROdata%Lev1a%Range%v_GNS(mx)

  ROdata%Lev1a%v_GNS(:,1) = rads*COS(lats)*COS(lons)
  ROdata%Lev1a%v_GNS(:,2) = rads*COS(lats)*SIN(lons)
  ROdata%Lev1a%v_GNS(:,3) = rads*SIN(lats)


  lons = (2.0_dp*randf(1:np, 3) - 1.0_dp)*pi
  lats = (2.0_dp*randf(1:np, 4) - 1.0_dp)*pi/2.0_dp
  rads = 6.2e6_dp + randf(1:np, 5)*(ROdata%Lev1a%Range%r_LEO(mx)-6.2e6_dp)

  ROdata%Lev1a%r_LEO(:,1) = rads*COS(lats)*COS(lons)
  ROdata%Lev1a%r_LEO(:,2) = rads*COS(lats)*SIN(lons)
  ROdata%Lev1a%r_LEO(:,3) = rads*SIN(lats)


  lons = (2.0_dp*randf(1:np, 6) - 1.0_dp)*pi
  lats = (2.0_dp*randf(1:np, 7) - 1.0_dp)*pi/2.0_dp
  rads = randf(1:np, 8)*ROdata%Lev1a%Range%v_LEO(mx)

  ROdata%Lev1a%v_LEO(:,1) = rads*COS(lats)*COS(lons)
  ROdata%Lev1a%v_LEO(:,2) = rads*COS(lats)*SIN(lons)
  ROdata%Lev1a%v_LEO(:,3) = rads*SIN(lats)

  DEALLOCATE (lons, lats, rads)

  ROdata%Lev1a%Phase_Qual =  ROdata%Lev1a%Range%Phase_Qual(mn)  &
                          + (ROdata%Lev1a%Range%Phase_Qual(mx)  &
                           - ROdata%Lev1a%Range%Phase_Qual(mn)) &
                          * randf(1:np,9)

!----------------------------------------------------------------
! 5. Level 1b profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev1b%Npoints

  ROdata%Lev1b%Lat_tp           =  ROdata%Lev1b%Range%Lat_tp(mn)  &
                                + (ROdata%Lev1b%Range%Lat_tp(mx)  &
                                 - ROdata%Lev1b%Range%Lat_tp(mn)) &
                                * randf(1:np,1)

  ROdata%Lev1b%Lon_tp           =  ROdata%Lev1b%Range%Lon_tp(mn)  &
                                + (ROdata%Lev1b%Range%Lon_tp(mx)  &
                                 - ROdata%Lev1b%Range%Lon_tp(mn)) &
                                * randf(1:np,2)

  ROdata%Lev1b%Azimuth_tp       =  ROdata%Lev1b%Range%Azimuth_tp(mn)  &
                                + (ROdata%Lev1b%Range%Azimuth_tp(mx)  &
                                 - ROdata%Lev1b%Range%Azimuth_tp(mn)) &
                                * randf(1:np,3)

  ROdata%Lev1b%Impact_L1        =  ROdata%Lev1b%Range%Impact(mn)  &
                                + (ROdata%Lev1b%Range%Impact(mx)  &
                                 - ROdata%Lev1b%Range%Impact(mn)) &
                                * randf(1:np,4)

  ROdata%Lev1b%Impact_L2        =  ROdata%Lev1b%Range%Impact(mn)  &
                                + (ROdata%Lev1b%Range%Impact(mx)  &
                                 - ROdata%Lev1b%Range%Impact(mn)) &
                                * randf(1:np,5)

  ROdata%Lev1b%Impact           =  ROdata%Lev1b%Range%Impact(mn)  &
                                + (ROdata%Lev1b%Range%Impact(mx)  &
                                 - ROdata%Lev1b%Range%Impact(mn)) &
                                * randf(1:np,6)

  ROdata%Lev1b%Impact_Opt       =  ROdata%Lev1b%Range%Impact(mn)  &
                                + (ROdata%Lev1b%Range%Impact(mx)  &
                                 - ROdata%Lev1b%Range%Impact(mn)) &
                                * randf(1:np,7)

  ROdata%Lev1b%Bangle_L1        =  ROdata%Lev1b%Range%Bangle(mn)  &
                                + (ROdata%Lev1b%Range%Bangle(mx)  &
                                 - ROdata%Lev1b%Range%Bangle(mn)) &
                                * randf(1:np,8)

  ROdata%Lev1b%Bangle_L2        =  ROdata%Lev1b%Range%Bangle(mn)  &
                                + (ROdata%Lev1b%Range%Bangle(mx)  &
                                 - ROdata%Lev1b%Range%Bangle(mn)) &
                                * randf(1:np,9)

  ROdata%Lev1b%Bangle           =  ROdata%Lev1b%Range%Bangle(mn)  &
                                + (ROdata%Lev1b%Range%Bangle(mx)  &
                                 - ROdata%Lev1b%Range%Bangle(mn)) &
                                * randf(1:np,10)

  ROdata%Lev1b%Bangle_Opt       =  ROdata%Lev1b%Range%Bangle(mn)  &
                                + (ROdata%Lev1b%Range%Bangle(mx)  &
                                 - ROdata%Lev1b%Range%Bangle(mn)) &
                                * randf(1:np,1)

  ROdata%Lev1b%Bangle_L1_Sigma  =  ROdata%Lev1b%Range%Bangle_Sigma(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Sigma(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Sigma(mn)) &
                                * randf(1:np,2) * 0.9485 ! range limit for BUFR

  ROdata%Lev1b%Bangle_L2_Sigma  =  ROdata%Lev1b%Range%Bangle_Sigma(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Sigma(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Sigma(mn)) &
                                * randf(1:np,3) * 0.9485 ! range limit for BUFR

  ROdata%Lev1b%Bangle_Sigma     =  ROdata%Lev1b%Range%Bangle_Sigma(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Sigma(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Sigma(mn)) &
                                * randf(1:np,4) * 0.9485 ! range limit for BUFR

  ROdata%Lev1b%Bangle_Opt_Sigma =  ROdata%Lev1b%Range%Bangle_Sigma(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Sigma(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Sigma(mn)) &
                                * randf(1:np,5)

  ROdata%Lev1b%Bangle_L1_Qual   =  ROdata%Lev1b%Range%Bangle_Qual(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Qual(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Qual(mn)) &
                                * randf(1:np,6)

  ROdata%Lev1b%Bangle_L2_Qual   =  ROdata%Lev1b%Range%Bangle_Qual (mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Qual (mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Qual (mn)) &
                                * randf(1:np,7)

  ROdata%Lev1b%Bangle_Qual      =  ROdata%Lev1b%Range%Bangle_Qual(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Qual(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Qual(mn)) &
                                * randf(1:np,8)

  ROdata%Lev1b%Bangle_Opt_Qual  =  ROdata%Lev1b%Range%Bangle_Qual(mn)  &
                                + (ROdata%Lev1b%Range%Bangle_Qual(mx)  &
                                 - ROdata%Lev1b%Range%Bangle_Qual(mn)) &
                                * randf(1:np,9)

!----------------------------------------------------------------
! 6. Level 2a profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev2a%Npoints

  ROdata%Lev2a%Alt_Refrac     = ROdata%Lev2a%Range%Alt_Refrac(mn)   &
                              + (ROdata%Lev2a%Range%Alt_Refrac(mx)  &
                               - ROdata%Lev2a%Range%Alt_Refrac(mn)) &
                              * randf(1:np,1)

  ROdata%Lev2a%Geop_Refrac    = ROdata%Lev2a%Range%Geop_Refrac(mn)   &
                              + (ROdata%Lev2a%Range%Geop_Refrac(mx)  &
                               - ROdata%Lev2a%Range%Geop_Refrac(mn)) &
                              * randf(1:np,2)

  ROdata%Lev2a%Refrac         = ROdata%Lev2a%Range%Refrac(mn)   &
                              + (ROdata%Lev2a%Range%Refrac(mx)  &
                               - ROdata%Lev2a%Range%Refrac(mn)) &
                              * randf(1:np,3)

  ROdata%Lev2a%Refrac_Sigma   = ROdata%Lev2a%Range%Refrac_Sigma(mn)   &
                              + (ROdata%Lev2a%Range%Refrac_Sigma(mx)  &
                               - ROdata%Lev2a%Range%Refrac_Sigma(mn)) &
                              * randf(1:np,4)

  ROdata%Lev2a%Refrac_Qual    = ROdata%Lev2a%Range%Refrac_Qual(mn)   &
                              + (ROdata%Lev2a%Range%Refrac_Qual(mx)  &
                               - ROdata%Lev2a%Range%Refrac_Qual(mn)) &
                              * randf(1:np,5)

  ROdata%Lev2a%Dry_Temp       = ROdata%Lev2a%Range%Dry_Temp(mn)   &
                              + (ROdata%Lev2a%Range%Dry_Temp(mx)  &
                               - ROdata%Lev2a%Range%Dry_Temp(mn)) &
                              * randf(1:np,6)

  ROdata%Lev2a%Dry_Temp_Sigma = ROdata%Lev2a%Range%Dry_Temp_Sigma(mn)   &
                              + (ROdata%Lev2a%Range%Dry_Temp_Sigma(mx)  &
                               - ROdata%Lev2a%Range%Dry_Temp_Sigma(mn)) &
                              * randf(1:np,7)

  ROdata%Lev2a%Dry_Temp_Qual  = ROdata%Lev2a%Range%Dry_Temp_Qual(mn)   &
                              + (ROdata%Lev2a%Range%Dry_Temp_Qual(mx)  &
                               - ROdata%Lev2a%Range%Dry_Temp_Qual(mn)) &
                              * randf(1:np,8)


!----------------------------------------------------------------
! 7. Level 2b profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev2b%Npoints

  ROdata%Lev2b%Geop        = ROdata%Lev2b%Range%Geop(mn)   &
                           + (ROdata%Lev2b%Range%Geop(mx)  &
                            - ROdata%Lev2b%Range%Geop(mn)) &
                           * randf(1:np,1)

  ROdata%Lev2b%Geop_Sigma  = ROdata%Lev2b%Range%Geop_Sigma(mn)   &
                           + (ROdata%Lev2b%Range%Geop_Sigma(mx)  &
                            - ROdata%Lev2b%Range%Geop_Sigma(mn)) &
                           * randf(1:np,2)

  ROdata%Lev2b%Press       = ROdata%Lev2b%Range%Press(mn)   &
                           + (ROdata%Lev2b%Range%Press(mx)  &
                            - ROdata%Lev2b%Range%Press(mn)) &
                           * randf(1:np,3)

  ROdata%Lev2b%Press_Sigma = ROdata%Lev2b%Range%Press_Sigma(mn)   &
                           + (ROdata%Lev2b%Range%Press_Sigma(mx)  &
                            - ROdata%Lev2b%Range%Press_Sigma(mn)) &
                           * randf(1:np,4)

  ROdata%Lev2b%Temp        = ROdata%Lev2b%Range%Temp(mn)   &
                           + (ROdata%Lev2b%Range%Temp(mx)  &
                            - ROdata%Lev2b%Range%Temp(mn)) &
                           * randf(1:np,5)

  ROdata%Lev2b%Temp_Sigma  = ROdata%Lev2b%Range%Temp_Sigma(mn)   &
                           + (ROdata%Lev2b%Range%Temp_Sigma(mx)  &
                            - ROdata%Lev2b%Range%Temp_Sigma(mn)) &
                           * randf(1:np,6)

  ROdata%Lev2b%SHum        = ROdata%Lev2b%Range%SHum(mn)   &
                           + (ROdata%Lev2b%Range%SHum(mx)  &
                            - ROdata%Lev2b%Range%Shum(mn)) &
                           * randf(1:np,7)

  ROdata%Lev2b%SHum_Sigma  = ROdata%Lev2b%Range%SHum_Sigma(mn)   &
                           + (ROdata%Lev2b%Range%SHum_Sigma(mx)  &
                            - ROdata%Lev2b%Range%SHum_Sigma(mn)) &
                           * randf(1:np,8)

  ROdata%Lev2b%Meteo_Qual  = ROdata%Lev2b%Range%Meteo_Qual(mn)   &
                           + (ROdata%Lev2b%Range%Meteo_Qual(mx)  &
                            - ROdata%Lev2b%Range%Meteo_Qual(mn)) &
                           * randf(1:np,9)

!----------------------------------------------------------------
! 8. Level 2c parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp

  ROdata%Lev2c%Geop_Sfc        = ROdata%Lev2c%Range%Geop_Sfc(mn)   &
                               + (ROdata%Lev2c%Range%Geop_Sfc(mx)  &
                                - ROdata%Lev2c%Range%Geop_Sfc(mn)) &
                               * randf(1,1)

  ROdata%Lev2c%Press_Sfc       = ROdata%Lev2c%Range%Press_Sfc(mn)   &
                               + (ROdata%Lev2c%Range%Press_Sfc(mx)  &
                                - ROdata%Lev2c%Range%Press_Sfc(mn)) &
                               * randf(2,1)

  ROdata%Lev2c%Press_Sfc_Sigma = ROdata%Lev2c%Range%Press_Sfc_Sigma(mn)   &
                               + (ROdata%Lev2c%Range%Press_Sfc_Sigma(mx)  &
                                - ROdata%Lev2c%Range%Press_Sfc_Sigma(mn)) &
                               * randf(3,1)

  ROdata%Lev2c%Press_Sfc_Qual  = ROdata%Lev2c%Range%Press_Sfc_Qual(mn)   &
                               + (ROdata%Lev2c%Range%Press_Sfc_Qual(mx)  &
                                - ROdata%Lev2c%Range%Press_Sfc_Qual(mn)) &
                               * randf(4,1)

  ROdata%Lev2c%TPH_Bangle      = ROdata%Lev2c%Range%TPH_Bangle(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Bangle(mx)  &
                                - ROdata%Lev2c%Range%TPH_Bangle(mn)) &
                               * randf(5,1)

  ROdata%Lev2c%TPA_Bangle      = ROdata%Lev2c%Range%TPA_Bangle(mn)   &
                               + (ROdata%Lev2c%Range%TPA_Bangle(mx)  &
                                - ROdata%Lev2c%Range%TPA_Bangle(mn)) &
                               * randf(6,1)

  ROdata%Lev2c%TPH_Bangle_Flag = ROdata%Lev2c%Range%TPH_Bangle_Flag(mn)       &
                               + NINT((ROdata%Lev2c%Range%TPH_Bangle_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Bangle_Flag(mn))     &
                               * randf(7,1))

  ROdata%Lev2c%TPH_Refrac      = ROdata%Lev2c%Range%TPH_Refrac(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Refrac(mx)  &
                                - ROdata%Lev2c%Range%TPH_Refrac(mn)) &
                               * randf(8,1)

  ROdata%Lev2c%TPN_Refrac      = ROdata%Lev2c%Range%TPN_Refrac(mn)   &
                               + (ROdata%Lev2c%Range%TPN_Refrac(mx)  &
                                - ROdata%Lev2c%Range%TPN_Refrac(mn)) &
                               * randf(9,1)

  ROdata%Lev2c%TPH_Refrac_Flag = ROdata%Lev2c%Range%TPH_Refrac_Flag(mn)       &
                               + NINT((ROdata%Lev2c%Range%TPH_Refrac_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Refrac_Flag(mn))     &
                               * randf(10,1))

  ROdata%Lev2c%TPH_Tdry_LRT    = ROdata%Lev2c%Range%TPH_Tdry_LRT(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Tdry_LRT(mx)  &
                                - ROdata%Lev2c%Range%TPH_Tdry_LRT(mn)) &
                               * randf(11,1)

  ROdata%Lev2c%TPT_Tdry_LRT    = ROdata%Lev2c%Range%TPT_Tdry_LRT(mn)   &
                               + (ROdata%Lev2c%Range%TPT_Tdry_LRT(mx)  &
                                - ROdata%Lev2c%Range%TPT_Tdry_LRT(mn)) &
                               * randf(12,1)

  ROdata%Lev2c%TPH_Tdry_LRT_Flag = ROdata%Lev2c%Range%TPH_Tdry_LRT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%TPH_Tdry_LRT_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Tdry_LRT_Flag(mn))     &
                               * randf(13,1))

  ROdata%Lev2c%TPH_Tdry_CPT    = ROdata%Lev2c%Range%TPH_Tdry_CPT(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Tdry_CPT(mx)  &
                                - ROdata%Lev2c%Range%TPH_Tdry_CPT(mn)) &
                               * randf(14,1)

  ROdata%Lev2c%TPT_Tdry_CPT    = ROdata%Lev2c%Range%TPT_Tdry_CPT(mn)   &
                               + (ROdata%Lev2c%Range%TPT_Tdry_CPT(mx)  &
                                - ROdata%Lev2c%Range%TPT_Tdry_CPT(mn)) &
                               * randf(15,1)

  ROdata%Lev2c%TPH_Tdry_CPT_Flag = ROdata%Lev2c%Range%TPH_Tdry_CPT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%TPH_Tdry_CPT_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Tdry_CPT_Flag(mn))     &
                               * randf(16,1))

  ROdata%Lev2c%PRH_Tdry_CPT    = ROdata%Lev2c%Range%PRH_Tdry_CPT(mn)   &
                               + (ROdata%Lev2c%Range%PRH_Tdry_CPT(mx)  &
                                - ROdata%Lev2c%Range%PRH_Tdry_CPT(mn)) &
                               * randf(17,1)

  ROdata%Lev2c%PRT_Tdry_CPT    = ROdata%Lev2c%Range%PRT_Tdry_CPT(mn)   &
                               + (ROdata%Lev2c%Range%PRT_Tdry_CPT(mx)  &
                                - ROdata%Lev2c%Range%PRT_Tdry_CPT(mn)) &
                               * randf(18,1)

  ROdata%Lev2c%PRH_Tdry_CPT_Flag = ROdata%Lev2c%Range%PRH_Tdry_CPT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%PRH_Tdry_CPT_Flag(mx) &
                                - ROdata%Lev2c%Range%PRH_Tdry_CPT_Flag(mn))     &
                               * randf(19,1))

  ROdata%Lev2c%TPH_Temp_LRT    = ROdata%Lev2c%Range%TPH_Temp_LRT(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Temp_LRT(mx)  &
                                - ROdata%Lev2c%Range%TPH_Temp_LRT(mn)) &
                               * randf(20,1)

  ROdata%Lev2c%TPT_Temp_LRT    = ROdata%Lev2c%Range%TPT_Temp_LRT(mn)   &
                               + (ROdata%Lev2c%Range%TPT_Temp_LRT(mx)  &
                                - ROdata%Lev2c%Range%TPT_Temp_LRT(mn)) &
                               * randf(21,1)

  ROdata%Lev2c%TPH_Temp_LRT_Flag = ROdata%Lev2c%Range%TPH_Temp_LRT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%TPH_Temp_LRT_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Temp_LRT_Flag(mn))     &
                               * randf(22,1))

  ROdata%Lev2c%TPH_Temp_CPT    = ROdata%Lev2c%Range%TPH_Temp_CPT(mn)   &
                               + (ROdata%Lev2c%Range%TPH_Temp_CPT(mx)  &
                                - ROdata%Lev2c%Range%TPH_Temp_CPT(mn)) &
                               * randf(23,1)

  ROdata%Lev2c%TPT_Temp_CPT    = ROdata%Lev2c%Range%TPT_Temp_CPT(mn)   &
                               + (ROdata%Lev2c%Range%TPT_Temp_CPT(mx)  &
                                - ROdata%Lev2c%Range%TPT_Temp_CPT(mn)) &
                               * randf(24,1)

  ROdata%Lev2c%TPH_Temp_CPT_Flag = ROdata%Lev2c%Range%TPH_Temp_CPT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%TPH_Temp_CPT_Flag(mx) &
                                - ROdata%Lev2c%Range%TPH_Temp_CPT_Flag(mn))     &
                               * randf(25,1))

  ROdata%Lev2c%PRH_Temp_CPT    = ROdata%Lev2c%Range%PRH_Temp_CPT(mn)   &
                               + (ROdata%Lev2c%Range%PRH_Temp_CPT(mx)  &
                                - ROdata%Lev2c%Range%PRH_Temp_CPT(mn)) &
                               * randf(26,1)

  ROdata%Lev2c%PRT_Temp_CPT    = ROdata%Lev2c%Range%PRT_Temp_CPT(mn)   &
                               + (ROdata%Lev2c%Range%PRT_Temp_CPT(mx)  &
                                - ROdata%Lev2c%Range%PRT_Temp_CPT(mn)) &
                               * randf(27,1)

  ROdata%Lev2c%PRH_Temp_CPT_Flag = ROdata%Lev2c%Range%PRH_Temp_CPT_Flag(mn)     &
                               + NINT((ROdata%Lev2c%Range%PRH_Temp_CPT_Flag(mx) &
                                - ROdata%Lev2c%Range%PRH_Temp_CPT_Flag(mn))     &
                               * randf(28,1))

!----------------------------------------------------------------
! 9. Level 2d parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev2d%Npoints

  ROdata%Lev2d%Level_Type    = ROdata%BG%Source

  ROdata%Lev2d%Level_Coeff_A = ROdata%Lev2d%Range%Level_Coeff_A(mn)   &
                             + (ROdata%Lev2d%Range%Level_Coeff_A(mx)  &
                              - ROdata%Lev2d%Range%Level_Coeff_A(mn)) &
                             * randf(1:np,1)

  ROdata%Lev2d%Level_Coeff_B = ROdata%Lev2d%Range%Level_Coeff_B(mn)   &
                             + (ROdata%Lev2d%Range%Level_Coeff_B(mx)  &
                              - ROdata%Lev2d%Range%Level_Coeff_B(mn)) &
                             * randf(1:np,2)

!----------------------------------------------------------------
! 10. Tidy up
!----------------------------------------------------------------

  DEALLOCATE ( randf )

END SUBROUTINE test2ropp_valid
!--------------------------------------------------------------------

SUBROUTINE test2ropp_invalid ( ROdata ) ! (inout)

!****x* test2ropp/test2ropp_invalid *
!
! NAME
!   test2ropp_invalid - generate random, invalid, test data
!
! SYNOPSIS
!   USE ropp_io_types
!   TYPE(ROprof) :: ROdata
!   CALL test2ropp_invalid(ROdata)
!
! INPUT
!   ROdata  dtyp  ROPP structure
!
! OUTPUT
!   ROdata  dtyp  ROPP structure
!
! DESCRIPTION
!   Fills a ROPP structure with random values with a linear PDF, but
!   all having out-of-range values (randomly positive or negative
!   where appropriate). Values will still be within sensible bounds
!   (ie not exceeding the missing data flag values or text file field
!   widths, or printable characters for string parameters).
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

! Modules

  USE typesizes,     dp => EightByteReal
  USE ropp_io_types

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (LEN=*), PARAMETER :: invalidchars = " !£$%^&*(){}[]+=:;@~#<>?" ! [A-Z,a-z,0-9,_,-] are valid
  INTEGER,           PARAMETER :: mn = 1     ! Index to minimum range value
  INTEGER,           PARAMETER :: mx = 2     ! Index to maximum range value
  INTEGER,           PARAMETER :: mr = 10    ! No. of random number sequences

! Argument list parameters

  TYPE (ROprof), INTENT(INOUT) :: ROdata

! Local variables

  INTEGER               :: i, j, np     ! loop/index numbers
  INTEGER               :: SeedArray(1000), MaxElem, Value
  REAL(dp)              :: v             ! some d.p. value

  INTEGER               :: nr            ! random number sequence length
  INTEGER,  ALLOCATABLE :: ic(:)         ! random character index
  REAL(dp), ALLOCATABLE :: randf(:,:)    ! random number array (0-1)
  REAL(dp), ALLOCATABLE :: val(:)        ! random scaled value array

!----------------------------------------------------------------
! 0. Initialise
!----------------------------------------------------------------

!   Calling RANDOM_SEED with no arguments is supposed to
!   set the internal random number seed to a value based on
!   the system clock date/time, thus giving a new pseudo-random
!   number sequence. The HP version fails to do this.
!   This code explicitly modifies the current seed
!   values using the system clock.

  CALL RANDOM_SEED  ( SIZE=MaxElem )

  CALL RANDOM_SEED  ( GET=SeedArray(1:MaxElem) )
  CALL SYSTEM_CLOCK ( COUNT=Value )
  DO i = 1, MaxElem
    SeedArray(i) = SeedArray(i) + MOD ( i+Value, 17 )
  END DO
  CALL RANDOM_SEED  ( PUT=SeedArray(1:MaxElem) )

  nr = MAX ( ROdata%Lev1a%Npoints, &
             ROdata%Lev1b%Npoints, &
             ROdata%Lev2a%Npoints, &
             ROdata%Lev2b%Npoints, &
             ROdata%Lev2d%Npoints, &
             247 )
  ALLOCATE ( randf(nr,mr) )
  ALLOCATE ( ic(nr)       )
  ALLOCATE ( val(nr)      )

!----------------------------------------------------------------
! 1. Header parameters
!----------------------------------------------------------------

! 1.1 Character-valued parameters

  CALL RANDOM_NUMBER ( randf )
  ic = 1 + NINT ( randf(:,1) * (LEN_TRIM(invalidchars)-1) )

  j = 1
  ROdata%Processing_centre = " "
  DO i = 1, 4
    ROdata%GNS_ID(i:i)            = invalidchars(ic(j):ic(j))
    ROdata%LEO_ID(i:i)            = invalidchars(ic(j+1):ic(j+1))
    ROdata%STN_ID(i:i)            = invalidchars(ic(j+2):ic(j+2))
    ROdata%Processing_centre(i:i) = invalidchars(ic(j+3):ic(j+3))
    j = j + 4
  END DO

  v = -randf(j,1) * 9.999
  WRITE ( ROdata%Software_Version, FMT="(A,F6.3)" ) "v", v
  j = j + 1

  DO i = 1, 10
    ROdata%POD_Method(i:i)    = invalidchars(ic(j):ic(j))
    ROdata%Phase_Method(i:i)  = invalidchars(ic(j+1):ic(j+1))
    ROdata%Bangle_Method(i:i) = invalidchars(ic(j+2):ic(j+2))
    ROdata%Refrac_Method(i:i) = invalidchars(ic(j+3):ic(j+3))
    ROdata%Meteo_Method(i:i)  = invalidchars(ic(j+4):ic(j+4))
    ROdata%Thin_Method(i:i)   = invalidchars(ic(j+5):ic(j+5))
    j = j + 6
  END DO

! 1.2 Occultation date/time. NB 'invalid' value range is one more
!    than that element's maximum valid range to one less than that
!    element's special missing data value

  i = 1 + NINT ( ( ROdata%DTocc%Range%Year(mx)   &
                 - ROdata%DTocc%Range%Year(mn)   - 1 ) * randf(j,1) )
  ROdata%DTocc%Year   = MIN ( ROdata%DTocc%Range%Year(mx)   + i, 9998 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Month(mx)  &
                 - ROdata%DTocc%Range%Month(mn)  - 1 ) * randf(j+1,1) )
  ROdata%DTocc%Month  = MIN ( ROdata%DTocc%Range%Month(mx)  + i, 98 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Day(mx)    &
                 - ROdata%DTocc%Range%Day(mn)    - 1 ) * randf(j+2,1) )
  ROdata%DTocc%Day    = MIN ( ROdata%DTocc%Range%Day(mx)    + i, 98 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Hour(mx)   &
                 - ROdata%DTocc%Range%Hour(mn)   - 1 ) * randf(j+3,1) )
  ROdata%DTocc%Hour   = MIN ( ROdata%DTocc%Range%Hour(mx)   + i, 98 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Minute(mx) &
                 - ROdata%DTocc%Range%Minute(mn) - 1 ) * randf(j+4,1) )
  ROdata%DTocc%Minute = MIN ( ROdata%DTocc%Range%Minute(mx) + i, 98 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Second(mx) &
                 - ROdata%DTocc%Range%Second(mn) - 1 ) * randf(j+5,1) )
  ROdata%DTocc%Second = MIN ( ROdata%DTocc%Range%Second(mx) + i, 98 )

  i = 1 + NINT ( ( ROdata%DTocc%Range%Msec(mx)   &
                 - ROdata%DTocc%Range%Msec(mn)   - 1 ) * randf(j+6,1) )
  ROdata%DTocc%Msec   = MIN ( ROdata%DTocc%Range%Msec(mx)   + i, 9998 )

! 1.3 Processing date/time left as-is

! 1.4 PCD - just set 'invalid' flag bit

  ROdata%PCD = IBSET ( 0, PCD_missing )

! 1.5 Overal Quality

  randf = randf - 0.5        ! make the random range -0.5 to +0.5

  v = ( ROdata%Range%Overall_Qual(mx) &
      - ROdata%Range%Overall_Qual(mn) ) * randf(j+8,1)
  IF ( v < 0 ) THEN
    ROdata%Overall_Qual = ROdata%Range%Overall_Qual(mn) + v - 1.0
  ELSE
    ROdata%Overall_Qual = ROdata%Range%Overall_Qual(mx) + v + 1.0
  END IF
  ROdata%Overall_Qual   = MIN ( MAX ( ROdata%Overall_Qual, -99.9_dp ), 999.9_dp )

  j = j + 9

!----------------------------------------------------------------
! 2. Geo-referencing parameters
!----------------------------------------------------------------

  v = ( ROdata%GeoRef%Range%Time_Offset(mx) &
      - ROdata%GeoRef%Range%Time_Offset(mn) ) * randf(j,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%Time_Offset = ROdata%GeoRef%Range%Time_Offset(mn) + v - 1.0
  ELSE
    ROdata%GeoRef%Time_Offset = ROdata%GeoRef%Range%Time_Offset(mx) + v + 1.0
  END IF
  ROdata%GeoRef%Time_Offset   = MIN ( MAX ( ROdata%GeoRef%Time_Offset, -99.999_dp ), 999.999_dp )

  v = ( ROdata%GeoRef%Range%Lat(mx) &
      - ROdata%GeoRef%Range%Lat(mn) )         * randf(j+1,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%Lat         = ROdata%GeoRef%Range%Lat(mn)         + v - 1.0
  ELSE
    ROdata%GeoRef%Lat         = ROdata%GeoRef%Range%Lat(mx)         + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%Lon(mx) &
      - ROdata%GeoRef%Range%Lon(mn) )         * randf(j+2,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%Lon         = ROdata%GeoRef%Range%Lon(mn)         + v - 1.0
  ELSE
    ROdata%GeoRef%Lon         = ROdata%GeoRef%Range%Lon(mx)         + v + 1.0
  END IF

  IF ( ROdata%GeoRef%Lon >  180.0 .AND. &
       ROdata%GeoRef%Lon <= 360.0 )     &
       ROdata%GeoRef%Lon = ROdata%GeoRef%Lon + 180.0

  v = ( ROdata%GeoRef%Range%RoC(mx) &
      - ROdata%GeoRef%Range%RoC(mn) )         * randf(j+3,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%RoC         = ROdata%GeoRef%Range%RoC(mn)         + v - 1.0
  ELSE
   ROdata%GeoRef%RoC          = ROdata%GeoRef%Range%RoC(mx)         + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%R_CoC(mx) &
      - ROdata%GeoRef%Range%R_CoC(mn) )       * randf(j+4,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%R_CoC(1)    = ROdata%GeoRef%Range%R_CoC(mn)       + v - 1.0
  ELSE
    ROdata%GeoRef%R_CoC(1)    = ROdata%GeoRef%Range%R_CoC(mx)       + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%R_CoC(mx) &
      - ROdata%GeoRef%Range%R_CoC(mn) )       * randf(j+5,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%R_CoC(2)    = ROdata%GeoRef%Range%R_CoC(mn)       + v - 1.0
  ELSE
    ROdata%GeoRef%R_CoC(2)    = ROdata%GeoRef%Range%R_CoC(mx)       + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%R_CoC(mx) &
      - ROdata%GeoRef%Range%R_CoC(mn) )       * randf(j+6,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%R_CoC(3)    = ROdata%GeoRef%Range%R_CoC(mn)       + v - 1.0
  ELSE
    ROdata%GeoRef%R_CoC(3)    = ROdata%GeoRef%Range%R_CoC(mx)       + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%Azimuth(mx) &
      - ROdata%GeoRef%Range%Azimuth(mn) )     * randf(j+7,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%Azimuth     = ROdata%GeoRef%Range%Azimuth(mn)     + v - 1.0
  ELSE
    ROdata%GeoRef%Azimuth     = ROdata%GeoRef%Range%Azimuth(mx)     + v + 1.0
  END IF

  v = ( ROdata%GeoRef%Range%Undulation(mx) &
      - ROdata%GeoRef%Range%Undulation(mn) )  * randf(j+8,1)
  IF ( v < 0 ) THEN
    ROdata%GeoRef%Undulation  = ROdata%GeoRef%Range%Undulation(mn)  + v - 1.0
  ELSE
    ROdata%GeoRef%Undulation  = ROdata%GeoRef%Range%Undulation(mx)  + v + 1.0
  END IF

  j = j + 9

!----------------------------------------------------------------
! 3. Meteo background parameters
!----------------------------------------------------------------

  DO i = 1, 10
    ROdata%BG%Source(i:i) = invalidchars(ic(j):ic(j))
    j = j + 1
  END DO

  randf = randf + 0.5   ! range 0-1 again

  i = 1 + NINT ( ( ROdata%BG%Range%Year(mx) &
                 - ROdata%BG%Range%Year(mn) )   * randf(j,1) )
  ROdata%BG%Year     = MIN ( ROdata%BG%Range%Year(mx)    + i, 9998 )

  i = 1 + NINT ( ( ROdata%BG%Range%Month(mx) &
                 - ROdata%BG%Range%Month(mn) )  * randf(j+1,1) )
  ROdata%BG%Month    = MIN ( ROdata%BG%Range%Month(mx)   + i, 98 )

  i = 1 + NINT ( ( ROdata%BG%Range%Day(mx) &
                 - ROdata%BG%Range%Day(mn) )    * randf(j+2,1) )
  ROdata%BG%Day      = MIN ( ROdata%BG%Range%Day(mx)     + i, 98 )

  i = 1 + NINT ( ( ROdata%BG%Range%Hour(mx) &
                 - ROdata%BG%Range%Hour(mn) )   * randf(j+3,1) )
  ROdata%BG%Hour     = MIN ( ROdata%BG%Range%Hour(mx)    + i, 98 )

  i = 1 + NINT ( ( ROdata%BG%Range%Minute(mx) &
                 - ROdata%BG%Range%Minute(mn) ) * randf(j+4,1) )
  ROdata%BG%Minute   = MIN ( ROdata%BG%Range%Minute(mx)  + i, 98 )

  i = 1 + NINT ( ( ROdata%BG%Range%FCperiod(mx) &
               - ROdata%BG%Range%FCperiod(mn) ) * randf(j+5,1) )
  ROdata%BG%FCperiod = MIN ( ROdata%BG%Range%FCperiod(mx) + i, 998.0_dp )

!----------------------------------------------------------------
! 4. Level 1a profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev1a%Npoints

  val = ( ROdata%Lev1a%Range%Dtime(mx) &
        - ROdata%Lev1a%Range%Dtime(mn) )      * randf(1:np,1)

  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Dtime      = ROdata%Lev1a%Range%Dtime(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Dtime      = ROdata%Lev1a%Range%Dtime(mx) + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1a%Dtime        = MIN ( MAX ( ROdata%Lev1a%Dtime, -99.999_dp ), 999.999_dp )

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,2)

  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L1ca   = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L1ca   = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L1p    = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L1p    = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L2p    = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L2p    = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%Phase(mx) &
        - ROdata%Lev1a%Range%Phase(mn) )      * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Phase_L1   = ROdata%Lev1a%Range%Phase(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Phase_L1   = ROdata%Lev1a%Range%Phase(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%Phase(mx) &
        - ROdata%Lev1a%Range%Phase(mn) )      * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Phase_L2   = ROdata%Lev1a%Range%Phase(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Phase_L2   = ROdata%Lev1a%Range%Phase(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,1) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,1) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,2) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,2) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,3) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,3) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,10)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,1) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,1) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,2) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,2) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,3) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,3) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,1) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,1) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,2) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,2) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,3) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,3) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,1) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,1) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,2) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,2) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,3) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,3) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%Phase_Qual(mx) &
        - ROdata%Lev1a%Range%Phase_Qual(mn) ) * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Phase_Qual = ROdata%Lev1a%Range%Phase_Qual(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Phase_Qual = ROdata%Lev1a%Range%Phase_Qual(mx) + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1a%Phase_Qual   = MIN ( MAX ( ROdata%Lev1a%Phase_Qual, -99.9_dp ), 999.9_dp )

!----------------------------------------------------------------
! 5. Level 1b profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev1b%Npoints

  val = ( ROdata%Lev1b%Range%Lat_tp(mx) &
        - ROdata%Lev1b%Range%Lat_tp(mn) )       * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Lat_tp              = ROdata%Lev1b%Range%Lat_tp(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Lat_tp              = ROdata%Lev1b%Range%Lat_tp(mx)     + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Lon_tp(mx) &
        - ROdata%Lev1b%Range%Lon_tp(mn) )       * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Lon_tp              = ROdata%Lev1b%Range%Lon_tp(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Lon_tp              = ROdata%Lev1b%Range%Lon_tp(mx)     + val(1:np) + 1.0
  ENDWHERE

  WHERE ( ROdata%Lev1b%Lon_tp >  180.0 .AND. &
          ROdata%Lev1b%Lon_tp <= 360.0 )     &
          ROdata%Lev1b%Lon_tp = ROdata%Lev1b%Lon_tp + 180.0

  val = ( ROdata%Lev1b%Range%Azimuth_tp(mx) &
        - ROdata%Lev1b%Range%Azimuth_tp(mn) )   * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Azimuth_tp          = ROdata%Lev1b%Range%Azimuth_tp(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Azimuth_tp          = ROdata%Lev1b%Range%Azimuth_tp(mx) + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Impact(mx) &
        - ROdata%Lev1b%Range%Impact(mn) )       * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Impact_L1           = ROdata%Lev1b%Range%Impact(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Impact_L1           = ROdata%Lev1b%Range%Impact(mx)     + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Impact(mx) &
        - ROdata%Lev1b%Range%Impact(mn) )       * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Impact_L2           = ROdata%Lev1b%Range%Impact(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Impact_L2           = ROdata%Lev1b%Range%Impact(mx)     + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Impact(mx) &
        - ROdata%Lev1b%Range%Impact(mn) )       * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Impact              = ROdata%Lev1b%Range%Impact(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Impact              = ROdata%Lev1b%Range%Impact(mx)     + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Impact(mx) &
        - ROdata%Lev1b%Range%Impact(mn) )       * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Impact_Opt          = ROdata%Lev1b%Range%Impact(mn)     + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Impact_Opt          = ROdata%Lev1b%Range%Impact(mx)     + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L1           = ROdata%Lev1b%Range%Bangle(mn)     + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L1           = ROdata%Lev1b%Range%Bangle(mx)     + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L2           = ROdata%Lev1b%Range%Bangle(mn)    + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L2           = ROdata%Lev1b%Range%Bangle(mx)    + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,10)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle           = ROdata%Lev1b%Range%Bangle(mn)       + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle           = ROdata%Lev1b%Range%Bangle(mx)       + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Opt       = ROdata%Lev1b%Range%Bangle(mn)       + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_Opt       = ROdata%Lev1b%Range%Bangle(mx)       + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle_Sigma(mx) &
        - ROdata%Lev1b%Range%Bangle_Sigma(mn) ) * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L1_Sigma  = ROdata%Lev1b%Range%Bangle_Sigma(mn) + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L1_Sigma  = ROdata%Lev1b%Range%Bangle_Sigma(mx) + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle_Sigma(mx) &
        - ROdata%Lev1b%Range%Bangle_Sigma(mn) ) * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L2_Sigma  = ROdata%Lev1b%Range%Bangle_Sigma(mn) + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L2_Sigma  = ROdata%Lev1b%Range%Bangle_Sigma(mx) + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle_Sigma(mx) &
        - ROdata%Lev1b%Range%Bangle_Sigma(mn) ) * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Sigma     = ROdata%Lev1b%Range%Bangle_Sigma(mn) + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_Sigma     = ROdata%Lev1b%Range%Bangle_Sigma(mx) + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle_Sigma(mx) &
        - ROdata%Lev1b%Range%Bangle_Sigma(mn) ) * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Opt_Sigma = ROdata%Lev1b%Range%Bangle_Sigma(mn) + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_Opt_Sigma = ROdata%Lev1b%Range%Bangle_Sigma(mx) + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle_Qual(mx) &
        - ROdata%Lev1b%Range%Bangle_Qual (mn) ) * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L1_Qual   = ROdata%Lev1b%Range%Bangle_Qual (mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Bangle_L1_Qual   = ROdata%Lev1b%Range%Bangle_Qual (mx) + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1b%Bangle_L1_Qual     = MIN ( MAX ( ROdata%Lev1b%Bangle_L1_Qual, -99.9_dp ), 999.9_dp )

  val = ( ROdata%Lev1b%Range%Bangle_Qual(mx) &
        - ROdata%Lev1b%Range%Bangle_Qual(mn) )  * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L2_Qual   = ROdata%Lev1b%Range%Bangle_Qual(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Bangle_L2_Qual   = ROdata%Lev1b%Range%Bangle_Qual(mx)  + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1b%Bangle_L2_Qual     = MIN ( MAX ( ROdata%Lev1b%Bangle_L2_Qual, -99.9_dp ), 999.9_dp )

  val = ( ROdata%Lev1b%Range%Bangle_Qual(mx) &
        - ROdata%Lev1b%Range%Bangle_Qual(mn) )  * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Qual      = ROdata%Lev1b%Range%Bangle_Qual(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Bangle_Qual      = ROdata%Lev1b%Range%Bangle_Qual(mx)  + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1b%Bangle_Qual        = MIN ( MAX ( ROdata%Lev1b%Bangle_Qual, -99.9_dp ), 999.9_dp )

  val = ( ROdata%Lev1b%Range%Bangle_Qual(mx) &
        - ROdata%Lev1b%Range%Bangle_Qual(mn) )  * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Opt_Qual  = ROdata%Lev1b%Range%Bangle_Qual(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1b%Bangle_Opt_Qual  = ROdata%Lev1b%Range%Bangle_Qual(mx)  + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev1b%Bangle_Opt_Qual    = MIN ( MAX ( ROdata%Lev1b%Bangle_Opt_Qual, -99.9_dp ), 999.9_dp )

!----------------------------------------------------------------
! 6. Level 2a profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev2a%Npoints

  val = ( ROdata%Lev2a%Range%Alt_Refrac(mx) &
        - ROdata%Lev2a%Range%Alt_Refrac(mn) )   * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Alt_Refrac   = ROdata%Lev2a%Range%Alt_Refrac(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Alt_Refrac   = ROdata%Lev2a%Range%Alt_Refrac(mx)  + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Geop_Refrac(mx) &
        - ROdata%Lev2a%Range%Geop_Refrac(mn) )  * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Geop_Refrac  = ROdata%Lev2a%Range%Geop_Refrac(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Geop_Refrac  = ROdata%Lev2a%Range%Geop_Refrac(mx) + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Refrac(mx) &
        - ROdata%Lev2a%Range%Refrac(mn) )       * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Refrac       = ROdata%Lev2a%Range%Refrac(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Refrac       = ROdata%Lev2a%Range%Refrac(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Refrac_Sigma(mx) &
        - ROdata%Lev2a%Range%Refrac_Sigma(mn) ) * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Refrac_Sigma = ROdata%Lev2a%Range%Refrac_Sigma(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Refrac_Sigma = ROdata%Lev2a%Range%Refrac_Sigma(mx) + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Refrac_Qual(mx) &
        - ROdata%Lev2a%Range%Refrac_Qual(mn) )  * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Refrac_Qual  = ROdata%Lev2a%Range%Refrac_Qual(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Refrac_Qual  = ROdata%Lev2a%Range%Refrac_Qual(mx) + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev2a%Refrac_Qual    = MIN ( MAX ( ROdata%Lev2a%Refrac_Qual, -99.9_dp ), 999.9_dp )

  val = ( ROdata%Lev2a%Range%Dry_Temp(mx) &
        - ROdata%Lev2a%Range%Dry_Temp(mn) )      * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Dry_Temp       = ROdata%Lev2a%Range%Dry_Temp(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Dry_Temp       = ROdata%Lev2a%Range%Dry_Temp(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Dry_Temp_Sigma(mx) &
        - ROdata%Lev2a%Range%Dry_Temp_Sigma(mn) ) * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Dry_Temp_Sigma = ROdata%Lev2a%Range%Dry_Temp_Sigma(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Dry_Temp_Sigma = ROdata%Lev2a%Range%Dry_Temp_Sigma(mx) + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2a%Range%Dry_Temp_Qual(mx) &
        - ROdata%Lev2a%Range%Dry_Temp_Qual(mn) )  * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Dry_Temp_Qual  = ROdata%Lev2a%Range%Dry_Temp_Qual(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Dry_Temp_Qual  = ROdata%Lev2a%Range%Dry_Temp_Qual(mx) + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev2a%Dry_Temp_Qual    = MIN ( MAX ( ROdata%Lev2a%Dry_Temp_Qual, -99.9_dp ), 999.9_dp )

!----------------------------------------------------------------
! 7. Level 2b profile parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev2b%Npoints

  val = ( ROdata%Lev2b%Range%Geop(mx) &
        - ROdata%Lev2b%Range%Geop(mn) )        * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Geop        = ROdata%Lev2b%Range%Geop(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Geop        = ROdata%Lev2b%Range%Geop(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Geop_Sigma(mx) &
        - ROdata%Lev2b%Range%Geop_Sigma(mn) )  * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Geop_Sigma  = ROdata%Lev2b%Range%Geop_Sigma(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Geop_Sigma  = ROdata%Lev2b%Range%Geop_Sigma(mx)  + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Press (mx) &
        - ROdata%Lev2b%Range%Press (mn) )      * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Press       = ROdata%Lev2b%Range%Press (mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Press       = ROdata%Lev2b%Range%Press (mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Press_Sigma(mx) &
        - ROdata%Lev2b%Range%Press_Sigma(mn) ) * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Press_Sigma = ROdata%Lev2b%Range%Press_Sigma(mn) + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Press_Sigma = ROdata%Lev2b%Range%Press_Sigma(mx) + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Temp(mx) &
        - ROdata%Lev2b%Range%Temp(mn) )        * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Temp        = ROdata%Lev2b%Range%Temp(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Temp        = ROdata%Lev2b%Range%Temp(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Temp_Sigma(mx) &
        - ROdata%Lev2b%Range%Temp_Sigma(mn) )  * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Temp_Sigma  = ROdata%Lev2b%Range%Temp_Sigma(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Temp_Sigma  = ROdata%Lev2b%Range%Temp_Sigma(mx)  + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%SHum(mx) &
        - ROdata%Lev2b%Range%SHum(mn) )        * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%SHum        = ROdata%Lev2b%Range%SHum(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%SHum        = ROdata%Lev2b%Range%SHum(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%SHum_Sigma(mx) &
        - ROdata%Lev2b%Range%SHum_Sigma(mn) )  * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%SHum_Sigma  = ROdata%Lev2b%Range%SHum_Sigma(mn)  + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%SHum_Sigma  = ROdata%Lev2b%Range%SHum_Sigma(mx)  + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Meteo_Qual(mx) &
        - ROdata%Lev2b%Range%Meteo_Qual(mn) )  * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Meteo_Qual  = ROdata%Lev2b%Range%Meteo_Qual(mn)   + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Meteo_Qual  = ROdata%Lev2b%Range%Meteo_Qual(mx)   + val(1:np) + 1.0
  ENDWHERE
  ROdata%Lev2b%Meteo_Qual    = MIN ( MAX ( ROdata%Lev2b%Meteo_Qual, -99.9_dp ), 999.9_dp )

!----------------------------------------------------------------
! 8. Level 2c parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5

  v = ( ROdata%Lev2c%Range%Geop_Sfc(mx) &
      - ROdata%Lev2c%Range%Geop_Sfc(mn) )          * randf(1,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%Geop_Sfc        = ROdata%Lev2c%Range%Geop_Sfc(mn)        + v - 1.0
  ELSE
    ROdata%Lev2c%Geop_Sfc        = ROdata%Lev2c%Range%Geop_Sfc(mx)        + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%Press_Sfc(mx) &
      - ROdata%Lev2c%Range%Press_Sfc(mn) )         * randf(2,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%Press_Sfc       = ROdata%Lev2c%Range%Press_Sfc(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%Press_Sfc       = ROdata%Lev2c%Range%Press_Sfc(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%Press_Sfc_Sigma(mx) &
      - ROdata%Lev2c%Range%Press_Sfc_Sigma(mn) )   * randf(3,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%Press_Sfc_Sigma = ROdata%Lev2c%Range%Press_Sfc_Sigma(mn) + v - 1.0
  ELSE
    ROdata%Lev2c%Press_Sfc_Sigma = ROdata%Lev2c%Range%Press_Sfc_Sigma(mx) + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%Press_Sfc_Qual(mx)  &
      - ROdata%Lev2c%Range%Press_Sfc_Qual(mn) )    * randf(4,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%Press_Sfc_Qual  = ROdata%Lev2c%Range%Press_Sfc_Qual(mn)  + v - 1.0
  ELSE
    ROdata%Lev2c%Press_Sfc_Qual  = ROdata%Lev2c%Range%Press_Sfc_Qual(mx)  + v + 1.0
  END IF
  ROdata%Lev2c%Press_Sfc_Qual    = MIN ( MAX ( ROdata%Lev2c%Press_Sfc_Qual, -99.9_dp ), 999.9_dp )


  v = ( ROdata%Lev2c%Range%TPH_Bangle(mx) &
      - ROdata%Lev2c%Range%TPH_Bangle(mn) )        * randf(5,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Bangle       = ROdata%Lev2c%Range%TPH_Bangle(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Bangle       = ROdata%Lev2c%Range%TPH_Bangle(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPA_Bangle(mx) &
      - ROdata%Lev2c%Range%TPA_Bangle(mn) )        * randf(6,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPA_Bangle       = ROdata%Lev2c%Range%TPA_Bangle(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPA_Bangle       = ROdata%Lev2c%Range%TPA_Bangle(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Bangle_Flag = ROdata%Lev2c%Range%TPH_Bangle_Flag(mx) + 1


  v = ( ROdata%Lev2c%Range%TPH_Refrac(mx) &
      - ROdata%Lev2c%Range%TPH_Refrac(mn) )        * randf(7,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Refrac       = ROdata%Lev2c%Range%TPH_Refrac(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Refrac       = ROdata%Lev2c%Range%TPH_Refrac(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPN_Refrac(mx) &
      - ROdata%Lev2c%Range%TPN_Refrac(mn) )        * randf(8,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPN_Refrac       = ROdata%Lev2c%Range%TPN_Refrac(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPN_Refrac       = ROdata%Lev2c%Range%TPN_Refrac(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Refrac_Flag = ROdata%Lev2c%Range%TPH_Refrac_Flag(mn) - 1


  v = ( ROdata%Lev2c%Range%TPH_Tdry_LRT(mx) &
      - ROdata%Lev2c%Range%TPH_Tdry_LRT(mn) )        * randf(9,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Tdry_LRT       = ROdata%Lev2c%Range%TPH_Tdry_LRT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Tdry_LRT       = ROdata%Lev2c%Range%TPH_Tdry_LRT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPT_Tdry_LRT(mx) &
      - ROdata%Lev2c%Range%TPT_Tdry_LRT(mn) )        * randf(10,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPT_Tdry_LRT       = ROdata%Lev2c%Range%TPT_Tdry_LRT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPT_Tdry_LRT       = ROdata%Lev2c%Range%TPT_Tdry_LRT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Tdry_LRT_Flag = ROdata%Lev2c%Range%TPH_Tdry_LRT_Flag(mx) + 1


  v = ( ROdata%Lev2c%Range%TPH_Tdry_CPT(mx) &
      - ROdata%Lev2c%Range%TPH_Tdry_CPT(mn) )        * randf(11,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Tdry_CPT       = ROdata%Lev2c%Range%TPH_Tdry_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Tdry_CPT       = ROdata%Lev2c%Range%TPH_Tdry_CPT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPT_Tdry_CPT(mx) &
      - ROdata%Lev2c%Range%TPT_Tdry_CPT(mn) )        * randf(12,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPT_Tdry_CPT       = ROdata%Lev2c%Range%TPT_Tdry_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPT_Tdry_CPT       = ROdata%Lev2c%Range%TPT_Tdry_CPT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Tdry_CPT_Flag = ROdata%Lev2c%Range%TPH_Tdry_CPT_Flag(mx) + 1


  v = ( ROdata%Lev2c%Range%PRH_Tdry_CPT(mx) &
      - ROdata%Lev2c%Range%PRH_Tdry_CPT(mn) )        * randf(13,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%PRH_Tdry_CPT       = ROdata%Lev2c%Range%PRH_Tdry_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%PRH_Tdry_CPT       = ROdata%Lev2c%Range%PRH_Tdry_CPT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%PRT_Tdry_CPT(mx) &
      - ROdata%Lev2c%Range%PRT_Tdry_CPT(mn) )        * randf(14,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%PRT_Tdry_CPT       = ROdata%Lev2c%Range%PRT_Tdry_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%PRT_Tdry_CPT       = ROdata%Lev2c%Range%PRT_Tdry_CPT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%PRH_Tdry_CPT_Flag = ROdata%Lev2c%Range%PRH_Tdry_CPT_Flag(mx) + 1


  v = ( ROdata%Lev2c%Range%TPH_Temp_LRT(mx) &
      - ROdata%Lev2c%Range%TPH_Temp_LRT(mn) )        * randf(15,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Temp_LRT       = ROdata%Lev2c%Range%TPH_Temp_LRT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Temp_LRT       = ROdata%Lev2c%Range%TPH_Temp_LRT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPT_Temp_LRT(mx) &
      - ROdata%Lev2c%Range%TPT_Temp_LRT(mn) )        * randf(16,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPT_Temp_LRT       = ROdata%Lev2c%Range%TPT_Temp_LRT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPT_Temp_LRT       = ROdata%Lev2c%Range%TPT_Temp_LRT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Temp_LRT_Flag = ROdata%Lev2c%Range%TPH_Temp_LRT_Flag(mn) - 1


  v = ( ROdata%Lev2c%Range%TPH_Temp_CPT(mx) &
      - ROdata%Lev2c%Range%TPH_Temp_CPT(mn) )        * randf(17,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPH_Temp_CPT       = ROdata%Lev2c%Range%TPH_Temp_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPH_Temp_CPT       = ROdata%Lev2c%Range%TPH_Temp_CPT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%TPT_Temp_CPT(mx) &
      - ROdata%Lev2c%Range%TPT_Temp_CPT(mn) )        * randf(18,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%TPT_Temp_CPT       = ROdata%Lev2c%Range%TPT_Temp_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%TPT_Temp_CPT       = ROdata%Lev2c%Range%TPT_Temp_CPT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%TPH_Temp_CPT_Flag = ROdata%Lev2c%Range%TPH_Temp_CPT_Flag(mn) - 1


  v = ( ROdata%Lev2c%Range%PRH_Temp_CPT(mx) &
      - ROdata%Lev2c%Range%PRH_Temp_CPT(mn) )        * randf(19,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%PRH_Temp_CPT       = ROdata%Lev2c%Range%PRH_Temp_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%PRH_Temp_CPT       = ROdata%Lev2c%Range%PRH_Temp_CPT(mx)       + v + 1.0
  END IF

  v = ( ROdata%Lev2c%Range%PRT_Temp_CPT(mx) &
      - ROdata%Lev2c%Range%PRT_Temp_CPT(mn) )        * randf(20,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%PRT_Temp_CPT       = ROdata%Lev2c%Range%PRT_Temp_CPT(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%PRT_Temp_CPT       = ROdata%Lev2c%Range%PRT_Temp_CPT(mx)       + v + 1.0
  END IF

  ROdata%Lev2c%PRH_Temp_CPT_Flag = ROdata%Lev2c%Range%PRH_Temp_CPT_Flag(mn) - 1

!----------------------------------------------------------------
! 9. Level 2d parameters
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  ic = 1 + NINT ( randf(:,1) * (LEN_TRIM(invalidchars)-1) )
  np = ROdata%Lev2d%Npoints

  DO i = 1, 10
    ROdata%Lev2d%Level_Type(i:i) = invalidchars(ic(i):ic(i))
    j = j + 1
  END DO

  randf = randf - 0.5

  val = ( ROdata%Lev2d%Range%Level_Coeff_A(mx) &
        - ROdata%Lev2d%Range%Level_Coeff_A(mn) ) * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2d%Level_Coeff_A = ROdata%Lev2d%Range%Level_Coeff_A(mn)   + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2d%Level_Coeff_A = ROdata%Lev2d%Range%Level_Coeff_A(mx)   + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2d%Range%Level_Coeff_B(mx) &
        - ROdata%Lev2d%Range%Level_Coeff_B(mn) ) * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2d%Level_Coeff_B = ROdata%Lev2d%Range%Level_Coeff_B(mn)   + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2d%Level_Coeff_B = ROdata%Lev2d%Range%Level_Coeff_B(mx)   + val(1:np) + 1.0
  ENDWHERE

!----------------------------------------------------------------
! 10. Tidy up
!----------------------------------------------------------------

  DEALLOCATE ( randf, ic )

END SUBROUTINE test2ropp_invalid
!--------------------------------------------------------------------

SUBROUTINE test2ropp_badprof ( ROdata ) ! (inout)

!****x* test2ropp/test2ropp_badprof *
!
! NAME
!   test2ropp_badprof - generate a bad profile with sensible vertical
!                       coordinates, but random, invalid, profile values
!
! SYNOPSIS
!   USE ropp_io_types
!   TYPE(ROprof) :: ROdata
!   CALL test2ropp_badprof(ROdata)
!
! INPUT
!   ROdata  dtyp  ROPP structure
!
! OUTPUT
!   ROdata  dtyp  ROPP structure
!
! DESCRIPTION
!   Fills a ROPP structure with (more-or-less) sensible time & vertical
!   coordinate parameter values, but the observed profile is filled with
!   random values with a linear PDF, but all having out-of-range values
!   (randomly positive or negative where appropriate). Values will
!   still be within sensible bounds (ie not exceeding the missing data
!   flag values. Header values are filled with valid values as per
!   test2ropp_valid. Sigma and Quality parameters are left missing.
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

! Modules

  USE typesizes,     dp => EightByteReal
  USE DateTimeProgs, ONLY: DateTimeOffset
  USE ropp_io_types

  IMPLICIT NONE

! Fixed parameters

  INTEGER, PARAMETER :: mn = 1     ! Index to minimum range value
  INTEGER, PARAMETER :: mx = 2     ! Index to maximum range value
  INTEGER, PARAMETER :: mr = 30    ! No. of random number sequences

  INTEGER, PARAMETER :: ngid = 3
  INTEGER, PARAMETER :: nlid = 14
  INTEGER, PARAMETER :: nsid = 26
  INTEGER, PARAMETER :: npid = 5
  INTEGER, PARAMETER :: nbid = 4
  INTEGER, PARAMETER :: npod = 6
  INTEGER, PARAMETER :: nphs = 6
  INTEGER, PARAMETER :: nban = 6
  INTEGER, PARAMETER :: nref = 6
  INTEGER, PARAMETER :: nmet = 2
  INTEGER, PARAMETER :: nthn = 8

  CHARACTER (LEN=*), PARAMETER :: validgid(ngid) = (/ "G", "R", "E" /)
  CHARACTER (LEN=*), PARAMETER :: validlid(nlid) = (/ "META", "METB", "METC", &
                                                      "OERS", "CHMP",         &
                                                      "GRAA", "GRAB", "TSRX", &
                                                      "CO01", "CO02", "CO03", &
                                                      "CO04", "CO05", "CO06" /)
  CHARACTER (LEN=*), PARAMETER :: validsid(nsid) = (/ "ACOR", "BOGO", "CANT", &
                                                      "DENT", "EIJS", "FRAG", &
                                                      "GOPE", "HERS", "INVE", &
                                                      "JOEN", "KIRU", "LAMP", &
                                                      "MALL", "NEWL", "ONSA", &
                                                      "PADO", "QUAL", "ROVE", &
                                                      "SFER", "TERS", "UNPG", &
                                                      "VILL", "WTZR", "XFER", &
                                                      "YEBE", "ZIMM" /)
  CHARACTER (LEN=*), PARAMETER :: validpid(npid) = (/ "DMI     ", "GFZ     ", "UCAR    ", &
                                                      "NESDIS  ", "EUMETSAT" /)
  CHARACTER (LEN=*), PARAMETER :: validbid(nbid) = (/ "ECMWF ","METO  ", "HIRLAM", "NCEP  " /)

  CHARACTER (LEN=*), PARAMETER :: validpod(npod) = (/ "POD_1",    "POD_2",    "POD_3",    &
                                                      "POD_4",    "POD_5",    "POD_6"    /)
  CHARACTER (LEN=*), PARAMETER :: validphs(nphs) = (/ "PHASE_1",  "PHASE_2",  "PHASE_3",  &
                                                      "PHASE_4",  "PHASE_5",  "PHASE_6"  /)
  CHARACTER (LEN=*), PARAMETER :: validban(nban) = (/ "BANGLE_1", "BANGLE_2", "BANGLE_3", &
                                                      "BANGLE_4", "BANGLE_5", "BANGLE_6" /)
  CHARACTER (LEN=*), PARAMETER :: validref(nref) = (/ "REFRAC_1", "REFRAC_2", "REFRAC_3", &
                                                      "REFRAC_4", "REFRAC_5", "REFRAC_6" /)
  CHARACTER (LEN=*), PARAMETER :: validmet(nmet) = (/ "T-DRY ",   "1D-VAR" /)
  CHARACTER (LEN=*), PARAMETER :: validthn(nthn) = (/ "NONE  ",   "SAMPLE", &
                                                      "LIN   ",   "LOG   ", &
                                                      "SGLIN ",   "SGLOG ", &
                                                      "ASGLIN",   "ASGLOG" /)

! Argument list parameters

  TYPE (ROprof), INTENT(INOUT) :: ROdata

! Local variables

  CHARACTER (LEN=10)    :: number        ! number as string
  INTEGER               :: i, j, np      ! loop/index numbers
  INTEGER               :: SeedArray(1000), MaxElem, Value
  REAL(dp)              :: v             ! some d.p. value

  INTEGER               :: nr            ! random number sequence length
  INTEGER,  ALLOCATABLE :: ic(:)         ! random character index
  REAL(dp), ALLOCATABLE :: randf(:,:)    ! random number array (0-1)
  REAL(dp), ALLOCATABLE :: val(:)        ! random scaled value array
  REAL                  :: strt, incr    ! start & increment values
  INTEGER, DIMENSION(8) :: DT1, DT2      ! Date/time arrays
  INTEGER               :: OffDay        ! random offset days in the past

!----------------------------------------------------------------
! 0. Initialise
!----------------------------------------------------------------

!   Calling RANDOM_SEED with no arguments is supposed to
!   set the internal random number seed to a value based on
!   the system clock date/time, thus giving a new pseudo-random
!   number sequence. The HP version fails to do this.
!   This code explicitly modifies the current seed
!   values using the system clock.

  CALL RANDOM_SEED  ( SIZE=MaxElem )

  CALL RANDOM_SEED  ( GET=SeedArray(1:MaxElem) )

  CALL SYSTEM_CLOCK ( COUNT=Value )

  DO i = 1, MaxElem
    SeedArray(i) = SeedArray(i) + MOD ( i+Value, 17 )
  END DO
  CALL RANDOM_SEED  ( PUT=SeedArray(1:MaxElem) )

  nr = MAX ( ROdata%Lev1a%Npoints, &
             ROdata%Lev1b%Npoints, &
             ROdata%Lev2a%Npoints, &
             ROdata%Lev2b%Npoints, &
             ROdata%Lev2d%Npoints, &
             247 )
  ALLOCATE ( randf(nr,mr) )
  ALLOCATE ( ic(nr)       )
  ALLOCATE ( val(nr)      )

!----------------------------------------------------------------
! 1. Header parameters
!----------------------------------------------------------------

! 1.1 Character-valued parameters - choose from pre-set lists

  CALL RANDOM_NUMBER ( randf )

  i = 1 + NINT(randf(1,1)*(ngid-1))
  WRITE ( ROdata%GNS_ID, FMT="(A,I3.3)" ) validgid(i), &
                                          1+NINT(randf(1,1)*23.0) ! 1-24
  i = 1 + NINT(randf(1,2)*(nlid-1))
  ROdata%LEO_ID = validlid(i)

  i = 1 + NINT(randf(1,3)*(nsid-1))
  ROdata%STN_ID = validsid(i)

  i = 1 + NINT(randf(1,4)*(npid-1))
  ROdata%Processing_centre = validpid(i)

  i = 1 + NINT(randf(1,5)*(npod-1))
  ROdata%POD_Method = validpod(i)

  i = 1 + NINT(randf(1,6)*(nphs-1))
  ROdata%Phase_Method = validphs(i)

  i = 1 + NINT(randf(1,7)*(nban-1))
  ROdata%Bangle_Method = validban(i)

  i = 1 + NINT(randf(1,8)*(nref-1))
  ROdata%Refrac_Method = validref(i)

  i = 1 + NINT(randf(1,9)*(nmet-1))
  ROdata%Meteo_Method = validmet(i)

  i = 1 + NINT(randf(1,10)*(nthn-1))
  ROdata%Thin_Method = validthn(i)

  v = randf(1,11) * 99.999
  WRITE ( number, FMT="(F6.3)" ) v
  IF ( v < 10.0_dp ) THEN
    ROdata%Software_Version = "V0"//ADJUSTL(number)
  ELSE
    ROdata%Software_Version = "V" //ADJUSTL(number)
  END IF

! 1.2 Occultation date/time - some time in the past 30 days.

  DT1 = (/ROdata%DTpro%Year,ROdata%DTpro%Month,ROdata%DTpro%Day, &
          0,0,0,0,0/)

  OffDay = NINT ( randf(1,12) * 30.0 )
  DT2 = (/0,0,OffDay,0,0,0,0,0/)

  CALL DateTimeOffset ( DT1, "-", DT2 )

  ROdata%DTocc%Year  = DT1(1)
  ROdata%DTocc%Month = DT1(2)
  ROdata%DTocc%Day   = DT1(3)
  ROdata%DTocc%Hour   = ROdata%DTocc%Range%Hour(mn)         &
                      + NINT((ROdata%DTocc%Range%Hour(mx)   &
                      - ROdata%DTocc%Range%Hour(mn))        &
                      * randf(1,13))

  ROdata%DTocc%Minute = ROdata%DTocc%Range%Minute(mn)       &
                      + NINT((ROdata%DTocc%Range%Minute(mx) &
                      - ROdata%DTocc%Range%Minute(mn)) &
                      * randf(1,14))

  ROdata%DTocc%Second = ROdata%DTocc%Range%Second(mn)        &
                      + NINT((ROdata%DTocc%Range%Second(mx)  &
                      - ROdata%DTocc%Range%Second(mn))       &
                      * randf(1,15))

  ROdata%DTocc%Msec   = ROdata%DTocc%Range%Msec(mn)          &
                      + NINT((ROdata%DTocc%Range%Msec(mx)    &
                      - ROdata%DTocc%Range%Msec(mn))         &
                      * randf(1,16))

! 1.3 Processing date/time left as-is

! 1.4 PCD left missing

! 1.5 Overal Quality left missing

  j = j + 9

!----------------------------------------------------------------
! 2. Geo-referencing parameters
!    - all randomly valid
!----------------------------------------------------------------

  ROdata%GeoRef%Time_Offset =  ROdata%GeoRef%Range%Time_Offset(mn)  &
                            + (ROdata%GeoRef%Range%Time_Offset(mx)  &
                             - ROdata%GeoRef%Range%Time_Offset(mn)) &
                            * randf(1,19)

  ROdata%GeoRef%Lat         =  ROdata%GeoRef%Range%Lat(mn)  &
                            + (ROdata%GeoRef%Range%Lat(mx)  &
                             - ROdata%GeoRef%Range%Lat(mn)) &
                            * randf(1,20)

  ROdata%GeoRef%Lon         =  ROdata%GeoRef%Range%Lon(mn)  &
                            + (ROdata%GeoRef%Range%Lon(mx)  &
                             - ROdata%GeoRef%Range%Lon(mn)) &
                            * randf(1,21)

  ROdata%GeoRef%RoC         =  ROdata%GeoRef%Range%RoC(mn)  &
                            + (ROdata%GeoRef%Range%RoC(mx)  &
                             - ROdata%GeoRef%Range%RoC(mn)) &
                            * randf(1,22)

  ROdata%GeoRef%R_CoC(1:3)  =  ROdata%GeoRef%Range%R_CoC(mn)  &
                            + (ROdata%GeoRef%Range%R_CoC(mx)  &
                             - ROdata%GeoRef%Range%R_CoC(mn)) &
                            * randf(1,23:25)

  ROdata%GeoRef%Azimuth     =  ROdata%GeoRef%Range%Azimuth(mn)  &
                            + (ROdata%GeoRef%Range%Azimuth(mx)  &
                             - ROdata%GeoRef%Range%Azimuth(mn)) &
                            * randf(1,26)

  ROdata%GeoRef%Undulation  =  ROdata%GeoRef%Range%Undulation(mn)  &
                            + (ROdata%GeoRef%Range%Undulation(mx)  &
                             - ROdata%GeoRef%Range%Undulation(mn)) &
                            * randf(1,27)

  j = j + 9

!----------------------------------------------------------------
! 3. Meteo background parameters
!    - Left missing
!----------------------------------------------------------------

!----------------------------------------------------------------
! 4. Level 1a profile parameters
!    - Delta times increasing within valid range
!    - SNR & Phase, LEO & GNSS POD values randomly invalid
!    - Sigmas & Quality left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev1a%Npoints

  strt =   0.0
  incr = ( 60.0 - strt ) / (np+2)
  DO i = 1, np
    ROdata%Lev1a%Dtime(i) = strt + incr * i
  END DO

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L1ca   = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L1ca   = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L1p    = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L1p    = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%SNR(mx) &
        - ROdata%Lev1a%Range%SNR(mn) )        * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%SNR_L2p    = ROdata%Lev1a%Range%SNR(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%SNR_L2p    = ROdata%Lev1a%Range%SNR(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%Phase(mx) &
        - ROdata%Lev1a%Range%Phase(mn) )      * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Phase_L1   = ROdata%Lev1a%Range%Phase(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Phase_L1   = ROdata%Lev1a%Range%Phase(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%Phase(mx) &
        - ROdata%Lev1a%Range%Phase(mn) )      * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%Phase_L2   = ROdata%Lev1a%Range%Phase(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%Phase_L2   = ROdata%Lev1a%Range%Phase(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,1) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,1) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,2) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,2) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_GNS(mx) &
        - ROdata%Lev1a%Range%r_GNS(mn) )      * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_GNS(:,3) = ROdata%Lev1a%Range%r_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_GNS(:,3) = ROdata%Lev1a%Range%r_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,10)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,1) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,1) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,2) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,2) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_GNS(mx) &
        - ROdata%Lev1a%Range%v_GNS(mn) )      * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_GNS(:,3) = ROdata%Lev1a%Range%v_GNS(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_GNS(:,3) = ROdata%Lev1a%Range%v_GNS(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,1) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,1) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,4)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,2) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,2) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%r_LEO(mx) &
        - ROdata%Lev1a%Range%r_LEO(mn) )      * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%r_LEO(:,3) = ROdata%Lev1a%Range%r_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%r_LEO(:,3) = ROdata%Lev1a%Range%r_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,6)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,1) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,1) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,2) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,2) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev1a%Range%v_LEO(mx) &
        - ROdata%Lev1a%Range%v_LEO(mn) )      * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1a%v_LEO(:,3) = ROdata%Lev1a%Range%v_LEO(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev1a%v_LEO(:,3) = ROdata%Lev1a%Range%v_LEO(mx)      + val(1:np) + 1.0
  ENDWHERE

!----------------------------------------------------------------
! 5. Level 1b profile parameters
!    - Latitudes, Longitudes & Azimuths set to GeoRef values
!    - Impact parameters increasing within valid ranges
!    - Bending Angle values randomly invalid
!    - Sigmas and Quality left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf(:,:) = randf(:,:)*0.98_dp
  np = ROdata%Lev1b%Npoints

  ROdata%Lev1b%Lat_tp     = ROdata%GeoRef%Lat
  ROdata%Lev1b%Lon_tp     = ROdata%GeoRef%Lon
  ROdata%Lev1b%Azimuth_tp = ROdata%GeoRef%Azimuth

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev1b%Npoints

  strt =   ROdata%Lev1b%Range%Impact(mn)
  incr = ( ROdata%Lev1b%Range%Impact(mx) &
         - ROdata%Lev1b%Range%Impact(mn) ) / (np+2)
  DO i = 1, np
    ROdata%Lev1b%Impact(i)     = strt + incr * i
    ROdata%Lev1b%Impact_L1(i)  = ROdata%Lev1b%Impact(i) + randf(1,4) * 10.0
    ROdata%Lev1b%Impact_L2(i)  = ROdata%Lev1b%Impact(i) + randf(1,5) * 10.0
    ROdata%Lev1b%Impact_Opt(i) = ROdata%Lev1b%Impact(i) + randf(1,5) * 10.0
  END DO

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,8)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L1           = ROdata%Lev1b%Range%Bangle(mn)     + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L1           = ROdata%Lev1b%Range%Bangle(mx)     + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,9)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_L2           = ROdata%Lev1b%Range%Bangle(mn)    + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_L2           = ROdata%Lev1b%Range%Bangle(mx)    + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,10)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle           = ROdata%Lev1b%Range%Bangle(mn)       + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle           = ROdata%Lev1b%Range%Bangle(mx)       + val(1:np) + 0.001
  ENDWHERE

  val = ( ROdata%Lev1b%Range%Bangle(mx) &
        - ROdata%Lev1b%Range%Bangle(mn) )       * randf(1:np,1)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev1b%Bangle_Opt       = ROdata%Lev1b%Range%Bangle(mn)       + val(1:np) - 0.001
  ELSEWHERE
    ROdata%Lev1b%Bangle_Opt       = ROdata%Lev1b%Range%Bangle(mx)       + val(1:np) + 0.001
  ENDWHERE

!----------------------------------------------------------------
! 6. Level 2a profile parameters
!    - Geopotential and Altitudes increasing within valid ranges
!    - Refrativity values randomly invalid
!    - Sigmas and Quality left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev2a%Npoints

  strt = ROdata%GeoRef%Undulation
  incr = ( 80000.0 + strt ) / (np+2)
  DO i = 1, np
    ROdata%Lev2a%Alt_Refrac(i)  = strt + incr * i
    ROdata%Lev2a%Geop_Refrac(i) = ROdata%Lev2a%Alt_Refrac(i) * 1.01 &
                                - ROdata%GeoRef%Undulation
  END DO

  val = ( ROdata%Lev2a%Range%Refrac(mx) &
        - ROdata%Lev2a%Range%Refrac(mn) )       * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2a%Refrac       = ROdata%Lev2a%Range%Refrac(mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2a%Refrac       = ROdata%Lev2a%Range%Refrac(mx)      + val(1:np) + 1.0
  ENDWHERE

!----------------------------------------------------------------
! 7. Level 2b profile parameters
!    - Geopotential increasing within valid range
!    - P,T,q values randomly invalid
!    - Sigmas and Quality left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5      ! +/- 0.5
  np    = ROdata%Lev2b%Npoints

  strt =   0.0
  incr = ( 80000.0 - strt ) / (np+2)
  DO i = 1, np
    ROdata%Lev2b%Geop(i) = strt + incr * i
  END DO

  val = ( ROdata%Lev2b%Range%Press (mx) &
        - ROdata%Lev2b%Range%Press (mn) )      * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Press       = ROdata%Lev2b%Range%Press (mn)      + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Press       = ROdata%Lev2b%Range%Press (mx)      + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%Temp(mx) &
        - ROdata%Lev2b%Range%Temp(mn) )        * randf(1:np,5)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%Temp        = ROdata%Lev2b%Range%Temp(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%Temp        = ROdata%Lev2b%Range%Temp(mx)        + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2b%Range%SHum(mx) &
        - ROdata%Lev2b%Range%SHum(mn) )        * randf(1:np,7)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2b%SHum        = ROdata%Lev2b%Range%SHum(mn)        + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2b%SHum        = ROdata%Lev2b%Range%SHum(mx)        + val(1:np) + 1.0
  ENDWHERE

!----------------------------------------------------------------
! 8. Level 2c parameters
!    - Geopotential a random valid value
!    - P value randomly invalid
!    - Sigma, Quality and all TPH fields left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  randf = randf - 0.5

  v = ( ROdata%Lev2c%Range%Geop_Sfc(mx) &
      - ROdata%Lev2c%Range%Geop_Sfc(mn) )          * randf(1,1)
  ROdata%Lev2c%Geop_Sfc          = ROdata%Lev2c%Range%Geop_Sfc(mn) + ABS(v)

  v = ( ROdata%Lev2c%Range%Press_Sfc(mx) &
      - ROdata%Lev2c%Range%Press_Sfc(mn) )         * randf(2,1)
  IF ( v < 0.0 ) THEN
    ROdata%Lev2c%Press_Sfc       = ROdata%Lev2c%Range%Press_Sfc(mn)       + v - 1.0
  ELSE
    ROdata%Lev2c%Press_Sfc       = ROdata%Lev2c%Range%Press_Sfc(mx)       + v + 1.0
  END IF

!----------------------------------------------------------------
! 9. Level 2d parameters
!    All values randomly invalid of left missing
!----------------------------------------------------------------

  CALL RANDOM_NUMBER ( randf )
  np = ROdata%Lev2d%Npoints
  randf = randf - 0.5

  val = ( ROdata%Lev2d%Range%Level_Coeff_A(mx) &
        - ROdata%Lev2d%Range%Level_Coeff_A(mn) ) * randf(1:np,2)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2d%Level_Coeff_A = ROdata%Lev2d%Range%Level_Coeff_A(mn)   + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2d%Level_Coeff_A = ROdata%Lev2d%Range%Level_Coeff_A(mx)   + val(1:np) + 1.0
  ENDWHERE

  val = ( ROdata%Lev2d%Range%Level_Coeff_B(mx) &
        - ROdata%Lev2d%Range%Level_Coeff_B(mn) ) * randf(1:np,3)
  WHERE ( val(1:np) < 0.0 )
    ROdata%Lev2d%Level_Coeff_B = ROdata%Lev2d%Range%Level_Coeff_B(mn)   + val(1:np) - 1.0
  ELSEWHERE
    ROdata%Lev2d%Level_Coeff_B = ROdata%Lev2d%Range%Level_Coeff_B(mx)   + val(1:np) + 1.0
  ENDWHERE

!----------------------------------------------------------------
! 10. Tidy up
!----------------------------------------------------------------

  DEALLOCATE ( randf, ic )

END SUBROUTINE test2ropp_badprof

!--------------------------------------------------------------------
  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Generate non-scientific test data in ROPP netCDF format'
    PRINT *, 'Usage:'
    PRINT *, '  > test2ropp <test> [-o opfile] [-n nsamp] [-d] [-h] [-v]'
    PRINT *, ' where <test> is one of the following:'
    PRINT *, '   MISSING: all data is set to default missing data flag values'
    PRINT *, '   VALID  : data is set to random valid, in-range, values'
    PRINT *, '   INVALID: data is set to random invalid, out-of-range values'
    PRINT *, '   BADPROF: coordinate (time, height) data is set to increasing valid'
    PRINT *, '            values, but observed parameters are set to random invalid,'
    PRINT *, '            out-of-range values. Headers are randomly valid, sigmas &'
    PRINT *, '            quality values are set missing'
    PRINT *, '   MISPROF: As BADPROF except that all observed profile parameters are'
    PRINT *, '            set to default missing data flag values.'
    PRINT *, 'Options:'
    PRINT *, '   -o specifies an output file name'
    PRINT *, '   -n sets the number of vertical samples in the profile'
    PRINT *, '   -d outputs a sampled dump of the profile header(s)'
    PRINT *, '      and other diagnostics to stdout'
    PRINT *, '   -h this help'
    PRINT *, '   -v version information'
    PRINT *, 'Defaults:'
    PRINT *, '  Test type        : VALID'
    PRINT *, '  No. of samples   : 247'
    PRINT *, '  Output file name : from occultation ID'
    PRINT *, 'See test2ropp(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage

!-------------------------------------------------------------------------------
! 17. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'test2ropp - generate (non-scientific) test data'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

!-------------------------------------------------------------------------------
END PROGRAM test2ropp

