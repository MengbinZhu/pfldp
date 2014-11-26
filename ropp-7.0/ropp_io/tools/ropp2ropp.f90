! $Id: ropp2ropp.f90 3831 2013-08-27 15:22:48Z idculv $

PROGRAM ropp2ropp

!****x* Programs/ropp2ropp *
!
! NAME
!   ropp2ropp
!
! SYNOPSIS
!   Copy/rename/range-check/reformat/thin/reorder a ROPP file
!
!   > ropp2ropp ip_file [ip_file...] [-o op_file]
!                       [-p lev_file] [-t 2d_bg_file]
!                       [-i] [-u] [-m] [-a] [-l] [-d] [-h|?] [-v]
!
! ARGUMENTS
!  ip_file - one or more ROPP netCDF files.
!            In the special case of an input file name 'DUMMY', ropp2ropp
!            will internally generate and output a dummy ROPP file
!            filled with missing data flag values.
!
! OPTIONS
!   -o specifies an output file name - mandatory argument if used
!      (default: name generated from the occultation ID)
!   -p specifies a sampling levels control file for profile thinning
!      or a non-negative integer representing the maximum no. of levels
!      for implied SAMPLE mode (default: '0' = no thinning)
!   -t read two-dimensional background data
!   -u 'unordered' - disables the default re-ordering of output
!      profiles to ascending.
!      NB: using -u, profiles thinned using one of the interpolation methods
!      will retain the order of the fixed levels in the control file; other
!      methods will retain the ordering of the input profiles.
!   -i thins on impact altitudes (IP - RoC - undulation)
!   -m causes the output into a multifile (i.e., a single netCDF file
!      holding multiple profiles).
!   -a appends to an already existing file (default is to
!      overwrite an already existing file) (-a implies -m)
!   -l outputs a brief contents listing (OccID & lat/lon) only
!      to stdout - suppresses ROPP file o/p
!   -d outputs a sampled dump of the file header to stdout
!      plus other diagnostics
!   -h help
!   -v version information
!
! DESCRIPTION
!   ropp2ropp is a general-purpose ROPP file conversion tool with
!   range-checking and thinning facility.
!   It can 'copy/rename' one netCDF ROPP file to another.
!   Multiple input files can be processed either by explicitly specifying
!   more than one file name on the command line and/or by wildcards.
!   Existing multifiles can be split into single files. Profiles are by
!   default ordered to be in ascending heights but this can be disabled.
!   Profiles can optionally be thinned.
!
! MODULES
!   ropp_utils
!   ropp_io_types
!   ropp_io
!   ncdf
!   DateTimeProgs
!   DateTimeTypes
!   messages
!
! CALLS
!   Usage
!   ropp_io_ascend
!   ropp_io_nrec
!   ropp_io_read
!   ropp_io_write
!   ropp_io_occid
!   ropp_io_thin
!   ropp_io_free
!   ropp_io_rangecheck
!   ropp_io_version
!   MonthOfYear
!   File_Delete
!   message
!   message_set_routine
!   To_Lower
!
! REFERENCES
!   1. ROPP User Guide - Part I
!      SAF/ROM/METO/UG/ROPP/002
!   2. ROPP Thinner Algorithm
!      SAF/GRAS/METO/REP/GSR/008
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

  USE messages
  USE ropp_utils,    ONLY: File_Delete,   &
                           To_Lower,      &
                           ropp_MDTV
  USE ropp_io_types, ONLY: ROprof,        &
                           ROprof2d
  USE ropp_io,       ONLY: ropp_io_nrec,  &
                           ropp_io_read,  &
                           ropp_io_write, &
                           ropp_io_occid, &
                           ropp_io_free,  &
                           ropp_io_rangecheck, &
                           ropp_io_thin,  &
                           ropp_io_version
  USE ncdf,          ONLY: ncdf_isatt,    &
                           is_netcdf
  USE DateTimeProgs, ONLY: MonthOfYear
  USE DateTimeTypes

  IMPLICIT NONE

  CHARACTER (LEN=*), PARAMETER :: chfmt  = "(1X,2A)"
  CHARACTER (LEN=*), PARAMETER :: gefmt1 = "(A,F10.3,2F8.1)"
  CHARACTER (LEN=*), PARAMETER :: gefmt2 = "(A,F10.1,5F8.1)"
  CHARACTER (LEN=*), PARAMETER :: bgfmt  = &
                      "(2A,2X,I2.2,':',I2.2,1X,I2.2,'-',A,'-',I4)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                      "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt2 = &
                      "(A,2(I2.2,':'),I2.2,'.',I3.3,1X,I2.2,'-',A3,'-',I4.4)"

  TYPE(ROprof)   :: ROdata
  TYPE(ROprof2d) :: ROdata2d

  CHARACTER (LEN=256), ALLOCATABLE :: ipfile(:)
  CHARACTER (LEN=256)              :: opfile, thfile, arg
  CHARACTER (LEN=64)               :: pcd
  CHARACTER (LEN=80)               :: outstr
  CHARACTER (LEN=10)               :: MonthName

  INTEGER :: i, ierr, narg, iarg, k, istatus
  INTEGER :: nprf, nipf, Nprof, nptot=0
  REAL    :: lat, lon, toff, roc, coc(3), azim, undu

  LOGICAL :: multi     = .FALSE.
  LOGICAL :: newfile   = .TRUE.
  LOGICAL :: first     = .TRUE.
  LOGICAL :: list      = .FALSE.
  LOGICAL :: unordered = .FALSE.
  LOGICAL :: twod      = .FALSE.
  LOGICAL :: impactalt = .FALSE.
  LOGICAL :: ranchk    = .TRUE.
  LOGICAL :: zapem     = .TRUE.
  LOGICAL :: exists

! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

!--------------------------------------------------------------
!  1. Initalise
!--------------------------------------------------------------

  CALL message_set_routine ( "ropp2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                ROPP-to-ROPP generic netCDF tool'                     )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!--------------------------------------------------------------
! 2. Parse command line
!--------------------------------------------------------------

  narg = IARGC()
  ALLOCATE ( ipfile(narg), STAT=istatus )
  IF ( istatus /= 0 ) THEN
    CALL message ( msg_fatal, "Failed to allocate memory for ipfile array" )
  END IF
  nipf = 0

  ipfile = " "
  opfile = " "
  thfile = "0"

  nipf = 1
  iarg = 1
  DO WHILE ( iarg <= narg )
    CALL GETARG ( iarg, arg )
    SELECT CASE (arg)
      CASE ("-a","-A")
        newfile = .FALSE.
        multi   = .TRUE.

      CASE ("-d","-D")
        msg_MODE = VerboseMode

      CASE ("-h","-H","--help","?")
        narg = -1

      CASE ("-i")
        impactalt = .TRUE.

      CASE ("-l","-L")
        list = .TRUE.

      CASE ("-m","-M")
        multi = .TRUE.

      CASE ("-o","-O")
        CALL GETARG ( iarg+1, arg )
        opfile = arg
        iarg   = iarg + 1

      CASE ("-p","-P")
        CALL GETARG ( iarg+1, arg )
        thfile = arg
        iarg   = iarg + 1

      CASE ("-u","-U")
        unordered = .TRUE.

      CASE ("-t","-T")
        twod = .TRUE.

      CASE ("-v","-V","--version")
        CALL version_info()
        CALL EXIT(0)

      CASE ("--no-ranchk") ! skip range checking; for testing of Q/C only,
        CALL message ( msg_warn, "Range checking is disabled" )
        ranchk = .FALSE.   ! do not document as a user option.

      CASE ("--no-zapem")  ! don't 'Zero if All Profile Elements Missing'
        CALL message ( msg_warn, "Zeroing of empty profiles disabled" )
        zapem = .FALSE.    ! do not document as a user option.

      CASE ("--both")      ! no-ranchk and no-zapem
        CALL message ( msg_warn, "Range checking is disabled" )
        ranchk = .FALSE.   ! do not document as a user option.
        CALL message ( msg_warn, "Zeroing of empty profiles disabled" )
        zapem = .FALSE.    ! do not document as a user option.

      CASE DEFAULT
         IF ( arg(1:1) /= "-" ) THEN
            ipfile(nipf) = arg
            nipf         = nipf + 1
         ELSE
           CALL message (msg_fatal, "Unrecognized argument '"//&
                                    TRIM(arg)//"'" )
         END IF
    END SELECT
    iarg = iarg + 1
  END DO

  nipf = nipf - 1

  IF ( nipf == 0 .AND. narg /= -1 ) THEN
    CALL message ( msg_error, "No input file(s) specified" )
    narg = 0
  END IF

  IF ( narg <= 0 ) THEN
    CALL Usage()
    CALL EXIT(0)
  END IF

!-------------------------------------------------------------
! 2. Loop over all input files
!-------------------------------------------------------------

  DO k = 1, nipf

    IF ( ipfile(k) /= "DUMMY" ) THEN
      INQUIRE ( FILE=ipfile(k), EXIST=exists )
      IF ( .NOT. exists ) THEN
        CALL message ( msg_error, "ROPP input file  "// &
                                  TRIM(ipfile(k))//" not found" )
        CYCLE
      END IF

!-------------------------------------------------------------
! 2.1 Read no. of records (profiles) in this file
!-------------------------------------------------------------

      CALL message ( msg_info, "Reading "//TRIM(ipfile(k)) )

      IF ( is_netcdf(ipfile(k)) ) THEN
        IF ( ncdf_isatt('title', ncfile=ipfile(k)) ) THEN
          nprf = ropp_io_nrec ( ipfile(k) )
          IF ( nprf < 0 ) nprf = 1
        ELSE
          CALL message ( msg_fatal, "ROPP netCDF input file not detected "// &
             " (missing 'title' attribute).\n"// &
             "Only ROPP netCDF input is supported by this tool - "// &
             "check input file type.\n"// &
             "Use conversion tools to convert to ROPP netCDF if required." )
        ENDIF
      ELSE
        CALL message ( msg_fatal, "netCDF input file not detected.\n"// &
           "Only ROPP netCDF input is supported by this tool - "// &
           "check input file type.\n"//  &
           "Use conversion tools to convert to ROPP netCDF if required." )
      ENDIF

    ELSE
      nprf = 1

    END IF

!-------------------------------------------------------------
! 2.2 Loop over all profiles in this input file
!-------------------------------------------------------------

    DO i = 1, nprf

      IF ( ipfile(k) /= "DUMMY" ) THEN
        IF ( twod ) THEN
          CALL ropp_io_read ( ROdata2d,       &
                              file=ipfile(k), &
                              rec=i,          &
                              ranchk=ranchk,  &
                              ierr=ierr )

        ELSE
          CALL ropp_io_read ( ROdata,         &
                              file=ipfile(k), &
                              rec=i,          &
                              ranchk=ranchk,  &
                              ierr=ierr )
        ENDIF
        IF ( ierr /= 0 ) THEN
          WRITE ( outstr, FMT="(I3)" ) ierr
          CALL message ( msg_fatal, "I/O error - code "//TRIM(ADJUSTL(outstr)) )
        END IF
      END IF

!-------------------------------------------------------------
! 2.2.1 Thin profile (optional);
!       range check (optional but strongly recommended);
!       remove missing profiles (optional)
!-------------------------------------------------------------

      IF ( .NOT. list ) THEN
        IF ( .NOT. twod ) THEN
          CALL ropp_io_thin ( ROdata,              &
                              thfile,              &
                              impactalt=impactalt, &
                              ranchk=ranchk )
        END IF
      ELSE IF ( ranchk ) THEN
        CALL ropp_io_rangecheck ( ROdata )
      END IF
      
      IF ( zapem ) THEN
        IF ( ROdata%Lev1a%Missing ) ROdata%Lev1a%Npoints = 0
        IF ( ROdata%Lev1b%Missing ) ROdata%Lev1b%Npoints = 0
        IF ( ROdata%Lev2a%Missing ) ROdata%Lev2a%Npoints = 0
        IF ( ROdata%Lev2b%Missing ) ROdata%Lev2b%Npoints = 0
        IF ( ROdata%Lev2c%Missing ) ROdata%Lev2c%Npoints = 0
        IF ( ROdata%Lev2d%Missing ) ROdata%Lev2d%Npoints = 0
      END IF

!-------------------------------------------------------------
! 2.2.3 Ensure output profiles are in ascending order (optional)
!-------------------------------------------------------------

      IF ( .NOT. list .AND. .NOT. unordered ) THEN
        CALL message ( msg_diag, "Ensuring all profiles are in "//&
                                 "ascending height order" )
        CALL ropp_io_ascend ( ROdata )
      ELSE
        CALL message ( msg_diag, "Leaving all profiles in original "// &
                                 "height order")
      END IF

!-------------------------------------------------------------
! 2.2.4 Check/create output file name; explcitly delete any
!       existing potential multifile file (unless appending)
!-------------------------------------------------------------

      IF ( .NOT. twod ) CALL ropp_io_occid ( ROdata )
      IF ( opfile == " " ) THEN
        opfile = TRIM(ROdata%occ_id)//".nc"
        CALL To_Lower ( opfile )
        first = .TRUE.
      END IF

      IF ( first ) THEN
        INQUIRE ( FILE=opfile, EXIST=exists )
        IF ( exists ) THEN
          IF ( newfile .AND. multi ) THEN
            CALL file_delete ( opfile, ierr )
          ELSE
            Nprof = ropp_io_nrec ( opfile )
            IF ( Nprof < 0 ) Nprof = 1
          END IF
        ELSE
          newfile = .TRUE.
        END IF
        first = .FALSE.
      END IF

!-------------------------------------------------------------
! 2.2.5 Optional dump/list of some header parameters
!-------------------------------------------------------------

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

      IF ( list ) THEN
        WRITE ( outstr, FMT="(A,I5,A,2X,F5.1,',',F6.1)" ) &
                        "Profile", i, " : "//TRIM(ROdata%occ_id), lat, lon

      ELSE
        WRITE ( outstr, FMT="(A,I5,A)" ) &
                        "Profile", i, " : "//TRIM(ROdata%occ_id)
      END IF
      CALL message ( msg_info, TRIM(outstr) )

      CALL message ( msg_diag, "FmtVer:  "//TRIM(ROdata%FmtVersion) )
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
      WRITE ( outstr, FMT=DTfmt2 ) "DTocc :  ",         &
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
      WRITE ( outstr, FMT=DTfmt2 ) "DTpro :  ",         &
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

      CALL message ( msg_diag, "Levels:    L1a  L1b  L2a  L2b  L2c  L2d" )
      WRITE ( outstr, FMT="(A,6I5)" ) "Npts  :  ",    &
                                ROdata%lev1a%Npoints, &
                                ROdata%lev1b%Npoints, &
                                ROdata%lev2a%Npoints, &
                                ROdata%lev2b%Npoints, &
                                ROdata%lev2c%Npoints, &
                                ROdata%lev2d%Npoints
      CALL message ( msg_diag, TRIM(outstr) )
      WRITE ( outstr, FMT="(A,6L5)" ) "Missng:  ",    &
                                ROdata%lev1a%Missing, &
                                ROdata%lev1b%Missing, &
                                ROdata%lev2a%Missing, &
                                ROdata%lev2b%Missing, &
                                ROdata%lev2c%Missing, &
                                ROdata%lev2d%Missing
      CALL message ( msg_diag, TRIM(outstr) )


!-------------------------------------------------------------
! 2.2.6 Write output file (unless just listing)
!-------------------------------------------------------------

      IF ( .NOT. list ) THEN
        CALL message ( msg_info, "Writing "//TRIM(opfile) )
        IF ( twod ) THEN
          CALL ropp_io_write ( ROdata2d,                 &
                               file=opfile,              &
                               append=multi,             &
                               output_precision='float', &
                               ranchk=ranchk )
        ELSE
          CALL ropp_io_write ( ROdata,                   &
                               file=opfile,              &
                               append=multi,             &
                               output_precision='float', &
                               ranchk=ranchk )
        ENDIF
        IF ( .NOT. multi ) opfile = " "
      END IF

!--------------------------------------------------------------
! 2.2.7 Free memory ready for next profile
!--------------------------------------------------------------

    CALL ropp_io_free ( ROdata )
    CALL ropp_io_free ( ROdata2d )

    END DO          ! end profile loop

    nptot = nptot + nprf
    IF ( list ) THEN
      IF ( nprf == 1 ) THEN
        outstr = "1 profile"
      ELSE
        WRITE ( outstr, FMT="(I5,A)" ) nprf, " profiles"
      END IF
      CALL message ( msg_info, TRIM(ADJUSTL(outstr))//" in "//TRIM(ipfile(k)) )
    ENDIF

  END DO            ! end file loop

!--------------------------------------------------------------
! 3. Show summary of outcome
!--------------------------------------------------------------

  IF ( nptot > 1 .AND. .NOT. list ) THEN
    WRITE ( outstr, FMT="(I6)" ) nptot
    CALL message ( msg_info, TRIM(outstr)//" profiles processed" )

    IF ( .NOT. newfile ) THEN
       nprf = ropp_io_nrec ( opfile )
       WRITE ( outstr, FMT="(I6)" ) nprf
       CALL message ( msg_info, TRIM(outstr)//" profiles now in "// &
                                TRIM(opfile) )
    END IF
  END IF

!--------------------------------------------------------------
! 4. Tidy up
!--------------------------------------------------------------

  IF ( ALLOCATED(ipfile) ) DEALLOCATE ( ipfile )

!--------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------

  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Copy/rename/reformat/range-check/thin/order a ROPP file'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp2ropp ip_file [ip_file...] [-o op_file] '
    PRINT *, '                      [-p thin_file|maxsamp] [-t 2d_bg_file]'
    PRINT *, '                      [-u] [-i] [-m] [-a] [-d] [-l] [-h] [-v]'
    PRINT *, ' where ip_file(s) are ROPP netCDF file(s)'
    PRINT *, 'Options:'
    PRINT *, '  -o specifies an output file name'
    PRINT *, '  -p specifies a thinning control file name or max. no. samples'
    PRINT *, '  -t read two-dimensional input background data'
    PRINT *, '     Other input options are not supported for 2D bg data.'
    PRINT *, '  -u disables the default height re-ordering of profiles to'
    PRINT *, '      ascending if the input profiles are descending'
    PRINT *, '  -i thins on impact altitudes (IP - RoC - undulation)'
    PRINT *, '  -m forces the output into a multifile (i.e. a single netCDF file'
    PRINT *, '     holding multiple profiles)'
    PRINT *, '  -a appends to an already existing file. -a implies -m'
    PRINT *, '  -l lists input profile(s) occultation ID & lat/lon to stdout'
    PRINT *, '     only - does not produce any profile output file(s).'
    PRINT *, '  -d outputs a sampled dump of the profile header(s) and other'
    PRINT *, '     diagnostics to stdout'
    PRINT *, '  -h this help'
    PRINT *, '  -v version information'
    PRINT *, '  --no-ranchk disable range-checking (not recommended)'
    PRINT *, '  --no-zapem  disable zeroing of empty profiles (not recommended)'
    PRINT *, '  --both: --no-ranchk and --no-zapem (not recommended)'
    PRINT *, 'Defaults:'
    PRINT *, '  Input  file name : required'
    PRINT *, '  Output file name : from (first) occultation ID'
    PRINT *, '  Output mode      : one output file per input profile'
    PRINT *, '  Append mode      : existing file is overwritten'
    PRINT *, '  Thinning         : none'
    PRINT *, '  Re-ordering      : descending profiles re-ordered to ascending'
    PRINT *, '  Thining on alts  : off'
    PRINT *, 'See ropp2ropp(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage

!-------------------------------------------------------------------------------
! 17. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'ropp2ropp - generic ROPP netCDF conversion tool'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp2ropp
