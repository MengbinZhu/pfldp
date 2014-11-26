! $Id: ropp2bufr_mo.f90 2738 2011-03-22 15:34:18Z frdo $

PROGRAM ropp2bufr_mo

!****x* Programs/ropp2bufr_mo *
!
! NAME
!   ropp2bufr    (ropp2bufr_mo.f90)
!
! SYNOPSIS
!   Encode an ROPP file to WMO FM-94 (BUFR) [MetDB library]
!
!   > export BUFR_LIBRARY=bufr_directory_path
!   > ropp2bufr ropp_file [ropp_file...] [-o bufr_file]
!                         [-g[i]] [-s csn_file]
!                         [-p thin_file] [-t time]
!                         [-u] [-l] [-m] [-h|?] [-v] [-d]
! ARGUMENTS
!   ropp_file - one or more ROPP netCDF files.
!
! OPTIONS
!   Option switches can be in any order and are case-insensitive;
!   any space(s) between a switch and its (madatory) argument is
!   optional.
!     -o  specifies the BUFR output file name
!     -g  specifies that GTS routing headers/trailers are required
!     -gi speciifes that GTS headers include a 10-byte leading size/type
!         (required for some TCP/IP (FTP) implimentations for GTS)
!     -s  specifies a channel sequence number file
!     -p  specifies a thinning control file or max. no. of levels
!     -t  specifies a time (age) rejection threshold
!     -u  leave profiles unordered - disables the default re-ordering
!         of output profiles to ascending.
!         NB: using -u, profiles thinned using one of the interpolation methods
!         will retain the order of the fixed levels in the control file; other
!         methods will retain the ordering of the input profiles.
!     -l  do not encode L1+L2 data (bending angle, Level 1b), if present
!     -m  do not encode met.  data (geophysical, Level 2b,c), if present
!     -d  to output additional diagnostics
!     -h  help
!     -v  version information
!   Defaults
!     Input file name         : ropp.nc
!     Output file name        : <occid>.bufr
!     GTS routing headers     : not generated
!     Channel sequence        : initialised at 001
!     Time threshold          : 00:00 (no cut-off) unless one of
!                               -g options present, when 23:50
!     Encode                  : all available Level 1b, 2b & 2c data
!     Thinning                : sample to no more than 375 levels
!
! INPUTS
!   ropp_file is the input file(s) which must be in ROPP netCDF
!             format (See Ref.1)
!
! OUTPUTS
!   bufr_file is the output file, which will contain one encoded
!             BUFR message per input profile (See Ref.2)
!             The output file name is optional, and if not specified,
!             is generated from the occulation ID.
!
! CALLS
!   IARGC
!   ConvertROPPtoBUFR
!   EncodeBUFR
!   GetOptions
!   ropp_io_ascend
!   ropp_io_occid
!   ropp_io_read
!   ropp_io_thin
!   ropp_io_free
!   ropp_io_version
!   ConvertDescriptor
!   METDB_COPEN
!   METDB_CWRITE
!   METDB_CCLOSE
!   GTShdrCSN
!   GTShdrIPH
!   CalToJul
!   DateTimeOffset
!   To_Lower
!   File_Delete
!   message
!   message_set_routine
!
! MODULES
!   ropp2bufr           - fixed parameter definitions & common routines
!   ropp_io             - ROPP I/O file support
!   ropp_io_types       - ROPP data type definitions
!   DateTimeProgs       - Date & Time conversion routines
!   DateTimeTypes       - Date & Time conversion definitions
!   BUFRutils           - BUFR utility routines
!   messages            - ROPP message library
!   GTShdrs             - routines to add WMO/GTS routing header/trailer
!
! DEPENDENCIES
!   MetDB BUFR package  - BUFR kernel routines
!   ROPP I/O library    - ROPP file I/O support
!   ROPP Utils library  - ROPP utility routines
!   netCDF library      - netCDF file support
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY        - path for run-time files
!
! ERRORS
!   Program (shell) return codes:
!     0 = OK
!    -1 = Occultation rejected as too old for GTS
!     1 = I/O error
!     2 = Memory allocation failure
!
! DESCRIPTION
!   A BUFR encoder for Radio Occultation data.
!   Reads from one or more ROPP netCDF files and encodes data therein
!   to one BUFR message per profile output to a single BUR file.
!   Various options are provided to control the generation of GTS
!   routing headers and rejection based on the age of the data
!   and to skip encoding certain profile subsets and thinning (see Refs.3,4).
!   BUFR tables and other run-time files are found via the environment
!   variable 'BUFR_LIBRARY'.
!
! REFERENCES
!   1) ROPP User Guide - Part I
!      SAF/ROM/METO/UG/ROPP/002
!   2) WMO FM94 (BUFR) Specification for ROM SAF Processed Radio
!      Occultation Data.
!      SAF/ROM/METO/FMT/BUFR/001
!   3) Monodimensional data thinning for GPS radio occultations
!      SAF/GRAS/METO/ALG/ROPP/001
!   4) ROPP thinner algorithm
!      SAF/GRAS/METO/REP/GSR/8
!
! SEE ALSO
!   ropp2bufr(1), bufr2ropp(1)
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
  USE ropp2bufr
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_nrec,   &
                           ropp_io_occid,  &
                           ropp_io_read,   &
                           ropp_io_thin,   &
                           ropp_io_free
  USE ropp_utils,    ONLY: File_Delete,    &
                           Get_IO_Unit
  USE DateTimeProgs, ONLY: DateTimeOffset, &
                           MonthOfYear,    &
                           TimeSince
  USE DateTimeTypes, ONLY: IdxYear, IdxMonth, IdxDay, &
                           IdxHour, IdxMinute
  USE GTShdrs,       ONLY: GTShdrCSN, &
                           GTShdrIPH
  USE BUFRutils,     ONLY: ConvertDescriptor

  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                     "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"
  CHARACTER (LEN=3), PARAMETER :: Month(0:12) = &
                     (/ "???", "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" /)

! Local variables

  CHARACTER (LEN=256),  &
          DIMENSION(:), &
          ALLOCATABLE :: ROPPdsn        ! Name(s) of input ROPP file(s)
  CHARACTER (LEN=256) :: BUFRdsn        ! Output (BUFR) file name
  CHARACTER (LEN=256) :: CSNdsn         ! Channel sequence file name
  CHARACTER (LEN=256) :: Thindsn        ! Thinning control file name
  CHARACTER (LEN=4)   :: OrigICAO       ! Originating centre ICAO code
  INTEGER             :: BUFRunit       ! BUFR I/O unit (Fortran stream unit)
  INTEGER             :: nFiles         ! No. of file names on command line
  INTEGER             :: OrigCentre     ! Originating centre BUFR code
  INTEGER             :: SubCentre      ! Originating sub-centre BUFR code
  INTEGER             :: GTShdrType     ! Code for GTS header generation
  INTEGER             :: RejTimeDiff    ! reject obs older than this
  LOGICAL             :: CorrOnly       ! Flag for Corrected only
  LOGICAL             :: nomet          ! Flag for no met. data

  INTEGER,  ALLOCATABLE :: Descr(:)     ! Descriptor sequence
  INTEGER,  ALLOCATABLE :: RepFac(:)    ! Replication Factors
  REAL(dp), ALLOCATABLE :: Values(:)    ! Data values for BUFR (d.p.)
  REAL,     ALLOCATABLE :: Values1(:)   ! Data values for BUFR (s.p)

  CHARACTER (LEN=10) :: cIPH            ! Dummy IPH end-of-file sequence
  INTEGER :: LenBUF                     ! Length of BUFR message/bulletin
  INTEGER :: LenEOF                     ! Length of end-of-file message
  INTEGER :: LenIPH                     ! Length of IPH sequence
  INTEGER :: LenTot = 0                 ! Total length of all bulletins (BUFR
                                        ! messages + headers) written out to
                                        ! BUFR file
  INTEGER :: nDescr                     ! Number of descriptors in sequence
  INTEGER :: nElem                      ! No. of elements
  INTEGER :: nValues                    ! No. of elements after thinning
  INTEGER :: nRepFac                    ! No. of Replication Factors
  INTEGER :: minExtra = 2000            ! Min. expansion elements for Decsr
  INTEGER :: nExtra                     ! Extra expansion elements for Descr
  INTEGER :: nObs = 1                   ! No. of observations
  INTEGER :: CSN                        ! Channel sequence number
  INTEGER :: ierr, status               ! File error & return status codes
  INTEGER :: nmsg = 0                   ! Count of BUFR messages
  INTEGER :: nfreq  = 0                 ! No. of Level 1b frequencies
  INTEGER :: nvalid = 0                 ! No. of valid L1 b/angles
  INTEGER :: FXXYYY                     ! Descriptor in decimal format
  INTEGER :: F, X, Y                    ! Elements of a descriptor
  INTEGER :: i, in, iprof, ifile        ! Loop counters
  LOGICAL :: exists                     ! File present flag
  LOGICAL :: first = .TRUE.             ! First profile flag
  LOGICAL :: unordered                  ! Enable ordering of profiles to ascending
  CHARACTER (len=20)    :: resolution   ! (Resolution group of EUM netCDF 4 files)

  TYPE(ROprof)          :: ROdata       ! ROPP data structure
  INTEGER               :: nprofs       ! No. of profiles in i/p file
  INTEGER               :: tprofs = 0   ! Total profiles in all files

  INTEGER, DIMENSION(8) :: DT8, Offset  ! Date/time arrays
  CHARACTER (LEN=80)    :: outmsg       ! Output text string
  CHARACTER (LEN=10)    :: MonthName    ! Month name
  CHARACTER (LEN=10)    :: number       ! Numeric value as string
  REAL(dp)              :: MinRej       ! Rejection threshold (minutes since 00:00UT 1-Jan-2000)
  REAL(dp)              :: MinObs       ! Ob time (minutes since 00:00UT 1-Jan-2000)

  ! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

!--------------------------------------------------------------
!  1. Initalise
!--------------------------------------------------------------

  CALL message_set_routine ( "ropp2bufr" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                     ROPP to BUFR Encoder'                            )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

  BUFRunit = Get_IO_unit()

!--------------------------------------------------------------
! 2. Parse command line options
!--------------------------------------------------------------

  nFiles = MAX ( IARGC(), 1 )
  ALLOCATE ( ROPPdsn(nFiles) )
  CALL GetOptions ( ROPPdsn,     &
                    nFiles,      &
                    BUFRdsn,     &
                    CSNdsn,      &
                    Thindsn,     &
                    GTShdrType,  &
                    RejTimeDiff, &
                    CorrOnly,    &
                    nomet,       &
                    unordered,   &
                    resolution)

!--------------------------------------------------------------
! 3. If time rejection on, set time rejection threshold in
!    minutes since 00:00UT 1-Jan-2000 for specified period
!    back from 'now'.
!--------------------------------------------------------------

  IF ( RejTimeDiff > 0 ) THEN
    Offset = (/0,0,0,0,0,RejTimeDiff,0,0/)
    CALL DateTimeOffset ( DT8, "-", Offset )
    CALL TimeSince      ( DT8, MinRej, 1, Base="JM2000" )
    CALL MonthOfYear    ( DT8(IdxMonth), MonthName, 1 )
    WRITE ( outmsg, FMT=DTfmt1 ) DT8(IdxHour),   &
                                 DT8(IdxMinute), &
                                 DT8(IdxDay),    &
                                 MonthName(1:3), &
                                 DT8(IdxYear)
    CALL message ( msg_diag, " Rejecting occultations older than "// &
                             TRIM(outmsg) )
  ELSE
    MinRej = 0.0_dp
  END IF

!--------------------------------------------------------------
! 4. If GTS headers to be generated, read the last used
!    channel sequence number
!--------------------------------------------------------------

  IF ( GTShdrType /= NOhdrs ) &
    CALL GTShdrCSN ( CSNdsn,  &
                     CSN,     &
                     'Read' )

!--------------------------------------------------------------
! 5. Loop over input files
!--------------------------------------------------------------

  DO ifile = 1, nFiles

    INQUIRE ( FILE=ROPPdsn(ifile), EXIST=exists )
    IF ( .NOT. exists ) THEN
      CALL message ( msg_error, "ROPP input file  "// &
                                TRIM(ROPPdsn(ifile))//" not found" )
      CYCLE
    ENDIF

    CALL message ( msg_info, "Reading  ROPP data from "// &
                             TRIM(ROPPdsn(ifile)) )

    nprofs = ropp_io_nrec ( ROPPdsn(ifile) )
    IF ( nprofs < 0 ) nprofs = 1            ! assume 1 profile for text type
    tprofs = tprofs + nprofs

!--------------------------------------------------------------
! 6. Loop over occultations from current file
!    (If a read error, skip to next file)
!--------------------------------------------------------------

    DO iprof = 1, nprofs

      CALL ropp_io_read ( ROdata,          &
                          file=ROPPdsn(ifile), &
                          rec=iprof,       &
                          ierr=status )

      IF ( status /= 0 ) THEN
        CALL message ( msg_fatal, "Failed to read file" )
      END IF

!--------------------------------------------------------------
! 7. On first profile, open output file for BUFR (default name
!    from first occultation ID)
!--------------------------------------------------------------

      CALL ropp_io_occid ( ROdata )
      IF ( first ) THEN
        IF ( BUFRdsn == " " ) THEN
          BUFRdsn = TRIM(ROdata%Occ_id) // ".bufr"
          CALL To_Lower ( BUFRdsn )
        END IF

        CALL METDB_COPEN ( BUFRunit,      &
                           TRIM(BUFRdsn), &
                           Output,        &
                           ierr )
        IF ( ierr /= 0 ) THEN
          CALL message ( msg_error, "Failed to open BUFR output file "// &
                                    TRIM(BUFRdsn) )
          CALL EXIT(ErrIO)
        END IF
        first = .FALSE.
      END IF

      WRITE( number, FMT="(I4)") iprof
      CALL message ( msg_info, "Encoding profile "//TRIM(number)// &
                               " : "//TRIM(ROdata%Occ_ID) )

!--------------------------------------------------------------
! 7.1 If GTS time rejection on, skip if occultation time
!     is too old
!--------------------------------------------------------------

      IF ( MinRej > 0.5_dp ) THEN
        DT8 = (/ROdata%DTocc%Year,   ROdata%DTocc%Month,  &
                ROdata%DTocc%Day,    0,                   &
                ROdata%DTocc%Hour,   ROdata%DTocc%Minute, &
                ROdata%DTocc%Second, 0/)
        CALL TimeSince ( DT8, MinObs, 1, Base="JM2000" )
        IF ( MinObs < MinRej ) THEN
          CALL message ( msg_warn, "Occultation is too old for GTS "// &
                                   "- not encoded." )
          CYCLE
        END IF
      END IF

!--------------------------------------------------------------
! 7.2 Use (at most) one Level 1a data for nominal POD
!     Only encode Level 1b L1+L2 data if:
!     a) 'Corrected only' option not taken and
!     b) there is at least one valid L1 bending angle value present
!--------------------------------------------------------------

      ROdata%Lev1a%Npoints = MIN ( 1, ROdata%Lev1a%Npoints )

      IF ( .NOT. CorrOnly ) THEN
        nvalid = 0
        DO in = 1, ROdata%Lev1b%Npoints
          IF ( ROdata%Lev1b%BAngle_L1(in) > 0.0 .AND. &
               ROdata%Lev1b%BAngle_L1(in) < 0.082 ) THEN
            nvalid = nvalid + 1
          END IF
        END DO
        IF ( nvalid < 1 ) CorrOnly = .TRUE.
      END IF

      IF ( ROdata%Lev1b%Npoints > 0 ) THEN
        IF ( CorrOnly ) THEN
          nfreq = 1
        ELSE
          nfreq = 3
        END IF
      END IF

!--------------------------------------------------------------
! 7.3 Only encode 'Met' data if:
!     a) 'No Met' option not taken and
!     b) there is at least one valid temperature value present.
!     Level 2c (Surface) data is always encoded, but show as '0'
!     if not valid.
!     Ignore any level 2d data
!--------------------------------------------------------------

      nvalid = 0
      DO in = 1, ROdata%Lev2b%Npoints
        IF ( ROdata%Lev2b%Temp(in) > 150.0 .AND. &
             ROdata%Lev2b%Temp(in) < 350.0 ) THEN
          nvalid = nvalid + 1
        END IF
      END DO
      IF ( nvalid < 1 ) nomet = .TRUE.

      IF ( nomet ) THEN
        IF ( ROdata%Lev2b%Npoints > 0 ) THEN
          ROdata%Lev2b%Npoints = 0
          ROdata%Lev2c%Npoints = 0
        END IF
      END IF

      IF ( ROdata%Lev2c%Geop_Sfc < -1000.0 .OR. &
           ROdata%Lev2c%Geop_Sfc > 10000.0 )    &
           ROdata%Lev2c%Npoints = 0

      ROdata%Lev2d%Npoints = 0

! Skip this profile if no valid bending angles, refractivity,
! met. or surface met. present

      IF ( ROdata%Lev1b%Npoints <= 0 .AND. &
           ROdata%Lev2a%Npoints <= 0 .AND. &
           ROdata%Lev2b%Npoints <= 0 .AND. &
           ROdata%Lev2c%Npoints <= 0 ) THEN
        CALL message ( msg_warn, "No. of L1b,2a,2b,2c samples"// &
                                 " all zero - skipping this profile" )
        CYCLE
      END IF

!--------------------------------------------------------------
! 7.4 Thin BA, N & T,q,p profiles as required; ensure all
!     profiles to be encoded are in ascending height order
!--------------------------------------------------------------

      CALL ropp_io_thin ( ROdata, Thindsn )

      IF ( .NOT. unordered ) THEN
        CALL message ( msg_diag, "Ensuring all profiles are in "// &
                                 "ascending height order..." )
        CALL ropp_io_ascend ( ROdata )
      END IF

!--------------------------------------------------------------
! 7.5 Calculate total number of BUFR elements for this profile
!     and allocate working arrays for BUFR-interface data values
!--------------------------------------------------------------

! No. of BUFR elements expected

      nElem = 37                                            &  ! Header
            +  1 + ROdata%Lev1b%Npoints * ( 5 + nfreq * 6 ) &  ! Level 1b
            +  1 + ROdata%Lev2a%Npoints * 6                 &  ! Level 2a
            +  1 + ROdata%Lev2b%Npoints * 10                &  ! Level 2b
            +  7                                               ! Level 2c

      ALLOCATE ( Values(1:nElem), Values1(1:nElem), STAT=status )
      IF ( status /= 0 ) THEN
        CALL message ( msg_error, "Failed to allocate memory for "// &
                                  "Values array" )
        CALL EXIT(ErrMem)
      END IF
      Values(:)  = 0.0_dp
      Values1(:) = 0.0

! No. of BUFR Replication Factors expected
! (not used - for compatibility with ECMWF version)

      nRepFac = ROdata%Lev1b%Npoints*nfreq + 3
      ALLOCATE ( RepFac(1:nRepFac), STAT=status )
      IF ( status /= 0 ) THEN
        CALL message ( msg_error, "Failed to allocate memory for "// &
                                  "RepFac array" )
        CALL EXIT(ErrMem)
      END IF

!--------------------------------------------------------------
! 7.6 Convert RO data to BUFR array
!--------------------------------------------------------------

      CALL ConvertROPPtoBUFR ( ROdata,                          &
                               CorrOnly,                        &
                               OrigICAO, OrigCentre, SubCentre, &
                               Values,   nValues,               &
                               RepFac,   nRepFac )
      WHERE ( ABS(Values-RVIND) < 1.0_dp ) Values = RMDFV
      Values1(:) = REAL(Values(:))

!--------------------------------------------------------------
! 7.7 Allocate working array for descriptors, based on thinned
!     number of data values, and allowing headroom for expansion.
!--------------------------------------------------------------

      nExtra = MAX(minExtra,(nValues*5/10))
      ALLOCATE ( Descr(1:nValues+nExtra), STAT=status )
      IF ( status /= 0 ) THEN
        CALL message ( msg_error, "Failed to allocate memory for "// &
                                  "Descr array" )
        CALL EXIT(ErrMem)
      ELSE
        WRITE ( number, FMT="(I6)" ) nValues+nExtra
        CALL message ( msg_diag, "  Allocated Descriptor space   :"// &
                                 TRIM(number) )
      END IF
      Descr(:) = 0

!--------------------------------------------------------------
! 7.8 Diagnostics of what we're about to encode
!--------------------------------------------------------------

      CALL message ( msg_diag, "Encoding the following data: " )

      CALL MonthOfYear ( ROdata%DTocc%Month, MonthName, 1 )
      WRITE ( outmsg, FMT=DTfmt1 ) ROdata%DTocc%Hour,   &
                                   ROdata%DTocc%Minute, &
                                   ROdata%DTocc%Day,    &
                                   MonthName(1:3),      &
                                   ROdata%DTocc%Year
      CALL message ( msg_diag, "  Nominal time of occultation  : "//TRIM(outmsg) )

      IF ( ROdata%GeoRef%Lon > 180.0 ) &
             ROdata%GeoRef%Lon = ROdata%GeoRef%Lon - 360.0
      WRITE ( outmsg, FMT="(F6.2,',',F7.2)") ROdata%GeoRef%Lat, ROdata%GeoRef%Lon
      CALL message ( msg_diag, "  Nominal occ lat/lon location : "//TRIM(outmsg) )

      WRITE ( number, FMT="(I6)") ROdata%Lev1a%Npoints
      CALL message ( msg_diag, "  No. of orbit state vectors   : "//TRIM(number) )
      WRITE ( number, FMT="(I6)") ROdata%Lev1b%Npoints
      CALL message ( msg_diag, "  No. of bending angle samples : "//TRIM(number) )
      IF ( ROdata%Lev1b%Npoints > 0 ) THEN
        IF ( CorrOnly) THEN
          CALL message ( msg_diag, "  Bending angles present       : " // &
                                   "Corrected only" )
        ELSE
          CALL message ( msg_diag, "  Bending angles present       : " // &
                                   "L1+L2+Corrected" )
        ENDIF
      ENDIF
      WRITE ( number, FMT="(I6)") ROdata%Lev2a%Npoints
      CALL message ( msg_diag, "  No. of refractivity samples  : "//TRIM(number) )
      WRITE ( number, FMT="(I6)") ROdata%Lev2b%Npoints
      CALL message ( msg_diag, "  No. of geophysical samples   : "//TRIM(number) )
      WRITE ( number, FMT="(I6)") ROdata%Lev2c%Npoints
      CALL message ( msg_diag, "  No. of surface geo. samples  : "//TRIM(number) )
      WRITE ( number, FMT="(I6)") ROdata%Lev2d%Npoints
      CALL message ( msg_diag, "  No. of model coeff. levels   : "//TRIM(number) )

      IF ( nValues == nElem ) THEN
        WRITE ( number, FMT="(I6)" ) nElem
        CALL message ( msg_diag, "  Total no. of BUFR elements   :"//  &
                                 TRIM(number) )
      ELSE
        WRITE ( number, FMT="(I6)" ) nValues
        CALL message ( msg_diag, "  Thinned no. of BUFR elements :"// &
                                 TRIM(number) )
      END IF
      WRITE ( number, FMT="(I6)" ) nValues+nExtra
      CALL message ( msg_diag, "  Allocated Descriptor space   :"// &
                               TRIM(number) )

!--------------------------------------------------------------
! 7.9 Encode this occultation & write it to output BUFR file
!--------------------------------------------------------------

      FXXYYY = ROdescr
      CALL ConvertDescriptor ( Descr(1), FXXYYY, F,X,Y, 2 )
      nDescr   = 1

      CALL EncodeBUFR ( BUFRunit,   &
                        CSN,        &
                        OrigICAO,   &
                        OrigCentre, &
                        SubCentre,  &
                        Descr,      &
                        nDescr,     &
                        Values1,    &
                        nObs,       &
                        GTShdrType, &
                        LenBUF )

      LenTot = LenTot + LenBUF
      IF ( LenBUF > 0 ) nmsg = nmsg + 1

      IF ( ALLOCATED ( Values1 ) ) DEALLOCATE ( Values1 )
      IF ( ALLOCATED ( Values ) ) DEALLOCATE ( Values )
      IF ( ALLOCATED ( Descr  ) ) DEALLOCATE ( Descr )
      IF ( ALLOCATED ( RepFac ) ) DEALLOCATE ( RepFac )

    END DO                       ! end of profiles loop

!--------------------------------------------------------------
! 7.10 Free memory ready for next file
!--------------------------------------------------------------

    CALL ropp_io_free ( ROdata )

  END DO                         ! end of file loop

!--------------------------------------------------------------
! 8. Generate & output end-of-file dummy bulletin for IP
!    if required.
!    Close output file. Delete it if no messages were written
!--------------------------------------------------------------

  IF ( GTShdrType == IPhdrs .AND. &
       nmsg       >  0 ) THEN
    LenEOF = 0
    CALL GTShdrIPH ( LenEOF, cIPH, LenIPH )
    WRITE( number, FMT="(I6)") LenIPH
    CALL message ( msg_diag, "Writing "//TRIM(number)// &
                             " bytes (for EOF) to "//TRIM(BUFRdsn) )
    CALL METDB_CWRITE ( BUFRunit,          &
                        cIPH(1:LenIPH), &
                        LenIPH )

    IF ( LenIPH <= 0 ) THEN
      CALL message ( msg_error, "Writing end-of-file IPH to BUFR file "// &
                                TRIM(BUFRdsn) )
      ierr = 2
    ELSE
      LenTot = LenTot + LenIPH
    END IF
  ENDIF

  IF ( LenTot > 0 ) THEN
    WRITE( number, FMT="(I6)") LenTot
    CALL message ( msg_info, "Total of "//TRIM(number)// &
                             " bytes written to "//TRIM(BUFRdsn) )
  END IF

  CALL METDB_CCLOSE ( BUFRunit )

  IF ( nmsg == 0 ) THEN
    CALL File_Delete ( BUFRdsn, ierr )
  END IF

!--------------------------------------------------------------
! 9. If GTS headers were generated, save last used bulletin
!    sequence number
!--------------------------------------------------------------

  IF ( GTShdrType /= NOhdrs ) &
    CALL GTShdrCSN ( CSNdsn,  &
                     CSN,     &
                     'Write' )

!--------------------------------------------------------------
! 10. Tidy up & finish
!--------------------------------------------------------------

  IF ( nmsg == 0 ) THEN
    CALL message ( msg_warn, "No profiles were encoded "// &
                             "or written to the BUFR file")
  ELSE IF ( nmsg < tprofs ) THEN
    CALL message ( msg_warn, "Some profiles were not encoded "// &
                             "or written to the BUFR file")
  END IF

  IF ( nmsg > 0 ) THEN
    WRITE( number, FMT="(I6)") nmsg
    IF ( GTShdrType == NOhdrs ) THEN
      outmsg = "BUFR message"
    ELSE
      outmsg = "GTS bulletin"
    END IF
    IF ( nmsg /= 1 ) outmsg = TRIM(outmsg)//"s"
    CALL message ( msg_info, "Generated "//TRIM(ADJUSTL(number))//" "// &
                             TRIM(outmsg)//" to "//TRIM(BUFRdsn)//"\n" )
  END IF

  IF ( ALLOCATED(ROPPdsn) ) DEALLOCATE (ROPPdsn)
!!!  CALL EXIT(ErrOK)

CONTAINS
!----------------------------------------------------------------------------

SUBROUTINE EncodeBUFR ( BUFRunit,   & ! (in)
                        CSN,        & ! (inout)
                        OrigICAO,   & ! (in)
                        OrigCentre, & ! (in)
                        SubCentre,  & ! (in)
                        Descr,      & ! (inout)
                        nDescr,     & ! (inout)
                        Values,     & ! (in)
                        nObs,       & ! (in)
                        GTShdrType, & ! (in)
                        LenMSG )      ! (out)
!
!****s* ropp2bufr/EncodeBUFR_mo *
!
! NAME
!   EncodeBUFR
!
! SYNOPSIS
!   Encode converted RO data to BUFR message & write it out
!
!    USE ropp2bufr
!    INTEGER :: bufrunit, csn, origcentre, subcentre
!    INTEGER :: nobs, ndescr, gtshdrtype, lenmsg
!    INTEGER :: descr(nd)
!    REAL    :: values(ne,no)
!    CHARACTER (LEN=4) :: origicao
!    CALL encodebufr ( bufrunit, csn, &
!                      origicao, origcentre, subcentre, &
!                      descr, ndescr, values, nobs, &
!                      gtshdrtype, lenmsg )
!
! INPUTS
!   BUFRunit    int  Output BUFR file unit
!   CSN         int  Channel sequence number (001-999)
!   OrigICAO    chr  4-chr ICAO location indicator for originator centre
!   OrigCentre  int  Originator centre BUFR common code value
!   SubCentre   int  Processing centre code value
!   Descr       int  Array of descriptors for type
!   nDescr      int  No. of initial descriptors in descr
!   Values      flt  Array(ne) of converted values for BUFR encoder
!   nObs        int  No. of observations
!   GTShdrType  int  GTS header type indicator:
!                     0: no headers
!                     1: ARH
!                     2: IPH + ARH
!
! OUTPUTS
!   CSN         int  Updated channel sequence number (001-999)
!   Descr       int  Array of expanded descriptors
!   nDescr      int  No. of expanded descriptors in descr
!   LenMSG      int  No. of bytes in BUFR message (including any IPH+ARH)
!                    Returns zero if encoding or I/O error
!
! DEPENDENCIES:
!   MetDB BUFR package  - BUFR kernel routines
!   GTShdrs             - routines to add WMO/GTS routing header/trailer
!
! CALLS
!   GTShdrIPH
!   GTShdrARH
!   GTShdrEOM
!   ENBUFV4
!   METDB_CWRITE
!
! CALLED BY
!   ropp2bufr
!
! DESCRIPTION
!   Encodes data in array "Values" (pre-converted to BUFR standard) to a BUFR
!   message, and (with any optional IPH, ARH and EOM) ships it out to the file
!   pre-opened on "BUFRunit". The data in the array must be consistent with the
!   master descriptor sequence in "Desc(1)". On exit, Descr will contain an
!   updated nDesc expanded descriptor list, so this array should be sized to
!   the maximum expected expansion.
!   On return, "LenMSG" gives the total number of bytes written out (BUFR
!   message plus any ARH and IPH); or zero if any encoding or I/O error.
!   An optional GTS Abbreviated Routing Header (ARH) is pre-pended and a
!   trailer End-of-Message (EOM) appended) if GTShdrType=1. Further, an 8-byte
!   length + 2-byte data type header is output first if GTShdrType=2, for
!   compatability with GTS transmission via TCP/IP (FTP). In this case, a dummy
!   IPH must be written after the last BUFR message, before closing the file.
!   No headers are generated at all if GTShdrType is not 1 or 2.
!
! REFERENCE
!   MetDB Technote 1: Decoding and Encoding BUFR messages
!   File metdb/dmtn1.html in MetDB BUFR library package
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

  USE ropp2bufr
  USE messages
  USE GTShdrs

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (LEN=1) :: NULL = CHAR(0)

! Argument list parameters

  INTEGER,           INTENT(IN)    :: BUFRunit   ! BUFR I/O unit
  INTEGER,           INTENT(INOUT) :: CSN        ! Channel sequence number
  CHARACTER (LEN=*), INTENT(IN)    :: OrigICAO   ! originating centre ICAO code
  INTEGER,           INTENT(IN)    :: OrigCentre ! originating centre code value
  INTEGER,           INTENT(IN)    :: SubCentre  ! processing centre code value
  INTEGER,           INTENT(INOUT) :: Descr(:)   ! descriptor sequence
  INTEGER,           INTENT(INOUT) :: nDescr     ! no. of descriptors in seq.
  REAL,              INTENT(IN)    :: Values(:)  ! data to encode
  INTEGER,           INTENT(IN)    :: nObs       ! no. of observations
  INTEGER,           INTENT(IN)    :: GTShdrType ! Code for GTS header generation
  INTEGER,           INTENT(OUT)   :: LenMSG     ! Total no. of bytes written out

! Local variables

  CHARACTER (LEN=100000) :: cBUF          ! BUFR message
  CHARACTER (LEN=100)    :: cValues = " " ! Character elements (not used)
  CHARACTER (LEN=10)     :: number        ! Number string
  CHARACTER (LEN=50)     :: routine       ! Saved routine name
  INTEGER :: DT6(6)                       ! Date & Time of data (yr,mth,day,hr,min,sec)
  INTEGER :: lat, lon                     ! Nominal mean ob. location
  INTEGER :: nElem                        ! No. of elements
  INTEGER :: ierr                         ! I/O error status
  LOGICAL :: compress     = .FALSE.       ! Compression flag

  LOGICAL   :: ExtraSect1 = .FALSE.       ! Nothing extra
  CHARACTER :: CharSect1  = " "

  LOGICAL   :: ExtraSect2 = .FALSE.       ! Nothing extra
  CHARACTER :: CharSect2  = " "

  CHARACTER (LEN=10) :: cIPH              ! IP Header sequence
  CHARACTER (LEN=31) :: cARH              ! Abbreviated Routing Header sequence
  CHARACTER (LEN=4)  :: cEOM              ! End-of-Message sequence
  INTEGER :: LenIPH, LenARH, LenEOM       ! Lengths of IPH, ARH & EOM (bytes)
  INTEGER :: LenBUF                       ! Length  of BUFR message (bytes)

!-------------------------------------------------------------
! 1. Initialise
!-------------------------------------------------------------

  CALL message_get_routine ( routine )
  CALL message_set_routine ( "EncodeBUFR" )

  nElem = SIZE ( Values )

  DO i = 1, LEN(cBUF)
    cBUF(i:i) = NULL
  END DO
  LenBUF = 0
  LenMSG = 0
  LenIPH = 0
  LenARH = 0
  LenEOM = 0

  ierr = 0

!-------------------------------------------------------------
! 2. Extract date/time of first ob. for Section 1.
!-------------------------------------------------------------

 DT6 = NINT(Values(7:12)) ! Ob. Date/Time (Y,M,D,h,m,s) for Section 1

!-------------------------------------------------------------
! 3. Do the encode
!-------------------------------------------------------------

  CALL ENBUFV4 ( Descr,       Values,        &
                 nDescr,      nElem, nObs,   &
                 cValues,     DT6,           &
                 cBUF,                       &
                 compress,                   &
                 LenBUF,                     &
                 Edition,                    &
                 MasterTable, VerMasTable,   &
                 OrigCentre,  SubCentre,     &
                 DataType,    LoclSubType,   &
                 IntlSubType, VerLocTable,   &
                 ExtraSect1,  CharSect1,     &
                 ExtraSect2,  CharSect2,     &
                 Sec3Type_mo )

  IF ( LenBUF > 0 ) THEN

! ENBUFV2 ought to return nDescr as the no. of expanded descriptors,
! but it doesn't - the returned value is the number of data elements
! which we already know (nElem). So we scan the Descr array to find
! the last valid descriptor.

    nDescr = SIZE(descr)
    DO
      IF ( nDescr       == 0 ) EXIT
      IF ( Descr(nDescr) > 0 ) EXIT
      nDescr = nDescr - 1
    END DO
    CALL message ( msg_diag, " Encoding results:" )
    WRITE( number, FMT="(I7)") nDescr
    CALL message ( msg_diag, "   No. of expanded decsr. :"// &
                             TRIM(number) )
    WRITE( number, FMT="(I7)") nElem
    CALL message ( msg_diag, "   No. of encoded values  :"// &
                             TRIM(number) )
    WRITE( number, FMT="(I7)") LenBUF
    CALL message ( msg_diag, "   Length of BUFR message :"// &
                             TRIM(number)//" octets" )

!-------------------------------------------------------------
! 4. Generate IP header, WMO bulletin routing header
!    and End-of-Message sequences as required
!-------------------------------------------------------------

    IF ( GTShdrType /= NOhdrs) THEN
      Lat = NINT(Values(30))
      Lon = NINT(Values(31))
      CALL GTShdrARH ( CSN,      &
                       TTAAII,   &
                       Lat, Lon, &
                       OrigICAO, &
                       DT6,      &
                       cARH, LenARH )
      CALL GTShdrEOM ( cEOM, LenEOM )
      LenMSG = LenARH + LenBUF + LenEOM
      IF ( GTShdrType == IPhdrs ) &
        CALL GTShdrIPH ( LenMSG, cIPH, LenIPH )
      WRITE( number, FMT="(I7)") LenMSG
      CALL message ( msg_diag, "   Length of GTS bulletin :"// &
                               TRIM(number)//" octets" )
    END IF

!-------------------------------------------------------------
! 5. Write the coded sections to the BUFR file
!-------------------------------------------------------------

    IF ( GTShdrType /= NOhdrs) THEN
      IF ( GTShdrType == IPhdrs ) THEN
        CALL METDB_CWRITE ( BUFRunit, cIPH(1:LenIPH), LenIPH )
        IF ( LenIPH <= 0 ) THEN
          CALL message ( msg_error, "Writing IPH to BUFR file " )
          LenIPH = 0
        END IF
      END IF

      CALL METDB_CWRITE ( BUFRunit, cARH(1:LenARH), LenARH )
      IF ( LenARH <= 0 ) THEN
        CALL message ( msg_error, "Writing ARH to BUFR file" )
        LenARH = 0
      END IF
    END IF

    CALL METDB_CWRITE ( BUFRunit, cBUF(1:LenBUF), LenBUF )
    IF ( LenBUF <= 0 ) THEN
      CALL message ( msg_error, "Writing BUFR message to BUFR file" )
      LenBUF = 0
    END IF

    IF ( GTShdrType /= NOhdrs) THEN
      CALL METDB_CWRITE( BUFRunit, cEOM(1:LenEOM), LenEOM )
      IF ( LenEOM <= 0) THEN
        CALL message ( msg_error, "Writing EOM to BUFR file" )
        LenEOM = 0
      END IF
    END IF

    IF ( GTShdrType /= NOhdrs ) THEN
      DO i = 1, LenARH
        IF ( LLT(cARH(i:i)," ") ) cARH(i:i) = "."   ! change non-printing chrs
      END DO
      IF ( LenIPH > 0 ) THEN
        CALL message ( msg_diag, " GTS bulletin header (IPH+ARH): "// &
                                 cIPH(1:LenIPH)//cARH(1:LenARH) )
      ELSE
        CALL message ( msg_diag, " GTS bulletin header (ARH)    : "// &
                                 cARH(1:LenARH) )
      END IF
    END IF
    LenMSG = LenIPH + LenARH + LenBUF + LenEOM

    WRITE ( number, FMT="(I10)" ) LenMSG
    CALL message ( msg_diag, " No. of bytes written to file : "// &
                               TRIM(ADJUSTL(number)) )

!-------------------------------------------------------------
! 6. Failed to encode the RO profile :-(
!-------------------------------------------------------------

  ELSE
    CALL message ( msg_error, "Error generating BUFR message" )
    LenMSG = 0
  END IF

  CALL message_set_routine ( routine )

END SUBROUTINE EncodeBUFR
!----------------------------------------------------------------------------

END PROGRAM ropp2bufr_mo

!-------------------------------------------------------------------------------
! 7. Version information
!-------------------------------------------------------------------------------

SUBROUTINE version_info()
  USE ropp_io, ONLY : ropp_io_version
  CHARACTER (LEN=40) :: version
  version = ropp_io_version()
  PRINT *, 'ropp2bufr -  ROPP netCDF to BUFR encoder [MetDB library]'
  PRINT *, ''
  PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
  PRINT *, ''
END SUBROUTINE version_info

