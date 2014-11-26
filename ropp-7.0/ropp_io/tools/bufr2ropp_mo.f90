! $Id: bufr2ropp_mo.f90 2738 2011-03-22 15:34:18Z frdo $

PROGRAM bufr2ropp_mo

!****x* Programs/bufr2ropp_mo *
!
! NAME
!   bufr2ropp    (bufr2ropp_mo.f90)
!
! SYNOPSIS
!   Decode RO BUFR message(s) to ROPP file(s) [MetDB library]
!
!   > export BUFR_LIBRARY=bufr_directory_path
!   > bufr2ropp bufr_file [bufr_file...] [-o ropp_file] [-m] [-a]
!                         [-f first] [-n number] [-d] [-h] [-v]
!
! ARGUMENTS
!   bufr_file - one or more files containing BUFR messages
!
! OPTIONS
!   Option switches can be in any order and are case-insensitive;
!   any space(s) between a switch and its (madatory) argument is
!   optional.
!     -o  specify the ROPP output file name. Not recommended if more
!         than one RO BUFR message could be decoded.
!     -f  specify the first RO BUFR message to decode
!         (ie skip first-1 messages)
!     -n  specify the maximum number of RO messages to decode
!     -d  to output additional diagnostics
!     -h  help
!     -v  version information
!   Defaults:
!     Input file name  : ropp.bufr
!     Output file name : <occid>.nc  (netCDF)
!     first            : 1
!     number           : 999999 (all)
!
! INPUTS
!   bufr_file is the input file containing BUFR message(s).
!             The input file can contain any number of aribitrary
!             BUFR messages, but any which do not contain RO data
!             (or have coding errors) will be ignored. (See Ref.1)
!
! OUTPUTS
!   ropp_file is the output file, which will be created in ROPP netCDF.
!             The output file name is optional, and if not specified,
!             is generated from the occulation ID.
!             One output file will be written for each input RO
!             BUFR message. NB: as any existing output file will be
!             overwritten, using the -o option could lose data if
!             there is more than one RO BUFR message in the input
!             file. If this is a possibility, let the program
!             generate default (and unique) file names. (See Ref.2)
!
! CALLS
!   IARGC
!   GetOptions
!   ReplicationCount
!   ConvertMOtoEC
!   ConvertBUFRtoROPP
!   ropp_io_init
!   ropp_io_write
!   ropp_io_nrec
!   ropp_io_free
!   ropp_io_version
!   BUFRREAD
!   DEBUFR
!   Date_and_Time_UTC
!   File_Delete
!   message
!   message_set_routine
!
! MODULES
!   bufr2ropp      - fixed parameter definitions & common routines
!   messages       - ROPP message library
!   ropp_io        - ROPP I/O file support
!   ropp_io_types  - ROPP derived type definitions
!   BUFRutils      - BUFR utility routines
!   DateTimeProgs  - Date & Time conversion routines
!
! DEPENDENCIES
!   MetDB BUFR package  - BUFR kernel routines
!   ROPP I/O library    - ROPP file I/O support
!   ROPP Utils library  - ROPP utility routines
!   netCDF library      - netCDF file support
!   roppbufrcodes.nl    - NAMELIST file (in BUFR_LIBRARY)
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY        - path for run-time files
!
! ERRORS
!   Program (shell) return codes:
!     0 = OK
!     1 = I/O error
!     2 = Memory allocation failure
!
! DESCRIPTION
!   A BUFR decoder for Radio Occultation data.
!   By default, outputs one ROPP file per RO BUFR message in the
!   input file.
!   Options are available to skip a first block of messages,
!   to limit the number of decoded messages and to append decoded
!   messages to a single output file.
!   BUFR tables are found via the environment variable 'BUFR_LIBRARY'.
!
! REFERENCES
!   1) WMO FM94 (BUFR) Specification for ROM SAF Processed Radio
!      Occultation Data.
!      SAF/ROM/METO/FMT/BUFR/001
!   2) ROPP User Guide - Part 1: UTILS and IO
!      SAF/GRAS/METO/FMT/ROPP/002
!
! SEE ALSO
!   bufr2ropp(1), ropp2bufr(1)
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

  USE bufr2ropp
  USE messages
  USE ropp_utils,    ONLY: File_Delete
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_init,  &
                           ropp_io_write, &
                           ropp_io_nrec,  &
                           ropp_io_free
  USE BUFRutils,     ONLY: ReplicationCount, &
                           ConvertMOtoEC,    &
                           ExtractMOinfo
  USE DateTimeProgs, ONLY: MonthOfYear

  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Fmt1 = &
                     "(1X,I4,' Non-RO BUFR Message(s) ',A)"
  CHARACTER (LEN=*), PARAMETER :: Fmt2 = &
                     "(1X,I4,'     RO BUFR Message(s) ',A)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                     "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"
  CHARACTER (LEN=*), PARAMETER :: FmtNum = "(I10)"

! Local variables

  CHARACTER (LEN=256),    &
            DIMENSION(:), &
            ALLOCATABLE :: BUFRdsn           ! Name(s) of input file(s)
  CHARACTER (LEN=256)   :: ROPPdsn           ! Name of output file

  INTEGER               :: nfiles            ! No. of BUFR input files
  INTEGER               :: ierr = 0          ! Error code
  INTEGER               :: nElem             ! Expected no. of BUFR elements
  INTEGER               :: nObs              ! Local no. of observations
  INTEGER               :: ifile             ! File loop counter
  INTEGER               :: fMsgToDecode      ! First  RO BUFR message  to decode
  INTEGER               :: nMsgToDecode      ! No. of RO BUFR messages to decode
  INTEGER               :: nMsgRead    = 0   ! No. of RO BUFR messages read
  INTEGER               :: nMsgDecoded = 0   ! No. of RO BUFR messages decoded
  INTEGER               :: nMsgSkipped = 0   ! No. of RO BUFR messages skipped
  INTEGER               :: nMsgIgnored = 0   ! No. of non-RO BUFR Messages ignored
  INTEGER               :: nMsgWritten = 0   ! Total no. of messages written
  INTEGER               :: nProf  = 0        ! No of profiles in an existing file
  INTEGER               :: ExitErr           ! Exit error flag
  LOGICAL               :: multi             ! Multifile output if .T. else single files
  LOGICAL               :: newfile           ! Append output if .F. else start a new file
  LOGICAL               :: dispsec4 = .FALSE.! don't display Section 4
  LOGICAL               :: exists            ! file exists flag
  LOGICAL               :: first    = .TRUE. ! first output profile flag
  LOGICAL               :: skip              ! skip message flag

  CHARACTER (LEN=nb)    :: cBUF              ! BUFR message
  INTEGER               :: LenBUF            ! Length of BUFR as read (bytes)

  CHARACTER (LEN=32767) :: pValues           ! Decoded packed character values
  CHARACTER (LEN=80)    :: cValues(nv)       ! Decoded unpacked character values
  REAL                  :: Values1(nv)       ! Decoded numeric   values (s.p)
  REAL(dp)              :: Values2(nv)       ! Decoded numeric   values (d.p.)

  INTEGER               :: nDescr            ! No. of descriptors in Section 3
  INTEGER               :: nExpDescr         ! No. of expanded descriptors
  INTEGER               :: Descr(nd)         ! Descriptor(s) in Section 3 (fxxyyy form)
  INTEGER               :: ExpDescr(nd)      ! Expanded descriptors

  INTEGER               :: Supp(9)           ! Array for Supplimentary info
  INTEGER               :: Sec0(3)           ! Array for Section 0 info
  INTEGER               :: Sec1(40)          ! Array for Section 1 info
  INTEGER               :: Sec2(4096)        ! Array for Section 2 info
  INTEGER               :: Sec3(4)           ! Array for Section 3 info
  INTEGER               :: Sec4(2)           ! Array for Section 4 info
  INTEGER               :: Sec5(2)           ! Array for Section 5 info

  CHARACTER (LEN=80)    :: outmsg            ! Output text string
  CHARACTER (LEN=10)    :: number            ! Numeric value as string
  CHARACTER (LEN=10)    :: MonthName         ! Month name

  TYPE (ROprof) ROdata

  INTEGER :: nLev1a, nLev1b                  ! No. of Level 1 samples in BUFR
  INTEGER :: nLev2a, nLev2b, nLev2c, nLev2d  ! No. of Level 2 samples in BUFR
  INTEGER :: nFreq                           ! No. of frequency sets  in BUFR

! Functions

  ! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

!--------------------------------------------------------------
!  1. Initalise
!--------------------------------------------------------------

  ExitErr = ErrOK

  CALL message_set_routine ( "bufr2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                      BUFR to ROPP Decoder'                           )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!--------------------------------------------------------------
! 2. Parse command line
!--------------------------------------------------------------

  nfiles = MAX ( IARGC(), 1 )
  ALLOCATE ( BUFRdsn(nfiles) )
  CALL GetOptions ( BUFRdsn,      &
                    nfiles,       &
                    ROPPdsn,      &
                    multi,        &
                    newfile,      &
                    fMsgToDecode, &
                    nMsgToDecode )

!--------------------------------------------------------------
! 3. Loop over input BUFR files
!--------------------------------------------------------------

  DO ifile = 1, nfiles

    nMsgRead    = 0
    nMsgDecoded = 0
    nMsgSkipped = 0
    nMsgIgnored = 0

!--------------------------------------------------------------
! 3.1 Check that input BUFR file exists & try to open it
!--------------------------------------------------------------

    INQUIRE ( FILE=BUFRdsn(ifile), EXIST=exists )
    IF ( .NOT. exists ) THEN
      CALL message ( msg_error, "Input BUFR file "// &
                                TRIM(BUFRdsn(ifile))//" not found" )
      ExitErr = ErrIO
      CYCLE
    END IF

    CALL message ( msg_info, "Reading "//TRIM(BUFRdsn(ifile)) )

!--------------------------------------------------------------
! 3.2 Loop over required messages in input file, skipping
!     any unwanted messages, stop on EOF or read error
!--------------------------------------------------------------

    skip = .TRUE.

    DO
      IF ( nMsgDecoded+1 == fMsgToDecode ) skip = .FALSE.   ! stop skipping

      CALL BUFRREAD ( TRIM(BUFRdsn(ifile)), &
                      cBUF,                 &
                      LenBUF,               &
                      ierr )

      IF ( ierr == 0 ) THEN
        nMsgRead = nMsgRead + 1
        WRITE (number, FMT="(I6)" ) nMsgRead
        IF ( skip ) THEN
          CALL message ( msg_diag, "Skip message "//TRIM(number) )
        ELSE
          CALL message ( msg_diag, "Read message "//TRIM(number) )
        END IF

      ELSE IF ( ierr < 0 ) THEN
        IF  ( skip ) THEN
          CALL message ( msg_error, "End-of-file reached during "// &
                                    "skip operation" )
        ELSE
          IF ( nMsgDecoded == 0 ) THEN
            CALL message ( msg_warn, "End-of-file reached before any "// &
                                     "RO messages could be decoded" )
            IF ( fMsgToDecode > 1 ) &
              CALL message ( msg_cont, "- try again without the -f option" )
          END IF
        END IF
        ierr = 0  ! normal EOF - proceed to next file
        EXIT

      ELSE IF ( ierr == 2 ) THEN
        CALL message ( msg_error, "Error reading BUFR file" )

      ELSE IF ( ierr == 3 ) THEN
        CALL message ( msg_error, "End-of-message tag '7777' not found" )

      END IF

      IF ( ierr /= 0 ) THEN
        ExitErr = ErrIO
        EXIT
      END IF
      IF ( skip ) CYCLE

!--------------------------------------------------------------
! 3.3 Decode Message
!--------------------------------------------------------------

      nExpDescr = nd
      nObs      = nv
      CALL DEBUFR ( ExpDescr,  &
                    Values1,   &
                    pValues,   &
                    nExpDescr, &
                    nObs,      &
                    cBUF,      &
                    dispsec4 )

      IF ( nExpDescr == 0 .OR. &
           nObs      == 0 ) THEN
        CALL message ( msg_error, "Error decoding BUFR message" )
        CYCLE
      END IF

!--------------------------------------------------------------
! 3.4 Extract unexpanded descriptor list
!     Ignore if this isn't an RO message (first unexpanded
!     descriptor must be same as ROdescr [030026])
!--------------------------------------------------------------

      CALL ExtractMOinfo     ( cBUF, Supp, Sec0, Sec1, Sec2, &
                               Sec3, Sec4, Sec5, nDescr, Descr )

      IF ( Descr(1) /= ROdescr ) THEN
        nMsgIgnored = nMsgIgnored + 1
        CALL message ( msg_diag, "Message does not contain RO data - ignored")
        CYCLE
      ELSE
        IF ( skip ) THEN
          nMsgSkipped = nMsgSkipped + 1
          CALL message ( msg_diag, "Message contains RO data - skipped")
          CYCLE
        ELSE
          nMsgDecoded = nMsgDecoded + 1
          CALL message ( msg_diag, "Message contains RO data - decoded")
        END IF
      END IF

!--------------------------------------------------------------
! 3.5 Extract replication counts & initialise RO data structures.
!     - Reserve a single Level 1a sample for nominal POD set in BUFR
!     - Reserve a single Level 2c (surface geophysical data) sample
!       if any Level 2b (geophysical profile) samples
!     - No Level 2d data in BUFR
!--------------------------------------------------------------

      CALL ConvertMOtoEC     ( nExpDescr, ExpDescr, nObs, &
                               Values1, Values2,          &
                               pValues, cValues )
      nLev1a = 1
      nLev1b = ReplicationCount ( ExpDescr, Values2, 005001 )
      nLev2a = ReplicationCount ( ExpDescr, Values2, 007007 )
      nLev2b = ReplicationCount ( ExpDescr, Values2, 007009 )
      IF ( nLev2b == 0 ) THEN
        nLev2c = 0
      ELSE
        nLev2c = 1
      END IF
      nLev2d = 0
      nFreq  = ReplicationCount ( ExpDescr, Values2, 002121 )

      CALL ropp_io_init ( ROdata,         &
                          nLev1a, nLev1b, &
                          nLev2a, nLev2b, nLev2c, nLev2d )

!--------------------------------------------------------------
! 3.6 No. of BUFR elements expected
!--------------------------------------------------------------

      nElem = 37                                           &  ! Header
            + 1 + ROdata%Lev1b%Npoints * ( 5 + nFreq * 6 ) &  ! Level 1b
            + 1 + ROdata%Lev2a%Npoints * 6                 &  ! Level 2a
            + 1 + ROdata%Lev2b%Npoints * 10                &  ! Level 2b
            + 7                                               ! Level 2c

      CALL message ( msg_diag, "Decoded the following data: " )
      WRITE(number, FMT="(I6)") LenBUF
      CALL message ( msg_diag, " Length of BUFR message       : "//TRIM(number)// &
                               " octets" )
      WRITE(number, FMT="(I6)") nExpDescr
      CALL message ( msg_diag, " No. of expanded BUFR descr.  : "//TRIM(number) )
      WRITE(number, FMT="(I6)") nElem
      CALL message ( msg_diag, " Total no. of BUFR elements   : "//TRIM(number) )
      WRITE(number, FMT="(I6)") ROdata%Lev1a%Npoints
      CALL message ( msg_diag, " No. of orbit state vectors   : "//TRIM(number) )
      WRITE(number, FMT="(I6)") ROdata%Lev1b%Npoints
      CALL message ( msg_diag, " No. of bending angle samples : "//TRIM(number) )
      IF ( ROdata%Lev1b%Npoints > 0 ) THEN
        IF ( nFreq == 1 ) THEN
          CALL message ( msg_diag, " Bending angles present       : "// &
                                   " Corrected only" )
        ELSE
          CALL message ( msg_diag, " Bending angles present       : "// &
                                   " L1+L2+Corrected" )
        END IF
      END IF
      WRITE(number, FMT="(I6)") ROdata%Lev2a%Npoints
      CALL message ( msg_diag, " No. of refractivity samples  : "//TRIM(number) )
      WRITE(number, FMT="(I6)") ROdata%Lev2b%Npoints
      CALL message ( msg_diag, " No. of geophysical samples   : "//TRIM(number) )
      WRITE(number, FMT="(I6)") ROdata%Lev2c%Npoints
      CALL message ( msg_diag, " No. of surface geo. samples  : "//TRIM(number) )
      WRITE(number, FMT="(I6)") ROdata%Lev2d%Npoints
      CALL message ( msg_diag, " No. of model coeff. levels   : "//TRIM(number) )

      IF ( nLev1b == 0 .AND. &
           nLev2a == 0 .AND. &
           nLev2b == 0 .AND. &
           nFreq  == 0 ) THEN
        CALL message ( msg_warn, "No recognisable replications - skipping" )
        CYCLE
      END IF

!-------------------------------------------------------------
! 3.7 Convert BUFR array to ROPP derived type parameters
!-------------------------------------------------------------

      CALL ConvertBUFRtoROPP ( Values2, &
                               nFreq,   &
                               ROdata,  &
                               ierr )
      IF ( ierr /= 0 ) CYCLE

      WRITE( number, FMT="(I4)") nMsgDecoded
      CALL message ( msg_info, "Decoded profile "//TRIM(number)// &
                               " : "//TRIM(ROdata%Occ_ID) )
      CALL MonthOfYear ( ROdata%DTocc%Month, MonthName, 1 )
      WRITE ( outmsg, FMT=DTfmt1 ) ROdata%DTocc%Hour,   &
                                   ROdata%DTocc%Minute, &
                                   ROdata%DTocc%Day,    &
                                   MonthName(1:3),      &
                                   ROdata%DTocc%Year
      WRITE ( number, FMT="(F8.1)" ) ROdata%georef%lat
      outmsg = TRIM(outmsg)//" ("//ADJUSTL(number)
      WRITE ( number, FMT="(F8.1)" ) ROdata%georef%lon
      outmsg = TRIM(outmsg)//","//TRIM(ADJUSTL(number))//")"
      CALL message ( msg_diag, REPEAT(" ",22)//": "//TRIM(outmsg) )

!-------------------------------------------------------------
! 3.8 Check/create output file name; explcitly delete any
!     existing potential multifile (unless appending)
!-------------------------------------------------------------

      IF ( ROPPdsn == " " ) THEN
        ROPPdsn = TRIM(ROdata%occ_id) // '.nc'
        CALL To_Lower ( ROPPdsn )
        first = .TRUE.
      END IF

      IF ( first ) THEN
        INQUIRE ( FILE=ROPPdsn, EXIST=exists )
        IF ( exists ) THEN
          IF ( newfile .AND. multi ) THEN
            CALL File_Delete ( ROPPdsn, ierr )
          ELSE
            nProf = ropp_io_nrec ( ROPPdsn )
            IF ( nProf < 0 ) nProf = 1
          END IF
        ELSE
          newfile = .TRUE.
        END IF
        first = .FALSE.
      END IF

!-------------------------------------------------------------
! 3.9 write to output file
!-------------------------------------------------------------

      CALL message ( msg_info, "Writing "//TRIM(ROPPdsn) )
      CALL ropp_io_write ( ROdata, file=ROPPdsn, append=multi )
      nMsgWritten = nMsgWritten + 1       ! increment total no. of saved profiles

      IF ( .NOT. multi ) ROPPdsn = " "

!--------------------------------------------------------------
! 3.10 Free memory ready for next profile
!--------------------------------------------------------------

      CALL ropp_io_free ( ROdata )

      IF ( nMsgDecoded == nMsgToDecode ) EXIT
    END DO         ! end messages loop

!--------------------------------------------------------------
! 3.11 Summary for this file
!--------------------------------------------------------------

    WRITE ( outmsg, FMT=Fmt2 ) nMsgRead,    "read successfully"
    CALL message ( msg_diag, TRIM(outmsg) )
    WRITE ( outmsg, FMT=Fmt1 ) nMsgIgnored, "read but ignored"
    CALL message ( msg_diag, TRIM(outmsg) )
    WRITE ( outmsg, FMT=Fmt2 ) nMsgSkipped, "read but skipped"
    CALL message ( msg_diag, TRIM(outmsg) )
    WRITE ( outmsg, FMT=Fmt2 ) nMsgDecoded, "decoded sucessfully"
    CALL message ( msg_diag, TRIM(outmsg) )
    WRITE ( outmsg, FMT=Fmt2 ) nMsgWritten, "saved"
    CALL message ( msg_diag, TRIM(outmsg) )

  END DO           ! end files loop

!--------------------------------------------------------------
! 4. Show summary over all files
!--------------------------------------------------------------

  WRITE(number, FMT="(I7)") nMsgWritten
  IF ( nMsgWritten == 0 ) THEN
    CALL message ( msg_info, "No new profiles saved" )
  ELSE IF ( nMsgWritten == 1 ) THEN
    CALL message ( msg_info, "      1 new profile saved" )
  ELSE
    CALL message ( msg_info, TRIM(number)//" new profiles saved in total" )
  END IF

  IF ( nMsgWritten > 0 .AND. .NOT. newfile ) THEN
    WRITE ( number, FMT="(I7)" ) nProf + nMsgWritten
    CALL message ( msg_info, TRIM(number)//" profiles now in "//TRIM(ROPPdsn) )
  END IF

!--------------------------------------------------------------
! 5. Signal final exit error code
!--------------------------------------------------------------

!!!  CALL EXIT(ExitErr)
  IF ( ExitErr /= ErrOK ) CALL EXIT(ExitErr)

END PROGRAM bufr2ropp_mo

!-------------------------------------------------------------------------------
! 6. Version information
!-------------------------------------------------------------------------------

SUBROUTINE version_info()
  USE ropp_io, ONLY : ropp_io_version
  CHARACTER (LEN=40) :: version
  version = ropp_io_version()
  PRINT *, 'bufr2ropp -  BUFR to ROPP netCDF decoder [MetDB library]'
  PRINT *, ''
  PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
  PRINT *, ''
END SUBROUTINE version_info

