! $Id: decbufr.f90 32 2011-07-29 13:39:55Z frdo $

PROGRAM DecBUFR

!****e* BUFR/DECBUFR/DecBUFR *
!-----------------------------------------------------------------------
!
! NAME
!   decbufr  (decbufr.f90)
!
! SYNOPSIS
!   Decode arbitary BUFR messages
!
!   $ decbufr <bufr-file> [options...]
!
! CALLS
!   GetOptions
!   PrintInfo
!   OutputVals
!   BUFRREAD
!   DEBUFR
!
! USES
!   libmetdbbufr.a - MetDB BUFR kernel library
!
! FILES
!   TABLEB       : BUFR Table B (elements)
!   TABLED       : BUFR Table D (sequences)
!   CODEFIG      : BUFR Code/Flag tables
!   MASTERTABLE  : Master Table & versions
!   DATACATEGORY : data categories & sub-categories
!   ORIGCENTRE   : originating centres & sub-centres
!        - all found via environment variable 'BUFR_LIBRARY'
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY  - path to above files (terminated with '/')
!
! DESCRIPTION
!   Provides a simple generic BUFR decoder user interface, decoding
!   - and optionally displaying - arbitrary BUFR-encoded data.
!   BUFR tables are found via the environment variable 'BUFR_LIBRARY'.
!   Invoke as:
!     > decbufr bufr_file [-d[h]] [-o output_file]
!                         [-f first|-s skip]
!                         [-l last|-n number]
!                         [-h|--help|?] [-v|--version]
!    where:
!      bufr_file   is the input file containing BUFR message(s)
!      -d          writes an interpreted diagnostic dump to stdout
!      -dh         writes an interpreted headers dump only to stdout
!      output_file is a (formatted text) data file
!      first       is the first message to decode (ie skip first-1)
!      skip        is the number of messages to skip (ie first=skip+1)
!      last        is the last message to decode (ie number=first-last+1)
!      number      is the no. of messages to decode from first.
!    Arguments can be in any order and any space(s) between a
!    switch and its value is optional. If both -f & -s or both
!    -l & -n options are present, only the second is taken.
!    If the bufr_file is not specified, or if '?' or '-h', options
!    are present, a usage reminder only is printed and no decoding
!    is done. Similarly -v to print program version info.
!    The output file is optional, and if not specified, is not created. e.g.:
!      > decbufr bufr.gts -o bufr.out
!    will decode all messages from file bufr.gts and output a
!    formatted data file bufr.out; no dump data is written.
!    Limitations:
!    a) up to 5000 parameters per observation
!    b) up to  500 observations per message
!    c) does not output data from section 2 if present.
!    d) the BUFR details for the data must be present in at
!       least Tables B & D for the decode to be successful.
!
! ERRORS
!    Error return code to shell level as follows:
!     0 - OK
!     1 - I/O error
!     2 - Memory allocation error
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2011, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!-----------------------------------------------------------------------
!****

! Modules

  USE BUFRutils, ONLY: ConvertMOtoEC, &
                       ExtractMOinfo

  IMPLICIT NONE

! Fixed values

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Version = "V1.12  7-Jul-2011"

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(P = 13, R = 307)

  INTEGER, PARAMETER :: nb      =   500000  ! Max length of BUFR message
  INTEGER, PARAMETER :: nd      =   200000  ! Max. no. of descriptors
  INTEGER, PARAMETER :: no      =      500  ! Max. no. of observations
  INTEGER, PARAMETER :: nv      =    nd*no  ! Max. no. of data values
  INTEGER, PARAMETER :: Missing = -9999999  ! Missing data indicator
  INTEGER, PARAMETER :: Ounit   =       11  ! Output data stream unit no.

  INTEGER, PARAMETER :: ErrOK   =        0  ! No  error
  INTEGER, PARAMETER :: ErrIO   =        1  ! I/O error
  INTEGER, PARAMETER :: ErrMem  =        2  ! Memory allocation error

  CHARACTER (LEN=*), PARAMETER :: Fmt91 = &
                     "(1X,'BUFR Message',I4,':',I5,' observation')"
  CHARACTER (LEN=*), PARAMETER :: Fmt92 = &
                     "(1X,'BUFR Message',I4,':',I5,' observations')"
  CHARACTER (LEN=*), PARAMETER :: Fmt2 = &
                     "(1X,I6,' BUFR Message(s) ',A)"
! Local variables

  CHARACTER (LEN=256)   :: ProgNam           ! Program name (& path)
  CHARACTER (LEN=100)   :: BUFRdsn           ! Name of input file
  CHARACTER (LEN=100)   :: Outdsn            ! Name of output file

  INTEGER               :: fMsgToDecode      ! First   message to decode
  INTEGER               :: nMsgToDecode      ! No. of messages to decode
  INTEGER               :: nMsgRead     = 0  ! No. of BUFR Messages read
  INTEGER               :: nMsgDecoded  = 0  ! No. of BUFR Messages decoded
  INTEGER               :: nMsgSkipped  = 0  ! No. of BUFR Messages skipped
  INTEGER               :: TableVersion      ! BUFR table version
  INTEGER               :: ExitErr           ! Exit error flag
  INTEGER               :: ierr              ! Error code
  INTEGER               :: i                 ! Loop counter
  LOGICAL               :: displayinfo       ! Display headers flag
  LOGICAL               :: displaysec4       ! Display Section 4 flag
  LOGICAL               :: exists            ! file exists flag
  LOGICAL               :: skip              ! skip message flag

  CHARACTER (LEN=nb)    :: cBUF              ! BUFR message string
  INTEGER               :: LenBUF            ! Length of message read

  CHARACTER (LEN=50000) :: pValues           ! Decoded packed character data
  CHARACTER (LEN=80), &
            ALLOCATABLE :: cValues(:)        ! Unpacked list of character data

  REAL                  :: Values1(nv)       ! Decoded values array (s.p)
  REAL(dp), ALLOCATABLE :: Values2(:)        ! Decoded values array (d.p)
  INTEGER               :: nValues           ! No. of values (nObs*nExpDescr)
  INTEGER               :: nObs              ! No. of observations

  INTEGER               :: Descr(nd)         ! Unexpanded descriptor list
  INTEGER               :: nDescr            ! No. of unexpanded descriptors
  INTEGER               :: ExpDescr(nd)      ! Expanded descriptor list
  INTEGER               :: nExpDescr         ! No. of descriptors

  INTEGER               :: Supp(9)           ! Array for Supplimentary info
  INTEGER               :: Sec0(3)           ! Array for Section 0 info
  INTEGER               :: Sec1(40)          ! Array for Section 1 info
  INTEGER               :: Sec2(4096)        ! Array for Section 2 info
  INTEGER               :: Sec3(4)           ! Array for Section 3 info
  INTEGER               :: Sec4(2)           ! Array for Section 4 info
  INTEGER               :: Sec5(2)           ! Array for Section 5 info

!---------------------------------------------------------------
! 0. Start-up
!---------------------------------------------------------------

  CALL GETARG ( 0, ProgNam )
  i = INDEX(ProgNam, "/", BACK=.TRUE.)
  ProgNam = Prognam(i+1:)
  IF ( ProgNam == " " ) ProgNam = "decbufr"
  WRITE ( *, "(/A/A/A/)" ) REPEAT("-",55), &
                           REPEAT(" ",10)//"Met Office Generic BUFR Decoder", &
                           REPEAT("-",55)

!---------------------------------------------------------------
! 1. Get command line arguments & options; check file input file
!---------------------------------------------------------------

  CALL GetOptions ( Prognam,      &
                    BUFRdsn,      &
                    Outdsn,       &
                    displayinfo,  &
                    displaysec4,  &
                    fMsgToDecode, &
                    nMsgToDecode )
!!!print*,'BUFRdsn:',trim(bufrdsn)
!!!print*,'Outdsn :',trim(outdsn)
!!!print*,'Dsplinf:',displayinfo
!!!print*,'Dsplsc4:',displaysec4
!!!print*,'FrstMsg:',fmsgtodecode
!!!print*,'NumMsgs:',nmsgtodecode
!!!call exit(0)

  INQUIRE ( FILE=BUFRdsn, EXIST=exists )
  IF ( .NOT. exists ) THEN
    WRITE ( *, FMT="(A)" ) "Input BUFR file ", TRIM(BUFRdsn), " not found"
    CALL EXIT(ErrIO)
  END IF
  WRITE ( *, FMT="(A)" ) "Reading "//TRIM(BUFRdsn)

!---------------------------------------------------------------
! 2. Attempt to open output file if wanted.
!    (Input file is opened by first call to BUFRREAD)
!---------------------------------------------------------------

  IF ( Outdsn /= " " ) THEN
    OPEN ( UNIT=Ounit,     &
           FILE=Outdsn,    &
         STATUS="UNKNOWN", &
         IOSTAT=ierr )
    IF ( ierr /= 0 ) Outdsn = " "
  END IF

!---------------------------------------------------------------
! 3. Loop over messages in input file, skipping
!    any unwanted messages, stop on EOF or read error
!---------------------------------------------------------------

  skip = .TRUE.

  DO
    IF ( nMsgRead+1  == fMsgToDecode ) skip = .FALSE.   ! stop skipping
    IF ( nMsgDecoded == nMsgToDecode ) EXIT             ! stop decoding

!---------------------------------------------------------------
! 3.1 Read next message
!---------------------------------------------------------------

    CALL BUFRREAD ( TRIM(BUFRdsn), &
                    cBUF,          &
                    LenBUF,        &
                    ierr )

    IF ( ierr == 0 ) THEN
      nMsgRead = nMsgRead + 1
      IF ( .NOT. displayinfo ) THEN
        IF ( skip ) THEN
          WRITE ( *, FMT="(A,I6)" ) "Skip message ", nMsgRead
        ELSE
          WRITE ( *, FMT="(A,I6)" ) "Read message ", nMsgRead
        END IF
      END IF

    ELSE IF ( ierr < 0 ) THEN
      IF  ( skip ) THEN
        WRITE ( *, FMT="(A)" ) "End-of-file reached during "// &
                               "skip operation"
      ELSE
        IF ( nMsgDecoded == 0 ) THEN
          WRITE ( *, FMT="(A)" ) "End-of-file reached before any "// &
                                 "messages could be decoded"
          IF ( fMsgToDecode > 1 ) &
            WRITE ( *, FMT="(A)" ) "- try again without the -f option"
        END IF
      END IF
      ExitErr = ErrOK  ! normal EOF
      EXIT

    ELSE IF ( ierr == 2 ) THEN
      WRITE ( *, FMT="(A)" ) "Error reading BUFR file"

    ELSE IF ( ierr == 3 ) THEN
      WRITE ( *, FMT="(A)" ) "End-of-message tag '7777' not found"

    END IF

    IF ( ierr /= 0 ) THEN
      ExitErr = ErrIO
      EXIT
    END IF
    IF ( skip ) THEN
      nMsgSkipped = nMsgSkipped + 1
      CYCLE
    END IF

!---------------------------------------------------------------
! 3.2 Decode Message, with optional dump
!---------------------------------------------------------------

    CALL ExtractMOinfo ( cBUF,             &
                         Supp,             &
                         Sec0, Sec1, Sec2, &
                         Sec3, Sec4, Sec5, &
                         nDescr, Descr )

    IF ( displayinfo ) &
      CALL PrintInfo ( nMsgRead,         &
                       Sec0, Sec1, Sec2, &
                       Sec3, Sec4, Sec5, &
                       nDescr, Descr )

    IF ( displaysec4 ) WRITE ( *, "(/A/)" ) " Data in Section 4:"
    nExpDescr = nd
    nObs      = nv
    CALL DEBUFR ( ExpDescr,  &
                  Values1,   &
                  pValues,   &
                  nExpDescr, &
                  nObs,      &
                  cBUF,      &
                  displaysec4 )
    IF ( nExpDescr > 0 ) nMsgDecoded = nMsgDecoded + 1

!---------------------------------------------------------------
! 3.3 Optionally output decoded values
!---------------------------------------------------------------

    IF ( Outdsn /= " " ) THEN
      WRITE ( Ounit, "(/A/)" ) " Data in Section 4:"
      IF ( nObs == 1 ) THEN
        WRITE ( Ounit, FMT=Fmt91 ) nMsgSkipped+nMsgDecoded, nObs
      ELSE
        WRITE ( Ounit, FMT=Fmt92 ) nMsgSkipped+nMsgDecoded, nObs
      END IF

      nValues = nObs * nExpDescr
      ALLOCATE ( Values2(nValues), cValues(nValues), STAT=ierr )
      IF ( ierr /= 0 ) THEN
        WRITE ( *, FMT="(A)") &
                "Failed to allocate memory for temporary 1d arrays"
        CALL EXIT(ErrMem)
      END IF

      CALL ConvertMOtoEC ( nExpDescr, ExpDescr, &
                           nObs,                &
                           Values1,   Values2,  &
                           pValues,   cValues )

      CALL OutputVals ( Values2,   &
                        ExpDescr,  &
                        cValues,   &
                        nExpDescr, &
                        nObs,      &
                        TableVersion )

      DEALLOCATE ( Values2, cValues )
    END IF

    IF ( displayinfo ) WRITE ( *, * )
  END DO                    ! processing messages

!---------------------------------------------------------------
! 4. Close files, show summary & finish
!---------------------------------------------------------------

  IF ( Outdsn /= " " )  &
    CLOSE ( UNIT=Ounit, &
          IOSTAT=ierr )

  WRITE ( *, FMT=Fmt2 ) nMsgRead,    "read"
  WRITE ( *, FMT=Fmt2 ) nMsgSkipped, "skipped"
  WRITE ( *, FMT=Fmt2 ) nMsgDecoded, "decoded"

! Signal final exit error code

  CALL EXIT(ExitErr)

CONTAINS
!-------------------------------------------------------------------------

SUBROUTINE Usage ( ProgNam ) ! (in)

!****s* BUFR/DECBUFR/Usage *
!
! NAME
!   Usage    (DecBUFR.f90)
!
! SYNOPIS
!   USE DecBUFR
!   CHARACTER (LEN=n) :: ProgNam
!   CALL Usage ( ProgNam )
!
! INPUTS
!   ProgNam  chr  Program name (command)
!
! OUTPUTS
!   Summary usage text to stdout
!
! CALLED BY
!   GetOptions
!
! DESCRIPTION
!   Prints a summary of program usage (help) to stdout.
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2011, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!****

  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*) :: ProgNam    ! Program command name

  WRITE ( *, FMT="(A)" ) "Purpose: generic BUFR decoder"
  WRITE ( *, FMT="(A)" ) "Usage: "
  WRITE ( *, FMT="(A)" ) "  > "//TRIM(ProgNam)//" bufr_file [-o decode_file]"
  WRITE ( *, FMT="(A)" ) REPEAT(" ",LEN_TRIM(ProgNam)+15)// &
                         "[-f first|-s skip] [-l last|-n number]"
  WRITE ( *, FMT="(A)" ) REPEAT(" ",LEN_TRIM(ProgNam)+15)// &
                         "[-d[h]] [-h|--help|?] [-v|--version]"
  WRITE ( *, FMT="(/A)") "Defaults: bufr_file   - required"
  WRITE ( *, FMT="(A)" ) "          dump output - no output"
  WRITE ( *, FMT="(A)" ) "          decode_file - no output"
  WRITE ( *, FMT="(A)" ) "          first       - 1  or"
  WRITE ( *, FMT="(A)" ) "          skip        - 0"
  WRITE ( *, FMT="(A)" ) "          last        - 999999  or"
  WRITE ( *, FMT="(A)" ) "          number      - 999999"
  WRITE ( *, FMT="(A/)") "See decbufr(1) for details."

END SUBROUTINE Usage
!-------------------------------------------------------------------------

SUBROUTINE GetOptions ( ProgNam,      & ! (in)
                        BUFRdsn,      & ! (out)
                        Outdsn,       & ! (out)
                        displayinfo,  & ! (out)
                        displaysec4,  & ! (out)
                        fMsgToDecode, & ! (out)
                        nMsgToDecode )   ! (out)

!****s* BUFR/DECBUFR/GetOptions *
!-----------------------------------------------------------------------
!
! NAME
!   GetOptions   (decbufr.f90)
!
! SYNOPSIS
!   Command line options interface for generic BUFR decoder
!
!   CHARACTER (LEN=100) :: BUFRdsn, Outdsn
!   LOGICAL             :: displayinfo, displaysec4
!   INTEGER             :: fMsgToDecode, nMsgToDecode
!   CALL GetOptions ( BUFRdsn, Outdsn, displayinfo, displaysec4, &
!                     fMsgToDecode, nMsgToDecode )
!
! INPUTS
!   ProgNam       chr  Program name
!
! OUTPUTS
!   BUFRdsn       chr  BUFR input file name (required)
!   Outdsn        chr  data output file name (optional)
!   displayinfo   log  decoded header info dump display (default: no)
!   displaysec4   log  decoded Section 4   dump display (default: no)
!   fMsgToDecode  int  first message to read (default: 1)
!   nMsgToDecode  int  no. of messages to read (default: all)
!
! CALLS
!   GETARG
!   IARGC
!
! CALLED BY
!   DecBUFR
!
! DESCRIPTION
!   Command line interface:
!     > decbufr bufr_file [-d[h]]   [-o output_file]
!                         [-f first | -s skip]
!                         [-l last  | -n number]
!                         [-h | ?] [-v | --version]
!    where:
!      bufr_file   is the input file containing BUFR message(s)
!      -d          writes an interpreted diagnostic dump to stdout
!      -dh         writes an interpreted headers dump only to stdout
!      output_file is a (formatted text) data file
!      first       is the first message to decode (ie skip first-1)
!      skip        is the number of messages to skip (ie first=skip+1)
!      last        is the last message to decode (ie number=first-last+1)
!      number      is the no. of messages to decode from first.
!    Arguments can be in any order and any space(s) between a switch and its
!    value is optional. If both -f & -s or both -l & -n options are present,
!    only the second is taken. If the bufr_file is not specified, or if '?'
!    or '-h', options are present, a usage reminder only is printed and no
!    decoding is done; similarly, -v just outputs the program version ID.
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2011, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!-----------------------------------------------------------------------
!****

  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)  :: ProgNam       ! Program name
  CHARACTER (LEN=*), INTENT(OUT) :: BUFRdsn       ! BUFR (input)    dataset name
  CHARACTER (LEN=*), INTENT(OUT) :: Outdsn        ! Decode (output) dataset name
  INTEGER,           INTENT(OUT) :: fMsgToDecode  ! First message to decode
  INTEGER,           INTENT(OUT) :: nMsgToDecode  ! Number of messages to decode
  LOGICAL,           INTENT(OUT) :: displayinfo   ! Display info on/off flag
  LOGICAL,           INTENT(OUT) :: displaysec4   ! Display Section 4 on/off flag

! Local parameters

  CHARACTER (LEN=100) :: arg     ! command line argument
  CHARACTER (LEN=100) :: Value   ! value extracted from arg
  INTEGER             :: i, n    ! counters
  INTEGER             :: Narg    ! number of command line arguments
  INTEGER             :: ierr    ! I/O error

  INTEGER             :: IARGC   ! system function to return no. of cmd line args

!---------------------------------------------------------------
! 0. Initialize defaults
!---------------------------------------------------------------

  BUFRdsn      = " "
  Outdsn       = " "
  displayinfo  = .FALSE.
  displaysec4  = .FALSE.
  fMsgToDecode = 1
  nMsgToDecode = 999999

!---------------------------------------------------------------
! 1. Process command line arguments & options
!---------------------------------------------------------------

  Narg = IARGC()
  i = 1

  DO WHILE ( i <= Narg )
    CALL GETARG ( i, arg )

!---------------------------------------------------------------
! 1.1 Help wanted
!---------------------------------------------------------------

    IF ( arg(1:1) == "?"  .OR. &
         arg(1:2) == "-h" .OR. &
         arg(1:2) == "-H" .OR. &
         arg(1:6) == "--help" ) THEN
      CALL Usage ( ProgNam )
      CALL EXIT(0)

!---------------------------------------------------------------
! 1.2 Dump output (-d for all sections, -dh for headers only)
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-d"  .OR. &
              arg(1:2) == "-D" ) THEN
      displayinfo = .TRUE.
      IF ( arg(3:3) /= "h" .AND. &
           arg(3:3) /= "H" ) displaysec4 = .TRUE.

!---------------------------------------------------------------
! 1.3 Output file
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-o"  .OR. &
              arg(1:2) == "-O" ) THEN
      Outdsn = arg(3:)
      IF ( Outdsn == " " ) THEN
        i = i + 1
        CALL GETARG ( i, Outdsn )
      END IF

!---------------------------------------------------------------
! 1.4 Start Message (first)
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-f"  .OR. &
              arg(1:2) == "-F" ) THEN
      Value = arg(3:)
      IF ( Value == " " ) THEN
        i = i + 1
        CALL GETARG ( i, Value )
      END IF
      READ ( Value, *, IOSTAT=ierr ) n
      IF ( ierr == 0 .AND. &
           n    >  1 ) fMsgToDecode = n

!---------------------------------------------------------------
! 1.5 Start Message (skip)
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-s"  .OR. &
              arg(1:2) == "-S" ) THEN
      Value = arg(3:)
      IF ( Value == " " ) THEN
        i = i + 1
        CALL GETARG ( i, Value )
      END IF
      READ ( Value, *, IOSTAT=ierr ) n
      IF ( ierr == 0 .AND. &
           n    >  0 ) fMsgToDecode = n + 1

!---------------------------------------------------------------
! 1.6 Number of Messages
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-n"  .OR. &
              arg(1:2) == "-N" ) THEN
      Value = arg(3:)
      IF ( Value == " " ) THEN
        i = i + 1
        CALL GETARG ( I, Value )
      END IF
      READ ( Value, *, IOSTAT=ierr ) n
      IF ( ierr == 0 .AND. &
           n    >  0 ) nMsgToDecode = n

!---------------------------------------------------------------
! 1.7 Last Message
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-l"  .OR. &
              arg(1:2) == "-L" ) THEN
      Value = arg(3:)
      IF ( Value == " " ) THEN
        i = i + 1
        CALL GETARG ( i, Value )
      END IF
      READ ( Value, *, IOSTAT=ierr ) n
     IF ( ierr == 0 .AND. &
          n    >  0 ) nMsgToDecode = n - fMsgToDecode + 1

!---------------------------------------------------------------
! 1.8 Program version
!---------------------------------------------------------------

    ELSE IF ( arg(1:2) == "-v" .OR. &
              arg(1:2) == "-V" .OR. &
              arg(1:9) == "--version" ) THEN
      WRITE ( *, FMT="(A/)" ) TRIM(ProgNam)//": Version "//TRIM(Version)
      CALL EXIT(0)

!---------------------------------------------------------------
! 1.9 If not a (unknown) switch, assume it's the input file Name
!---------------------------------------------------------------

    ELSE IF ( arg(1:1) /= "-" ) THEN
      BUFRdsn = arg

!---------------------------------------------------------------
! 1.10 Ignore anything else
!---------------------------------------------------------------

    ELSE
      CONTINUE

    END IF

    i = i + 1    ! next arguent

  END DO         ! argument loop

!---------------------------------------------------------------
! 3. Processed all arguments - was input file name given?
!---------------------------------------------------------------

  IF ( BUFRdsn == " " ) THEN
    CALL Usage ( ProgNam )
    CALL EXIT(0)
  END IF

END SUBROUTINE GetOptions
!-----------------------------------------------------------------

SUBROUTINE PrintInfo ( nMsg,             & ! (in)
                       Sec0, Sec1, Sec2, & ! (in)
                       Sec3, Sec4, Sec5, & ! (in)
                       nDescr, Descr )     ! (in)

!****s* BUFR/DECBUFR/PrintInfo *
!-----------------------------------------------------------------------
!
! NAME
!   PrintInfo  (decbufr.f90)
!
! SYNOPSIS
!   Print BUFR information from Sections 0-5 (except decoded Section 4 data)
!
!    CHARACTER (LEN=nnn) :: cBUF
!    INTEGER :: Sec0(3), Sec1(40), Sec2(4096)
!    INTEGER :: Sec3(4), Sec4(2),  Sec5(2)
!    INTEGER :: nDescr, Descr(nd)
!    CALL PrintInfo ( Sec0, Sec1, Sec2, Sec3, &
!                     Sec4, Sec5, nDescr, Descr )
!
! INPUTS
!   nMsg    int  Message sequence number
!   Sec0    int  Array(3)    of Section 0 info
!   Sec1    int  Array(40)   of Section 1 info
!   Sec2    int  Array(4096) of Section 2 info
!   Sec3    int  Array(4)    of Section 3 info
!   Sec4    int  Array(2)    of Section 4 info
!   Sec5    int  Array(2)    of Section 5 info
!   nDecsr  int  No. of descriptors in Section 3
!   Descr   int  Array(nd)   of  descriptors (fxxyyy format)
!
! OUTPUTS
!   Prints info to stdout
!
! CALLS
!   MASTERTABLE
!   DATACATEGORY
!   ORIGCENTRE
!
! CALLED BY
!   DecBUFR
!
! USES
!   sec1tables.mod   (sec1tables,f90)
!
! DESCRIPTION
!   Analyses a BUFR (Edition 3 or 4) message; checks all sections
!   but skips any data in Section 2 (just flags whether present or
!   not). The actual data in section 4 is not interpreted. Info is
!   written to stdout.
!
! REFERENCES
!   Manual on codes. WMO-306, Vol 1.2, Part B, Nov 2007.
!   FM 94-XIII Ext. BUFR Regulations. WMO, 2008
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2011, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!-----------------------------------------------------------------------
!****

! Modules

  USE BUFRUtils
  USE Sec1Tables

  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=3), PARAMETER :: Months(0:12) = &
            (/ "???","Jan","Feb","Mar","Apr","May","Jun", &
                     "Jul","Aug","Sep","Oct","Nov","Dec" /)

  CHARACTER (LEN=*), PARAMETER :: Fmt91 = &
           "(/1X,'BUFR Section',I2 )"
  CHARACTER (LEN=*), PARAMETER :: Fmt01 = &
           "( 2X,'Total Message length:    ',I6,' octets')"
  CHARACTER (LEN=*), PARAMETER :: Fmt02 = &
           "( 2X,'BUFR Edition:              ',I4 )"
  CHARACTER (LEN=*), PARAMETER :: Fmt11 = &
           "( 2X,'Length of section:         ',I4,' octets' )"
  CHARACTER (LEN=*), PARAMETER :: Fmt12 = &
           "( 2X,'BUFR Master Table:         ',I4,' (',A,')'/"   // &
             "2X,'Originating Centre:      ',  I6,' (',A,')'/"   // &
             "2X,'Originating Sub-Centre:  ',  I6,' (',A,')'/"   // &
             "2X,'Update Sequence No:        ',I4,1X,A/"         // &
             "2X,'Data  Category:            ',I4,' (',A,')'/"   // &
             "2X,'Inter sub-Category:        ',I4,' (',A,')'/"   // &
             "2X,'Local sub-Category:        ',I4/"              // &
             "2X,'Master Table version:      ',I4,' (',A,')'/"   // &
             "2X,'Local  Table version:      ',I4,1X,A/"         // &
             "2X,'Message time:          ',I2.2,2(':',I2.2),1X," // &
                                        "I2.2,'-',A3,'-',I4 )"
  CHARACTER (LEN=*), PARAMETER :: Fmt21 = &
           "( 2X,'Length of section:       ',  I6,' octets' )"
  CHARACTER (LEN=*), PARAMETER :: Fmt31 = &
           "( 2X,'Length of section:       ',  I6,' octets' )"
  CHARACTER (LEN=*), PARAMETER :: Fmt32 = &
           "( 2X,'No. of obs. in Section 4:  ',I4 )"
  CHARACTER (LEN=*), PARAMETER :: Fmt33 = &
           "( 2X,'Descriptor', I4,': ',I6.6 )"
  CHARACTER (LEN=*), PARAMETER :: Fmt41 = &
           "( 2X,'Length of data:          ',  I6,' octets' )"
  CHARACTER (LEN=*), PARAMETER :: FmtNum = "(I10)"

! Argument list parameters

  INTEGER, INTENT(IN) :: nMsg       ! Sequence no. of this message
  INTEGER, INTENT(IN) :: Sec0(:)    ! Array for Section 0 info
  INTEGER, INTENT(IN) :: Sec1(:)    ! Array for Section 1 info
  INTEGER, INTENT(IN) :: Sec2(:)    ! Array for Section 2 info
  INTEGER, INTENT(IN) :: Sec3(:)    ! Array for Section 3 info
  INTEGER, INTENT(IN) :: Sec4(:)    ! Array for Section 4 info
  INTEGER, INTENT(IN) :: Sec5(:)    ! Array for Section 5 info
  INTEGER, INTENT(IN) :: nDescr     ! No. of descripors
  INTEGER, INTENT(IN) :: Descr(:)   ! Descriptor list

! Local parameters

  CHARACTER (LEN=30) :: Mtab        ! Master Table discipline
  CHARACTER (LEN=30) :: Mver        ! Master Table version
  CHARACTER (LEN=50) :: Category    ! Table A data category
  CHARACTER (LEN=50) :: IntnlCategory ! International data sub-category
  CHARACTER (LEN=30) :: Centre      ! Centre name
  CHARACTER (LEN=50) :: SubCentre   ! Sub-centre name
  CHARACTER (LEN=4)  :: ICAO        ! Centre ICAO code
  CHARACTER (LEN=10) :: number      ! Number representation
  CHARACTER (LEN=10) :: update      ! update/original indicator
  CHARACTER (LEN=10) :: nolocal     ! Local table used or not indicator

  INTEGER :: len0, len1, len2, len3 ! Lengths (bytes) of Sections 0, 1, 2 & 3
  INTEGER :: len4, len5, lens, lenm ! Lengths (bytes) of Sections 4, 5, sum & total
  INTEGER :: Edition                ! BUFR Edition in Section 0
  INTEGER :: Year, Month,  Day      ! Date in Section 1
  INTEGER :: Hour, Minute, Second   ! Time in Section 1
  INTEGER :: DataCat                ! Data ategory in Section 1
  INTEGER :: IntnlSubCat            ! International sub-category in S1 (Ed.4)
  INTEGER :: LocalSubCat            ! Local sub-category in Section 1
  INTEGER :: is2                    ! Is Section 2 present? flag in Section 1
  INTEGER :: BufrMasterTable        ! Master table ID in Section 1
  INTEGER :: OCentre                ! Orig. centre code in Section 1
  INTEGER :: OSubCentre             ! Orig. sub-centre code in Section 1
  INTEGER :: UpdtSeqNo              ! Update sequence no. in Section 1
  INTEGER :: MasterTableVer         ! Master table version in Section 1
  INTEGER :: LocalTableVer          ! Local table version in Section 1
  INTEGER :: nObs                   ! No. of observations in Section 3
  INTEGER :: idata                  ! Observed data & compression flags in S3
  INTEGER :: i                      ! Loop counter

!---------------------------------------------------------------
! 0. Initialize
!---------------------------------------------------------------

  WRITE ( number, FMT=FmtNum ) nMsg
  WRITE ( *, FMT="(/A)" ) " ===== Message no. "//TRIM(ADJUSTL(number))// &
                          " ====="

!---------------------------------------------------------------
! 1. Section 0 ('BUFR', Edition)
!---------------------------------------------------------------

  len0    = Sec0(1)
  lenm    = Sec0(2)
  Edition = Sec0(3)

  WRITE ( *, FMT=Fmt91 ) 0
  WRITE ( *, FMT=Fmt21 ) len0
  WRITE ( *, FMT=Fmt01 ) lenm
  WRITE ( *, FMT=Fmt02 ) Edition

!---------------------------------------------------------------
! 2. Section 1 (identification)
!---------------------------------------------------------------

  len1            = Sec1(1)
  BufrMasterTable = Sec1(14)
  OSubCentre      = Sec1(16)
  OCentre         = Sec1(3)
  UpdtSeqNo       = Sec1(4)
  is2             = Sec1(5)
  DataCat         = Sec1(6)
  IntnlSubCat     = Sec1(17)
  LocalSubCat     = Sec1(7)
  MasterTableVer  = Sec1(15)
  LocalTableVer   = Sec1(8)
  Year            = Sec1(9)
  Month           = Sec1(10)
  Day             = Sec1(11)
  Hour            = Sec1(12)
  Minute          = Sec1(13)
  Second          = Sec1(18)

  CALL MasterTable  ( BufrMasterTable, &
                      MasterTableVer,  &
                      Mtab, Mver )
  IF ( LocalTableVer == 0 .OR. &
       LocalTableVer == 255 ) THEN
    nolocal = "(not used)"
  ELSE
    nolocal = " "
  END IF

  CALL OrigCentre ( OCentre, OSubCentre, &
                    Centre,  SubCentre, ICAO )

  IF ( UpdtSeqNo == 0 ) THEN
    update = "(Original)"
  ELSE
    update = " "
  ENDIF

  CALL DataCategory ( DataCat,  IntnlSubCat, &
                      Category, IntnlCategory )
  IF ( Edition == 3 ) THEN
    IF ( DataCat == 255 ) Category = "Local sub-category"
    IntnlCategory = "not present in Edition 3"
  END IF

  WRITE ( *, FMT=Fmt91 ) 1
  WRITE ( *, FMT=Fmt21 ) len1
  WRITE ( *, FMT=Fmt12 ) BufrMasterTable, TRIM(Mtab),               &
                         OCentre,         TRIM(ADJUSTL(Centre)),    &
                         OSubCentre,      TRIM(ADJUSTL(SubCentre)), &
                         UpdtSeqNo,       TRIM(update),             &
                         DataCat,         TRIM(Category),           &
                         IntnlSubCat,     TRIM(IntnlCategory),      &
                         LocalSubCat,                               &
                         MasterTableVer,  TRIM(Mver),               &
                         LocalTableVer,   TRIM(nolocal),            &
                         Hour, Minute, Second,                      &
                         Day, Months(Month), Year

!---------------------------------------------------------------
! 3. Optional Section 2 (local data)
!---------------------------------------------------------------

  len2 = Sec2(1)

  WRITE ( *, FMT=Fmt91 ) 2
  IF ( BTEST ( is2, 7 ) ) THEN
    WRITE ( *, FMT=Fmt21 ) len2
    WRITE ( *, FMT="(A)" ) "  Section 2 present but skipped"
  ELSE
    WRITE ( *, FMT="(A)" ) "  Section 2 not present"
  END IF

!---------------------------------------------------------------
! 4. Section 3 (Descriptors)
!---------------------------------------------------------------

  len3 = Sec3(1)
  nObs = Sec3(3)
  idata = Sec3(4)


  WRITE ( *, FMT=Fmt91 ) 3
  WRITE ( *, FMT=Fmt31 ) len3
  IF ( nObs > 0 ) THEN
    WRITE ( *, FMT=Fmt32 ) nObs
  ELSE
    WRITE ( *, FMT="(A)" ) "  *** No observations present"
  END IF

  IF ( BTEST ( idata, 7 ) ) THEN
    WRITE ( *, FMT="(A)" ) "  Message contains observed data"
  ELSE
    WRITE ( *, FMT="(A)" ) "  Message contains non-observed data"
  END IF

  IF ( BTEST ( idata, 6 ) ) THEN
    WRITE ( *, FMT="(A)" ) "  Data is compressed"
  ELSE
    WRITE ( *, FMT="(A)" ) "  Data is not compressed"
  END IF

  IF ( nDescr > 0 ) THEN
    DO i = 1, nDescr
      WRITE ( *, FMT=Fmt33 ) i, Descr(i)
    END DO
  ELSE
    WRITE ( *, FMT="(A)" ) "  *** No Descriptors present"
  END IF

!---------------------------------------------------------------
! 5. Section 4 (data)
!---------------------------------------------------------------

  len4 = Sec4(1)

  WRITE ( *, FMT=Fmt91 ) 4
  WRITE ( *, FMT=Fmt41 ) len4

!---------------------------------------------------------------
! 6. Section 5 ('7777')
!---------------------------------------------------------------

  len5 = Sec5(1)
  lens = Sec5(2)

  WRITE ( *, FMT=Fmt91 ) 5
  IF ( len5 == 0 ) THEN
    WRITE ( *, FMT="(A)" ) "  Message Delimiter '7777' not present"
  ELSE IF ( lens /= lenm ) THEN
    WRITE ( *, FMT="(A)" ) "  Section lengths to not sum to total message length"
  ELSE
    WRITE ( *, FMT="(A)" ) "  OK"
  END IF

END SUBROUTINE PrintInfo
!-----------------------------------------------------------------

SUBROUTINE OutputVals ( Values,      & ! (in)
                        ExpDescr,    & ! (in)
                        cValues,     & ! (in)
                        nExpDescr,   & ! (in)
                        nObs,        & ! (in)
                        TableVersion ) ! (in)

!****s* BUFR/DECBUFR/OutputVals *
!-----------------------------------------------------------------------
!
! NAME
!   OutputVals   (decbufr.f90)
!
! SYNOPSIS
!   Output BUFR Section 4 decoded data for one message.
!
!   CHARACTER (LEN=10000) :: cValues
!   INTEGER :: nDescrr=<nd>, nObs=<no>
!   INTEGER :: Values(nObs), ExpDescr(nExpDescr)
!   CALL OutputVals ( Values, ExpDescr, cValues, nExpDescr, nObs )
!
! INPUTS
!   Values    dflt  Array of decoded values
!   ExpDescr   int  Array of expanded descriptors
!   cValues    chr  String of character data
!   nExpDescr  int  No. of descriptors
!   nObs       int  No. of observations
!
! OUTPUTS
!   Formatted values printed to stdout
!
! CALLS
!   TABLEB
!
! CALLED BY
!   DecBUFR
!
! USES
!   DecBUFRmod  - fixed parameter definitions
!
! DESCRIPTION
!   Outputs decoded BUFR message values in a simple list format.
!   Output is written to stream #8
!
! REFERENCES
!   Manual on codes. WMO-306, Vol 1.2, Part B, Nov 2007.
!   FM 94-XIII Ext. BUFR Regulations. WMO, 2008
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2011, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!-----------------------------------------------------------------------
!****

! Modules

 USE BUFRutils, ONLY: ConvertDescriptor

 IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Fmt1 = "(I6,1X,I6.6,I11)"
  CHARACTER (LEN=*), PARAMETER :: Fmt2 = "(I6,1X,I6.6,1X,A)"
  CHARACTER (LEN=*), PARAMETER :: Fmt4 = "(I6,1X,I6.6,E15.7)"

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN) :: cValues(:)   ! Decoded character values
  REAL(dp),          INTENT(IN) :: Values(:)    ! Decoded numeric values
  INTEGER,           INTENT(IN) :: ExpDescr(:)  ! Expanded list of descriptors
  INTEGER,           INTENT(IN) :: nExpDescr    ! No. of descriptors
  INTEGER,           INTENT(IN) :: nObs         ! No. of observations
  INTEGER,           INTENT(IN) :: TableVersion ! BUFR table version

! Local variables

  CHARACTER (LEN=1)  :: Format                 ! Table B element format type
  CHARACTER (LEN=60) :: Name                   ! Table B element name
  CHARACTER (LEN=24) :: Units                  ! Table B element units
  CHARACTER (LEN=30) :: Fmt3                   ! Format descriptor
  INTEGER            :: ierr                   ! Error value
  INTEGER            :: i, j                   ! Loop counters
  INTEGER            :: Descr                  ! Descriptor (16-bit format)
  INTEGER            :: FXXYYY                 ! Descriptor (fxxyyy format)
  INTEGER            :: F,X,Y                  ! Descriptor parts
  INTEGER            :: Scale                  ! Table B scale
  INTEGER            :: RefVal                 ! Table B reference value
  INTEGER            :: Width                  ! Table B bit width
  REAL(dp)           :: rVal                   ! Temporary value
  INTEGER            :: iVal                   ! Temporary value
  CHARACTER (LEN=80) :: cVal                   ! Temporary value

  REAL(dp),           &
         ALLOCATABLE :: NumVals(:,:)           ! Temporary numeric   values table
  CHARACTER (LEN=80), &
         ALLOCATABLE :: ChrVals(:,:)           ! Temporary character values table

!---------------------------------------------------------------
! 1. Allocate temporary space for re-arranged obs. store
!   and Copy data array to (obs,elem) order
!---------------------------------------------------------------

  ALLOCATE ( NumVals(1:nObs,1:nExpDescr), &
             ChrVals(1:nObs,1:nExpDescr), STAT=ierr )
  IF ( ierr /= 0 ) THEN
    WRITE ( *, FMT="(A)") &
            "Failed to allocate memory for temporary 2d arrays"
    CALL EXIT(ErrMem)
  END IF
  NumVals = RESHAPE ( Values,  (/nObs,nExpDescr/) )
  ChrVals = RESHAPE ( cValues, (/nObs,nExpDescr/) )

!---------------------------------------------------------------
! 2. Loop over all observations...
!---------------------------------------------------------------

  DO i = 1, nObs
    WRITE ( Ounit, "(/1X,'Observation number: ',I5/)") i

!---------------------------------------------------------------
! 2.1. Loop over descriptors for this ob....
!---------------------------------------------------------------

    DO j = 1, nExpDescr
      FXXYYY = ExpDescr(j)
      CALL ConvertDescriptor ( Descr, FXXYYY, F,X,Y, 2 )

      rVal = NumVals(i,j)
      iVal = NINT ( rVal )
      cVal = ChrVals(i,j)

      IF ( cVal == " " ) THEN       ! numeric element
        IF ( iVal /= Missing ) THEN

          CALL TABLEB ( X, Y,   TableVersion,  &
                        Scale,  RefVal, Width, &
                        Format, Name,   Units )

          IF ( Format == "C" .OR.  &
               Format == "F" ) THEN
            WRITE ( Ounit, FMT=Fmt1 ) j, FXXYYY, iVal
          ELSE
            IF ( Scale > 0 ) THEN
               IF ( rVal < 1.0E10 ) THEN
                 WRITE ( Fmt3, FMT="(A,I2,A,I2.2,A)" ) &
                        "(I6,1X,I6.6,F", 12+Scale, ".", Scale, ")"
                 WRITE ( Ounit, FMT=Fmt3 ) j, FXXYYY, rVal
               ELSE
                 WRITE ( Ounit, FMT=Fmt4 ) j, FXXYYY, rVal
               END IF
            ELSE
               IF ( rVal < 1.0E10 ) THEN
                 WRITE ( Ounit, FMT=Fmt1 ) j, FXXYYY, iVal
               ELSE
                 WRITE ( Ounit, FMT=Fmt4 ) j, FXXYYY, rVal
               END IF
            END IF
          END IF
        ELSE
          WRITE ( Ounit, FMT=Fmt1 ) j, FXXYYY, Missing
        END IF

      ELSE                                ! character element
        WRITE ( Ounit, FMT=fmt2 ) j, FXXYYY, TRIM(cVal)
     END IF

    END DO              ! descr loop

  END DO                ! nObs loop

!---------------------------------------------------------------
! 3. De-allocate temporary array
!---------------------------------------------------------------

  DEALLOCATE ( NumVals, ChrVals )

END SUBROUTINE OutputVals
!-------------------------------------------------------------------------

END PROGRAM DecBUFR
