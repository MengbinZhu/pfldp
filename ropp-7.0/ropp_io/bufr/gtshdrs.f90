! $Id: gtshdrs.f90 3696 2013-06-17 08:48:37Z idculv $

MODULE GTShdrs

!****m* BUFR/GTSHDRS/GTShdrs *
!
! NAME
!   GTShdrs    (gtshdrs.f90)
!
! SYNOPSIS
!   Module defining fixed values & subroutines for generating WMO GTS/RMDCN
!   routing headers, etc.
!
!   USE GTShdrs
!   INTEGER            :: CSN, DateTime(6), un
!   CHARACTER (LEN=50) :: CSNfile
!   CHARACTER (LEN=6)  :: TTAAII
!   CHARACTER (LEN=4)  :: CCCC
!   CHARACTER (LEN=10) :: cIPH
!   CHARACTER (LEN=31) :: cARH
!   CHARCATER (LEN=4)  :: cEOM
!   INTEGER            :: Lat, Lon, lIPH, lARH, lEOM
!   GTSHDR_DEBUG = .TRUE.  ! optionally enable debug mode
!   CALL GTShdrIPH ( cIPH, lIPH )
!   CALL GTShdrARH ( CSN, TTAAII, Lat, Lon, CCCC, DateTime, &
!                    cARH, lARH )
!   CALL GTShdrEOM ( cEOM, lEOM )
!   CALL GTShdrCSN ( CSNfile, CSN, 'R'|'W' )
!   un = GTShdrUNT ()
!
! USED BY
!   BUFR or GRIB encoding applications requiring GTS headers
!
! DESCRIPTION
!   A 'naked' BUFR (or GRIB) "message" starts with the characters 'BUFR'
!   (or 'GRIB') and terminates with '7777' (Ref.1). However, such a message
!   cannot be transmitted over WMO GTS (or RMDCN) links wihtout some form
!   of routing information. This is provided by appending a standard
!   "Abbreviated Routing Header" (ARH) (Ref.2) and terminating with an
!   "end-of-message" (EOM) sequence. A message with ARH & EOM wrappers
!   is known as a "bulletin" and is the basic block of bytes transmitted
!   over the GTS/RMDCN.
!   Further, as protocols allow for more than one bulletin in a physical
!   file, each bulletin is usually (though not universally, depending on
!   local GTS node procedures) prefixed by a "Internet Protocol Header" (IPH)
!   or leader sequence which contains the total bulletin length in bytes
!   (including the IPH itself) and a data format identifier. The file is
!   terminated with a dummy IPH after the last bulletin (Ref.3).
!   This module provides routines to generate an IPH (actual or dummy), an
!   ARH and an EOM sequence. The routines return these sequences of bytes
!   as plain ASCII character strings (containing non-printing control
!   characters) which can be output 'as-is' using a write routine such as
!   METDB_CWRITE() from the MetDB BUFR library (Ref.4) or - with pre-packing
!   to integers - with PBWRITE() from the ECMWF BUFR library (Ref.5).
!
! EXAMPLES
!   1) Generate an ARH with CSN sequence cycling between runs
!
!     USE GTShdrs
!     INTEGER            :: CSN, DateTime(6)=(/2011,06,30,13,32,0/)
!     CHARACTER (LEN=5)  :: TTAAII="ISX?14"
!     CHARACTER (LEN=4)  :: CCCC="EGRR"
!     CHARACTER (LEN=31) :: cARH
!     CHARACTER (LEN=50) :: CSNfile = "/data/gwv/csn.dat"
!     INTEGER            :: Lat=50, Lon=10
!     INTEGER            :: BUFRunit, lARH
!     GTSHDR_DEBUG = .TRUE.                    ! optionally enable debug mode
!     CALL GTShdrCSN ( CSNfile, CSN, 'Read' )  ! read last used CSN
!     CALL GTShdrARH ( CSN, TTAAII, Lat, Lon, CCCC, DateTime, &
!                      cARH, lARH )            ! generate ARH
!     CALL GTShdrCSN ( CSNfile, CSN, 'Write' ) ! save last CSN
!
!   2) Write ARH to BUFR file using MetDB BUFR library
!
!     INTEGER :: BUFRunit  ! from METDB_COPEN()
!     CALL METDB_CWRITE ( BUFRunit, cARH(1:lARH), lARH )
!
!   3) Write ARH to BUFR file using ECMWF BUFR library
!      (pack bytes from chr to int first)
!
!     INTEGER :: BUFRunit  ! from PBOPEN()
!     INTEGER :: uARH(31), pARH(8), ierr
!     DO i = 1, lARH
!       uARH(i) = IACHAR(cARH(i:i))
!     END DO
!     CALL SBYTES  ( pARH, uARH, 0, 8, 0, lARH )
!     CALL PBWRITE ( BUFRunit, pARH, lARH, ierr )
!
! REFERENCES
!   1) Manual on the Global Telecommunications System.
!      Vol.1, Part II. WMO, Geneva, 1986 (and updates)
!   2) WMO (2009). Operational Procedures for the GTS, Attachment II-5,
!      Data Designators T1T2A1A2ii in Abbreviated Headings, Table C3.
!      WMO, Geneva, 4 November 2009.
!   3) Nightingale, S. (2008). Exchange of data with the
!      Met Office FROST message switch.
!      FROST FTP Protocol Document, V2.1, 2 October 2008.
!   4) Met Office (2011). Decoding and Encoding BUFR messages.
!      MetDB Technote 1, Rev.4, February 2011 [dmtn1.html].
!   5) Dragosavac, Milan (2009). BUFR User's Guide.
!      ECMWF Operations Department Technical Note, July 2009.
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!---------------------------------------------------------------------------------
!****

! Fixed local parameters

  INTEGER, PARAMETER, PRIVATE :: nbpc = 8                 ! bits per character (or byte)
  INTEGER, PARAMETER, PRIVATE :: nbpw = KIND(nbpc) * nbpc ! bits per word (default INTEGER)
  INTEGER, PARAMETER, PRIVATE :: nbsk = 0                 ! no. bits to skip when packing

  LOGICAL :: GTSHDR_DEBUG = .FALSE. ! local debug flag

CONTAINS
!---------------------------------------------------------------------------------

SUBROUTINE GTShdrIPH ( Lenm, & ! (in)
                       cIPH, & ! (out)
                       lIPH )  ! (out)

!---------------------------------------------------------------------------------
!****s* BUFR/GTSHDRS/GTShdrIPH *
!
! NAME
!   GTShdrIPH   (gtshdrs.f90)
!
! SYNOPIS
!   Generate IP start-of-message header (IPH) sequence
!
!   USE GTShdrsMod
!   CHARACTER (LEN=10) :: cIPH
!   INTEGER :: Lenm, lIPH
!   CALL GTShdrIPH ( Lenm, cIPH, lIPH )
!
! INPUT
!   Lenm  int  Length of GTS bulletin in bytes (including ARH but not IPH)
!              to set into IPH (0-99999989)
!
! OUTPUT
!   cIPH  chr  Chracter string of at least (10) for IPH sequence
!   lIPH  int  Length of IPH in bytes (normally 10)
!
! DESCRIPTION
!   Generates a 10-byte start-of-message header sequence for IP transmission
!   protocols according to WMO Format 00 (see Reference 1).
!   This header should be output immediately before the GTS bulletin
!   (BUFR message plus ARH & start/end wrappers generated by GTShdrARH &
!   GTShdrEOM).
!   The length of the bulletin, plus the IP header itself, is set into
!   the header. If Len is given as 0, this is assumed to be a dummy
!   IP header, and 10 '0's are returned in IPH. This dummy IPH is used to
!   terminate a file containing one or more bulletins intended for
!   transmission using IP (GTS/RMDCN etc). The IPH sequence is returned
!   as a plain ASCII string.
!
! REFERENCES
!   1) Nightingale, S. (2008). Exchange of data with the
!      Met Office FROST message switch.
!      FROST FTP Protocol Document, V2.1, 2 October 2008.
!
! SEE ALSO
!   GTShdrARH() - to generate a GTS Abbreviated Routing Header
!   GTShdrEOM() - to generate a GTS End-of-Message sequence
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!---------------------------------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  INTEGER, PARAMETER   :: niph = 10  ! Length of IPH

! Argument list parameters

  INTEGER,           INTENT(IN)  :: Lenm ! GTS bulletin length (incl. ARH+EOM)
  CHARACTER (LEN=*), INTENT(OUT) :: cIPH ! Plain character IPH
  INTEGER,           INTENT(OUT) :: lIPH ! Length of IPH (bytes)

! Check IPH is big enough - need at least 10 characters

  IF ( LEN(cIPH) < niph ) THEN
    cIPH = " "
    WRITE ( *, FMT="(A,I2)" ) "ERROR: (GTShdrIPH) String is too small for"//&
                              " IPH. LEN must be at least ",niph

! If Lenm<=0 generate dummy IPH with 8 zeros (no Format Identifier),
! else the message length is coded into bytes 1-8 with the Format Identifier
! (Bytes 9-10) always '00' (See Ref.1)

  ELSE
    IF ( Lenm >  0 .AND. &
         Lenm <= 999999999 ) THEN
      WRITE ( cIPH, FMT="(I8.8,A2)" ) Lenm, "00"
    ELSE
      cIPH = "00000000"
    END IF

  END IF

  lIPH = LEN_TRIM(cIPH)

END SUBROUTINE GTShdrIPH
!---------------------------------------------------------------------------------

SUBROUTINE GTShdrARH ( CSN,      & ! (inout)
                       TTAAII,   & ! (in)
                       Lat,      & ! (in)
                       Lon,      & ! (in)
                       CCCC,     & ! (in)
                       DateTime, & ! (in)
                       cARH,     & ! (out)
                       lARH )      ! (out)

!---------------------------------------------------------------------------------
!****s* BUFR/GTSHDRS/GTShdrARH *
!
! NAME
!   GTShdrARH    (gtshdrs.f90)
!
! SYNOPSIS
!   Generate WMO Abbreviated Routing Header (ARH) sequence
!   for GTS dissemination
!
!   INTEGER            :: CSN, DateTime(6), Lat, Lon, lARH
!   CHARACTER (LEN=6)  :: TTAAII
!   CHARACTER (LEN=4)  :: CCCC
!   CHARACTER (LEN=31) :: cARH
!   CALL GTShdrARH ( CSN, TTAAII, Lat, Lon, CCCC, DateTime, cARH, lARH )
!
! INPUTS
!   CSN       int  Channel (GTS bulletin) sequence number (001-999)
!   TTAAII    chr  6-chr data ID (t1t2a1a2ii) Note that the a2 character
!                  will be set by this routine from (Lat,Lon) info.
!   Lat       int  Latitude  of data (-90  to +90 deg.)
!   Lon       int  Longitude of data (-180 to +180 or 0 to 360 deg.)
!   CCCC      chr  4-chr GTS source node ID (ICAO code) (cccc)
!   DateTime  int  6-element date/time (yr,mon,day,hr,min,sec)
!
! OUTPUTS
!   CSN       int  Incremented Channel (GTS bulletin) sequence number (001-999)
!   cARH      chr  Character string of at least (31) for ARH sequence
!   lARH      int  Length of ARH in bytes (normally 31)
!
! DESCRIPTION
!   Codes data routing header information into WMO/GTS standard
!   31-byte abbreviated routing header (ARH) (see references) of the form:
!     'sssrrriii'
!   where:
!     'sss' is a 10-byte message start sequence: 'SOH/CR/CR/LF/CSN/CR/CR/LF'
!     'rrr' is the 18-byte routing header:       't1t2a1a2ii cccc YYGGgg'
!     'iii' is a 3-byte data introducer iii:     'CR/CR/LF'.
!   The channel sequence number (CSN) should be a sequential 3-digit
!   integer cycling from 001 to 999, and is coded from the CSN value.
!   Data ID (t1t2a1 and ii) is coded as given. Geographical area code
!   (a2) is derived from given lat/lon of data. Source node (cccc) is
!   the 4-chr ICAO code of the centre coding the data (e.g. 'EGRR' for
!   Met Office, Exeter). Day of month (YY), hour (GG) and minute (gg)
!   are coded from DateTime values. The ARH is returned as a plain ASCII
!   string (containing non-printing control characters). The message
!   should then be terminated with an end-of-message sequence, e.g. as
!   generated with GTShdrEOM() to form a complete GTS bulletin. An optional
!   IPH may be needed for transmission of files via TCP/IP protocol;
!   this can be generated using GTShdrIPH().
!
! REFERENCES
!   1) WMO (2009). Operational Procedures for the GTS, Attachment II-5,
!      Data Designators T1T2A1A2ii in Abbreviated Headings, Table C3.
!      WMO, Geneva, 4 November 2009.
!   2) Nightingale, S. (2008). Exchange of data with the
!      Met Office FROST message switch.
!      FROST FTP Protocol Document, V2.1, 2 October 2008.
!
! SEE ALSO
!   GTShdrIPH() - to generate a GTS IP Header
!   GTShdrEOM() - to generate a GTS End-of-Message sequence
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!---------------------------------------------------------------------------------
!****
!

  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: ALPHAB = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  CHARACTER (LEN=1), PARAMETER :: SOH    = ACHAR(1)
  CHARACTER (LEN=3), PARAMETER :: CRCRLF = ACHAR(13)//ACHAR(13)//ACHAR(10)
  INTEGER,           PARAMETER :: narh   = 31  ! Length of ARH

! Argument list parameters

  INTEGER,           INTENT(INOUT) :: CSN         ! Channel sequence no.
  CHARACTER (LEN=*), INTENT(IN)    :: TTAAII      ! T1T2A1A2ii part of ARH
  INTEGER,           INTENT(IN)    :: Lat, Lon    ! Latitude & Longitude
  CHARACTER (LEN=*), INTENT(IN)    :: CCCC        ! Orininating centre ICAO code
  INTEGER,           INTENT(IN)    :: DateTime(:) ! Date/Time (Y,M,D,h,m,s)
  CHARACTER (LEN=*), INTENT(OUT)   :: cARH        ! Plain character ARH
  INTEGER,           INTENT(OUT)   :: lARH        ! ARH length (bytes)

! Local parameters

  INTEGER                        :: LL          ! Location area code index

! Check ARH is big enough - need at least 31 characters

  IF ( LEN(cARH) < narh ) THEN
    cARH    = " "
    lARH    = 0
    WRITE ( *, FMT="(A,I2)" ) "ERROR: (GTShdrARH) String is too small for"//&
                              " ARH. LEN must be at least ",narh

  ELSE

! Start-of-message sequence (SOH/CR/CR/LF/CSN/CR/CR/LF) [10 bytes]
! Increment CSN (cycle in valid range 001-999) before use

    cARH(1:4)  = SOH // CRCRLF
    CSN = MOD ( MAX(CSN,0), 999 ) + 1
    WRITE ( cARH(5:7), FMT="(I3.3)" ) CSN
    cARH(8:10) = CRCRLF

! Data ID (T1T2A1A2ii) [6 bytes]

    cARH(11:16) = TTAAII

! Area code A-L (A2) for Longitude segments 0-90W, 90W-180, 180-90E, 90E-0
! and for Latitude bands 90N-30N, 30N-30S, 30S-90S

    LL = 4 - MOD(Lon+360,360) / 90
    LL = MIN ( MAX ( LL, 1 ), 4 )
    IF ( Lat .LT. -30 ) THEN
      LL = LL + 8
    ELSE IF ( Lat .LT. 30 ) THEN
      LL = LL + 4
    END IF
    cARH(14:14) = ALPHAB(LL:LL)

! Originating centre (ICAO code CCCC) [6 bytes incl. spaces]

    cARH(17:22) = " " // CCCC // " "

! Day of month & time [6 bytes]

    WRITE ( cARH(23:28), FMT="(3I2.2)" ) DateTime(3:5)

! Data introducer sequence (CR/CR/LF) [3 bytes]

    cARH(29:31) = CRCRLF

    lARH = narh
  END IF

END SUBROUTINE GTShdrARH
!---------------------------------------------------------------------------------

SUBROUTINE GTShdrEOM ( cEOM, & ! (out)
                       lEOM )  ! (out)

!---------------------------------------------------------------------------------
!****s* BUFR/GTSHDRS/GTShdrEOM *
!
! NAME
!   GTShdrEOM   (gtshdrs.f90)
!
! SYNOPIS
!   Generate WMO GTS end-of-message (EOM) sequence
!
!   USE GTShdrsMod
!   CHARACTER (LEN=4) :: cEOM
!   INTEGER :: lEOM
!   CALL GTShdrEOM ( cEOM, lEOM )
!
! INPUT
!   NONE
!
! OUTPUT
!   cEOM  chr  Character string of at least (4) for plain character EOM
!   lEOM  int  Length of EOM in bytes (normally 4)
!
! DESCRIPTION
!   Generates WMO GTS 4-byte end-of-message sequence 'CR/CR/LF/ETX' used to
!   terminate a WMO/GTS bulletin (such as BUFR or GRIB message with routing
!   headers) according to WMO Format 00 (see Reference 1).
!   The EOM is returned as a plain ASCII string (containing non-printing
!   control chracters).
!   The message should have been prefixed with an ARH (e.g. as generated
!   using GTShdrARH()) and optionally by an IP leader (GTShdrIPH()) if
!   intended for transmission over TCP/IP links.
!
! REFERENCES
!   1) Bridgeman, D. & Little, C. (2004). Exchange of data with the
!      Met Office FROST message switch.
!      FROST FTP Protocol Document, V1.0, 31 October 2004.
!
! SEE ALSO
!   GTShdrARH() - to generate a GTS Abbreviated Routing Header
!   GTShdrIPH() - to generate an IP Header for TCP/IP transmission
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!---------------------------------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  INTEGER,              PARAMETER :: neom = 4                       ! Length of EOM
  CHARACTER (LEN=neom), PARAMETER :: aEOM = ACHAR(13)//ACHAR(13)//& ! CR/CR/LF/ETX
                                            ACHAR(10)//ACHAR(3)

! Argument list parameters

  CHARACTER (LEN=*), INTENT(OUT) :: cEOM     ! Plain character EOM
  INTEGER,           INTENT(OUT) :: lEOM     ! Length of EOM (bytes)

! Check ARH is big enough - need at least 4 characters

  IF ( LEN(cEOM) < neom ) THEN
    cEOM    = " "
    lEOM    = 0
    WRITE ( *, FMT="(A,I2)" ) "ERROR: (GTShdrEOM) String is too small for"//&
                              " EOM. LEN must be at least ",neom

  ELSE
    cEOM = aEOM
    lEOM = neom
  END IF

END SUBROUTINE GTShdrEOM
!---------------------------------------------------------------------------

SUBROUTINE GTShdrCSN ( CSNfile, & !(in)
                       CSN,     & !(inout)
                       RorW )     !(in)

!****s* BUFR/GTSHDRS/GTShdrCSN *
!
! NAME
!   GTShdrCSN   (gtshdrs.f90)
!
! SYNOPSIS
!   Read or save a channel sequence number
!
!    CHARACTER (LEN=100) :: csnfile
!    INTEGER :: csn
!    CHARACTER (LEN=1) :: rw = ['R'|'W']
!    CALL GTShdrCSN ( csnfile, csn, rw )
!
! INPUTS
!   CSNfile  chr  Channel sequence number file name
!   CSN      int  Channel sequence number (001-999) (write)
!   RorW     chr  'R' or 'r' for input or 'W' or 'w' for output
!
! OUTPUTS
!   CSN      int  Channel sequence number (001-999) (read)
!
! ERRORS
!   If the CSN file cannot be opened for read, or the CSN is not a valid
!   integer, a default sequence initialisation CSN value will be returned as
!   zero. Otherwise, CSN values read from the file or passed to this routine
!   for write will always be returned in the range 001-999.
!
! CALLS
!   GTShdrUNT
!
! DESCRIPTION
!   Reads (if RorW='R') or writes (if RorW='W') a channel sequence number
!   (which should be in the range 001-999) from/to the given file. If the
!   file name is blank, or the first character of RorW is not 'R' or 'W',
!   nothing happens (RorW is case-insensitive).
!   Warning messages are written to stdout if any I/O error occurs (the value
!   of CSN is unchanged), but otherwise the action is silent unless the
!   debug mode is enabled.
!   The file name may include an absolute path, and should be accessible for
!   read & write. The file should also be in a stable location so as to be
!   available for subsequent program runs - don't use places such as /tmp or
!   /var/tmp, etc.
!   Note that GTShdrARH() will increment CSN (and keep it within the valid
!   range 1-999) before coding into the ARH; this routine merely reads the
!   value from the file and saves the given value back, unchanged (apart
!   from ensuring that the value is within valid range).
!
! SEE ALSO
!   GTShdrARH
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
!
!   Use, duplication or disclosure of this code is subject to the restrictions
!   as set forth in the contract. If no contract has been raised with this
!   copy of the code, the use, duplication or disclosure of it is strictly
!   prohibited. Permission to do so must first be obtained in writing from
!   the Head of Satellite Applications at the following address:
!      Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!---------------------------------------------------------------------------
!****

! Fixed parameters

  CHARACTER (LEN=*), PARAMETER :: fmt1 = "(A,I3.3,A)"
  CHARACTER (LEN=*), PARAMETER :: fmt2 = "(I3.3)"

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)    :: CSNfile
  INTEGER,           INTENT(INOUT) :: CSN
  CHARACTER (LEN=*), INTENT(IN)    :: RorW

! Local variables

  CHARACTER (LEN=10) :: number
  INTEGER :: ierr, num
  INTEGER :: CSNunit
  LOGICAL :: exists

  IF ( CSNfile /= " " ) THEN
    CSNunit = GTShdrUNT()

!-------------------------------------------------------------
! 1. Read last used bulletin sequence number...
!-------------------------------------------------------------

    IF ( RorW(1:1) == "R" .OR. &
         RorW(1:1) == "r" ) THEN
      CSN = 0                              ! default return value

      INQUIRE ( FILE=CSNfile, &            ! does the file pre-exist?
                EXIST=exists )

      IF ( exists ) THEN                   ! yes, attempt to open it
        OPEN ( FILE=CSNfile, &
               UNIT=CSNunit, &
             ACTION="READ",  &
             IOSTAT=ierr )

        IF ( ierr == 0 ) THEN              ! if ok, attempt to read CSN
          READ ( UNIT=CSNunit, &
                  FMT="(A)",   &
               IOSTAT=ierr ) number
          READ ( number, &
                  FMT=*, &
               IOSTAT=ierr ) num

          IF ( ierr == 0 ) THEN            ! if ok, check range & return value
            CSN = MOD ( MAX(num,1)-1, 999 ) + 1
            IF ( GTSHDR_DEBUG ) THEN
              WRITE ( *, FMT=fmt1 ) &
                       "DEBUG: (GTShdrCSN) Read CSN ", CSN, " from "// &
                       TRIM(CSNfile)
            END IF

          ELSE
            WRITE ( *, FMT=fmt1 ) &
                       "WARNING: (GTShdrCSN) Invalid last value '"// &
                       TRIM(ADJUSTL(number))//"' - using CSN ", CSN
          END IF

        ELSE
          WRITE ( *, FMT=fmt1 ) "WARNING: (GTShdrCSN) Read file open error "// &
                                 TRIM(CSNfile)//" - using CSN ", CSN
        END IF

      ELSE
        WRITE ( *, FMT=fmt1 ) "WARNING: (GTShdrCSN) File not found "// &
                               TRIM(CSNfile)//" - using CSN ", CSN

      END IF

!-------------------------------------------------------------
! 2. ...or save current bulletin sequence number
!-------------------------------------------------------------

    ELSE IF ( RorW(1:1) == "W" .OR. &
              RorW(1:1) == "w" ) THEN

      OPEN ( FILE=CSNfile, &               ! attempt to open file (create if new)
             UNIT=CSNunit, &
           ACTION="WRITE", &
           IOSTAT=ierr )

      IF ( ierr == 0 ) THEN                ! if ok, check range & save value
        CSN = MOD ( MAX(CSN,1)-1, 999 ) + 1
        WRITE ( UNIT=CSNunit, &
                 FMT=fmt2,    &
              IOSTAT=ierr ) CSN

        IF ( ierr == 0 ) THEN
          IF ( GTSHDR_DEBUG ) THEN
            WRITE ( *, FMT=fmt1 ) &
                       "DEBUG: (GTShdrCSN) Saved CSN ", CSN, " to "// &
                       TRIM(CSNfile)
          ENDIF

        ELSE
          WRITE ( *, FMT=fmt1 ) &
                       "WARNING: (GTShdrCSN) Write error to "// &
                       TRIM(CSNfile)//" - CSN not saved"
        END IF

      ELSE
        WRITE ( *, FMT=fmt1 ) &
                       "WARNING: (GTShdrCSN) File open error for "// &
                       TRIM(CSNfile)//" - CSN not saved"
      END IF

    END IF

    CLOSE ( UNIT=CSNunit, &
          IOSTAT=ierr )

  END IF

END SUBROUTINE GTShdrCSN
!-----------------------------------------------------------------------

FUNCTION GTShdrUNT() RESULT(unit)

!****f* BUFR/GTSHDRS/GTShdrUNT *
!-----------------------------------------------------------------------
!
! NAME
!   GTShdrUNT   (gtshdrs.f90)
!
! SYNOPSIS
!   Obtain a free Fortran unit number
!
!   USE GTShdrs
!   INTEGER :: unit
!   unit = GTShdrUNT()
!
! INPUTS
!   None
!
! OUTPUTS
!   GTShdrUNT   (funcn)  Unit number (10-999 or 0 if no free numbers)
!
! ERRORS
!   If there are no free unit numbers in the range 10-999, then
!   a warning message is written to stdout and 'unit' is returned
!   with a value of zero.
!
! CALLED BY
!   GTShdrCSN
!
! DESCRIPTION
!   This function returns the first available unit number larger than 10, but
!   no larger than 999.
!   If no available unit number is found, a warning message will be written
!   to stdout, and the unit number will be returned as zero. This may be
!   valid for most compilers, so if free, the file open may still work.
!   Other compilers may give a file OPEN() 'invalid unit number' error.
!
! AUTHOR
!   D. Offiler, Met Office, Exeter, UK.
!
! COPYRIGHT
!   (c) Crown copyright 2013, Met Office. All rights reserved.
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

  INTEGER :: unit
  LOGICAL :: unitopen

  unit     = 9
  unitopen = .TRUE.

  DO WHILE ( unitopen .AND. unit < 1000 )
    unit = unit + 1
    INQUIRE ( UNIT=unit, OPENED=unitopen )
  END DO

  IF ( unit > 999 ) THEN
    WRITE ( *, "(A)" ) "WARNING: (GTShdrUNT) No free units between 10 and 999"
    unit = 0
  ENDIF

END FUNCTION GTShdrUNT

!---------------------------------------------------------------------------------
END MODULE GTShdrs
