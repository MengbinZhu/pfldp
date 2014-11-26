! $Id: testtables.f90 89 2012-05-21 13:43:23Z frdo $

PROGRAM TestTables

!****e* BUFR/TESTTABLES/TestTables *
!-----------------------------------------------------------------------
!
! NAME
!   testtables    (testtables.f90)
!
! SYNOPSIS
!   Test BUFR look-up tables
!
!   > testtables
!   <interactive user input>
!
! CALLS
!   CODFLG
!   DESFXY
!   IDES
!   DataCategory
!   OrigCentre
!   MasterTables
!   TABELB
!   TABLED
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
!   BUFR_LIBRARY  - path to above files (terninated with '/')
!
! DESCRIPTION
!   When run, program prompts for a descriptor in FXXYYY format.
!   The program then detects the type of descriptor, (element,
!   replicator, operator, sequence) and for an element, looks up
!   the Table B details. If it's a code or flag table, it asks
!   for a value and also looks up the text associated with the
!   given value. For a sequence, the Table D sequence is displayed
!   (but not expanded). If an operator descriptor, the type is
!   listed by its XX value. If a replication descriptor, the
!   replication is interpreted by the XX & YYY values.
!   If the first character entered is 'M' then a following numeric
!   value (0-255) is assumed to be a Master Table Version number.
!   If the first character entered is 'A' then a following numeric
!   value (0-255) is assumed to be a Table A code value.
!   Compile with MetDB kernel BUFR library. Local BUFR tables must
!   be in the directory pointed to by the environment variable
!   'BUFR_LIBRARY'. Sumamry help can be obtained by entering ?, -h
!   or --help at the FXY prompt.
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
!-------------------------------------------------------
!****

! Modules

  USE sec1tables

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (LEN=*), PARAMETER :: Version = "V1.09 - 27-Mar-2011"

  INTEGER,           PARAMETER :: TableVersion = -99  ! latest

! Local variables

  CHARACTER (LEN=40) :: Input               ! User input string
  CHARACTER (LEN=80) :: Words               ! Code/flag description
  CHARACTER (LEN=1)  :: Format              ! Table B element type
  CHARACTER (LEN=24) :: Units               ! Table B element units
  CHARACTER (LEN=64) :: Name                ! Table B element name
  CHARACTER (LEN=4)  :: ICAO                ! Centre ICAO code letters
  CHARACTER (LEN=12) :: cScale, cRefVal, cWidth ! Values in character format
  CHARACTER (LEN=30) :: cLoVal, cHiVal      ! Values in character format
  CHARACTER (LEN=10) :: rfmt                ! Variable format descriptor for floats
  INTEGER            :: seq(99)             ! Table D descriptor sequence
  INTEGER            :: FXY                 ! Input descriptor (FXXYYY)
  INTEGER            :: F,X,Y               ! Descriptor parts
  INTEGER            :: Descr               ! Descriptor code
  INTEGER            :: Val, Val1, Val2     ! Value(s) to decode
  INTEGER            :: Ocen                ! Originating centre code
  INTEGER            :: last_FXY=001001     ! Previous descriptor
  INTEGER            :: Scale               ! Table B scale value
  INTEGER            :: RefVal              ! Table B refernce value
  INTEGER            :: Width               ! Table B bit width
  INTEGER            :: nseq                ! No. of descrs in Table D seq.
  INTEGER            :: i                   ! Loop counter
  INTEGER            :: ierr                ! Error code
  REAL               :: fLoVal, fHiVal      ! Valid range of values (floats)

! Functions

  INTEGER            :: IDES  ! Converts descriptor in fxxyyy to internal format

! Start

  DO
    WRITE ( *, FMT="(/1X,A,I6.6,A)", ADVANCE="NO" ) &
                          "FXY (e.g. ",last_FXY,") : "
    READ  ( *, FMT="(A)" ) Input

! User wants help

    IF ( Input(1:1) == "?"  .OR. &
         Input(1:2) == "-h" .OR. &
         Input(1:6) == "--help" ) THEN

      WRITE ( *, FMT="(/A/A/A,I6.6,A)" ) &
        "Usage:", &
        " > testtables", &
        "   FXY (e.g. ", last_FXY,") :  <user input>"
      WRITE ( *, FMT="(A)" ) &
        "where <user input> can be:", &
        " - blank                         : re-use last descriptor", &
        " - a Master Table &   version    : Mn,nnn",       &
        " - an Orig. centre & subcentre   : Onnn,nnn",     &
        " - a Data category & subcategory : Cnnn,nnn",     &
        " - a Table B element  descriptor : 0xxyyy",       &
        " - a Replication      descriptor : 1xxyyy",       &
        " - a Table C operator descriptor : 2xxyyy",       &
        " - a Table D sequence descriptor : 3xxyyy",       &
        " - this summary help             : ?|-h|--help",  &
        " - program version ID            : -v|--version", &
        " - exit program                  : E|e|Q|q|-"
      WRITE ( *, FMT="(A)" ) &
        "If the element descriptor is a Code or Flag Table entry, the program ", &
        "will also prompt for a code/flag value; enter any decimal number within", &
        "the valid range. In addition, flag values may be entred in hex, octal or", &
        "binary format by prefixing the number with x, o or b, respectively. E.g." , &
        "'b1101' will display the interpretation of bits 1, 3 and 4.", &
        "If decoding a local originating sub-centre, there will also be a prompt",&
        "for the originating centre code (enter 0-255 or any other value to use a",&
        "default table - normally 74, Met Office)."
      CYCLE

    ELSE IF ( Input(1:2) == "-v" .OR. &
              Input(1:9) == "--version" ) THEN
      WRITE ( *, FMT="(A)" ) "testtables: Version "//TRIM(Version)
      CYCLE

! User has finished - exit/quit

    ELSE IF ( Input(1:1) == "-" .OR. &
              Input(1:1) == "E" .OR. &
              Input(1:1) == "e" .OR. &
              Input(1:1) == "Q" .OR. &
              Input(1:1) == "q" ) THEN
      EXIT

! Master Table/Version

    ELSE IF ( Input(1:1) == "M" .OR. &
              Input(1:1) == "m" ) THEN
      READ ( Input(2:), *, IOSTAT=ierr ) Val1, Val2
      CALL MasterTable ( Val1, Val2, Name, Words )
      WRITE ( *, FMT="(1X,A)" ) " = "//TRIM(Name)  // " (" // &
                                       TRIM(Words) // ")"
      CYCLE

! Originating centre/Sub-Centre

    ELSE IF ( Input(1:1) == "O" .OR. &
              Input(1:1) == "o" ) THEN
      READ ( Input(2:), *, IOSTAT=ierr ) Val1, Val2
      CALL OrigCentre ( Val1, Val2, Name, Words, ICAO )
      WRITE ( *, FMT="(1X,A)" ) " = "//TRIM(Name)  // " [" // &
                                       ICAO // "] ("       // &
                                       TRIM(Words) // ")"
      CYCLE

! Data Category/Sub-Category

    ELSE IF ( Input(1:1) == "C" .OR. &
              Input(1:1) == "c" ) THEN
      READ ( Input(2:), *, IOSTAT=ierr ) Val1, Val2
      CALL DataCategory ( Val1, Val2, Name, Words )
      WRITE ( *, FMT="(1X,A)" ) " = "//TRIM(Name)  // " (" // &
                                       TRIM(Words) // ")"
      CYCLE
    END IF

! Normal Table B/D descriptor

    READ  ( Input, FMT="(BN,I6)", IOSTAT=ierr ) FXY
    IF ( ierr /= 0 .OR. FXY < 0 ) THEN
      EXIT
    ELSE IF ( FXY == 0 ) THEN
      FXY = last_FXY
    ELSE
      last_FXY = FXY
    ENDIF

    F = FXY / 100000
    X = MOD ( FXY, 100000 ) / 1000
    Y = MOD ( FXY, 1000 )

    SELECT CASE (F)

! --- Table B element Descriptor ---

      CASE (0)
        CALL TABLEB ( X, Y, TableVersion,         &
                            Scale, RefVal, Width, &
                            Format, Name, Units )
        IF ( Width > 0 ) THEN
          WRITE ( cScale,  '(I12)', IOSTAT=ierr ) Scale
          WRITE ( cRefVal, '(I12)', IOSTAT=ierr ) RefVal
          WRITE ( cWidth,  '(I12)', IOSTAT=ierr ) Width
          WRITE ( *, FMT="(2X,'Name=',A/1X,'Units=',A,'  Format=',A,&
                           &'  Scale=',A,'  Ref=',A,'  Width=',A)" ) &
                          TRIM(ADJUSTL(Name)),    &
                          TRIM(ADJUSTL(Units)),   &
                          Format,                 &
                          TRIM(ADJUSTL(cScale)),  &
                          TRIM(ADJUSTL(cRefVal)), &
                          TRIM(ADJUSTL(cWidth))

          IF ( Format == "N" .AND. Scale == 0 ) THEN      ! Numeric (unscaled) value
            WRITE ( cLoVal, FMT='(I12)' ) RefVal
            WRITE ( cHiVal, FMT='(I12)' ) refVal + ( 2**Width - 2 )
            WRITE ( *, FMT='(" Range=",A," to ",A)' ) &
                           TRIM(ADJUSTL(cLoVal)),     &
                           TRIM(ADJUSTL(cHiVal))


          ELSE IF ( Format == "N" .AND. Scale /= 0 .OR. &
                    Format == "R" ) THEN                   ! Real (scaled float) value
            WRITE ( cScale,  '(I12)', IOSTAT=ierr )  MAX(Scale,0)
            WRITE ( rfmt, FMT='("(F",I2,".",A,")")' ) LEN(cHiVal), &
                                             TRIM(ADJUSTL(cScale))

            fLoVal = FLOAT ( RefVal ) / 10.0**Scale
            WRITE ( cLoVal, FMT=rfmt ) fLoVal
            IF ( Scale == 0 ) THEN
              i = INDEX ( cLoVal, "." )
              cLoVal(i:) = " "
            ELSE
              i = LEN(cLoVal)
              DO WHILE ( i               >   1  .AND. &
                         cLoVal(i:i)     == "0" .AND. &
                         cLoVal(i-1:i-1) /= "." )
                cLoVal(i:i) = " "
                i = i - 1
              END DO
            END IF
            fHiVal = ( 2.0d0**Width - 2.0d0 ) / 10.0**Scale + fLoVal
            WRITE ( cHiVal, FMT=rfmt ) fHiVal
            IF ( Scale == 0 ) THEN
              i = INDEX ( cHiVal, "." )
              cHiVal(i:) = " "
            ELSE
              i = LEN(cHiVal)
              DO WHILE ( i               >   1  .AND. &
                         cHiVal(i:i)     == "0" .AND. &
                         cHiVal(i-1:i-1) /= "." )
                cHiVal(i:i) = " "
                i = i - 1
              END DO
            END IF

            WRITE ( *, FMT='(" Range=",A," to ",A)' ) &
                           TRIM(ADJUSTL(cLoVal)),     &
                           TRIM(ADJUSTL(cHiVal))

          ELSE IF ( Format == "C" ) THEN                   ! Code table value
            WRITE ( cHiVal, FMT='(I12)' ) refVal + ( 2**Width - 2 )
            WRITE ( *, '(" Range=0 to ",A)' ) &
                          TRIM(ADJUSTL(cHiVal))

            WRITE ( *, FMT="(1X,A)", ADVANCE="NO" ) "Code value : "
            READ  ( *, *, IOSTAT=ierr ) Val
            IF ( ierr /= 0 .OR. Val < 0 .OR. Val >= 2**Width ) THEN
              Words = "INVALID"
            ELSE
              Descr = IDES ( FXY )
              IF ( FXY   == 001034 ) THEN
                WRITE ( *, FMT="(1X,A)", ADVANCE="NO" ) "Originating Centre : "
                READ  ( *, FMT="(A)",  IOSTAT=ierr ) Input
                READ  ( Input, "(I8)", IOSTAT=ierr ) Ocen
                IF ( ierr /= 0 ) &  ! not numeric - assume ICAO or name
                  CALL OrigCentre ( Ocen, Val1, Input, Words, ICAO, &
                                    Use="CenNam" )
                CALL OrigCentre ( Ocen, Val, Name, Words, ICAO )
                Words = TRIM(Words) // " [" // TRIM(Name) // &
                          "/" // ICAO // "]"
              ELSE
                CALL CODE ( Descr, Val, Words )
              END IF
              IF ( Words == " " ) THEN
                IF ( Val == 2**Width-1 ) THEN
                  Words = "MISSING"
                ELSE
                  Words = "UNKNOWN"
                END IF
              END IF
            END IF
            WRITE ( *, FMT="(1X,A)" ) " = "//TRIM(ADJUSTL(Words))

          ELSE IF ( Format == "F" ) THEN                   ! Flag table value
            WRITE ( cHiVal, FMT='(I12)' ) 2**(Width-1) - 1
            WRITE ( *, FMT='(" Range=0 to ",A)' )     &
                          TRIM(ADJUSTL(cHiVal))

            WRITE ( *, FMT="(1X,A)", ADVANCE="NO" ) "Flag value : "
            READ  ( *, FMT="(A)", IOSTAT=ierr ) Input
            SELECT CASE (Input(1:1))
              CASE ("x")                                ! hex
                READ ( Input(2:), "(BN,Z10)" ) Val
              CASE ("o")                                ! octal
                READ ( Input(2:), "(BN,O20)" ) Val
              CASE ("b")                                ! binary
                READ ( Input(2:), "(BN,B40)" ) Val
              CASE DEFAULT
                IF ( INDEX ( Input(1:1), "0123456789" ) > 0 ) Input(1:1) = " "
                READ ( Input, "(BN,I10)", IOSTAT=ierr ) Val
                IF ( ierr /= 0 ) Val = -999
            END SELECT

            IF ( Val < 0 .OR. Val >= 2**Width ) THEN
              WRITE ( *, FMT="(1X,A)" ) " = INVALID"
            ELSE IF ( Val == 2**Width-1 ) THEN
              WRITE ( *, FMT="(1X,A)" ) " = MISSING"
            ELSE
              Descr = IDES ( FXY )
              DO i = 0, Width-2
                IF ( BTEST ( Val, i ) ) THEN
                  CALL CODE ( Descr, i+1, Words )
                  IF ( Words .EQ. " " ) Words = "UNKNOWN"
                  WRITE ( *, FMT="(1X,I2,3A)" ) i+1,"=",TRIM(ADJUSTL(Words))
                END IF
              END DO
            END IF

          END IF

        ELSE
          WRITE ( *, * ) "Element not in Table B"
        END IF

! ---  Replication Descriptor ---

      CASE (1)
        WRITE ( *, FMT="(1X,A,I3,A,I4,A)" ) "Replicate next",  &
                                            X, " Descriptors", &
                                            Y, " times"

! --- Operator Descriptor ---

      CASE (2)
       SELECT CASE (X)
          CASE (01)
            IF ( Y == 0 ) THEN
              WRITE ( *, FMT="(1X,A)"      ) "Change data bit width back to Table B value"
            ELSE
              WRITE ( *, FMT="(1X,A,I4,A)" ) "Change data width by", Y-128," bits"
            END IF
          CASE (02)
            IF ( Y == 0 ) THEN
              WRITE ( *, FMT="(1X,A)"    ) "Change scale back to Table B value"
            ELSE
              WRITE ( *, FMT="(1X,A,I4)" ) "Change Scale by", Y-128
            END IF
          CASE (03)
            WRITE ( *, FMT="(1X,A,I4,A)" ) "Change reference value by next value,",&
                                           Y, " bits wide"
          CASE (04)
            WRITE ( *, FMT="(1X,A,I4,A)" ) "Add associated field of", Y, " bits"
          CASE (05)
            WRITE ( *, FMT="(1X,A,I4,A)" ) "Signify character:", Y,&
                                          " characters follow"
          CASE (06)
            WRITE ( *, FMT="(1X,A,I3,A)" ) "Signify data width", Y," bits"
          CASE (21)
            WRITE ( *, FMT="(1X,A)"      ) "Data not present"
          CASE (22)
            WRITE ( *, FMT="(1X,A)"      ) "Quality information follows"
          CASE (23)
            WRITE ( *, FMT="(1X,A)"      ) "Substituted values operator"
          CASE (24)
            WRITE ( *, FMT="(1X,A)"      ) "First order statistical values follow"
          CASE (25)
            WRITE ( *, FMT="(1X,A)"      ) "Difference statistical values follow"
          CASE (32)
            WRITE ( *, FMT="(1X,A)"      ) "Replace/retained values follow"
          CASE (35)
            WRITE ( *, FMT="(1X,A)"      ) "Cancel backward data reference"
          CASE (36)
            WRITE ( *, FMT="(1X,A)"      ) "Define data present bit-map"
          CASE (37)
            WRITE ( *, FMT="(1X,A)"      ) "Use/cancel defined data present bit-map"

          CASE DEFAULT
            WRITE ( *, * ) "Unknown operator desciptor: X not 01-06,21-25,32,35-37"
        END SELECT

! --- Table D sequence Descriptor

      CASE (3)
        CALL TABLED ( X, Y, seq, nseq )
        IF ( nseq > 0 ) THEN
          DO i = 1, nseq
            CALL DESFXY ( seq(i), F,X,Y )
            WRITE ( *, FMT="(I3,I2,I2.2,I3.3)" ) i, F,X,Y
          END DO
        ELSE
          WRITE ( *, * ) "Sequence not in Table D"
        END IF

! --- Not a valid Descriptor ---

      CASE DEFAULT
        WRITE ( *, * ) "Unknown Descriptor type: F not 0-3"

    END SELECT

  END DO

  WRITE ( *, * )

END PROGRAM TestTables
