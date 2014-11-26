! $Id: sec1tables.f90 65 2012-01-23 16:28:16Z frdo $

MODULE Sec1Tables
CONTAINS

!****m* BUFR/SEC1TABLES/Sec1Tables *
!-----------------------------------------------------------------------
!
! NAME
!   Sec1Tables   (Sec1Tables.f90)
!
! SYNOPSIS
!   Module of BUFR Section 1 Code Table look-up routines
!
!   CALL OrigCentre   ( Cen, SubCen, &     : Orig. (sub-)centre
!                       CenNam, SubNam, ICAO &
!                    [, Use=use] )
!
!   CALL DataCategory ( Code, SubCode, &   : Data (sub-)category
!                       Cat,  SubCat )
!
!   CALL MasterTable  ( Mtab, Mver, &      : Master Table version
!                       Tab,  Ver )
!
!   CALL LoadCTable   ( Table, Default, &
!                       Entry, SubEntry &  : Load a code table
!
!   CALL CTUpper      ( string )           : Convert lowercase to UPPERCASE
!
! DESCRIPTION
!   This module provides routines to translate BUFR codes to text strings
!   (and for OrigCentre also the reverse) for some special tables used
!   to decode (and encode) data in Section 1 of a BUFR message). None
!   of these special tables or routines are necessary to the normal
!   binary encoding/decoding of BUFR (Section 4) messages.
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****
!
!-----------------------------------------------------------------------

SUBROUTINE OrigCentre ( Centre,    & ! (inout)
                        SubCentre, & ! (inout)
                        CenNam,    & ! (inout)
                        SubNam,    & ! (inout)
                        ICAO,      & ! (inout)
                        Use )        ! (optional/in)

!****s* BUFR/SEC1TABLES/OrigCentre *
!-----------------------------------------------------------------------
!
! NAME
!   OrigCentre    (Sec1Tables.f90)
!
! SYNOPSIS
!   Look up the Originating/Generating Centre name and associated
!   ICAO Location code and sub-centre name from their codes, or
!   reverse-look up codes from names.
!
!    USE Sec1Tables
!    INTEGER            :: Centre, SubCen
!    CHARACTER (LEN=4)  :: ICAO
!    CHARACTER (LEN=60) :: CenNam, SubNam
!    CALL OrigCentre(Centre,SubCen, CenNam,SubNam, ICAO [,Use=<use>] )
!
! INPUTS
!   Centre     int  Originating/Generating Centre code value (0-255)
!   SubCentre  int  Sub-centre code for Originating Centre (0-255)
!   CenNam     chr  Originating/Generating Centre name
!   SubName    chr  Originating Sub-centre name
!   ICAO       chr  ICAO 4-character code for Orig/Gen Centre
!   Use        chr  [Optional] which argument to use: one of:
!                  'CODES', 'CENNAM', 'SUBNAM' or 'ICAO'
!                   Default: 'CODES'
!   All string inputs are case-insensitive; matching is performed
!   internally in upper case.
!
! OUTPUTS
!   Centre     int  Originating/Generating Centre code value (0-255)
!   SubCentre  int  Sub-centre code for Originating Centre (0-255)
!   CenNam     chr  Originating/Generating Centre name
!   SubName    chr  Originating Sub-centre name
!   ICAO       chr  ICAO 4-character code for Orig/Gen Centre
!
! FILES
!   ORIGCENTRE  - in path defined by BUFR_LIBRARY
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY - path to run-time BUFR tables
!
! DESCRIPTION
!   Common Code Table C-1 is a list of codes identifying an originating or
!   generating centre. Most centres have an associated 4-chr ICAO code used,
!   for instance, in WMO GTS abbreviated routing headers and several
!   also have a set of sub-centres (defined in Common Code Table C-12).
!   This routine provides a look up translation between code values and
!   name strings or vice-versa.
!
!   The code lookup table is loaded on the first call, but if the file
!   cannot be loaded, the returned name is 'TABLE ORIGCENTRE NOT LOADED',
!   except for code 255 ('MISSING'). Any missing entries in the file are
!   assumed to be 'Reserved' with the ICAO being 'ZZZZ', except that
!   sub-centre code 0 is always returned as 'none'. The file name is
!   'ORIGCENTRE' and is located via environment variable 'BUFR_LIBRARY'.
!
!   There is one forward (default) and three reverse look-up options:
!
!   1) Forward look-up (default or Use='CODES')
!   Given an Originating/Generating Centre code value (0-254) and an
!   associated sub-centre code value (0-255), this routine returns the
!   corresponding centre name from C-1, the sub-centre name from C-12 plus
!   its associated ICAO code (where known). Codes not yet defined are
!   returned as 'Reserved'; Codes out of range (< 0 or >255) are returned
!   'INVALID CENTRE CODE VALUE' or 'INVALID SUBCENTRE CODE VALUE'.
!   Where ICAO codes are not known, or the code value is invalid, the ICAO
!   string will be returned as 'ZZZZ'.
!
!   2) Reverse look-up (Use='CENNAM')
!   Given a centre name (or partial name), look up the centre's code value.
!   The routine will return the first match anywhere in the centre name, so
!   the input string should be unique to ensure a correct match. Matching is
!   case-insensitive. The code value, full centre name and ICAO code are
!   returned to the caller. With this option, the sub-centre code and name
!   are not used for input; on exit they will be set to 0 and 'none'.
!   The centre name may also be (or include) an ICAO code, as the whole entry
!   string from the lookup table is used for the matching. However, since an
!   arbitrary 4-character string may not be unique, using the Use="ICAO"
!   option may be more robust. If there is no match, the centre code will be
!   returned as 256, the centre name 'CENTRE NAME NOT MATCHED' and ICAO code
!   'ZZZZ'.
!
!   3) Reverse look-up (Use='SUBNAM')
!   Given a sub-centre name (or partial name) and the centre code for
!   this sub-centre, look up the sub-centre code value. Matching is case-
!   insensitive. The sub-centre code value, full sub-centre name and centre
!   ICAO code are returned to the caller. (See EXAMPLES to look-up codes from
!   both centre and sub-centre names.) If there is no match, the sub-centre
!   code will be returned as 256, the sub-centre name 'SUBCENTRE NAME NOT
!   MATCHED' and ICAO code "ZZZZ".
!
!   4) Reverse look-up (Use='ICAO')
!   Given an ICAO code, look up the centre code and full name. Matching is
!   case-insenitive. With this option, the sub-centre code and name are
!   unused for input; on exit they will be set to 0 and 'none'. This option
!   restricts the matching to the ICAO code field, so wil not falsely match
!   on arbitrary text in the whole name string, but otherwise operates the
!   same as the Use="CENNAM" option. If there is no match, the centre code
!   will be 256 and the name 'ICAO CODE NOT MATCHED'. The input ICAO code
!   will be returned in uppercase, but otherwise unchanged.
!
! EXAMPLES
!    1) Simple forward lookup:
!       Centre=74
!       SubCen=3
!       CALL OrigCentre(Centre,SubCen,CenNam,SubNam,ICAO)
!    => CenNam : "UK Met Office - Exeter (RSMC)"
!       SubNam : "Tromso"
!       ICAO   : "EGRR"
!
!    2) Lookup an ICAO code
!       ICAO="EDZW"
!       CALL OrigCentre(Centre,SubCen,CenNam,SubNam,ICAO, Use="ICAO")
!    => Centre = 78
!       CenNam = "Offenbach (RSMC)"
!       SubCen = 0
!       SubNam = 'none'
!
!    3) Lookup a sub-centre name for a particular centre name.
!       First lookup the centre code from the centre name...
!       CenNam = "NESDIS"
!       CALL OrigCentre(Centre,SubCen,CenNam,SubNam,ICAO, Use="cennam")
!    => Centre : 160
!       CenNam : "US NOAA/NESDIS"
!       ICAO   : "KNES"
!       ...and then the sub-centre for this centre code
!       SubNam = "mcmurdo"
!       CALL OrigCentre(Centre,SubCen,CenNam,SubNam,ICAO, Use="SubNam")
!    => SubCen = 11
!       SubNam = "McMurdo (Antarctica)"
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Tdsn  = "ORIGCENTRE" ! Table file name
  INTEGER,           PARAMETER :: me    = 256          ! max centres+1
  INTEGER,           PARAMETER :: ms    = 256          ! max subcentres+1

! Argument list parameters

  INTEGER,           INTENT(INOUT) :: Centre
  INTEGER,           INTENT(INOUT) :: SubCentre
  CHARACTER (LEN=*), INTENT(INOUT) :: CenNam
  CHARACTER (LEN=*), INTENT(INOUT) :: SubNam
  CHARACTER (LEN=*), INTENT(INOUT) :: ICAO
  CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Use

! Local parameters

  CHARACTER (LEN=60), ALLOCATABLE :: Entry(:)
  CHARACTER (LEn=60), ALLOCATABLE :: SubEntry(:,:)
  CHARACTER (LEN=60), ALLOCATABLE :: UC_Entry(:)
  CHARACTER (LEN=60)              :: LocNam
  CHARACTER (LEN=10)              :: LocUse
  CHARACTER (LEN=4)               :: LocICAO
  INTEGER                         :: i
  LOGICAL                         :: Loaded = .FALSE.

  SAVE Entry
  SAVE SubEntry
  SAVE UC_Entry
  SAVE Loaded

! For first entry, load Originating Centre Table

  IF ( .NOT. Loaded ) THEN
    ALLOCATE ( Entry(0:me) )
    ALLOCATE ( SubEntry(0:me,0:ms) )
    ALLOCATE ( UC_Entry(0:me) )
    Entry(0:me-2)    = "ZZZZ TABLE ORIGCENTRES NOT LOADED"
    Entry(me-1)      = "ZZZZ MISSING"
    Entry(me)        = "ZZZZ INVALID"
    SubEntry(:,:)    = "Reserved"
    SubEntry(:,0)    = "none"
    SubEntry(:,ms-1) = "MISSING"
    SubEntry(:,ms)   = "INVALID"
    CALL LoadCTable ( Tdsn, "ZZZZ Reserved", &
                      Entry, SubEntry )
    Loaded = .TRUE.
  END IF

! What to use for look-up? User-option or Centre code
! as default.

  IF ( PRESENT(Use) ) THEN
    LocUse = TRIM(ADJUSTL(Use))
    CALL CTupper ( LocUse )
  ELSE
    LocUse = "CENTRE"
  ENDIF

  SELECT CASE (LocUse)

! Reverse-look up (partial) Centre name (including ICAO code)
! Sub-centre code & name are not applicable to this option,

    CASE ("CENNAM")
      LocNam = ADJUSTL(CenNam)
      CALL CTUpper ( LocNam )
      Entry(me) = "ZZZZ CENTRE NAME NOT MATCHED"

      DO i = 0, me
        UC_Entry(i) = Entry(i)
        CALL CTUpper ( UC_Entry(i) )
      END DO

      Centre = 0
      DO
        IF ( Centre == me .OR. &
             INDEX ( UC_Entry(Centre), TRIM(LocNam) ) > 0 ) EXIT
        Centre = Centre + 1
      END DO

      ICAO = Entry(Centre)(1:4)
      IF ( ICAO == " " ) ICAO = "ZZZZ"
      CenNam = Entry(Centre)(6:)

      SubCentre = 0
      SubNam    = "none"

! Reverse-look up (partial) Sub-centre name to find
! Sub-centre code (plus Centre name & ICAO code)
! Centre code must be valid for this option

    CASE ("SUBNAM")
      LocNam = ADJUSTL(SubNam)
      CALL CTUpper ( LocNam )
      IF ( Centre < 0 .OR. &
           Centre > me ) Centre = me
      SubEntry(Centre,ms) = "SUBCENTRE NAME NOT MATCHED"

      DO i = 0, ms
        UC_Entry(i) = SubEntry(Centre,i)
        CALL CTUpper ( UC_Entry(i) )
      END DO

      SubCentre = 0
      DO
        IF ( SubCentre == ms .OR. &
             INDEX ( UC_Entry(SubCentre), TRIM(LocNam) ) > 0 ) EXIT
        SubCentre = SubCentre + 1
      END DO

      ICAO = Entry(Centre)(1:4)
      IF ( ICAO == " " ) ICAO = "ZZZZ"
      CenNam = Entry(Centre)(6:)
      SubNam = SubEntry(Centre,SubCentre)

! Reverse-look up ICAO code to find Centre code & name
! Sub-centre code & name are not applicable to this option,

    CASE ("ICAO")
      LocICAO = ADJUSTL(ICAO)
      CALL CTUpper ( LocICAO )
      Entry(me) = LocICAO//" ICAO CODE NOT MATCHED"

      Centre = 0
      DO
        IF ( Centre == me .OR. &
             UC_Entry(Centre)(1:4) == LocICAO ) EXIT
        Centre = Centre + 1
      END DO

      ICAO = Entry(Centre)(1:4)
      IF ( ICAO == " " ) ICAO = "ZZZZ"
      CenNam = Entry(Centre)(6:)

      SubCentre = 0
      SubNam    = "none"

! Forward-lookup given Centre & Sub-centre code values to
! Centre & Sub-centre names & ICAO Location code

    CASE DEFAULT
      IF ( Centre <= 255 ) THEN
        ICAO = Entry(Centre)(1:4)
        IF ( ICAO == " " ) ICAO = "ZZZZ"
        CenNam = Entry(Centre)(6:)
        IF ( SubCentre >= 0 .AND. &
             SubCentre <= 255 ) THEN
          SubNam = SubEntry(Centre,SubCentre)
        ELSE
          SubNam = "INVALID SUBCENTRE CODE VALUE"
        END IF
      ELSE
        CenNam = "INVALID CENTRE CODE VALUE"
        SubNam = "INVALID CENTRE CODE VALUE"
        ICAO   = "ZZZZ"
      END IF

  END SELECT

END SUBROUTINE OrigCentre
!-----------------------------------------------------------------------

SUBROUTINE DataCategory ( Code,     &   ! (in)
                          SubCode,  &   ! (in)
                          Category, &   ! (out))
                          SubCategory ) ! (out)

!****s* BUFR/SEC1TABLES/DataCategory *
!-----------------------------------------------------------------------
!
! NAME
!   DataCategory     (Sec1Tables.f90)
!
! SYNOPSIS
!   Look up the data category from BUFR Table A nd its
!   sub-category from CCT C1-3
!
!    USE Sec1Tables
!    INTEGER            :: Code, SubCode
!    CHARACTER (LEN=40) :: Category, SubCategory
!    CALL DataCategory(Code,SubCode,Category,SubCategory)
!
! INPUTS
!   Code    int  Table A Data Category Code (0-255)
!   SbCode  int  International Data Sub-Category code (0-255)
!
! OUTPUTS
!   Category     chr  Dta Category text
!   SubCategory  chr  Data Sub-category text
!
! FILES
!   DATACATEGORY  - in path defined by BUFR_LIBRARY
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY - path to run-time BUFR tables
!
! DESCRIPTION
!   Given a data category code value (0-255) from BUFR Table A
!   and an associated data subc-ategory code value (0-255) from
!   Common Code Table C-13, returns the corresponding
!   data category and sib-category descriptions.
!   Codes not yet defined are returned as 'Reserved'
!   Codes out of range are returned 'INVALID CODE VALUE' or
!   'INVALID SUBCODE VALUE'.
!   The BUFR Table A file is loaded on the first call, but if the
!   file cannot be loaded, the return is 'TABLE A NOT LOADED'.
!   The file name is 'DATACATEGORY' and is located via environment
!   variable 'BUFR_LIBRARY'.
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Tdsn  = "DATACATEGORY" ! file name
  INTEGER,           PARAMETER :: me    = 255       ! max category
  INTEGER,           PARAMETER :: ms    = 255       ! max sub-category

! Argument list parameters

  INTEGER,           INTENT(IN)  :: Code
  INTEGER,           INTENT(IN)  :: SubCode
  CHARACTER (LEN=*), INTENT(OUT) :: Category
  CHARACTER (LEN=*), INTENT(OUT) :: SubCategory

! Local parameters

  CHARACTER (LEN=60), ALLOCATABLE :: Entry(:)
  CHARACTER (LEN=60), ALLOCATABLE :: SubEntry(:,:)
  LOGICAL                         :: Loaded = .FALSE.

  SAVE Entry
  SAVE SubEntry
  SAVE Loaded

! For first entry, load Table A

  IF ( .NOT. Loaded ) THEN
    ALLOCATE ( Entry(0:me) )
    ALLOCATE ( SubEntry(0:me,0:ms) )
    Entry(:)        = " "
    SubEntry(:,:)   = " "
    SubEntry(:,0)   = "No sub-category"
    SubEntry(:,255) = "Undefined"
    SubEntry(255,:) = "No sub-category"
    CALL LoadCTable ( Tdsn, "Reserved", Entry, SubEntry )
    Loaded = .TRUE.
  END IF

! Decode given Table A code & subcode values to textual meanings

  IF ( Code >= 0 .AND. &
       Code <= me ) THEN
    Category = Entry(Code)
    IF ( SubCode >= 0 .AND. &
         SubCode <= ms ) THEN
      SubCategory = SubEntry(Code,SubCode)
    ELSE
      SubCategory = "INVALID SUBCODE VALUE"
    END IF
  ELSE
    Category    = "INVALID CODE VALUE"
    SubCategory = "INVALID CODE VALUE"
  END IF

END SUBROUTINE DataCategory
!-----------------------------------------------------------------------

SUBROUTINE MasterTable ( MasTab, & ! (in)
                         MasVer, & ! (in)
                         MasNam, & ! (out)
                         VerNam )  ! (out)

!****s* BUFR/SEC1TABLES/MasterTable *
!-----------------------------------------------------------------------
!
! NAME
!   MasterTable     (Sec1Tables.f90)
!
! SYNOPSIS
!   Look up the Master Table version
!
!    USE Sec1Tables
!    INTEGER            :: MasTab, MasVer
!    CHARACTER (LEN=20) :: MasNam, MasVer
!    CODE=12
!    CALL MasterTable(MasTab,MasVer,MasNam,MasVer)
!
! INPUTS
!   MasTab   int  Master Table discipline (currently only
!                 0 for Meteorology)
!   MasVer   int  Master Table Version number (>=0)
!
! OUTPUTS
!   MasNam   chr  Master Table descipline name
!   Name     chr  Master Table version name
!
! FILES
!   MASTERTABLE  - in path defined by BUFR_LIBRARY
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY - path to run-time BUFR tables
!
! DESCRIPTION
!   Given a Master Table (normally zero for standard meteorology
!   BUFR tables) and version number (0-14 at Oct 2009), returns
!   the corresponding table discipline and version descriptions.
!   This will normally be 'Meteorology' with the version indicating
!   the date that this version was implemented operationally.
!   Disciplines and versions not yet defined are returned as
!   'Reserved'; Tables out of range are returned 'INVALID TABLE VALUE'
!   and Versions out of range are returned 'INVALID VERSION VALUE'.
!   The Master Table file is loaded on the first call, but if the
!   file cannot be loaded, the returns are 'MASTER TABLE NOT LOADED'.
!   The file name is 'MASTERTABLE' and is located via environment
!   variable 'BUFR_LIBRARY'.
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: Tdsn  = "MASTERTABLE"  ! file name
  INTEGER,           PARAMETER :: me    = 2              ! max table
  INTEGER,           PARAMETER :: ms    = 20             ! max version

! Argument list parameters

  INTEGER,           INTENT(IN)  :: MasTab
  INTEGER,           INTENT(IN)  :: MasVer
  CHARACTER (LEN=*), INTENT(OUT) :: MasNam
  CHARACTER (LEN=*), INTENT(OUT) :: VerNam

! Local parameters

  CHARACTER (LEN=60), ALLOCATABLE :: Entry(:)
  CHARACTER (LEN=60), ALLOCATABLE :: SubEntry(:,:)
  LOGICAL                         :: Loaded = .FALSE.

  SAVE Entry
  SAVE SubEntry
  SAVE Loaded

! For first entry, load Master Table

  IF ( .NOT. Loaded ) THEN
    ALLOCATE ( Entry(0:me) )
    ALLOCATE ( SubEntry(0:me,0:ms) )
    Entry(:)      = " "
    SubEntry(:,:) = " "
    CALL LoadCTable ( Tdsn, "Reserved", &
                      Entry, SubEntry )
    Loaded = .TRUE.
  END IF

! Decode given Master Table value & version to textual meanings

  IF ( MasTab >= 0 .AND. &
       MasTab <= me ) THEN
    MasNam = Entry(MasTab)
    IF ( MasVer >= 0 .AND. &
         MasVer <= ms ) THEN
      VerNam = SubEntry(MasTab,MasVer)
    ELSE
      VerNam = "INVALID VERSION VALUE"
    END IF
  ELSE
    MasNam = "INVALID TABLE VALUE"
    VerNam = "INVALID TABLE VALUE"
  END IF

END SUBROUTINE MasterTable
!-------------------------------------------------------

SUBROUTINE LoadCTable ( Table,     & ! (in)
                        Default,   & ! (in)
                        Entry,     & ! (inout)
                        SubEntry )   ! (inout)

!****s* BUFR/SEC1TABLES/LoadCTable *
!-----------------------------------------------------------------------
!
! NAME
!   LoadCTable     (Sec1Tables.f90)
!
! SYNOPSIS
!   Subroutine to load a special BUFR code table
!
!    USE Sec1Tables
!    CHARACTER (LEN=20) :: Table           = 'TABLEA'
!    CHARACTER (LEN=20) :: Default         = 'Reserved'
!    CHARACTER (LEN=60) :: Entry(0:255)    = " "
!    CHARACTER (LEN=60) :: SubEntry(0:255,0:255) = " "
!    Entry(255)="MISSING"
!    CALL LoadCTable ( Table, Default, Entry, SubEntry )
!
! INPUTS
!   Table     chr  Name of code table file
!   Default   chr  Default string (usually 'Reserved')
!   Entry     chr  array (rank 1) of code table entries (0:n)
!                  Non-blank elements on entry will be retained.
!   SubEntry  chr  array (rank 2) of code table sub-entries (0:n,0:m)
!                  Non-blank elements on entry will be retained.
!
! OUTPUTS
!   Entry     chr  array (rank 1) of code table entries (0:n)
!   SubEntry  chr  array (rank 2) of code table sub-entries (0:n,0:m)
!
! FILES
!   'Table' string - in path defined by BUFR_LIBRARY. Uses stream 8.
!
! ENVIRONMENT VARIABLES
!   BUFR_LIBRARY - path to run-time BUFR tables
!
! DESCRIPTION
!   Loads a special BUFR code table from a file. The file is found
!   in the path defined by environment variable 'BUFR_LIBRARY'.
!   The file may contain any number main entries (usually 0:255)
!   each with any number of sub-entries (usually 0:255),
!   formatted thus:
!    nnn main entry text
!        mmm sub-entry txt
!        mmm sub-entry text
!    nnn main entry text
!    ...etc
!   where 'nnn' for a main entry starts in column 1 and is a single
!   3-digit number 000-255 and 'text' is free-format text (mixed case)
!   up to 60 characters.
!   The sub-entry lines follow the same rules except 'mmm' must start
!   in column 5.
!   Codes may appear in any order, but numerically ascending is
!   advised!
!   Text for any duplicate codes will overwrite any earlier ones.
!   Any line not having a valid 3-digit number starting in column 1
!   or column 5 (with columns 1-4 blank) is ignored.
!   If the file does not contain entries for all possible codes
!   then the 'Default' string (usually 'Reserved') is used to pad the
!   'Entry' and SubEntry' arrays for intially blank elements.
!   Before calling this subroutine, the 'Entry' and 'SubEntry'
!   arrays must be initialised to at least blanks; any blank elemnt
!   of 'Entry' is first initialised to the string: 'TABLE <table> NOT
!   LOADED' so that should the file not be found or not read, this
!   string will indicate that fact. Any element pre-initialised to a
!   non-blank string before calling this subroutine will not be
!   re-initialised, so that known codes can be set even if
!   the file cannot be loaded. E.g. Entry(255) may be 'Missing' or
!   'Not used'. A valid file entry will always overwrite a
!    pre-initialised entry.
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****
!
  IMPLICIT NONE

! Fixed values

  CHARACTER (LEN=*), PARAMETER :: BufrDirEnv = "BUFR_LIBRARY"
  INTEGER,           PARAMETER :: Tunit      = 8

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)            :: Table
  CHARACTER (LEN=*), INTENT(IN)            :: Default
  CHARACTER (LEN=*), INTENT(OUT)           :: Entry(0:)
  CHARACTER (LEN=*), INTENT(OUT)           :: SubEntry(0:,0:)

! Local variables

  CHARACTER (LEN=80)  :: Line
  CHARACTER (LEN=100) :: Dir
  CHARACTER (LEN=3)   :: mxe, mxs
  INTEGER             :: ierr
  INTEGER             :: me, ms
  INTEGER             :: i1, i2

! Initialise

  WHERE ( Entry == " " )
    Entry = "TABLE "//TRIM(Table)//" NOT LOADED"
  END WHERE
  WHERE ( SubEntry == " " )
    SubEntry = Default
  END WHERE

  me = UBOUND(Entry,   DIM=1)
  ms = UBOUND(SubEntry,DIM=2)
  WRITE ( mxe, "(I3.3)" ) me
  WRITE ( mxs, "(I3.3)" ) ms

  CALL GETENV ( BufrDirEnv, Dir )

! Open Table file

  OPEN ( UNIT=Tunit,            &
         FILE=TRIM(Dir)//Table, &
       ACTION="READ",           &
       STATUS="OLD",            &
       IOSTAT=ierr )

! Read Table entries.
! If cols 1-3 contain a valid number, save text in
! main Entry; else if cols 5-7 contain a valid number,
! save in Sub-entry; else ignore the line.

  i1 = 0
  i2 = 0
  DO WHILE ( ierr == 0 )
    READ ( UNIT=Tunit, &
            FMT="(A)", &
         IOSTAT=ierr ) Line
    IF ( ierr /= 0 ) THEN
      EXIT
    ELSE IF ( LGE(Line(1:3),"000") .AND. &
              LLE(Line(1:3), mxe ) ) THEN
      READ ( Line(1:3), "(I3)" ) i1
      Entry(i1) = Line(5:)
    ELSE IF ( Line(1:4) == " "     .AND. &
              LGE(Line(5:7),"000") .AND. &
              LLE(Line(5:7), mxs ) ) THEN
      READ ( Line(5:7), "(I3)" ) i2
      SubEntry(i1,i2) = Line(9:)
    ELSE
      CYCLE
    END IF
  END DO
  CLOSE ( UNIT=Tunit )

! If table successfully loaded, fill in any undefined entries
! with a default string

  IF ( ierr <= 0 ) THEN
    DO i1 = 0, me
      IF ( INDEX(Entry(i1), "NOT LOADED") > 0 ) THEN
        Entry(i1) = Default
        DO i2 = 0, ms
          SubEntry(i1,i2) = Default
        END DO
      END IF
    END DO
  END IF

END SUBROUTINE LoadCTable
!-----------------------------------------------------------------------

SUBROUTINE CTUpper ( string )

!****s* BUFR/SEC1TABLES/CTUpper *
!-----------------------------------------------------------------------
!
! NAME
!   CTUpper     (Sec1Tables.f90)
!
! SYNOPSIS
!   Subroutine to convert alphabetic characters from lower to upper case
!
!    USE Sec1Tables
!    CHARACTER (LEN=20) :: string = " ABcdE!"
!    CALL CTUpper ( string )
!    [string = " ABCDE!"]
!
! ARGUMENTS
!   string  (inout)  chr  string to be converted
!
! DESCRIPTION
!   Scans through input string for lowercase characters [a-z]
!   and replaces them with uppercase equivalents [A-Z]
!   This version taken from ToLower in chars.f90 in Genlib package.
!
! MODIFICATION HISTORY
!   Version  Date         Change                       By
!  -----------------------------------------------------------------
!   1.00     1-Nov-2009  First release                D. Offiler
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2009, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-------------------------------------------------------
!****

  IMPLICIT NONE

  CHARACTER (LEN=*),  INTENT(INOUT) :: string

  CHARACTER (LEN=26), PARAMETER :: UPPER="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  CHARACTER (LEN=26), PARAMETER :: lower="abcdefghijklmnopqrstuvwxyz"
  INTEGER :: i, j

  DO i = 1, LEN_TRIM(string)
    j = INDEX ( lower, string(i:i) )
    IF ( j > 0 ) string(i:i) = UPPER(j:j)
  END DO

END SUBROUTINE CTUpper
!-------------------------------------------------------

END MODULE Sec1Tables
