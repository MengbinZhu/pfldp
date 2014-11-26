SUBROUTINE TABLED(XREQ,YREQ,SEQ,ND)

!-----------------------------------------------------------------------
!
! PROGRAM       : TABLED
!
! PURPOSE       : to expand a BUFR sequence
!
! DESCRIPTION   :
!           The input consists of one or more lines for each sequence;
!           sequences may or may not be separated by blank lines.
!           Each sequence has a Table D descriptor, a 3-figure count,
!           and that many descriptors defining the sequence.
!             The descriptors are separated by spaces, so there is
!           room for up to 10 defining descriptors on the 1st line.
!           Leave a blank line if there are exactly 10.  If there are
!           more than 10, restart in column 1 on subsequent lines.
!             The descriptors are in BUFR form, but Table D is for
!           both BUFR & CREX.  Confusingly, most sequences are in
!           the BUFR section of the Manual on Codes, but a few are
!           in the CREX section.  But conceptually there is only one
!           Table D, so these two sources are combined in the input
!           used here: sequences with delayed replication will need
!           031001 deleted before use.
!             (One CREX sequence, D05006, has operators to change
!           the temperature units from C to K, a practice "not
!           recommended" by regulation 95.3.4.2; this will need
!           special treatment if the CREX version is ever used.)
!
! CALLED BY     : DECODE, ENCODE, CREXDAT
!
! ARGUMENTS     : (1)  X                                   (input)
!               : (2)  Y                                   (input)
!               : (3)  sequence                            (output)
!               : (4)  number of descriptors in sequence   (output)
!                      (returned as zero if sequence not found)
!
! REVISION INFO :
!
! $Workfile: tabled.F90$ $Folder: BUFR Package$
! $Revision: 2$  $Date: 16/02/2012 11:42:12$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         16/02/2012 11:42:12    Sheila Needham
!       Increase array size
!  1    Met_DB_Project 1.0         18/02/2011 13:10:06    Sheila Needham  BUFR
!       package Release 19. Edition 4 encoding and Table B version 14.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE inquire_mod
USE int2ch_mod
#if defined (BPATH)
USE bufrpath_mod
#endif

IMPLICIT NONE

! Arguments

INTEGER, INTENT(IN)  ::  XREQ         ! argument 1
INTEGER, INTENT(IN)  ::  YREQ         ! argument 2
INTEGER, INTENT(OUT) ::  SEQ(*)       ! array for sequence to be returned
INTEGER, INTENT(OUT) ::  ND           !number of descriptors in sequence

! Local Variables

! MAXND is determined by a single byte for the count in the index.
! MAXNSEQ is arbitrary, MAXNDS limited by two bytes for pointers.

INTEGER ::  NDS = 0    ! number of descriptors in all sequences
INTEGER ::  NSEQ = 0   ! number of sequences
INTEGER,PARAMETER :: MAXND =255 ! max number of descriptors in a sequence
INTEGER,PARAMETER :: MAXNDS =8000 ! max number of descriptors in all sequences
INTEGER,PARAMETER :: MAXNSEQ =999  ! max number of sequences

INTEGER ::  F,X,Y             ! from sequence descriptor during table read
INTEGER ::  OLDX = 0          ! X & Y for previous sequence read in
INTEGER ::  OLDY = 0          !
INTEGER ::  IRC               ! input return code
INTEGER ::  LEN_DIR_NAME =0
INTEGER ::  I                 ! loop variable
LOGICAL ::  FEXIST            ! TRUE if TABLED exists
LOGICAL ::  INCORE = .FALSE.  ! set if table read in
LOGICAL ::  READY             ! set if FT81 open

INTEGER ::  IFIRST ! start of range in binary search
INTEGER ::  ILAST  ! end of range in binary search
INTEGER ::  NPOS   ! position of target in list
INTEGER ::  N      ! pointer to sequence in DESCHR

CHARACTER(LEN=2) :: XY     ! descriptor to be looked for in index
CHARACTER(LEN=5) :: INDEX(MAXNSEQ)  ! (descriptor, ND, pointer)*NSEQ
CHARACTER(LEN=6) :: DESCHR(MAXNDS)  ! descriptors in character form
CHARACTER(LEN=1) :: FLAG            ! not space if flagged for deletion
CHARACTER(LEN=80) :: REVISION       ! line to skip at start of input
CHARACTER(LEN=208) :: FILENAME = 'TABLED'    ! long string for use on HP
#if defined (BPATH)
CHARACTER(LEN=200) :: DIR_NAME   !- BUFR tables directory path
#endif

SAVE

ND = 0       
! Set 16-bit descriptor to look for in index

XY(1:1)=int2ch(3*64+XREQ)
XY(2:2)=int2ch(YREQ)

! Read table into core 1st time only.

IF (.NOT.INCORE) THEN

! See if unit=81 has been opened.
! If not, open it here - if there's a DDNAME TABLED.

  INQUIRE (81,OPENED=READY)
  IF (.NOT.READY) THEN
#if defined (BPATH)
    CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)
    FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)
    FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+6)='TABLED'
#endif
    LEN_DIR_NAME=LEN_DIR_NAME+6
    FEXIST=INQUIRE (FILENAME,'DDN')
    IF (.NOT.FEXIST) THEN
      WRITE(6,*)'TABLED: ERROR - File ',         &
     &      FILENAME(1:LEN_DIR_NAME),' not found'
      STOP
    END IF

#if defined (MVS)
    OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED',IOSTAT=IRC,  &
     &             ACTION='READ')
#else
    OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif

    IF (IRC /= 0) THEN
      WRITE(6,*)'TABLED: ERROR opening ',              &
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC
      STOP
    END IF
  END IF  !- ready

! Skip revision line at start

  READ (81,'(A80)') REVISION

! Loop round reading a whole sequence at a time, repeating the read if
! necessary to skip a blank line or (optional) sequence title.
! Read one more descriptor than expected, to check the count.
! Skip any sequence with a question mark after the count.

  IRC=0
  DO WHILE (IRC == 0)
   10     READ (81,1,IOSTAT=IRC) F,X,Y,ND,FLAG,(DESCHR(NDS+I),I=1,ND+1)
    1         FORMAT (I1,I2,I3, I3,A1, 10(A6,1X)/(10(A6,1X)))
    IF (IRC == 0) THEN
      IF (ND == 0) GO TO 10       ! to skip blank line
      IF (FLAG /= ' ') GO TO 10   ! to skip old sequence

! Make sure there are enough descriptors, & not too many or wrong count,
! that sequences being defined are in ascending order -
! & stop if only 9 on the first line when there should be 10.

      IF (F /= 3 .OR. X >= 64 .OR. Y >= 256) THEN
        PRINT *,F*100000+X*1000+Y,'in Table D: bad F=3 descriptor'
        STOP
      END IF

      IF (DESCHR(NDS+ND) == ' ') THEN
        PRINT*,F*100000+X*1000+Y,'in Table D: too few descriptors'
        STOP
      END IF

      IF (DESCHR(NDS+ND+1) /= ' ') THEN
        PRINT*,F*100000+X*1000+Y,'in Table D: count wrong?'
        STOP
      END IF

      IF (N >= 10 .AND. DESCHR(NDS+10) == ' ') THEN
        PRINT*,F*100000+X*1000+Y,'in Table D: no tenth descriptor'
        STOP
      END IF

      IF (X < OLDX .OR. (X == OLDX .AND. Y < OLDY)) THEN
        PRINT *,'Table D sequences out of order'
        PRINT*,F*100000+X*1000+Y,'follows',3*100000+OLDX*1000+OLDY
        STOP
      END IF

! If there's no error detected, index the sequence.

      INDEX(NSEQ+1)(1:1)=int2ch(3*64+X)
      INDEX(NSEQ+1)(2:2)=int2ch(Y)
      INDEX(NSEQ+1)(3:3)=int2ch(ND)
      INDEX(NSEQ+1)(4:4)=int2ch((NDS+1)/256)
      INDEX(NSEQ+1)(5:5)=int2ch(MOD(NDS+1,256))

! Update counts & reset last descriptor

      NSEQ=NSEQ+1
      NDS=NDS+ND
      OLDX=X
      OLDY=Y
    END IF
  ENDDO

  CLOSE (81)
  INCORE=.TRUE.
END IF

! Initialise variables for binary search

ND=0
IFIRST=0
ILAST=NSEQ

! Search as in ISRCH, looping until range is reduced to 1.
! Compare the target with an element near the middle of the range
! and reset the start or end of the range accordingly.

DO WHILE (IFIRST < ILAST)
  NPOS=(IFIRST+ILAST+1)/2

  IF (XY < INDEX(NPOS)(1:2)) THEN
    ILAST=NPOS-1
  ELSE
    IFIRST=NPOS
  END IF
END DO

! Set NPOS if target found, return if not

NPOS=IFIRST
IF (NPOS == 0 .OR. XY /= INDEX(NPOS)(1:2)) RETURN

! Return expansion of sequence.  N points to sequence in DESCHR.

ND=ICHAR(INDEX(NPOS)(3:3))
N=ICHAR(INDEX(NPOS)(4:4))*256+ICHAR(INDEX(NPOS)(5:5))
DO I=1,ND
  READ (DESCHR(N+I-1),'(I1,I2,I3)') F,X,Y
  SEQ(I)=F*16384+X*256+Y
END DO

RETURN
END SUBROUTINE TABLED
