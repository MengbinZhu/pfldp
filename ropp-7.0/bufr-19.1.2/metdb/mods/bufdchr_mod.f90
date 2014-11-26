MODULE BUFDCHR_mod
  INTERFACE
    SUBROUTINE BUFDCHR(STRING,NOBS,CMPRES,WIDTH,DSPLAY,NAME,    &
      &IBEFOR,VALUES,IVAL,INAM,NAMES,IRC)
      IMPLICIT NONE
      CHARACTER(LEN=*) :: STRING
      INTEGER :: NOBS
      LOGICAL :: CMPRES
      INTEGER :: WIDTH
      LOGICAL :: DSPLAY
      CHARACTER(LEN=60) :: NAME
      INTEGER :: IBEFOR
      REAL :: VALUES(*)
      INTEGER :: IVAL
      INTEGER :: INAM
      CHARACTER(LEN=*) :: NAMES
      INTEGER :: IRC
    END SUBROUTINE BUFDCHR
  END INTERFACE
END MODULE BUFDCHR_mod
