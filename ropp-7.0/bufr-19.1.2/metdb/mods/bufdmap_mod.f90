MODULE BUFDMAP_mod
  INTERFACE
    SUBROUTINE BUFDMAP(STRING,CMPRES,NEXBIT,LASBIT,DSPLAY,NZEROS)
      IMPLICIT NONE
      CHARACTER(LEN=*) :: STRING
      LOGICAL :: CMPRES
      INTEGER :: NEXBIT
      INTEGER :: LASBIT
      LOGICAL :: DSPLAY
      INTEGER :: NZEROS
    END SUBROUTINE BUFDMAP
  END INTERFACE
END MODULE BUFDMAP_mod
