SUBROUTINE ASC2EB(LENGTH,STRING)
! ---------------------------------------------------------------------
!
! Program       : ASC2EB
!
! Purpose       : to convert ASCII values to EBCDIC
!
! Calls         :
!
! Argument      :
!  (1) LENGTH   Length of string to be converted (i)
!  (2) STRING   String to be converted(i/o)
!
! Error returns : none
!
! REVISION INFO :
!
! $Workfile: asc2eb.f90$ $Folder: F95_source$
! $Revision: 4$ $Date: 09/02/2011 16:19:27$
!
! CHANGE RECORD :
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 16:19:27    Sheila Needham  Use
!       int2ch function to replace CHAR
!  3    MetDB_Refresh 1.2         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  2    MetDB_Refresh 1.1         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  1    MetDB_Refresh 1.0         07/10/2010 10:56:33    Sheila Needham
!       Version copied from MET.SRCELIB
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
!
! ---------------------------------------------------------------------

USE int2ch_mod

IMPLICIT NONE

! Arguments

INTEGER              ,INTENT(IN)    :: LENGTH
CHARACTER(LEN=LENGTH),INTENT(INOUT) :: STRING

! Local Variables

INTEGER :: J
INTEGER :: IBMCHA
INTEGER :: IASCCH(0:255)
DATA IASCCH/      &
         0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 13, 14, 15, &
        16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31, &
        64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97, &
       240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111, &
       124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
       215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109, &
       121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150, &
       151,152,153,162,163,164,165,166,167,168,169,192,106,208,161,  7, &
        32, 33, 34, 35, 36, 21,  6, 23, 40, 41, 42, 43, 44,  9, 10, 27, &
        48, 49, 26, 51, 52, 53, 54,  8, 56, 57, 58, 59,  4, 20, 62,225, &
        65, 66, 67, 68, 69, 70, 71, 72, 73, 81, 82, 83, 84, 85, 86, 87, &
        88, 89, 98, 99,100,101,102,103,104,105,112,113,114,115,116,117, &
       118,119,120,128,138,139,140,141,142,143,144,154,155,156,157,158, &
       159,160,170,171,172,173,174,175,176,177,178,179,180,181,182,183, &
       184,185,186,187,188,189,190,191,202,203,204,205,206,207,218,219, &
       220,221,222,223,234,235,236,237,238,239,250,251,252,253,254,255/
!     =====================================================
DO J=1,LENGTH
  IBMCHA = IASCCH( ICHAR(STRING(J:J)) )
  STRING(J:J) = int2ch(IBMCHA)
END DO
RETURN
END SUBROUTINE ASC2EB
