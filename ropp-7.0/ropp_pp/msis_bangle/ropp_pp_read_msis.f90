! $Id: ropp_pp_read_msis.f90 3551 2013-02-25 09:51:28Z idculv $

!****s* ModelRefraction/ropp_pp_read_MSIS *
!
! NAME
!    ropp_pp_read_MSIS - Read spherical harmonic coefficients from file
!
! SYNOPSIS
!    call ropp_pp_read_MSIS(file, month, coeff)
!
! DESCRIPTION
!    This subroutine reads a file of MSIS climatology spherical harmonic
!    coefficients for the month specified and fills the elements of a 
!    structure of derived type MSIScoeff.
!
! INPUTS
!    character(*)     :: file          Filename
!    integer,         :: month         Month of year
!
! OUTPUT
!    type(MSIScoeff)  :: coeff         Coefficients  (ref/ba)
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

!-------------------------------------------------------------------------------
! 1. Read refractivity coefficients
!-------------------------------------------------------------------------------

SUBROUTINE ropp_pp_read_MSIS_ref(file, month, coeff)

! 1.1 Declarations
!-----------------

  USE ropp_pp_MSIS, not_this => ropp_pp_read_MSIS_ref
  USE ncdf
  USE ropp_utils

  IMPLICIT NONE

  CHARACTER(len=*),     INTENT(in)  :: file
  INTEGER,              INTENT(in)  :: month
  TYPE(MSIScoeff_ref),  INTENT(out) :: coeff
  
  INTEGER            :: n0, n1, n2
  
! 1.2 Open file
!--------------

  CALL ncdf_open(file)

! 1.3 Obtain dimensions
!----------------------

  CALL ncdf_getsize('ref_Ac0', n0, dim = 1)
  CALL ncdf_getsize('ref_Ac0', n2, dim = 2)
  CALL ncdf_getsize('ref_Ac1', n1, dim = 1)
  
! 1.4 Initialise data structures and read data
!---------------------------------------------

  CALL callocate(coeff%Ac0, (/n0, n2/), ropp_MDFV)
  CALL callocate(coeff%Ac1, (/n1, n2/), ropp_MDFV)
  CALL callocate(coeff%Bc1, (/n1, n2/), ropp_MDFV)
  
  CALL ncdf_getvar('ref_Ac0', coeff%Ac0, rec=month)
  CALL ncdf_getvar('ref_Ac1', coeff%Ac1, rec=month)
  CALL ncdf_getvar('ref_Bc1', coeff%Bc1, rec=month)
  
  CALL callocate(coeff%Aa0, n0, ropp_MDFV)
  CALL callocate(coeff%Aa1, n1, ropp_MDFV)
  CALL callocate(coeff%Ba1, n1, ropp_MDFV)
  
  CALL ncdf_getvar('ref_Aa0', coeff%Aa0, rec=month)
  CALL ncdf_getvar('ref_Aa1', coeff%Aa1, rec=month)
  CALL ncdf_getvar('ref_Ba1', coeff%Ba1, rec=month)

  CALL callocate(coeff%Ab0, n0, ropp_MDFV)
  CALL callocate(coeff%Ab1, n1, ropp_MDFV)
  CALL callocate(coeff%Bb1, n1, ropp_MDFV)

  CALL ncdf_getvar('ref_Ab0', coeff%Ab0, rec=month)
  CALL ncdf_getvar('ref_Ab1', coeff%Ab1, rec=month)
  CALL ncdf_getvar('ref_Bb1', coeff%Bb1, rec=month)

  CALL callocate(coeff%Ad0, n0, ropp_MDFV)
  CALL callocate(coeff%Ad1, n1, ropp_MDFV)
  CALL callocate(coeff%Bd1, n1, ropp_MDFV)

  CALL ncdf_getvar('ref_Ad0', coeff%Ad0, rec=month)
  CALL ncdf_getvar('ref_Ad1', coeff%Ad1, rec=month)
  CALL ncdf_getvar('ref_Bd1', coeff%Bd1, rec=month)

! 1.5 Close file
!---------------

  CALL ncdf_close()
  
END SUBROUTINE ropp_pp_read_MSIS_ref

!-------------------------------------------------------------------------------
! 2. Read bending angle coefficients
!-------------------------------------------------------------------------------

SUBROUTINE ropp_pp_read_MSIS_ba(file, month, coeff)


! 2.1 Declarations
!-----------------

  USE ropp_pp_MSIS, not_this => ropp_pp_read_MSIS_ba
  USE ncdf
  USE ropp_utils

  IMPLICIT NONE

  CHARACTER(len=*),     INTENT(in)  :: file
  INTEGER,              INTENT(in)  :: month
  TYPE(MSIScoeff_ba),   INTENT(out) :: coeff
  
  INTEGER            :: n0, n1, n2
    
! 2.2 Open file
!--------------

  CALL ncdf_open(file)

! 2.3 Obtain dimensions
!----------------------

  CALL ncdf_getsize('ba_Ac0', n0, dim = 1)
  CALL ncdf_getsize('ba_Ac0', n2, dim = 2)
  CALL ncdf_getsize('ba_Ac1', n1, dim = 1)
  
! 2.4 Initialise data structures and read data
!---------------------------------------------

  CALL callocate(coeff%Ac0, (/n0, n2/), ropp_MDFV)
  CALL callocate(coeff%Ac1, (/n1, n2/), ropp_MDFV)
  CALL callocate(coeff%Bc1, (/n1, n2/), ropp_MDFV)
  
  CALL ncdf_getvar('ba_Ac0', coeff%Ac0, rec=month)
  CALL ncdf_getvar('ba_Ac1', coeff%Ac1, rec=month)
  CALL ncdf_getvar('ba_Bc1', coeff%Bc1, rec=month)
  
  CALL callocate(coeff%Aa0, n0, ropp_MDFV)
  CALL callocate(coeff%Aa1, n1, ropp_MDFV)
  CALL callocate(coeff%Ba1, n1, ropp_MDFV)
  
  CALL ncdf_getvar('ba_Aa0', coeff%Aa0, rec=month)
  CALL ncdf_getvar('ba_Aa1', coeff%Aa1, rec=month)
  CALL ncdf_getvar('ba_Ba1', coeff%Ba1, rec=month)

  CALL callocate(coeff%Ab0, n0, ropp_MDFV)
  CALL callocate(coeff%Ab1, n1, ropp_MDFV)
  CALL callocate(coeff%Bb1, n1, ropp_MDFV)

  CALL ncdf_getvar('ba_Ab0', coeff%Ab0, rec=month)
  CALL ncdf_getvar('ba_Ab1', coeff%Ab1, rec=month)
  CALL ncdf_getvar('ba_Bb1', coeff%Bb1, rec=month)

  CALL callocate(coeff%Ad0, n0, ropp_MDFV)
  CALL callocate(coeff%Ad1, n1, ropp_MDFV)
  CALL callocate(coeff%Bd1, n1, ropp_MDFV)

  CALL ncdf_getvar('ba_Ad0', coeff%Ad0, rec=month)
  CALL ncdf_getvar('ba_Ad1', coeff%Ad1, rec=month)
  CALL ncdf_getvar('ba_Bd1', coeff%Bd1, rec=month)

  CALL callocate(coeff%Ae0, n0, ropp_MDFV)
  CALL callocate(coeff%Ae1, n1, ropp_MDFV)
  CALL callocate(coeff%Be1, n1, ropp_MDFV)
  
  CALL ncdf_getvar('ba_Ae0', coeff%Ae0, rec=month)
  CALL ncdf_getvar('ba_Ae1', coeff%Ae1, rec=month)
  CALL ncdf_getvar('ba_Be1', coeff%Be1, rec=month)

! 2.5 Close file
!---------------

  CALL ncdf_close()
  
END SUBROUTINE ropp_pp_read_MSIS_ba

