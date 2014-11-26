! $Id: ropp_io_vlist_read.f90 3551 2013-02-25 09:51:28Z idculv $

!****is* Reading/ropp_io_vlist_read *
!
! NAME
!    ropp_io_vlist_read - Read additional variables in netCDF file not 
!                         explicitly assigned to ROprof structure
!
! SYNOPSIS
!    call ropp_io_vlist_read(ncdf_varid, vlist, irec)
!
! DESCRIPTION
!    This subroutine extends a given RO profile structure with variables
!    read from an open netCDF file. On reading the variable from a netCDF file  
!    name, long_name, units and range attributes are filled in the ROdata structure 
!    as standard together with the 0d, 1d or 2d data arrays.
!
! INPUTS
!    integer                     :: ncdf_varid    ! Open netCDF file id
!    type(VlisttypeD?d), pointer :: vlist         ! variable to be read in
!    integer                     :: irec          ! Record number to be read in 
!    
! OUTPUT
!    type(VlisttypeD?d), pointer :: vlist         ! variable to be read in
!
! SEE ALSO
!     ropp_io_read_ncdf_get
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
! 1. Read scalar variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_read_ncdf_get_vlistD0d(ncdf_varid, vlist, irec)

  USE ncdf
  USE ropp_io, not_this => ropp_io_read_ncdf_get_vlistD0d
  USE ropp_io_types, ONLY: VlisttypeD0d

  IMPLICIT NONE

! 1.1 Declarations
! ---------------- 

  TYPE(VlisttypeD0d), POINTER  :: vlist
  INTEGER, INTENT(in)          :: ncdf_varid
  INTEGER, INTENT(in)          :: irec

  INTEGER                      :: status

! 1.2 Step down to next list element if already read additional variable(s)
! -------------------------------------------------------------------------

  IF (ASSOCIATED(vlist)) THEN 
     
     CALL ropp_io_read_ncdf_get_vlistD0d(ncdf_varid, vlist%next, irec)

! 1.3 Read additional variable to vlist structure
! -----------------------------------------------

  ELSE
     
     ! 1.3.1 Allocate added variable structure

     ALLOCATE(vlist)
     
     status = nf90_inquire_variable(ncdf_ncid, ncdf_varid, vlist%name)
     
     ! 1.3.2 Fill elements of added variable structure

     CALL ncdf_getvar(vlist%name,  vlist%data,    &
                      units = vlist%units,        &
                      range = vlist%range,        &
                      rec = irec)
     
     ! 1.3.3 Get variable long_name attribute (if exists)
     
     status = nf90_get_att(ncdf_ncid, ncdf_varid, "long_name", vlist%long_name)
  
  ENDIF

END SUBROUTINE ropp_io_read_ncdf_get_vlistD0d

!-------------------------------------------------------------------------------
! 2. Read 1-dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_read_ncdf_get_vlistD1d(ncdf_varid, vlist, irec)

  USE ncdf
  USE ropp_io, not_this => ropp_io_read_ncdf_get_vlistD1d
  USE ropp_io_types, ONLY: VlisttypeD1d

  IMPLICIT NONE

! 2.1 Declarations
! ---------------- 

  TYPE(VlisttypeD1d), POINTER  :: vlist
  INTEGER, INTENT(in)          :: ncdf_varid
  INTEGER, INTENT(in)          :: irec

  INTEGER, DIMENSION(2)        :: dimids, dimlen
  INTEGER                      :: status, ndim, idim

! 2.2 Step down to next list element if already read additional variable(s)
! -------------------------------------------------------------------------

  IF (ASSOCIATED(vlist)) THEN 
     
     CALL ropp_io_read_ncdf_get_vlistD1d(ncdf_varid, vlist%next, irec)

! 2.3 Read additional variable to vlist structure
! -----------------------------------------------

  ELSE

     ! 2.3.1 Allocate added variable structure
     
     ALLOCATE(vlist)

     status = nf90_inquire_variable(ncdf_ncid, ncdf_varid, vlist%name, &
                                     ndims=ndim, dimids = dimids)
     DO idim=1,ndim
        status = nf90_inquire_dimension(ncdf_ncid, dimids(idim),       &
                                        len = dimlen(idim))
     ENDDO

     ALLOCATE(vlist%data(dimlen(1)))
     
     ! 2.3.2 Fill elements of added variable structure
      
     CALL ncdf_getvar(vlist%name, vlist%data,    &
                      units = vlist%units,  &
                      range = vlist%range,  &
                      rec = irec)
     
     ! 2.3.3 Get variable long_name attribute (if exists)
     
     status = nf90_get_att(ncdf_ncid, ncdf_varid, "long_name", vlist%long_name)
     
  ENDIF

END SUBROUTINE ropp_io_read_ncdf_get_vlistD1d

!-------------------------------------------------------------------------------
! 3. Read 2-dimensional variables
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE ropp_io_read_ncdf_get_vlistD2d(ncdf_varid, vlist, irec)

  USE ncdf
  USE ropp_io, not_this => ropp_io_read_ncdf_get_vlistD2d
  USE ropp_io_types, ONLY: VlisttypeD2d

  IMPLICIT NONE

! 3.1 Declarations
! ---------------- 

  TYPE(VlisttypeD2d), POINTER  :: vlist
  INTEGER, INTENT(in)          :: ncdf_varid
  INTEGER, INTENT(in)          :: irec

  INTEGER, DIMENSION(3)        :: dimids, dimlen
  INTEGER                      :: status, ndim, idim

! 3.2 Step down to next list element if already read additional variable(s)
! -------------------------------------------------------------------------

  IF (ASSOCIATED(vlist)) THEN 
     
     CALL ropp_io_read_ncdf_get_vlistD2d(ncdf_varid, vlist%next, irec)

! 3.3 Read additional variable to vlist structure
! -----------------------------------------------

  ELSE
     
     ! 3.3.1 Allocate added variable structure

     ALLOCATE(vlist)
     
     status = nf90_inquire_variable(ncdf_ncid, ncdf_varid, vlist%name, &
                                    ndims=ndim, dimids = dimids)
     DO idim=1,ndim
        status = nf90_inquire_dimension(ncdf_ncid, dimids(idim),       &
                                         len = dimlen(idim))
     ENDDO

     ALLOCATE(vlist%data(dimlen(1),dimlen(2)))
     
     ! 3.3.2 Fill elements of added variable structure
     
     CALL ncdf_getvar(TRIM(vlist%name), vlist%data,    &
                      units = vlist%units,             &
                      range = vlist%range,             &
                      rec = irec)

     ! 3.3.3 Get variable long_name attribute (if exists)
     
     status = nf90_get_att(ncdf_ncid, ncdf_varid, "long_name", vlist%long_name)

  ENDIF

END SUBROUTINE ropp_io_read_ncdf_get_vlistD2d
