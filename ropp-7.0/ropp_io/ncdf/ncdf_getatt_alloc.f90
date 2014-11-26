! $Id: ncdf_getatt_alloc.f90 3551 2013-02-25 09:51:28Z idculv $
!
!****s* Attributes/ncdf_getatt_alloc
!
! NAME
!    ncdf_getatt_alloc - Read an attribute from a netCDF data file.
!
! SYNOPSIS
!    call ncdf_getatt(attname, value)              ! global attribute
!      - or -
!    call ncdf_getatt(varname, attname, value)     ! variable's attribute
!
! DESCRIPTION
!    This subroutine reads attribute data from the current netCDF file.
!
! INPUTS
!    varname   Name of variable to which the attribute belongs. If varname
!                 is not given, a global attribute is read.
!    attname   Name of attribute to be read.
!
! OUTPUT
!    value
!
! SEE ALSO
!    ncdf_getatt
!    ncdf_putatt
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
! COPYRIGHT
!
!    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
!
!    All rights reserved.
!
!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software as well
!    as in supporting documentation.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
!    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
!    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getatl_1D_OneByteInt (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_OneByteInt

  implicit none

  character(len = *),        intent( in) :: attname
  integer(kind = OneByteInt), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_OneByteInt


subroutine ncdf_getatl_1D_TwoByteInt (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_TwoByteInt

  implicit none

  character(len = *),        intent( in) :: attname
  integer(kind = TwoByteInt), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_TwoByteInt


subroutine ncdf_getatl_1D_FourByteInt (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_FourByteInt

  implicit none

  character(len = *),        intent( in) :: attname
  integer(kind = FourByteInt), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_FourByteInt


subroutine ncdf_getatl_1D_EightByteInt (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_EightByteInt

  implicit none

  character(len = *),        intent( in) :: attname
  integer(kind = EightByteInt), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_EightByteInt


subroutine ncdf_getatl_1D_FourByteReal (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_FourByteReal

  implicit none

  character(len = *),        intent( in) :: attname
  real(kind = FourByteReal), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_FourByteReal


subroutine ncdf_getatl_1D_EightByteReal (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => ncdf_getatl_1D_EightByteReal

  implicit none

  character(len = *),        intent( in) :: attname
  real(kind = EightByteReal), dimension(:), &
                             pointer :: values
  character(len = *),        optional    :: varname
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  character(len = NF90_MAX_NAME)         :: aname
  integer                                :: status, varid, ncid_local, groupid
  integer                                :: len
  logical                                :: havegroup

  len = 0

! See if this is the current netcdf
! ---------------------------------

  if (present(ncfile)) then 
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(ncfile, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, attname, aname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// attname
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

! Get variable ID
! ---------------

  if (present(varname)) then
     if (varname == 'global') then
        varid = NF90_GLOBAL
     else
        status = nf90_inq_varid(ncid_local, varname, varid)
        if (status /= nf90_noerr) then
           WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// varname
           call ncdf_error_handler(status)
        endif
     endif
  else
     varid = NF90_GLOBAL
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_attribute(ncid_local, varid, aname, len = len)
  if (status /= nf90_noerr) then
    WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
    call ncdf_error_handler(status)
  endif

! Allocate Memory
! ---------------

  allocate(values(len))

! Read/write values
! -------------------

  status = nf90_get_att(ncid_local, varid, aname, values)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine ncdf_getatl_1D_EightByteReal


