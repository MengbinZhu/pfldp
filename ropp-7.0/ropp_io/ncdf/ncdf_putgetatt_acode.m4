dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl ncdf_getvar.f90.
dnl
dnl AUTHOR
dnl    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
dnl
dnl COPYRIGHT
dnl
dnl    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
dnl
dnl    All rights reserved.
dnl
dnl    Permission is hereby granted, free of charge, to any person obtaining
dnl    a copy of this software and associated documentation files (the
dnl    "Software"), to deal in the Software without restriction, including
dnl    without limitation the rights to use, copy, modify, merge, publish,
dnl    distribute, sublicense, and/or sell copies of the Software, and to
dnl    permit persons to whom the Software is furnished to do so, subject to
dnl    the following conditions:
dnl
dnl    The above copyright notice and this permission notice shall be
dnl    included in all copies or substantial portions of the Software as well
dnl    as in supporting documentation.
dnl
dnl    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
dnl    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
dnl    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
dnl    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
dnl    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
dnl    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
dnl    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
dnl
subroutine NCDF_AFUNATT (attname, values, varname, ncfile, ncid)

  use typeSizes
  use ncdf, not_this => NCDF_AFUNATT

  implicit none

  character(len = *),        intent( in) :: attname
  TYPE, dimension(COLONS), &
                             INTENT_OR_POINTER :: values
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

ifelse(POINTER, `yes',dnl
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
)dnl

! Read/write values
! -------------------

  status = `nf90_'PUTORGET`_att(ncid_local, varid, aname, values)'
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute not found: "// attname
     call ncdf_error_handler(status)
  endif

end subroutine NCDF_AFUNATT
