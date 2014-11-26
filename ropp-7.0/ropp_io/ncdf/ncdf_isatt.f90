! $Id: ncdf_isatt.f90 3551 2013-02-25 09:51:28Z idculv $

function ncdf_isatt(attname, varname, ncfile, ncid, xtype, len, attnum) result(it_is)

!****f* Files/ncdf_isatt
!
! NAME
!    ncdf_isatt - Check if attribute is present in a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    true_or_false = ncdf_isatt(attname, [varid, file, xtype, len, attnum])
!
! DESCRIPTION
!    This function inquires if a certain attribute (global if varid is not
!    present) exists in the netCDF file. Returns .true. if successful,
!    .false. otherwise.
!
! INPUTS
!    attname        - attribute name (global or local)
!    varname        - variable name (optional)
!    ncfile         - name of file to be searched (optional)
!    ncid           - ID of file to be searched (optional)
!
! OUTPUT
!    true_or_false  - logical value, .true. if attribute exists in netCDF data file.
!    xtype          - optional, type of attribute following netCDF library settings:
!                         NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT,
!                         NF90_FLOAT, and NF90_DOUBLE
!    len            - optional, length of attribute
!    attnum         - optional, attribute nubmer
!
! NOTES
!    By default, the current netCDF file is searched, else the optionally
!    file by name or by ID (name takes precedence if both are given)
!
! EXAMPLE
!    To check if an attribute exists in the currently open file,
!    and handle the situation accordingly:
!
!       if (ncdf_isatt('history')) then
!          call do_this()
!       else
!          call do_that()
!       endif
!    Check also for optional arguments:
!     - global attribute:
!       ncdf_isatt('software_version', ncfile=ncfile, xtype=xtype, len=len, attnum=attnum)
!     - local attribute of variable
!       ncdf_isatt('units', 'temperature', 'myfile.nc', xtype, len, attnum)
!
! SEE ALSO
!    ncdf_create
!    ncdf_open
!    ncdf_close
!    is_netcdf
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!    Modified arguments (ncfile now optional, added ncid) for compatibility
!    with ncdf_isvar() by ROM SAF ROPP Development Team
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
!***

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  use ncdf, not_this => ncdf_isatt
  use netcdf

  implicit none

  character (len = *), intent(in)            :: attname
  character (len = *), intent(in),  optional :: varname
  character (len = *), intent(in),  optional :: ncfile
  integer,             intent(in),  optional :: ncid
  integer,             intent(out), optional :: xtype, len, attnum

  logical                                    :: it_is

! local variables
  integer                                    :: lxtype, llen, lattnum, varid
  integer                                    :: istat
  integer                                    :: ncid_local

!-------------------------------------------------------------------------------
! 2. Initialization
!-------------------------------------------------------------------------------

  it_is  = .false.
  if (present(xtype))  xtype  = -1
  if (present(len))    len    = -1
  if (present(attnum)) attnum = -1

!-------------------------------------------------------------------------------
! 3. See if this is the current netcdf
!-------------------------------------------------------------------------------

  if (present(ncfile)) then
     if (ncfile == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        istat = nf90_open(ncfile, NF90_SHARE, ncid_local)
        if (istat /= NF90_NOERR) call ncdf_error_handler(istat)
     endif

  else if (present(ncid)) then
     ncid_local = ncid
  else
     ncid_local = ncdf_ncid
  endif

!-------------------------------------------------------------------------------
! 4. Check if file has attribute (either global or local)
!-------------------------------------------------------------------------------

  if (present(varname)) then
    istat = nf90_inq_varid(ncid_local, varname, varid)
    if (istat /= NF90_NOERR) return
    istat = nf90_inquire_attribute(ncid_local, varid, attname, lxtype, llen, lattnum)
    if (istat /= NF90_NOERR) return
  else
    istat = nf90_inquire_attribute(ncid_local, NF90_GLOBAL, attname, lxtype, llen, lattnum)
    if (istat /= NF90_NOERR) return
  end if

  it_is = .true.

! Set optional arguments
  if (present(xtype))  xtype  = lxtype
  if (present(len))    len    = llen
  if (present(attnum)) attnum = lattnum

end function ncdf_isatt
