! $Id: ncdf_isvar.f90 3551 2013-02-25 09:51:28Z idculv $

function ncdf_isvar(name, ncfile, ncid) result(it_is)

!****f* Variables/ncdf_isvar
!
! NAME
!    ncdf_isvar - Check if a given variable is in the current netCDF
!                 data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    true_or_false = ncdf_isvar(name)
! 
! DESCRIPTION
!    This function tries to inquire if a given named variable does exist
!    within the current netCDF data file, and returns .true. if it does,
!    or .false. otherwise.
!
! INPUTS
!    name (str)     -  variable name to be checked.
!
! OUTPUT
!    true_or_false  -  logical value, .true. if varname exist in the current 
!                         netCDF data file.
!
! NOTES
!    The netCDF data file must have been opened before.
!
! EXAMPLE
!    To check if a variable is contained in the current netCDF, and handle
!    the situation accordingly:
!
!       if (ncdf_isvar(name)) then
!          call do_this()
!       else
!          call do_that()
!       endif
!
! SEE ALSO
!    ncdf_create
!    ncdf_open
!    ncdf_close
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
!***

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  use ncdf, not_this => ncdf_isvar

  implicit none

  character(len = *), intent(in) :: name
  character(len = *), optional   :: ncfile
  integer,            optional   :: ncid
  logical                        :: it_is

  integer                        :: status
  integer                        :: varid, groupid
  integer                        :: ncid_local
  character(len = NF90_MAX_NAME) :: vname
  logical                        :: havegroup

!-------------------------------------------------------------------------------
! 2. See if this is the current netcdf
!-------------------------------------------------------------------------------

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

!-------------------------------------------------------------------------------
! 3. Get group ID
!-------------------------------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, name, vname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// name
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid

!-------------------------------------------------------------------------------
! 4. Get variable ID
!-------------------------------------------------------------------------------

  status = nf90_inq_varid(ncid_local, vname, varid)

  if (status == nf90_noerr) then
     it_is = .true.
  else
     it_is = .false.
  endif

end function ncdf_isvar
