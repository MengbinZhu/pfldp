! $Id: ncdf_close.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_close(ncfile, ncid)

!****s* Files/ncdf_close
!
! NAME
!    ncdf_close - Close a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_close([ncid])
!
! DESCRIPTION
!    This subroutine closes a netCDF data file. If ncid is not given,
!    the current netCDF is closed.
!
! INPUTS
!    character(len = *) :: ncfile    Name of netCDF data file.
!    integer            :: ncid      netCDF id.
!
!    Note that all arguments are optional.
!
! EXAMPLE
!    The usual steps for reading data from a netCDF file are:
!
!       call ncdf_open(file)
!          ...
!    !  Read data
!          ...
!       call ncdf_close()
!
! SEE ALSO
!    is_netcdf
!    ncdf_open
!    ncdf_create
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

!------------------------------------------------------------------------------
! 1. Declarations
!------------------------------------------------------------------------------

  use ncdf, not_this => ncdf_close

  implicit none

  character(len = *), optional    :: ncfile
  integer,            optional    :: ncid

  integer                         :: ncid_local
  integer                         :: status

!------------------------------------------------------------------------------
! 2. See if this is the current netcdf
!------------------------------------------------------------------------------

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

!------------------------------------------------------------------------------
! 2. Close netCDF data file
!------------------------------------------------------------------------------

  status = nf90_close(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!------------------------------------------------------------------------------
! 3. Reset global variables
!------------------------------------------------------------------------------

  if (ncid_local == ncdf_ncid) then
     ncdf_ncname = ''
     ncdf_ncid   = -1
  endif

  if (associated(ncdf_read)) deallocate(ncdf_read)
  
end subroutine ncdf_close
