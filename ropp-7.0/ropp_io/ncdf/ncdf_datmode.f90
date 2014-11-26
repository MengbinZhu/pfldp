! $Id: ncdf_datmode.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_datmode(ncfile, ncid)

!****f* Misc/ncdf_datmode
!
! NAME
!    ncdf_datmode - Put the current netCDF into data (i.e., out of define) mode.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_datmode()
! 
! DESCRIPTION
!    Takes an open netCDF dataset out of define mode. The changes made to the
!    netCDF dataset while it was in define mode are checked and committed to
!    disk if no problems occurred. After a successful call, variable data can
!    be read or written to the dataset.
!
! NOTES
!    The netCDF data file must have been opened before. If a netCDF
!    data has just been opened, it already is in data mode.
!
!    This routine does not check for errors; if switching into data
!    mode is not possible, this is either because it already is in
!    this mode (e.g., after being opened), in which case we don't
!    care, or some other problem, in which case the next operation is
!    likely to fail.
!
! EXAMPLE
!    To read or write variables or attributes from or to a netCDF data
!    file after having added new attributes and/or variables, do
!
!       call ncdf_defmode()
!          ...
!       <define variables, attributes, etc.>
!          ...
!       call ncdf_datmode()
!
! SEE ALSO
!    ncdf_defmode
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

  use ncdf, not_this => ncdf_datmode

  implicit none

  character(len = *), optional   :: ncfile
  integer,            optional   :: ncid

  integer                        :: status
  integer                        :: ncid_local

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
! 3. Go into data mode
!-------------------------------------------------------------------------------

  status = nf90_enddef(ncid)

end subroutine ncdf_datmode
