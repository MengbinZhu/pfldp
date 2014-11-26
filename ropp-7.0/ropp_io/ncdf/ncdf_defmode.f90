! $Id: ncdf_defmode.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_defmode(ncfile, ncid)

!****f* Misc/ncdf_defmode
!
! NAME
!    ncdf_defmode - Put the current netCDF into define mode.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_defmode()
! 
! DESCRIPTION
!    Puts the current netCDF dataset into define mode, so that
!    dimensions, variables, and attributes can be added or renamed
!    and attributes  can be deleted.
!
! NOTES
!    The netCDF data file must have been opened before. If a netCDF
!    data has just been created, it already is in define mode.
!
!    This routine does not check for errors; if switching into define
!    mode is not possible, this is either because it already is in
!    this mode (e.g., after being created), in which case we don't
!    care, or some other problem, in which case the next operation is
!    likely to fail.
!
! EXAMPLE
!    To define (additional) variables or attributes in a netCDF data
!    file, do
!
!       call ncdf_defmode()
!          ...
!       <define variables, attributes, etc.>
!          ...
!       call ncdf_datmode()
!
! SEE ALSO
!    ncdf_datmode
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

  use ncdf, not_this => ncdf_defmode

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
! 3. Go into define mode
!-------------------------------------------------------------------------------

  status = nf90_redef(ncid)

end subroutine ncdf_defmode
