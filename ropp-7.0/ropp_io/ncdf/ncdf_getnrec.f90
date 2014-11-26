! $Id: ncdf_getnrec.f90 3551 2013-02-25 09:51:28Z idculv $

function ncdf_getnrec(ncfile, ncid) result(n_unlimited)

!****f* Query/ncdf_getnrec
!
! NAME
!    ncdf_getnrec - Return number of records in a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    n_unlimited = ncdf_get_getnrec(ncfile = ..., ncid = ...)
!
! DESCRIPTION
!    This function returns the number of records (i.e., the maximum
!    value of a record number used for writing any variable along an
!    unlimited dimension) in a netCDF data file. Error handling is
!    done inside the routine.
!
! INPUTS
!    character(len = *) :: ncfile    Name of netCDF data file.
!    integer            :: ncid      netCDF id.
!
!    Note that all arguments are optional.
!
! OUTPUT
!    integer            :: n_unlimited
!
! EXAMPLE
!    Assume that you want to know how many records were already written
!    a data file and append it with another record. Try
!
!       use ncdf
!         ...
!       integer              :: n
!         ...
!       call ncdf_open('test.nc')
!         ...
!       n = ncdf_getnrec()
!       call ncdf_putvar('variable', data, rec = n + 1)
!         ...
!       call ncdf_close()
!
! SEE ALSO
!    ncdf_getshape
!    ncdf_getsize
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
  use ncdf, not_this => ncdf_getnrec

  implicit none

  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer                                :: n_unlimited

  integer                                :: status, dimid, ncid_local

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
! 3. Get unlimited dimension ID
!------------------------------------------------------------------------------

  ! FIXME: with the current EUM netCDF files, there is no unlimited dimension for profiles
  !        this should however actually be checked here somehow

  status = nf90_inquire(ncid_local, unlimitedDimId = dimid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!------------------------------------------------------------------------------
! 4. Get length of unlimited dimension
!------------------------------------------------------------------------------

  if ( dimid == -1 ) then
     n_unlimited=1
  else 
     status = nf90_inquire_dimension(ncid_local, dimid, len = n_unlimited)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

end function ncdf_getnrec
