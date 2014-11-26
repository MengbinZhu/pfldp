! $Id: ncdf_sync.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_sync(ncfile, ncid)

!****s* Files/ncdf_sync
!
! NAME
!    ncdf_sync - Synchronise the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_sync(ncfile = ..., ncid = ...)
!
! DESCRIPTION
!    This subroutine synchronises the current netCDF data file to disc.
!
! INPUTS
!    character(len = *) :: ncfile    Name of netCDF data file.
!    integer            :: ncid      netCDF id.
!
!    Note that all arguments are optional.
!
! OUTPUT
!    None.
!
! NOTES
!    NetCDF data files open for writing allow to continuously add data to
!    the file, but need to be synchronised to disc before newly added data
!    can be accessed. Synching the file's contents always happens when
!    closing the netCDF, but some information might be required earlier and
!    without having to close and reopen the file. An example is obtaining
!    the number of records written so far.
!
!    Calling ncdf_sync() only makes sense if data has been written to a
!    netCDF file before. It probably involves some system overhead.
!
! EXAMPLE
!    Assume that you have written several records to a data file and have
!    to infer about characteristics of the written data - like the number
!    of records already written. Try
!
!       use ncdf
!         ...
!       integer              :: n
!         ...
!       call ncdf_create('test.nc')
!         ...
!       <define variables>
!         ...
!       <write part of the data>
!         ...
!       call ncdf_sync()
!       n = ncdf_getnrec()
!         ...
!       call ncdf_close()
!
! SEE ALSO
!    ncdf_create
!    ncdf_open
!    ncdf_close
!    ncdf_putvar
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

  use ncdf, not_this => ncdf_sync

  implicit none

  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  integer                                :: status, ncid_local

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
! 3. Synchronise the file
!------------------------------------------------------------------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

end subroutine ncdf_sync
