! $Id: ncdf_error_handler.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_error_handler(status)

!****s* Misc/ncdf_error_handler
!
! NAME
!    ncdf_error_handler - (Internal) error handler for the ncdf library.
!
! SYNOPSIS
!    use ncdf
!      ...
!    status = <some netCDF function>
!    if (status /= nf90_noerr) call ncdf_error_handler(status)
!
! DESCRIPTION
!    This subroutine issues a textual error message from a netCDF error status.
!
! INPUTS
!    integer     :: status
!
! EXAMPLE
!    Assume that you want to read variables alpha_b and impact from a netCDF
!    data file 'test.nc', try
!
!       use ncdf
!         ...
!       integer, parameter   :: n_levels = <some_number>
!         ...
!       real(dp)             :: alpha(n_levels), impact(n_levels)
!         ...
!       call ncdf_create('test.nc')
!         ...
!       status = nf90_put_att(ncdf_ncid, nf90_global, "title", trim(title))
!        if (status /= nf90_noerr) call ncdf_error_handler(status)
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

  use messages
  use ncdf, not_this => ncdf_error_handler

  implicit none

  integer,  intent(in)  :: status

!  integer               :: istat

  character(len = 4096) :: text
  character(len = 1024) :: ncname

!------------------------------------------------------------------------------
! 2. Close (and possibly delete) the netcdf data file
!------------------------------------------------------------------------------

  ncname = ncdf_ncname

  call ncdf_close()

!  if (ncdf_delete_on_error) then
!     call file_delete(ncname, istat)
!  endif

!------------------------------------------------------------------------------
! 3. Issue an error
!------------------------------------------------------------------------------

  if (status /= nf90_noerr) then
     if (ncdf_delete_on_error) then
        text = trim(nf90_strerror(status)) // '\n   ' // 'File ' // &
               trim(ncname) // ' deleted.\n'
     else
        text = trim(nf90_strerror(status))
     endif
     call message(msg_fatal, trim(text), status)
  end if

end subroutine ncdf_error_handler
