! $Id: ncdf_create.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_create(ncfile, cmode, ncid)

!****s* Files/ncdf_create
!
! NAME
!    ncdf_create - Create a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_create(file [, mode] [, ncid])
!
! DESCRIPTION
!    This subroutine creates a new netCDF data file.
!
! INPUT
!    file     Name of the netCDF data file to be created.
!    cmode    Mode for create. Default is NF90_CLOBBER, i.e. an existing
!                file with the same name is overwritten without warning.
!                An alternative is to use NF90_NOCLOBBER, which prohibits
!                the accidental overwriting of an already existing file.
!
! OUTPUT
!    ncid     NetCDF id of the opened netCDF data file.
!
! EXAMPLE
!    To create a new netCDF file:
!
!       call ncdf_create(file)
!
!    Create the same file, but stop with an error message if a file of the
!    same name already exist:
!
!       call ncdf_create(file, mode = NF90_NOCLOBBER)
!
!    Two netCDF files are created by the following commands:
!
!       call ncdf_create(file_a, ncid = ncid_a)
!       call ncdf_create(file_b, ncid = ncid_b)
!
!    Note that the current netCDF data file known to all ncdf routines is
!    the one opened last; if the first file is to be accessed, the netCDF id
!    needs to be passed on to the corresponding routines via the optional
!    ncid keyword. E.g.
!
!       call ncdf_save('var_b', var)                ! --> file_b
!       call ncdf_save('var_a', var, nid = ncid_a)  ! --> file_a
!
! SEE ALSO
!    is_netcdf
!    ncdf_open
!    ncdl_close
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

  use ncdf, not_this => ncdf_create

  implicit none

  character (len = *), intent(in) :: ncfile
  integer,             optional   :: cmode
  integer,             optional   :: ncid

  integer                         :: status

!------------------------------------------------------------------------------
! 2. Open netCDF data file
!------------------------------------------------------------------------------

  if (present(cmode)) then
     status = nf90_create(ncfile, cmode, ncdf_ncid)
  else
     status = nf90_create(ncfile, NF90_CLOBBER, ncdf_ncid)
  endif

  if (status /= nf90_noerr) call ncdf_error_handler(status)

!------------------------------------------------------------------------------
! 3. Set global and return variables 
!------------------------------------------------------------------------------

  ncdf_ncname = trim(ncfile)

  if (present(ncid)) then
     ncid = ncdf_ncid
  endif

end subroutine ncdf_create

