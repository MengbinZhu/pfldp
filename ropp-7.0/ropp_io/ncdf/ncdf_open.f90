! $Id: ncdf_open.f90 2282 2009-10-22 09:49:29Z frhl $

subroutine ncdf_open(ncfile, ncid, mode, append)

!****s* Files/ncdf_open
!
! NAME
!    ncdf_open - Open a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_open(file [, ncid][, mode = ...][, append = [.true.|.false.]])
!
! DESCRIPTION
!    This subroutine opens an already existing netCDF data file. If the file
!    does not exist, an error will occur.
!
! INPUT
!    character(len = *) :: file      file to be opened.
!    integer            :: mode      mode in which the file is to be opened
!                                       (as defined in thenetCDF documentation)
!    logical            :: append    if .true., this is equivalent to
!                                       mode = nf90_write
!
! OUTPUT
!    ncid   - netCDF id of the opened netCDF data file.
!
! NOTES
!    The default is to open the netCDF in (shared) read-only mode. Setting the
!    logical argument append to .true. overwrites all mode = settings.
!
! EXAMPLE
!    To read the scalar variables var_a and var_b can be read from a netCDF
!    data file named 'test.nc':
!
!       real :: a, b
!         ...
!       call ncdf_open('test.nc')
!       call ncdf_getvar('var_a', a)
!       call ncdf_getvar('var_b', b)
!       call ncdf_close()
!
! SEE ALSO
!    is_netcdf
!    ncdf_create
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
!****

!------------------------------------------------------------------------------
! 1. Declarations
!------------------------------------------------------------------------------

  use ncdf, not_this => ncdf_open
  use messages

  implicit none

  character (len = *), intent(in) :: ncfile
  integer,             optional   :: ncid
  integer,             optional   :: mode
  logical,             optional   :: append

  integer                         :: status
  integer                         :: imode

  logical                         :: exist

!------------------------------------------------------------------------------
! 2. Make sure the file exists
!------------------------------------------------------------------------------

  inquire(file = ncfile, exist = exist)
  if (.not. exist) then
     call message(msg_fatal, &
          'File ' // trim(ncfile) // ' does not exist. Use ncdf_create() to ' // &
          'create a new netCDF data file.\n')
  endif

!------------------------------------------------------------------------------
! 3. Mode in which to open the file
!------------------------------------------------------------------------------

  if (present(mode)) then
     imode = mode
  else
     imode = NF90_SHARE
  endif

  if (present(append)) then
     if (append) then
        imode = NF90_WRITE
     endif
  endif

!------------------------------------------------------------------------------
! 4. Open netCDF data file
!------------------------------------------------------------------------------

  status = nf90_open(ncfile, imode, ncdf_ncid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


  status = nf90_inquire(ncdf_ncid, nVariables = ncdf_nvars)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!------------------------------------------------------------------------------
! 4. Set global and return variables 
!------------------------------------------------------------------------------

  ncdf_ncname = trim(ncfile)

  if (present(ncid)) then
     ncid = ncdf_ncid
  endif
  
  if(associated(ncdf_read)) deallocate(ncdf_read)
  allocate(ncdf_read(ncdf_nvars))
  ncdf_read(:) = .FALSE.

end subroutine ncdf_open

