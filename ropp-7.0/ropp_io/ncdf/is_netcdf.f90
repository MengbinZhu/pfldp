! $Id: is_netcdf.f90 2282 2009-10-22 09:49:29Z frhl $

function is_netcdf(file) result(it_is)

!****f* Files/is_netcdf
!
! NAME
!    is_netcdf - Check if a file is a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    true_or_false = is_netcdf(file)
! 
! DESCRIPTION
!    This function tries to open a file as a netCDF data file, and returns
!    .true. if successful, .false. otherwise.
!
! INPUTS
!    file           -  file to be checked.
!
! OUTPUT
!    true_or_false  -  logical value, .true. if file is a netCDF data file.
!
! NOTES
!    The netCDF data file is opened and (if successful) closed again.
!
! EXAMPLE
!    To check if a file is a netCDF, and handle the situation accordingly:
!
!       if (is_netcdf(file)) then
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

  use messages
  use netcdf

  implicit none

  character(len = *), intent(in) :: file
  logical                        :: it_is

  logical                        :: exist

  integer                        :: istat
  integer                        :: ncid

  character(len = 256)           :: routine

!-------------------------------------------------------------------------------
! 2. Error handling
!-------------------------------------------------------------------------------

  call message_get_routine(routine)
  call message_set_routine('is_netcdf')

!-------------------------------------------------------------------------------
! 3. Check if file exists
!-------------------------------------------------------------------------------

  inquire(file = file, exist = exist)
  if (.not. exist) then
     call message(msg_fatal, &
          trim(file) // ' does not exist.\n')
  endif

!-------------------------------------------------------------------------------
! 4. Check if file is a netcdf
!-------------------------------------------------------------------------------

  istat = nf90_open(file, NF90_NOWRITE, ncid)

  if (istat == NF90_NOERR) then
     it_is = .true.
     istat = nf90_close(ncid)
  else
     it_is = .false.
  endif

!-------------------------------------------------------------------------------
! 5. Clean up
!-------------------------------------------------------------------------------

  call message_set_routine(routine)

end function is_netcdf
