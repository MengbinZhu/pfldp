! $Id: ncdf_defdim.f90 1959 2008-11-13 12:15:18Z frhl $

!****s* Dimensions/ncdf_defdim
!
! NAME
!    ncdf_defdim - Define a new dimension in the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    dimid = ncdf_defdim(name, size [, ncid])
!
! DESCRIPTION
!    This subroutine creates a new dimension in the current netCDF data
!    file. Error handling is done inside the routine.
!
! INPUTS
!    character(len = *) :: name        Name of the new dimension.
!    integer            :: size        Size (length) of the new dimension.
!                                      The netCDF parameter NF90_UNLIMITED
!                                      may also be passed to create an
!                                      unlimited netCDF variable.
!
! OUTPUT
!    integer            :: dimid       netCDF id of the new dimension.
!
! NOTES
!    The returned dimid is a scalar; for use in the ncdf_defvar() function,
!    arrays of dimid's are required. An unlimited dimension can be created
!    by passing the appropriate len argument to this subroutine, i.e. by
!
!       call ncdf_defdim('...', nf90_unlimited)
!
! SEE ALSO
!    ncdf_defvar
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
! 1. Define a dimension
!------------------------------------------------------------------------------

function ncdf_defdim(name, n, ncid) result(dimid)

! Declarations
! ------------

  use ncdf, not_this => ncdf_defdim

  implicit none

  character(len = *),    intent(in) :: name
  integer,               intent(in) :: n
  integer,               optional   :: ncid
  integer                           :: dimid

  integer                           :: status, ncid_local

! netCDF ID
! ---------

  if (present(ncid)) then
     ncid_local = ncid
  else
     ncid_local = ncdf_ncid
  endif

! Create the dimension
! --------------------

  status = nf90_def_dim(ncdf_ncid, name, n, dimid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

end function ncdf_defdim
