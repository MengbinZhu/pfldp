! $Id: ncdf_getshape.f90 1959 2008-11-13 12:15:18Z frhl $

subroutine ncdf_getshape(name, shape, ncfile, ncid)

!****s* Query/ncdf_getshape
!
! NAME
!    ncdf_getshape - Return the shape of a variable in a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_getshape(varname, shape, ncfile = ..., ncid = ...)
!
! DESCRIPTION
!    This subroutine gets the shape (i.e., number of dimensions) from
!    a variable in a netCDF data file. Error handling is done inside
!    the routine.
!
! INPUTS
!    character(len = *) :: varname   Name of variable.
!    integer            :: shape     Shape of variable.
!    character(len = *) :: ncfile    Name of netCDF data file.
!    integer            :: ncid      netCDF id.
!
!    Note that all but the first two variables are optional.
!
! OUTPUT
!    integer            :: shape
!
! EXAMPLE
!    Assume that you want to know the shape of a variable named 'alpha_b'
!    contained in a netCDF data file. Try
!
!       use ncdf
!         ...
!       integer         :: shape
!         ...
!       call ncdf_open('test.nc')
!         ...
!       call ncdf_getshape('alpha_b', shape)
!         ...
!       call ncdf_close()
!
! SEE ALSO
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

  use ncdf, not_this => ncdf_getshape

  implicit none

  character(len = *),        intent(in)  :: name
  integer,                   intent(out) :: shape
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  integer                                :: status, varid, ncid_local
  integer, dimension(NF90_MAX_VAR_DIMS)  :: dimids

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
! 3. Get variable ID
!------------------------------------------------------------------------------

  status = nf90_inq_varid(ncid_local, name, varid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!------------------------------------------------------------------------------
! 4. Obtain information about the variables dimensionality
!------------------------------------------------------------------------------

  status = nf90_inquire_variable(ncid_local, varid, &
                                 ndims = shape, dimids = dimids)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

end subroutine ncdf_getshape
