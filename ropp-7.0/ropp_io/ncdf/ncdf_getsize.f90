! $Id: ncdf_getsize.f90 3551 2013-02-25 09:51:28Z idculv $

!****s* Query/ncdf_getsize
!
! NAME
!    ncdf_getsize - Return the size(s) of a variable in a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_getsize(varname, size [, dim], ncfile = ..., ncid = ...)
!
! DESCRIPTION
!    This subroutine gets the size of a variable (i.e., the number
!    of elements, or, if dim is specified, the number of elements
!    along the specified dimension) or all sizes of the variable
!    (i.e. number of elements along all dimensions in an array) from
!    this variable in a netCDF data file.
!
! INPUTS
!    character(len = *) :: varname   Name of variable.
!    integer            :: dim       Dimension (starting with 1).
!    character(len = *) :: ncfile    Name of netCDF data file.
!    integer            :: ncid      netCDF id of the netCDF data file.
!
!    Note that all but the first two arguments are optional.
!
! OUTPUT
!    integer                :: size
!      - or -
!    integer, dimension(:)  :: size
!
! EXAMPLE
!    Assume that you want to know the size of a 1d netCDF variable named
!    'data' and read that variable into an appropriately allocated array.
!    Try
!
!       use ncdf
!         ...
!       integer                             :: n
!         ...
!       real(wp), dimension(:), allocatable :: data
!         ...
!       call ncdf_open('test.nc')
!         ...
!       call ncdf_getsize('data', n)
!         ...
!       allocate(data(n))
!       call ncdf_getvar('data', data)
!         ...
!       call ncdf_close()
!
! NOTES
!    The routine, when called with a scalar size argument, will return the
!    total number of elements of a netCDF variable. To get the number of
!    elements along an individual dimension (if the variable has more than
!    one dimension), either call ncdf_getsize with the optional 'dim = '
!    argument, or with size being an integer array which will contain the
!    corresponding length for each dimension.
!
! SEE ALSO
!    ncdf_getshape
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

!---------------------------------------------------------------------
! 1. Full size of a variable or size along a dimension
!---------------------------------------------------------------------
 
subroutine ncdf_sgetsize(name, size_arr, dim, ncfile, ncid)

  use ncdf, not_this => ncdf_sgetsize

  implicit none

  character(len = *),        intent(in)  :: name
  integer,                   intent(out) :: size_arr
  integer,                   optional    :: dim
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: dimids, size_local
  character(len = NF90_MAX_NAME)         :: vname
  logical                                :: havegroup

! See if this is the current netcdf
! ---------------------------------

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

! Get group id if necessary and replace ncid_local with it
! --------------------------------------------------------

  status = ncdf_getgroupid(ncid_local, name, vname, groupid, havegroup)
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Group ID not found: "// name
     call ncdf_error_handler(status)
     return
  endif
  ncid_local = groupid



! Get variable ID
! ---------------

  status = nf90_inq_varid(ncid_local, vname, varid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Obtain information about the variables dimensionality
! -----------------------------------------------------

  status = nf90_inquire_variable(ncid_local, varid, &
                                 ndims = ndims, dimids = dimids)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Get information about the variables size
! ----------------------------------------

  if (present(dim)) then
     status = nf90_inquire_dimension(ncid_local, dimids(dim), len = size_arr)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  else
     do i = 1, ndims
        status = nf90_inquire_dimension(ncid_local, dimids(i), len = size_local(i))
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     enddo
     size_arr = product(size_local(:ndims))
  endif

end subroutine ncdf_sgetsize


!---------------------------------------------------------------------
! 2. Sizes of a variable along all dimensions
!---------------------------------------------------------------------
 
subroutine ncdf_agetsize(name, size_arr, ncfile, ncid)

  use ncdf, not_this => ncdf_agetsize

  implicit none

  character(len = *),        intent(in)  :: name
  integer, dimension(:),     intent(out) :: size_arr
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  integer                                :: status, varid, ncid_local
  integer                                :: ndims, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: dimids

! See if this is the current netcdf
! ---------------------------------

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

! Get variable ID
! ---------------

  status = nf90_inq_varid(ncid_local, name, varid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Obtain information about the variables dimensionality
! -----------------------------------------------------

  status = nf90_inquire_variable(ncid_local, varid, &
                                 ndims = ndims, dimids = dimids)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Get information about the variables size
! ----------------------------------------

  do i = 1, ndims
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = size_arr(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

end subroutine ncdf_agetsize
