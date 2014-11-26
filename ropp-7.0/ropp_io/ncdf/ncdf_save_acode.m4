dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl ncdf_save.f90.
dnl
dnl AUTHOR
dnl    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
dnl
dnl COPYRIGHT
dnl
dnl    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
dnl
dnl    All rights reserved.
dnl
dnl    Permission is hereby granted, free of charge, to any person obtaining
dnl    a copy of this software and associated documentation files (the
dnl    "Software"), to deal in the Software without restriction, including
dnl    without limitation the rights to use, copy, modify, merge, publish,
dnl    distribute, sublicense, and/or sell copies of the Software, and to
dnl    permit persons to whom the Software is furnished to do so, subject to
dnl    the following conditions:
dnl
dnl    The above copyright notice and this permission notice shall be
dnl    included in all copies or substantial portions of the Software as well
dnl    as in supporting documentation.
dnl
dnl    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
dnl    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
dnl    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
dnl    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
dnl    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
dnl    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
dnl    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
dnl
subroutine NCDF_ASAV (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => NCDF_ASAV

  implicit none

  character(len = *),        intent(in)  :: name
  TYPE, dimension(COLONS), &
                             INTENT_OR_POINTER :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: dimension
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  integer                                :: ndims, ndims_already
  integer                                :: dimsize, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: cnts, dimids
  character(len = NF90_MAX_NAME)         :: dimnam, dim_name
  character(len = 64)                    :: var_type
  logical                                :: use_as_dim

! See if this is the current netcdf
! ---------------------------------

  if (present(file)) then 
     if (file == ncdf_ncname) then
        ncid_local = ncdf_ncid
     else
        status = nf90_open(file, nf90_share, ncid)
        if (status /= nf90_noerr) call ncdf_error_handler(status)
     endif

  else if (present(ncid)) then

     ncid_local = ncid

  else

     ncid_local = ncdf_ncid

  endif

! Place netCDf into define mode
! -----------------------------

  status = nf90_redef(ncid_local)
  if (status /= nf90_noerr .and. status /= nf90_eindefine) then
     call ncdf_error_handler(status)
  endif

! Dimension name given?
! ---------------------

  use_as_dim = .false.

  if (present(dimension)) then
     use_as_dim = dimension
  endif

! Variable's type
! ---------------

  var_type = 'KINDVALUE'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Variable's size
! ---------------

  ndims = NUMDIMS

  cnts(:ndims) = shape(values)

! Check for (and create) dimensions
! ---------------------------------

  if (ndims == 1 .and. use_as_dim) then
     status = nf90_def_dim(ncid_local, name, cnts(1), dimids(1))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  else
     status = nf90_inquire(ncid_local, nDimensions = ndims_already)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
     do i = 1, ndims
        dim_name = 'Not found'
        do j = 1, ndims_already
           status = nf90_inquire_dimension(ncid_local, j, dimnam, dimsize)
           if (status /= nf90_noerr) call ncdf_error_handler(status)
           if (dimsize == cnts(i)) then
              dim_name = dimnam
              dimids(i) = j
              exit
           endif
        enddo
        if (dim_name == 'Not found') then
           ndims_already = ndims_already + 1
           write(dim_name, "('d',i3.3)") ndims_already
           status = nf90_def_dim(ncid_local, dim_name, cnts(i), dimids(i))
        endif
     enddo
  endif

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, dimids = dimids(1:ndims), &
                        varid = varid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Add standard attributes
! -----------------------

  if (present(long_name)) then
     status = nf90_put_att(ncid_local, varid, "long_name", long_name)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(units)) then
     if (len_trim(units) == 0) then
        status = nf90_put_att(ncid_local, varid, "units", '1')
     else
        status = nf90_put_att(ncid_local, varid, "units", units)
     endif
  endif
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! End define mode
! ---------------

  status = nf90_enddef(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Write data
! ----------

  status = nf90_put_var(ncid_local, varid, values)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

  if (present(overwrite)) then 
     continue
  endif

end subroutine NCDF_ASAV
