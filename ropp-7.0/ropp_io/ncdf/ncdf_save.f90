! $Id: $
!
!****s* Variables/ncdf_save
!
! NAME
!    ncdf_save - Save a variable to a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_save(name, values, long_name, units, dimension)
!
! INPUTS
!    character(len = *)           :: name       Name of the variable to be created.
!    (any numeric type)           :: values     Data to be written.
!    character(len = *), optional :: long_name  'long_name' attribute.
!    character(len = *), optional :: units      'units' attribute.
!    logical,            optional :: dimension  If .true., the variable is defines
!                                                 a coordinate dimension .
!
!    All optional paramaters should be accessed via keywords.
!
! DESCRIPTION
!    This subroutine defines and writes a variable to the current netCDF data
!    file. Error handling is done inside the routine.
!
! NOTES
!    The routine is intended to be used for the occasional storage of data
!    in netCDF data files, e.g. for diagnostic or debugging purposes, but
!    not for saving large amounts of data regularly. For the latter, it is
!    more efficient to first define and then write all variables to the
!    netCDF data file via the ncdf_defdim, ncdf_defvar and ncdf_varput
!    routines.
!
! EXAMPLE
!    Assume that you want to write two ariables x and y to a newly created
!    netCDF data file 'test.nc'. Try
!
!       use ncdf
!         ...
!       integer, parameter :: n_levels = <some_number>
!         ...
!       real               :: x(n_levels), y(n_levels)
!         ...
!       call ncdf_create('test.nc')
!       call ncdf_save('x', x, long_name = 'My coordinates', units = 'm', &
!                              dimension = .true.)
!       call ncdf_save('y', y, long_name = 'My function values', units = 'K')
!       call ncdf_close()
!
!    To add the same variables to an already existing netCDF data file 
!    'test2.nc', simply replace the ncdf_create() call by an ncdf_open() call:
!
!       call ncdf_open('test.nc')
!       call ncdf_save('x', x, long_name = 'My coordinates', units = 'm', &
!                              dimension = .true.)
!       call ncdf_save('y', y, long_name = 'My function values', units = 'K')
!       call ncdf_close()
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

!--------------------------------------------------------------------------
! 1. Scalar arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_OneByteInt (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'OneByteInt'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_OneByteInt


subroutine ncdf_save_TwoByteInt (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'TwoByteInt'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_TwoByteInt


subroutine ncdf_save_FourByteInt (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'FourByteInt'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_FourByteInt


subroutine ncdf_save_EightByteInt (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'EightByteInt'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_EightByteInt


subroutine ncdf_save_FourByteReal (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'FourByteReal'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_FourByteReal


subroutine ncdf_save_EightByteReal (name, values, long_name, units, file, &
                      ncid, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), &
                             intent(in)  :: values
  character(len = *),        optional    :: long_name
  character(len = *),        optional    :: units
  character(len = *),        optional    :: file
  integer,                   optional    :: ncid
  logical,                   optional    :: overwrite

  integer                                :: status, varid, ncid_local
  integer                                :: nc_type
  character(len = 64)                    :: var_type

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

! Variable's type
! ---------------

  var_type = 'EightByteReal'
  select case (var_type)
     case('OneByteInt', 'TwoByteInt','FourByteInt')
        nc_type = nf90_int
     case('FourByteReal')
        nc_type = nf90_float
     case('EightByteReal')
        nc_type = nf90_double
  end select

! Create the variable
! -------------------

  status = nf90_def_var(ncid_local, name, nc_type, varid = varid)
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

end subroutine ncdf_save_EightByteReal




!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_1D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 1

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

end subroutine ncdf_save_1D_OneByteInt


subroutine ncdf_save_1D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 1

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

end subroutine ncdf_save_1D_TwoByteInt


subroutine ncdf_save_1D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 1

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

end subroutine ncdf_save_1D_FourByteInt


subroutine ncdf_save_1D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 1

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

end subroutine ncdf_save_1D_EightByteInt


subroutine ncdf_save_1D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 1

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

end subroutine ncdf_save_1D_FourByteReal


subroutine ncdf_save_1D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_1D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 1

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

end subroutine ncdf_save_1D_EightByteReal



!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_2D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 2

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

end subroutine ncdf_save_2D_OneByteInt


subroutine ncdf_save_2D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 2

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

end subroutine ncdf_save_2D_TwoByteInt


subroutine ncdf_save_2D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 2

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

end subroutine ncdf_save_2D_FourByteInt


subroutine ncdf_save_2D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 2

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

end subroutine ncdf_save_2D_EightByteInt


subroutine ncdf_save_2D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 2

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

end subroutine ncdf_save_2D_FourByteReal


subroutine ncdf_save_2D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_2D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 2

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

end subroutine ncdf_save_2D_EightByteReal



!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_3D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 3

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

end subroutine ncdf_save_3D_OneByteInt


subroutine ncdf_save_3D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 3

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

end subroutine ncdf_save_3D_TwoByteInt


subroutine ncdf_save_3D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 3

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

end subroutine ncdf_save_3D_FourByteInt


subroutine ncdf_save_3D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 3

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

end subroutine ncdf_save_3D_EightByteInt


subroutine ncdf_save_3D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 3

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

end subroutine ncdf_save_3D_FourByteReal


subroutine ncdf_save_3D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_3D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 3

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

end subroutine ncdf_save_3D_EightByteReal



!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_4D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 4

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

end subroutine ncdf_save_4D_OneByteInt


subroutine ncdf_save_4D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 4

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

end subroutine ncdf_save_4D_TwoByteInt


subroutine ncdf_save_4D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 4

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

end subroutine ncdf_save_4D_FourByteInt


subroutine ncdf_save_4D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 4

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

end subroutine ncdf_save_4D_EightByteInt


subroutine ncdf_save_4D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 4

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

end subroutine ncdf_save_4D_FourByteReal


subroutine ncdf_save_4D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_4D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 4

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

end subroutine ncdf_save_4D_EightByteReal



!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_5D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 5

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

end subroutine ncdf_save_5D_OneByteInt


subroutine ncdf_save_5D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 5

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

end subroutine ncdf_save_5D_TwoByteInt


subroutine ncdf_save_5D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 5

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

end subroutine ncdf_save_5D_FourByteInt


subroutine ncdf_save_5D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 5

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

end subroutine ncdf_save_5D_EightByteInt


subroutine ncdf_save_5D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 5

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

end subroutine ncdf_save_5D_FourByteReal


subroutine ncdf_save_5D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_5D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 5

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

end subroutine ncdf_save_5D_EightByteReal



!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_6D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 6

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

end subroutine ncdf_save_6D_OneByteInt


subroutine ncdf_save_6D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 6

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

end subroutine ncdf_save_6D_TwoByteInt


subroutine ncdf_save_6D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 6

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

end subroutine ncdf_save_6D_FourByteInt


subroutine ncdf_save_6D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 6

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

end subroutine ncdf_save_6D_EightByteInt


subroutine ncdf_save_6D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 6

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

end subroutine ncdf_save_6D_FourByteReal


subroutine ncdf_save_6D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_6D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 6

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

end subroutine ncdf_save_6D_EightByteReal



!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_save_7D_OneByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_OneByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'OneByteInt'
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

  ndims = 7

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

end subroutine ncdf_save_7D_OneByteInt


subroutine ncdf_save_7D_TwoByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_TwoByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'TwoByteInt'
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

  ndims = 7

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

end subroutine ncdf_save_7D_TwoByteInt


subroutine ncdf_save_7D_FourByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_FourByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteInt'
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

  ndims = 7

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

end subroutine ncdf_save_7D_FourByteInt


subroutine ncdf_save_7D_EightByteInt (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_EightByteInt

  implicit none

  character(len = *),        intent(in)  :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteInt'
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

  ndims = 7

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

end subroutine ncdf_save_7D_EightByteInt


subroutine ncdf_save_7D_FourByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_FourByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'FourByteReal'
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

  ndims = 7

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

end subroutine ncdf_save_7D_FourByteReal


subroutine ncdf_save_7D_EightByteReal (name, values, long_name, units, file, &
                      ncid, dimension, overwrite)

  use typeSizes
  use ncdf, not_this => ncdf_save_7D_EightByteReal

  implicit none

  character(len = *),        intent(in)  :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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

  var_type = 'EightByteReal'
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

  ndims = 7

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

end subroutine ncdf_save_7D_EightByteReal


