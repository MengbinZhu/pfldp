! $Id: ncdf_defvar.f90 1959 2008-11-13 12:15:18Z frhl $

!****s* Variables/ncdf_defvar
!
! NAME
!    ncdf_defvar - Define a new variable in the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    varid = ncdf_defvar(name, long_name, units [, dimids])
!
! DESCRIPTION
!    This subroutine closes the current netCDF data file. Error handling
!    is done inside the routine.
!
! INPUTS
!    character(len = *)    :: name        Name of the newly created variable.
!    character(len = *)    :: long_name   Long name of the newly created variable.
!    character(len = *)    :: units       Units of the newly created variable.
!    integer, dimension(:) :: dimids      netCDF dimension ID's for the newly
!                                           created variable (see NOTES below).
!
! OUTPUT
!    integer               :: varid       netCDF id of newly created variable.
!
! NOTES
!    If dimids are not given, a scalar variable is created. An array variable
!    can be defined by specifying the ID's of the dimensions the variable
!    adheres to as elements in the dimids array.
!
! SEE ALSO
!    ncdf_defdim
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
! 1. Define a scalar variable
!------------------------------------------------------------------------------

function ncdf_defvar_sca(name, long_name, units, type, ncid, &
                         standard_name, positive, formula_terms, &
                         calendar, coordinates) &
                         result(varid)

  use ncdf, not_this => ncdf_defvar_sca

  implicit none

  character(len = *),    intent(in) :: name, long_name, units
  integer,               optional   :: type
  integer,               optional   :: ncid
  character(len = *),    optional   :: standard_name
  character(len = *),    optional   :: positive
  character(len = *),    optional   :: formula_terms
  character(len = *),    optional   :: calendar
  character(len = *),    optional   :: coordinates
  integer                           :: varid

  integer                           :: status, ncid_local

! netCDF ID
! ---------

  if (present(ncid)) then
     ncid_local = ncid
  else
     ncid_local = ncdf_ncid
  endif

! Create the variable
! -------------------

  if (present(type)) then
     status = nf90_def_var(ncid_local, name, type, varid = varid)
  else
     status = nf90_def_var(ncid_local, name, nf90_float, varid = varid)
  endif
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Add standard attributes
! -----------------------

  status = nf90_put_att(ncid_local, varid, "long_name", long_name)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

  if (present(type)) then
     if (type /= nf90_char) then
        if (len_trim(units) == 0) then
           status = nf90_put_att(ncid_local, varid, "units", '1')
        else
           status = nf90_put_att(ncid_local, varid, "units", units)
        endif
     endif
  else
     if (len_trim(units) == 0) then
        status = nf90_put_att(ncid_local, varid, "units", '1')
     else
        status = nf90_put_att(ncid_local, varid, "units", units)
     endif
  endif
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Add optional attributes
! -----------------------

  if (present(standard_name)) then
     status = nf90_put_att(ncid_local, varid, "standard_name", standard_name)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(positive)) then
     status = nf90_put_att(ncid_local, varid, "positive", positive)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(formula_terms)) then
     status = nf90_put_att(ncid_local, varid, "formula_terms", formula_terms)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(calendar)) then
     status = nf90_put_att(ncid_local, varid, "calendar", calendar)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(coordinates)) then
     status = nf90_put_att(ncid_local, varid, "coordinates", coordinates)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

end function ncdf_defvar_sca


!------------------------------------------------------------------------------
! 2. Define an array variable
!------------------------------------------------------------------------------

function ncdf_defvar_arr(name, long_name, units, dimids, type, ncid, &
                         standard_name, positive, formula_terms, &
                         calendar, coordinates) &
                         result(varid)

  use ncdf, not_this => ncdf_defvar_arr

  implicit none

  character(len = *),    intent(in) :: name, long_name, units
  integer, dimension(:), intent(in) :: dimids
  integer,               optional   :: type
  integer,               optional   :: ncid
  character(len = *),    optional   :: standard_name
  character(len = *),    optional   :: positive
  character(len = *),    optional   :: formula_terms
  character(len = *),    optional   :: calendar
  character(len = *),    optional   :: coordinates
  integer                           :: varid

  integer                           :: status, ncid_local

! netCDF ID
! ---------

  if (present(ncid)) then
     ncid_local = ncid
  else
     ncid_local = ncdf_ncid
  endif

! Create the variable
! -------------------

  if (present(type)) then
     status = nf90_def_var(ncid_local, name, type, dimids, varid)
  else
     status = nf90_def_var(ncid_local, name, nf90_float, dimids, varid)
  endif
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Add standard attributes
! -----------------------

  status = nf90_put_att(ncid_local, varid, "long_name", long_name)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

  if (present(type)) then
     if (type /= nf90_char) then
        if (len_trim(units) == 0) then
           status = nf90_put_att(ncid_local, varid, "units", '1')
        else
           status = nf90_put_att(ncid_local, varid, "units", units)
        endif
     endif
  else
     if (len_trim(units) == 0) then
        status = nf90_put_att(ncid_local, varid, "units", '1')
     else
        status = nf90_put_att(ncid_local, varid, "units", units)
     endif
  endif
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Add optional attributes
! -----------------------

  if (present(standard_name)) then
     status = nf90_put_att(ncid_local, varid, "standard_name", standard_name)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(positive)) then
     status = nf90_put_att(ncid_local, varid, "positive", positive)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(formula_terms)) then
     status = nf90_put_att(ncid_local, varid, "formula_terms", formula_terms)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(calendar)) then
     status = nf90_put_att(ncid_local, varid, "calendar", calendar)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

  if (present(coordinates)) then
     status = nf90_put_att(ncid_local, varid, "coordinates", coordinates)
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  endif

end function ncdf_defvar_arr
