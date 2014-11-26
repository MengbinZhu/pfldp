! $Id: ncdf_renvar.f90 2282 2009-10-22 09:49:29Z frhl $

!****s* Variables/ncdf_renvar
!
! NAME
!    ncdf_renvar - Rename a variable in a netCDF data file.
!
! SYNOPSIS
!    call ncdf_renvar(old_name, new_name [, file] [, ncid])
! 
! DESCRIPTION
!    This routine renames an exiting variable in the current netCDF data file
!    (or the netCDF file specified by either file or ncid).
!
! INPUTS
!    old_name  -  old variable name.
!    new_name  -  new variable name
!    file      -  file name of the netCDF data file.
!    ncid      -  netCDF id of the netCDF data file.
!
! NOTES
!    The optional file and ncid arguments are mutually exclusive.
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

subroutine ncdf_renvar(old_name, new_name, ncfile, ncid)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  use ncdf, not_this => ncdf_renvar
  use messages

  implicit none

  character(len = *),        intent(in)  :: old_name
  character(len = *),        intent(in)  :: new_name
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid

  integer                                :: status, varid, ncid_local

  character(len = 256)                   :: routine

!-------------------------------------------------------------------------------
! 2. Error handling
!-------------------------------------------------------------------------------

  call message_get_routine(routine)
  call message_set_routine('ncdf_renvar')

!-------------------------------------------------------------------------------
! 3. See if this is the current netcdf
!-------------------------------------------------------------------------------
 
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

!------------------------------------------------------------------------------- 
! 4. Get variable ID
!-------------------------------------------------------------------------------
 
  status = nf90_inq_varid(ncid_local, old_name, varid)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!-------------------------------------------------------------------------------
! 5. Rename variable
!-------------------------------------------------------------------------------

  status = nf90_redef(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

  status = nf90_rename_var(ncid_local, varid, new_name)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

  status = nf90_enddef(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

!-------------------------------------------------------------------------------
! X. Clean up
!-------------------------------------------------------------------------------

  call message_set_routine(routine)

end subroutine ncdf_renvar
