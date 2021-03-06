dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl ncdf_getvar.f90.
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
subroutine NCDF_SFUN (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => NCDF_SFUN

  implicit none

  character(len = *),        intent( in) :: name
  TYPE, &
                             intent(IN_OR_OUT`') :: values
  TYPE,    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  TYPE &
                                         :: rvalues
  TYPE,    dimension(2)                  :: rrange
  TYPE &
                                         :: scale_factor, add_offset
  logical                                :: have_scale, have_offset
  logical                                :: have_range
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
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Variable not found: "// name
     call ncdf_error_handler(status)
     return
  endif

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_variable(ncid_local, varid, &
                                 ndims = ndims, dimids = dimids)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Obtain scaling factors for the variable
! ---------------------------------------

  status = nf90_get_att(ncid_local, varid, 'scale_factor', scale_factor)
  if (status == nf90_enotatt) then
     have_scale = .false.
     status = nf90_noerr
  else
     have_scale = .true.
  endif
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute scale_factor not found for variable: "// name
     call ncdf_error_handler(status)
  endif

  status = nf90_get_att(ncid_local, varid, 'add_offset', add_offset)
  if (status == nf90_enotatt) then
     have_offset = .false.
     status = nf90_noerr
  else
     have_offset = .true.
  endif
  if (status /= nf90_noerr) then
     WRITE ( *, FMT="(A)" ) "ERROR: Attribute add_offset not found for variable: "// name
     call ncdf_error_handler(status)
  endif
! Prepare start and count arrays - these are the defaults
! -------------------------------------------------------

  strt = 1

! Special cases: record is given...
! ---------------------------------

  if (present(rec)) then

!    ...see if an unlimited (record) dimension is available...

     status = nf90_inquire(ncid_local, unlimitedDimID = dimrec)
     if (status /= nf90_noerr .or. dimrec == -1) &
                                call ncdf_error_handler(status)

!    ...make sure this is true for the variable in question...

     i = 1
     do while (i <= ndims)
        if (dimids(i) == dimrec) exit
        i = i + 1
     enddo

!    ...and set the start array.

     if (i <= ndims) then
        strt(i) = rec
     else
        call ncdf_error_handler(NF90_ENORECVARS)
     endif

! ...or start...
! --------------

  else if (present(start)) then

     strt(1:size(start)) = start

  endif

ifelse(PUTORGET,`put',dnl
! Convert units
! -------------

  if (present(units)) then
     ncdf_units(:) = ' '
     status = nf90_get_att(ncid_local, varid, 'units', ncdf_units)
     if (status /= nf90_noerr) then
        WRITE ( *, FMT="(A)" ) "ERROR: Attribute units not found for variable: "// name
        call ncdf_error_handler(status)
     endif
     call ut_convert(values, units, rvalues, ncdf_units)
     
     ! Only convert valid data
     status = nf90_get_att(ncid_local, varid, 'valid_range', rrange)
     if (status /= nf90_noerr) then
	have_range=.false.
     else
        have_range=.true.
	if(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
     endif  

  else
     rvalues = values
  endif

! Scale variables if necessary
! ----------------------------

  if (have_scale .and. have_offset) then
     rvalues = (rvalues - add_offset) / scale_factor
  else if (have_scale .and. (.not. have_offset)) then
     rvalues = rvalues / scale_factor
  else if ((.not. have_scale) .and. have_offset) then
     rvalues = rvalues - add_offset
  endif
,dnl
! Copy data values
! ----------------

  rvalues = values
)
! Read/write values
! -----------------

  status = `nf90_'PUTORGET`_var(ncid_local, varid, rvalues, start = strt)'
  if (status /= nf90_noerr) call ncdf_error_handler(status)

ifelse(PUTORGET,`put',dnl
! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

)dnl
ifelse(PUTORGET,`get',dnl
! Scale variables if necessary
! ----------------------------

  if (have_scale .and. have_offset) then
     rvalues = rvalues * scale_factor + add_offset
  else if (have_scale .and. (.not. have_offset)) then
     rvalues = rvalues * scale_factor
  else if ((.not. have_scale) .and. have_offset) then
     rvalues = rvalues + add_offset
  endif

! Obtain range values - use defaults if not present
! -------------------

  if (present(range))then
     status = nf90_get_att(ncid_local, varid, 'valid_range', rrange)
     if (status /= nf90_noerr) then
       rrange = range
       have_range = .false.
     else
       have_range = .true.
     endif
  endif

! Convert units - if present & not the same as target
! -------------

  if (present(units)) then
     ncdf_units(:) = ' '
     status = nf90_get_att(ncid_local, varid, 'units', ncdf_units)
     if (status /= nf90_noerr) then
        WRITE ( *, FMT="(A)" ) "ERROR: Attribute units not found for variable: "// name
        call ncdf_error_handler(status)
     endif
     call ut_convert(rvalues, ncdf_units, values, units)

     if(present(range) .and. have_range) then
     	 call ut_convert(rrange, ncdf_units, range, units)

	! Only convert valid data
	if(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read (deactivated for n4)
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.

)dnl

! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine NCDF_SFUN
