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
  use ncdf, not_this => NCDF_SFUN

  implicit none

  character(len = *),        intent( in) :: name
  character(len = *), &
                             intent(IN_OR_OUT`') :: values
  character(len = *), dimension(2), optional :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = 1), dimension(len(values)+1) &
                                         :: rvalues
  character(len(values))                 :: svalues
  character(len = NF90_MAX_NAME)         :: vname
  logical                                :: havegroup

ifelse(PUTORGET,`put',`
! pgf95 bugfix
! -----------
 
  svalues = values
'
)dnl
ifelse(PUTORGET,`get',`
! pgf95 bugfix
! -----------
 
  svalues(:) = " "
'
)dnl
	
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

! Obtain some information about the variables dimensionality
! ----------------------------------------------------------

  status = nf90_inquire_variable(ncid_local, varid, &
                                 ndims = ndims, dimids = dimids)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Prepare start and count arrays - these are the defaults
! -------------------------------------------------------

  strt = 1
  cnts = 0

! Note: First dim varies along string, second is record...
  status = nf90_inquire_dimension(ncid_local, dimids(1), len = cnts(1))
  if (status /= nf90_noerr) call ncdf_error_handler(status)
  cnts(1) = min(cnts(1), size(rvalues))

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
        cnts(i) = 1
     else
        call ncdf_error_handler(NF90_ENORECVARS)
     endif

! ...or start...
! --------------

  else if (present(start)) then

     strt(1:size(start)) = start

  endif
ifelse(PUTORGET,`put',`
! Copy values
! -----------

  do i = 1, len_trim(svalues)
     rvalues(i) = svalues(i:i)
  enddo
  if (i <= size(rvalues)) rvalues(i:) = achar(0)
'
)dnl
ifelse(PUTORGET,`get',`
! Initialise values
! -----------------

  rvalues(:) = achar(0)
'
)dnl
! Read/write values
! -----------------

  status = `nf90_'PUTORGET`_var(ncid_local, varid, rvalues, start = strt, count = cnts)'
  if (status /= nf90_noerr) call ncdf_error_handler(status)

ifelse(PUTORGET,`put',dnl
! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

)dnl
ifelse(PUTORGET,`get',`
! Copy values
! -----------

  do i = 1, len(values)
     if (iachar(rvalues(i)) > 0) then
        values(i:i) = rvalues(i)
     else
        values(i:i) = " "
     endif
  enddo

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.
'
)dnl

! Provide formal units
! --------------------

  if (present(units)) then
     units = ' '
  endif

! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine NCDF_SFUN
