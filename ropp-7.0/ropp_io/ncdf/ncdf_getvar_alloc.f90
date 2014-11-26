! $Id: ncdf_getvar_alloc.f90 3551 2013-02-25 09:51:28Z idculv $
!
!****f* Variables/ncdf_getvar_alloc
!
! NAME
!    ncdf_getvar_alloc - Read a variable from a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_getvar(varname, scalar, rec = ..., start = ...)             ! scalar
!      - or -
!    call ncdf_getvar(varname, array, rec = ..., start = ..., count = ...) ! array
!      - or -
!    call ncdf_getvar(ncfile, varname, scalar|array, ...)
!      - or -
!    call ncdf_getvar(ncid, varname, scalar|array, ...)
!
! INPUTS
!    character(len = *)     :: ncfile    Name of netCDF data file to read from.
!    integer                :: ncid      netCDF id of file to read from.
!
!    character(len = *)     :: varname   Name of variable to read.
!
!    integer, dimension(:)  :: start     array of start values (see netCDf documentation).
!    integer, dimension(:)  :: count     array of count values (see netCDF documentation).
!
!    Note that all keyword variables are optional.
!
! OUTPUT
!    real(dp)               :: scalar
!    real(dp), dimension(:) :: array
!
! DESCRIPTION
!    This subroutine reads a variable from the current netCDF data
!    file.
!
! EXAMPLE
!    Assume that you want to read variables alpha_b and impact from a netCDF
!    data file 'test.nc', try
!
!       use ncdf
!         ...
!       integer, parameter   :: n_levels = <some_number>
!         ...
!       real(dp)             :: alpha(n_levels), impact(n_levels)
!         ...
!       call ncdf_open('test.nc')
!       call ncdf_getvar('alpha_b', alpha)
!       call ncdf_getvar('impact',  impact)
!       call ncdf_close()
!
! SEE ALSO
!    ncdf_getvar
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

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_1D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_OneByteInt


subroutine ncdf_getval_1D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_TwoByteInt


subroutine ncdf_getval_1D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_FourByteInt


subroutine ncdf_getval_1D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_EightByteInt


subroutine ncdf_getval_1D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_FourByteReal


subroutine ncdf_getval_1D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_1D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 1
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 1) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1)))
  allocate(values(cnts(1)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_1D_EightByteReal



!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_2D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_OneByteInt


subroutine ncdf_getval_2D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_TwoByteInt


subroutine ncdf_getval_2D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_FourByteInt


subroutine ncdf_getval_2D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_EightByteInt


subroutine ncdf_getval_2D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_FourByteReal


subroutine ncdf_getval_2D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_2D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 2
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 2) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2)))
  allocate(values(cnts(1), cnts(2)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_2D_EightByteReal



!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_3D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_OneByteInt


subroutine ncdf_getval_3D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_TwoByteInt


subroutine ncdf_getval_3D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_FourByteInt


subroutine ncdf_getval_3D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_EightByteInt


subroutine ncdf_getval_3D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_FourByteReal


subroutine ncdf_getval_3D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_3D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 3
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 3) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3)))
  allocate(values(cnts(1), cnts(2), cnts(3)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_3D_EightByteReal



!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_4D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_OneByteInt


subroutine ncdf_getval_4D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_TwoByteInt


subroutine ncdf_getval_4D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_FourByteInt


subroutine ncdf_getval_4D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_EightByteInt


subroutine ncdf_getval_4D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_FourByteReal


subroutine ncdf_getval_4D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_4D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 4
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 4) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_4D_EightByteReal



!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_5D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_OneByteInt


subroutine ncdf_getval_5D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_TwoByteInt


subroutine ncdf_getval_5D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_FourByteInt


subroutine ncdf_getval_5D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_EightByteInt


subroutine ncdf_getval_5D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_FourByteReal


subroutine ncdf_getval_5D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_5D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 5
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 5) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_5D_EightByteReal



!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_6D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_OneByteInt


subroutine ncdf_getval_6D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_TwoByteInt


subroutine ncdf_getval_6D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_FourByteInt


subroutine ncdf_getval_6D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_EightByteInt


subroutine ncdf_getval_6D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_FourByteReal


subroutine ncdf_getval_6D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_6D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 6
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 6) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_6D_EightByteReal



!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_getval_7D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  integer(kind = OneByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = OneByteInt),                     dimension(2) :: rrange
  integer(kind = OneByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_OneByteInt


subroutine ncdf_getval_7D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  integer(kind = TwoByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = TwoByteInt),                     dimension(2) :: rrange
  integer(kind = TwoByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_TwoByteInt


subroutine ncdf_getval_7D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  integer(kind = FourByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = FourByteInt),                     dimension(2) :: rrange
  integer(kind = FourByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_FourByteInt


subroutine ncdf_getval_7D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  integer(kind = EightByteInt),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  integer(kind = EightByteInt),                     dimension(2) :: rrange
  integer(kind = EightByteInt) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_EightByteInt


subroutine ncdf_getval_7D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  real(kind = FourByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = FourByteReal),                     dimension(2) :: rrange
  real(kind = FourByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_FourByteReal


subroutine ncdf_getval_7D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_getval_7D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: values
  real(kind = EightByteReal),   dimension(2),      optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  integer, dimension(:),     optional    :: count
  character(len = *),        optional    :: units

  integer                                :: status, varid, ncid_local, groupid
  integer                                :: ndims, dimrec, i, j
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, cnts, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             allocatable :: rvalues
  real(kind = EightByteReal),                     dimension(2) :: rrange
  real(kind = EightByteReal) &
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

  j = 0


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
  cnts = 0

  do i = 1, 7
     status = nf90_inquire_dimension(ncid_local, dimids(i), len = cnts(i))
     if (status /= nf90_noerr) call ncdf_error_handler(status)
  enddo

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

! ...or start and count...

  else if (present(start) .and. present(count)) then

     strt(1:size(start)) = start
     cnts(1:size(count)) = count

! ...or only start...

  else if (present(start)) then

     strt(1:size(start)) = start
     cnts(1:ndims)       = strt(1:ndims) - cnts(1:ndims) + 1

! ...or only count...

  else if (present(count)) then

     cnts(1:size(count)) = count

  endif

! Check start and count arrays
! ----------------------------

  j = 0
  do i = 1, ndims
     if (cnts(i) > 1) then
       j = j + 1
     endif
  enddo

  if (j > 7) then
     call message_set_routine('ncdf_getvar')
     call message(msg_fatal, 'Data array does not have enough dimensions to hold requested data.')
  endif
! Allocate Memory
! ---------------

  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))
  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Read/write values
! -------------------

  status = nf90_get_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

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

! Convert units - if present & not the same as target and data valid
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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) values = rvalues
     endif	

  else
     values = rvalues
     if(present(range)) range = rrange
  endif

! Add to counter of number of variables read
! ------------------------------------------

  if (.not. (havegroup)) ncdf_read(varid) = .true.


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_getval_7D_EightByteReal


