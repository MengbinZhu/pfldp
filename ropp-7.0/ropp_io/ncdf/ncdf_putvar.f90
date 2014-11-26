! $Id: ncdf_putvar.f90 3551 2013-02-25 09:51:28Z idculv $
!
!****s* Variables/ncdf_putvar
!
! NAME
!    ncdf_putvar - Write a variable to a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_putvar(varname, data)
!      - or -
!    call ncdf_putvar(varname, data [, start = (/.../)[, count = (/.../)]])
!      - or -
!    call ncdf_putvar(varname, data [, rec = ...])
!      - or -
!    call ncdf_putvar(varname, data [, ncfile = ...], ...)
!      - or -
!    call ncdf_putvar(varname, data [, ncid = ...], ...)
!
! DESCRIPTION
!    This subroutine writes data into a variable of a netCDF data file. The
!    variable must have been defined previously.
!
!    If neither start and/or count nor rec are given, the data is simply
!    written to the specified netCDF variable. If start and count are given,
!    those define into which hyperslab of the netCDF variable the data is
!    written; see the netCDF documentation for details. If only start values
!    are given, the remaining part of the respective dimensions of the variable
!    in the netCDF file will be filled with data. If only count values are
!    given, data is written starting at the first element of each dimension
!    of the variable in the netCDF data file.
!
!    The optional keyword rec is provided as a faster alternative to the
!    start / count keywords for netCDF data files with an unlimited record
!    dimension, and is only applicable in this case (and only if the netCDF
!    variable to be written into is defined using this dimension); see examples
!    below. 
!
!    By default, data is written into the current netCDF data file. If either
!    ncid (the numeric id of a previously opened netCDF data file) or ncfile
!    (the full path name to an already existing netCDF data file) are given,
!    the corresponsing file will be used for output instead, and subsequently
!    becomes the 'current' netCDF data file for all tasks performed by the
!    ncdf library.
!
! INPUTS
!    character(len = *)    :: varname   Name of netCDF variable to write.
!    any type              :: data      Data to be written (scalar or array).
!
!    The following arguments are all optional:
!    integer               :: rec       Number or record for an unlimited
!                                         dimension (if it exists).
!    integer, dimension(:) :: start     start values.
!    integer, dimension(:) :: count     count values.
!    character(len = *)    :: ncfile    Name of netCDF data file to write to.
!    integer               :: ncid      netCDF id of file to write to.
!
! OUTPUT
!    None; however, data is written to the current (or otherwise specified)
!    netCDF data file.
!
! NOTES
!    The use of the optional arguments / keywords rec and start/count are
!    mutually exclusive, as are the keywords ncid and ncfile.
!
! EXAMPLE
!    To write an n-dimensional 'data' array into a netCDF variable named
!    'field' within the current netCDF data file, where the netCDF variable
!    has been defined to be consistent with the declaration of 'data', use
!
!       call ncdf_putvar('field', data)
!
!    Assume that a netCDF variable 'bulk' describes a 3 dimensional data
!    field along dimensions x, y, and z. Writing a two dimensional hyperslab
!    'data' filling all values along the x and z coordinates, but for only
!    one index 'i' in y, use
!
!       call ncdf_putvar('bulk', data, start = (/1,   i, 1/), $
!                                      count = (/n_x, 1, n_z/))
!
!    where n_x and n_z denote the number of data points along the x and z
!    dimensions, respectively.
!
!    Assume that the netCDF variable 'field' is to hold a horizontal field
!    depending on longitude, latitude and time; time is intended to vary
!    along an unlimited netCDF dimension. If the variable 'array' holds a
!    two-dimensional hyperslap of the data set valid at time index 'i',
!    this
!
!       call ncdf_putvar('field', array, rec = i)
!
!    is a slightly easier way to write that hyperslab compared to the
!    (otherwise equivalent) alternative
!
!       call ncdf_putvar('field', array, start = (/1,     1, i/), $
!                                        count = (/n_x, n_y, 1))
!
! SEE ALSO
!    ncdf_defvar
!    ncdf_getvar
!    ncdf_getvar_alloc
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

subroutine ncdf_putvar_OneByteInt (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_OneByteInt

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), &
                             intent(in ) :: values
  integer(kind = OneByteInt),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = OneByteInt) &
                                         :: rvalues
  integer(kind = OneByteInt),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_OneByteInt


subroutine ncdf_putvar_TwoByteInt (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_TwoByteInt

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), &
                             intent(in ) :: values
  integer(kind = TwoByteInt),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = TwoByteInt) &
                                         :: rvalues
  integer(kind = TwoByteInt),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_TwoByteInt


subroutine ncdf_putvar_FourByteInt (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_FourByteInt

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), &
                             intent(in ) :: values
  integer(kind = FourByteInt),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = FourByteInt) &
                                         :: rvalues
  integer(kind = FourByteInt),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_FourByteInt


subroutine ncdf_putvar_EightByteInt (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_EightByteInt

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), &
                             intent(in ) :: values
  integer(kind = EightByteInt),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  integer(kind = EightByteInt) &
                                         :: rvalues
  integer(kind = EightByteInt),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_EightByteInt


subroutine ncdf_putvar_FourByteReal (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_FourByteReal

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), &
                             intent(in ) :: values
  real(kind = FourByteReal),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = FourByteReal) &
                                         :: rvalues
  real(kind = FourByteReal),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_FourByteReal


subroutine ncdf_putvar_EightByteReal (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_EightByteReal

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), &
                             intent(in ) :: values
  real(kind = EightByteReal),    dimension(2),     optional    :: range
  character(len = *),        optional    :: ncfile
  integer,                   optional    :: ncid
  integer,                   optional    :: rec
  integer, dimension(:),     optional    :: start
  character(len = *),        optional    :: units

  integer                                :: status, varid, groupid, ncid_local
  integer                                :: ndims, dimrec, i
  integer, dimension(NF90_MAX_VAR_DIMS)  :: strt, dimids
  character(len = NF90_MAX_NAME)         :: ncdf_units
  real(kind = EightByteReal) &
                                         :: rvalues
  real(kind = EightByteReal),    dimension(2)                  :: rrange
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

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

end subroutine ncdf_putvar_EightByteReal


subroutine ncdf_putvar_text (name, values, ncfile, ncid, rec, start, units, range)

  use typeSizes
  use ncdf, not_this => ncdf_putvar_text

  implicit none

  character(len = *),        intent( in) :: name
  character(len = *), &
                             intent(in ) :: values
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


! pgf95 bugfix
! -----------
 
  svalues = values

	
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

! Copy values
! -----------

  do i = 1, len_trim(svalues)
     rvalues(i) = svalues(i:i)
  enddo
  if (i <= size(rvalues)) rvalues(i:) = achar(0)

! Read/write values
! -----------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


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

end subroutine ncdf_putvar_text



!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_1D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_OneByteInt


subroutine ncdf_putvar_1D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_TwoByteInt


subroutine ncdf_putvar_1D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_FourByteInt


subroutine ncdf_putvar_1D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_EightByteInt


subroutine ncdf_putvar_1D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_FourByteReal


subroutine ncdf_putvar_1D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_1D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:1) = shape(values)
  allocate(rvalues(cnts(1)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_1D_EightByteReal



!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_2D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_OneByteInt


subroutine ncdf_putvar_2D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_TwoByteInt


subroutine ncdf_putvar_2D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_FourByteInt


subroutine ncdf_putvar_2D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_EightByteInt


subroutine ncdf_putvar_2D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_FourByteReal


subroutine ncdf_putvar_2D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_2D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:2) = shape(values)
  allocate(rvalues(cnts(1), cnts(2)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_2D_EightByteReal



!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_3D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_OneByteInt


subroutine ncdf_putvar_3D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_TwoByteInt


subroutine ncdf_putvar_3D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_FourByteInt


subroutine ncdf_putvar_3D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_EightByteInt


subroutine ncdf_putvar_3D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_FourByteReal


subroutine ncdf_putvar_3D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_3D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:3) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_3D_EightByteReal



!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_4D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_OneByteInt


subroutine ncdf_putvar_4D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_TwoByteInt


subroutine ncdf_putvar_4D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_FourByteInt


subroutine ncdf_putvar_4D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_EightByteInt


subroutine ncdf_putvar_4D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_FourByteReal


subroutine ncdf_putvar_4D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_4D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:4) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_4D_EightByteReal



!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_5D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_OneByteInt


subroutine ncdf_putvar_5D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_TwoByteInt


subroutine ncdf_putvar_5D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_FourByteInt


subroutine ncdf_putvar_5D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_EightByteInt


subroutine ncdf_putvar_5D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_FourByteReal


subroutine ncdf_putvar_5D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_5D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:5) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_5D_EightByteReal



!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_6D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_OneByteInt


subroutine ncdf_putvar_6D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_TwoByteInt


subroutine ncdf_putvar_6D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_FourByteInt


subroutine ncdf_putvar_6D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_EightByteInt


subroutine ncdf_putvar_6D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_FourByteReal


subroutine ncdf_putvar_6D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_6D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:6) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_6D_EightByteReal



!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

subroutine ncdf_putvar_7D_OneByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_OneByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_OneByteInt


subroutine ncdf_putvar_7D_TwoByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_TwoByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_TwoByteInt


subroutine ncdf_putvar_7D_FourByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_FourByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_FourByteInt


subroutine ncdf_putvar_7D_EightByteInt (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_EightByteInt 

  implicit none

  character(len = *),        intent( in) :: name
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_EightByteInt


subroutine ncdf_putvar_7D_FourByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_FourByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_FourByteReal


subroutine ncdf_putvar_7D_EightByteReal (name, values, ncfile, ncid, rec, start, count, units, range)

  use typeSizes
  use messages
  use unitconvert, only: ut_convert
  use ncdf, not_this => ncdf_putvar_7D_EightByteReal 

  implicit none

  character(len = *),        intent( in) :: name
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in ) :: values
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
! Allocate memory
! ---------------

  cnts(:7) = shape(values)
  allocate(rvalues(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))


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
     cnts(i) = min(cnts(i), size(values, i))
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

! Convert units or copy data values
! ---------------------------------

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
	where(rvalues < rrange(1) .or. rvalues > rrange(2)) rvalues = values
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

! Read/write values
! -------------------

  status = nf90_put_var(ncid_local, varid, rvalues, start = strt, count = cnts)
  if (status /= nf90_noerr) call ncdf_error_handler(status)

! Synchronize netCDF
! ------------------

  status = nf90_sync(ncid_local)
  if (status /= nf90_noerr) call ncdf_error_handler(status)


! Dummy lines to avoid warnings - ignore
! --------------------------------------
  if (present(range)) then 
    continue
  endif

! Deallocate memory
! -----------------

  deallocate(rvalues)

end subroutine ncdf_putvar_7D_EightByteReal


