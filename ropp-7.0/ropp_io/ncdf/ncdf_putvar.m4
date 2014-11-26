dnl $Id: ncdf_putvar.m4 2282 2009-10-22 09:49:29Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf_putvar.f90 file.
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
dnl --------------------------------------------------------------------
dnl 1. m4 macro definitions
dnl --------------------------------------------------------------------
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(PUTGETVAR_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',$1)dnl
define(`PUTORGET', $2)dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_scode.m4)
')dnl
dnl
define(PUTGETVAR_SCA_TEXT, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',text)dnl
define(`PUTORGET', $1)dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_schar.m4)
')dnl
dnl
define(PUTGETVAR_ARR, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_acode.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID
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

PUTGETVAR_SCA(OneByteInt,    put)
PUTGETVAR_SCA(TwoByteInt,    put)
PUTGETVAR_SCA(FourByteInt,   put)
PUTGETVAR_SCA(EightByteInt,  put)
PUTGETVAR_SCA(FourByteReal,  put)
PUTGETVAR_SCA(EightByteReal, put)
PUTGETVAR_SCA_TEXT(put)

!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(1, OneByteInt,    put)
PUTGETVAR_ARR(1, TwoByteInt,    put)
PUTGETVAR_ARR(1, FourByteInt,   put)
PUTGETVAR_ARR(1, EightByteInt,  put)
PUTGETVAR_ARR(1, FourByteReal,  put)
PUTGETVAR_ARR(1, EightByteReal, put)

!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(2, OneByteInt,    put)
PUTGETVAR_ARR(2, TwoByteInt,    put)
PUTGETVAR_ARR(2, FourByteInt,   put)
PUTGETVAR_ARR(2, EightByteInt,  put)
PUTGETVAR_ARR(2, FourByteReal,  put)
PUTGETVAR_ARR(2, EightByteReal, put)

!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(3, OneByteInt,    put)
PUTGETVAR_ARR(3, TwoByteInt,    put)
PUTGETVAR_ARR(3, FourByteInt,   put)
PUTGETVAR_ARR(3, EightByteInt,  put)
PUTGETVAR_ARR(3, FourByteReal,  put)
PUTGETVAR_ARR(3, EightByteReal, put)

!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(4, OneByteInt,    put)
PUTGETVAR_ARR(4, TwoByteInt,    put)
PUTGETVAR_ARR(4, FourByteInt,   put)
PUTGETVAR_ARR(4, EightByteInt,  put)
PUTGETVAR_ARR(4, FourByteReal,  put)
PUTGETVAR_ARR(4, EightByteReal, put)

!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(5, OneByteInt,    put)
PUTGETVAR_ARR(5, TwoByteInt,    put)
PUTGETVAR_ARR(5, FourByteInt,   put)
PUTGETVAR_ARR(5, EightByteInt,  put)
PUTGETVAR_ARR(5, FourByteReal,  put)
PUTGETVAR_ARR(5, EightByteReal, put)

!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(6, OneByteInt,    put)
PUTGETVAR_ARR(6, TwoByteInt,    put)
PUTGETVAR_ARR(6, FourByteInt,   put)
PUTGETVAR_ARR(6, EightByteInt,  put)
PUTGETVAR_ARR(6, FourByteReal,  put)
PUTGETVAR_ARR(6, EightByteReal, put)

!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(7, OneByteInt,    put)
PUTGETVAR_ARR(7, TwoByteInt,    put)
PUTGETVAR_ARR(7, FourByteInt,   put)
PUTGETVAR_ARR(7, EightByteInt,  put)
PUTGETVAR_ARR(7, FourByteReal,  put)
PUTGETVAR_ARR(7, EightByteReal, put)
