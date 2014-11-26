dnl $Id: ncdf_getvar.m4 2282 2009-10-22 09:49:29Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf_getvar.f90 file.
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
!****s* Variables/ncdf_getvar
!
! NAME
!    ncdf_getvar - Read a variable from a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_getvar(varname, data)
!      - or -
!    call ncdf_getvar(varname, data [, start = (/.../)[, count = (/.../)]])
!      - or -
!    call ncdf_getvar(varname, data [, rec = ...])
!      - or -
!    call ncdf_getvar(varname, data [, ncfile = ...], ...)
!      - or -
!    call ncdf_getvar(varname, data [, ncid = ...], ...)
!
! DESCRIPTION
!    This subroutine reads data from a variable of a netCDF data file.
!
!    If neither start and/or count nor rec are given, the data is simply
!    read from the specified netCDF variable. If start and count are given,
!    those define into which hyperslab of the netCDF variable the data is
!    read; see the netCDF documentation for details. If only start values
!    are given, the remaining part of the respective dimensions of the variable
!    in the netCDF file will be read into data. If only count values are
!    given, data is read starting at the first element of each dimension
!    of the variable in the netCDF data file.
!
!    The optional keyword rec is provided as a faster alternative to the
!    start / count keywords for netCDF data files with an unlimited record
!    dimension, and is only applicable in this case (and only if the netCDF
!    variable to be read into is defined using this dimension); see examples
!    below.
!
!    By default, data is read from the current netCDF data file. If either
!    ncid (the numeric id of a previously opened netCDF data file) or ncfile
!    (the full path name to an already existing netCDF data file) are given,
!    the corresponding file will be used for input instead, and subsequently
!    becomes the 'current' netCDF data file for all tasks performed by the 
!    ncdf library.
!
! INPUTS
!    character(len = *)    :: varname   Name of netCDF variable to write.
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
!    any type              :: data      Data to be written (scalar or array).
!
! NOTES
!    The use of the optional arguments / keywords rec and start/count are
!    mutually exclusive, as are the keywords ncid and ncfile.
!
! EXAMPLE
!    To read an n-dimensional 'data' array from a netCDF variable named
!    'field' within the current netCDF data file, where the Fortran variable
!    has been declared to be consistent with the definition of the netCDF
!    variable, use
!
!       call ncdf_getvar('field', data)
!
!    Assume that a netCDF variable 'bulk' describes a 3 dimensional data
!    field along dimensions x, y, and z. Reading a two dimensional hyperslab
!    'data' containing all values along the x and z coordinates, but for only
!    one index 'i' in y, use
!
!       call ncdf_getvar('bulk', data, start = (/1,   i, 1/), $
!                                      count = (/n_x, 1, n_z/))
!
!    where n_x and n_z denote the number of data points along the x and z
!    dimensions, respectively.
!
!    Assume that the netCDF variable 'field' contains a horizontal field
!    depending on longitude, latitude and time; time varies along the
!    unlimited netCDF dimension. Reading a two-dimensional hyperslap of
!    the data set valid at time index 'i', this
!
!       call ncdf_getvar('field', array, rec = i)
!
!    is a slightly easier way to read that hyperslab compared to the
!    (otherwise equivalent) alternative
!
!       call ncdf_getvar('field', array, start = (/1,     1, i/), $
!                                        count = (/n_x, n_y, 1))
!
! SEE ALSO
!    ncdf_getvar_alloc
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
! 1. Scalar arguments
!--------------------------------------------------------------------------

PUTGETVAR_SCA(OneByteInt,    get)
PUTGETVAR_SCA(TwoByteInt,    get)
PUTGETVAR_SCA(FourByteInt,   get)
PUTGETVAR_SCA(EightByteInt,  get)
PUTGETVAR_SCA(FourByteReal,  get)
PUTGETVAR_SCA(EightByteReal, get)
PUTGETVAR_SCA_TEXT(get)

!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(1, OneByteInt,    get)
PUTGETVAR_ARR(1, TwoByteInt,    get)
PUTGETVAR_ARR(1, FourByteInt,   get)
PUTGETVAR_ARR(1, EightByteInt,  get)
PUTGETVAR_ARR(1, FourByteReal,  get)
PUTGETVAR_ARR(1, EightByteReal, get)

!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(2, OneByteInt,    get)
PUTGETVAR_ARR(2, TwoByteInt,    get)
PUTGETVAR_ARR(2, FourByteInt,   get)
PUTGETVAR_ARR(2, EightByteInt,  get)
PUTGETVAR_ARR(2, FourByteReal,  get)
PUTGETVAR_ARR(2, EightByteReal, get)

!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(3, OneByteInt,    get)
PUTGETVAR_ARR(3, TwoByteInt,    get)
PUTGETVAR_ARR(3, FourByteInt,   get)
PUTGETVAR_ARR(3, EightByteInt,  get)
PUTGETVAR_ARR(3, FourByteReal,  get)
PUTGETVAR_ARR(3, EightByteReal, get)

!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(4, OneByteInt,    get)
PUTGETVAR_ARR(4, TwoByteInt,    get)
PUTGETVAR_ARR(4, FourByteInt,   get)
PUTGETVAR_ARR(4, EightByteInt,  get)
PUTGETVAR_ARR(4, FourByteReal,  get)
PUTGETVAR_ARR(4, EightByteReal, get)

!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(5, OneByteInt,    get)
PUTGETVAR_ARR(5, TwoByteInt,    get)
PUTGETVAR_ARR(5, FourByteInt,   get)
PUTGETVAR_ARR(5, EightByteInt,  get)
PUTGETVAR_ARR(5, FourByteReal,  get)
PUTGETVAR_ARR(5, EightByteReal, get)

!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(6, OneByteInt,    get)
PUTGETVAR_ARR(6, TwoByteInt,    get)
PUTGETVAR_ARR(6, FourByteInt,   get)
PUTGETVAR_ARR(6, EightByteInt,  get)
PUTGETVAR_ARR(6, FourByteReal,  get)
PUTGETVAR_ARR(6, EightByteReal, get)

!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

PUTGETVAR_ARR(7, OneByteInt,    get)
PUTGETVAR_ARR(7, TwoByteInt,    get)
PUTGETVAR_ARR(7, FourByteInt,   get)
PUTGETVAR_ARR(7, EightByteInt,  get)
PUTGETVAR_ARR(7, FourByteReal,  get)
PUTGETVAR_ARR(7, EightByteReal, get)

