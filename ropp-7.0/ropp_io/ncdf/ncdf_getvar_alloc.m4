dnl $Id: ncdf_getvar_alloc.m4 1321 2008-02-27 15:08:24Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf_getvar_alloc.f90
dnl file.
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
define(GETVAR_ARR_ALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `yes')dnl
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

GETVAR_ARR_ALLOC(1, OneByteInt,    get)
GETVAR_ARR_ALLOC(1, TwoByteInt,    get)
GETVAR_ARR_ALLOC(1, FourByteInt,   get)
GETVAR_ARR_ALLOC(1, EightByteInt,  get)
GETVAR_ARR_ALLOC(1, FourByteReal,  get)
GETVAR_ARR_ALLOC(1, EightByteReal, get)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(2, OneByteInt,    get)
GETVAR_ARR_ALLOC(2, TwoByteInt,    get)
GETVAR_ARR_ALLOC(2, FourByteInt,   get)
GETVAR_ARR_ALLOC(2, EightByteInt,  get)
GETVAR_ARR_ALLOC(2, FourByteReal,  get)
GETVAR_ARR_ALLOC(2, EightByteReal, get)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(3, OneByteInt,    get)
GETVAR_ARR_ALLOC(3, TwoByteInt,    get)
GETVAR_ARR_ALLOC(3, FourByteInt,   get)
GETVAR_ARR_ALLOC(3, EightByteInt,  get)
GETVAR_ARR_ALLOC(3, FourByteReal,  get)
GETVAR_ARR_ALLOC(3, EightByteReal, get)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(4, OneByteInt,    get)
GETVAR_ARR_ALLOC(4, TwoByteInt,    get)
GETVAR_ARR_ALLOC(4, FourByteInt,   get)
GETVAR_ARR_ALLOC(4, EightByteInt,  get)
GETVAR_ARR_ALLOC(4, FourByteReal,  get)
GETVAR_ARR_ALLOC(4, EightByteReal, get)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(5, OneByteInt,    get)
GETVAR_ARR_ALLOC(5, TwoByteInt,    get)
GETVAR_ARR_ALLOC(5, FourByteInt,   get)
GETVAR_ARR_ALLOC(5, EightByteInt,  get)
GETVAR_ARR_ALLOC(5, FourByteReal,  get)
GETVAR_ARR_ALLOC(5, EightByteReal, get)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(6, OneByteInt,    get)
GETVAR_ARR_ALLOC(6, TwoByteInt,    get)
GETVAR_ARR_ALLOC(6, FourByteInt,   get)
GETVAR_ARR_ALLOC(6, EightByteInt,  get)
GETVAR_ARR_ALLOC(6, FourByteReal,  get)
GETVAR_ARR_ALLOC(6, EightByteReal, get)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

GETVAR_ARR_ALLOC(7, OneByteInt,    get)
GETVAR_ARR_ALLOC(7, TwoByteInt,    get)
GETVAR_ARR_ALLOC(7, FourByteInt,   get)
GETVAR_ARR_ALLOC(7, EightByteInt,  get)
GETVAR_ARR_ALLOC(7, FourByteReal,  get)
GETVAR_ARR_ALLOC(7, EightByteReal, get)
