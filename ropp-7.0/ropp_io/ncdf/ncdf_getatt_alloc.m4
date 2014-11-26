dnl $Id: ncdf_getatt_alloc.m4 1321 2008-02-27 15:08:24Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf_getatt_alloc.f90
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
define(GETATT_ARR_ALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `yes')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetatt_acode.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID
!
!****s* Attributes/ncdf_getatt_alloc
!
! NAME
!    ncdf_getatt_alloc - Read an attribute from a netCDF data file.
!
! SYNOPSIS
!    call ncdf_getatt(attname, value)              ! global attribute
!      - or -
!    call ncdf_getatt(varname, attname, value)     ! variable's attribute
!
! DESCRIPTION
!    This subroutine reads attribute data from the current netCDF file.
!
! INPUTS
!    varname   Name of variable to which the attribute belongs. If varname
!                 is not given, a global attribute is read.
!    attname   Name of attribute to be read.
!
! OUTPUT
!    value
!
! SEE ALSO
!    ncdf_getatt
!    ncdf_putatt
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

GETATT_ARR_ALLOC(1, OneByteInt,    get)
GETATT_ARR_ALLOC(1, TwoByteInt,    get)
GETATT_ARR_ALLOC(1, FourByteInt,   get)
GETATT_ARR_ALLOC(1, EightByteInt,  get)
GETATT_ARR_ALLOC(1, FourByteReal,  get)
GETATT_ARR_ALLOC(1, EightByteReal, get)
