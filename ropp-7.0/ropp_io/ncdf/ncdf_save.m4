dnl $Id: ncdf_save.m4 1321 2008-02-27 15:08:24Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf_save.f90 file.
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
define(SAVE_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',$1)dnl
define(`PUTORGET', $2)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_save_scode.m4)
')dnl
dnl
define(SAVE_SCA_TEXT, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',text)dnl
define(`PUTORGET', $1)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_save_schar.m4)
')dnl
dnl
define(SAVE_ARR, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_save_acode.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID
!
!****s* Variables/ncdf_save
!
! NAME
!    ncdf_save - Save a variable to a netCDF data file.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_save(name, values, long_name, units, dimension)
!
! INPUTS
!    character(len = *)           :: name       Name of the variable to be created.
!    (any numeric type)           :: values     Data to be written.
!    character(len = *), optional :: long_name  'long_name' attribute.
!    character(len = *), optional :: units      'units' attribute.
!    logical,            optional :: dimension  If .true., the variable is defines
!                                                 a coordinate dimension .
!
!    All optional paramaters should be accessed via keywords.
!
! DESCRIPTION
!    This subroutine defines and writes a variable to the current netCDF data
!    file. Error handling is done inside the routine.
!
! NOTES
!    The routine is intended to be used for the occasional storage of data
!    in netCDF data files, e.g. for diagnostic or debugging purposes, but
!    not for saving large amounts of data regularly. For the latter, it is
!    more efficient to first define and then write all variables to the
!    netCDF data file via the ncdf_defdim, ncdf_defvar and ncdf_varput
!    routines.
!
! EXAMPLE
!    Assume that you want to write two ariables x and y to a newly created
!    netCDF data file 'test.nc'. Try
!
!       use ncdf
!         ...
!       integer, parameter :: n_levels = <some_number>
!         ...
!       real               :: x(n_levels), y(n_levels)
!         ...
!       call ncdf_create('test.nc')
!       call ncdf_save('x', x, long_name = 'My coordinates', units = 'm', &
!                              dimension = .true.)
!       call ncdf_save('y', y, long_name = 'My function values', units = 'K')
!       call ncdf_close()
!
!    To add the same variables to an already existing netCDF data file 
!    'test2.nc', simply replace the ncdf_create() call by an ncdf_open() call:
!
!       call ncdf_open('test.nc')
!       call ncdf_save('x', x, long_name = 'My coordinates', units = 'm', &
!                              dimension = .true.)
!       call ncdf_save('y', y, long_name = 'My function values', units = 'K')
!       call ncdf_close()
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

SAVE_SCA(OneByteInt,    put)
SAVE_SCA(TwoByteInt,    put)
SAVE_SCA(FourByteInt,   put)
SAVE_SCA(EightByteInt,  put)
SAVE_SCA(FourByteReal,  put)
SAVE_SCA(EightByteReal, put)

dnl SAVE_SCA_TEXT(put)

!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(1, OneByteInt,    put)
SAVE_ARR(1, TwoByteInt,    put)
SAVE_ARR(1, FourByteInt,   put)
SAVE_ARR(1, EightByteInt,  put)
SAVE_ARR(1, FourByteReal,  put)
SAVE_ARR(1, EightByteReal, put)

!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(2, OneByteInt,    put)
SAVE_ARR(2, TwoByteInt,    put)
SAVE_ARR(2, FourByteInt,   put)
SAVE_ARR(2, EightByteInt,  put)
SAVE_ARR(2, FourByteReal,  put)
SAVE_ARR(2, EightByteReal, put)

!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(3, OneByteInt,    put)
SAVE_ARR(3, TwoByteInt,    put)
SAVE_ARR(3, FourByteInt,   put)
SAVE_ARR(3, EightByteInt,  put)
SAVE_ARR(3, FourByteReal,  put)
SAVE_ARR(3, EightByteReal, put)

!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(4, OneByteInt,    put)
SAVE_ARR(4, TwoByteInt,    put)
SAVE_ARR(4, FourByteInt,   put)
SAVE_ARR(4, EightByteInt,  put)
SAVE_ARR(4, FourByteReal,  put)
SAVE_ARR(4, EightByteReal, put)

!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(5, OneByteInt,    put)
SAVE_ARR(5, TwoByteInt,    put)
SAVE_ARR(5, FourByteInt,   put)
SAVE_ARR(5, EightByteInt,  put)
SAVE_ARR(5, FourByteReal,  put)
SAVE_ARR(5, EightByteReal, put)

!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(6, OneByteInt,    put)
SAVE_ARR(6, TwoByteInt,    put)
SAVE_ARR(6, FourByteInt,   put)
SAVE_ARR(6, EightByteInt,  put)
SAVE_ARR(6, FourByteReal,  put)
SAVE_ARR(6, EightByteReal, put)

!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

SAVE_ARR(7, OneByteInt,    put)
SAVE_ARR(7, TwoByteInt,    put)
SAVE_ARR(7, FourByteInt,   put)
SAVE_ARR(7, EightByteInt,  put)
SAVE_ARR(7, FourByteReal,  put)
SAVE_ARR(7, EightByteReal, put)
