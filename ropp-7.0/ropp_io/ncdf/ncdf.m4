dnl $Id: ncdf.m4 2282 2009-10-22 09:49:29Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working ncdf.f90 file.
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
define(PUTGETATT_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',$1)dnl
define(`PUTORGET', $2)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetatt_sitfc.m4)dnl
')dnl
dnl
define(PUTGETATT_ARR, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetatt_aitfc.m4)dnl
')dnl
dnl
define(PUTGETATT_ARR_ALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `yes')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetatt_aitfc.m4)
')dnl
define(PUTGETVAR_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',$1)dnl
define(`PUTORGET', $2)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_sitfc.m4)dnl
')dnl
dnl
define(PUTGETVAR_ARR, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_aitfc.m4)dnl
')dnl
dnl
define(PUTGETVAR_ARR_ALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', $3)dnl
define(`POINTER', `yes')dnl
include(ncdf_aux.m4)dnl
include(ncdf_putgetvar_aitfc.m4)
')dnl
define(SAVE_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4 
define(`KINDVALUE',$1)dnl
define(`PUTORGET', $2)dnl
define(`POINTER', `no')dnl
include(ncdf_aux.m4)dnl
include(ncdf_save_sitfc.m4)
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
include(ncdf_save_aitfc.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

module ncdf

!****m* Interface/Modules
!
! SYNOPSIS
!    use ncdf
! 
! DESCRIPTION
!    Access to the routines in the ncdf library is provided by various
!    Fortran 90 modules. 
!
! SEE ALSO
!    ncdf
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

!****m* Modules/ncdf
!
! NAME
!    ncdf - A simple interface to read and write netCDF data.
!
! SYNOPSIS
!    use ncdf
! 
! DESCRIPTION
!    This module provides a simple interface to the netCDF library and
!    allows for easy creation, writing and reading of netCDF data files.
!
! NOTES
!    For reading data from netCDF files, the ncdf_open, read_variable
!    and ncdf_close routines should be sufficient. For writing netCDF
!    data files, it is probably neecessary to write a 'ncf_create_mystuff'
!    subroutine that handles the creation of the netCDF file using both
!    ncdf and native netCDF calls; ncdf currently only provides little
!    for this task.
!
! EXAMPLE
!    Assume that you have written a subroutine ncdf_create_alpha which
!    creates a netCDF data file for a scalar 'R_curve' and array variables
!    namd 'lon', 'lat' and 'alpha_o' (along with several others). From a
!    Fortran 90 program, ncdf routines might then be called as follows:
!
!       use ncdf
!         ...
!       integer, parameter   :: n_levels = <some_number>
!         ...
!       real(dp)             :: alpha(n_levels), impact(n_levels)
!         ...
!       call ncdf_create_alpha('test.nc', n_levels)
!       call ncdf_putvar('alpha_b', alpha)
!       call ncdf_putvar('impact',  impact)
!         ...
!       call ncdf_close()
!
!    To read these variables from another Fortran 90 program, try this:
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
!         ...
!       call ncdf_close()
!
! SEE ALSO
!    High level routines:
!
!      ncdf_open         - Open an (already existing) netCDF data file.
!      ncdf_close        - Close a netCDF data file.
!      ncdf_putvar       - Write data into an (already defined) variable of
!                            the current netCDF data file..
!      ncdf_getvar       - Read data from a variable in the current netCDF
!                            data file.
!
!    Low level routines (mainly used for the creation of new netCDF data files)
!
!      ncdf_create       - Create a new netCDF data file.
!      ncdf_defvar       - Create a new variable in a netCDF data file.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

!------------------------------------------------------------------------------
! 1. Global variables
!------------------------------------------------------------------------------

!****iv* netCDF/ncdf_ncname
!
! NAME
!    ncdf_ncname - Name of the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!
! DESCRIPTION
!    The name (including the path) of the current netCDF data file; it is
!    known to all ncdf routines.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

!****iv* netCDF/ncdf_ncid
!
! NAME
!    ncdf_ncid - NetCDF id of the current netCDF data file.
!
! SYNOPSIS
!    use ncdf
!
! DESCRIPTION
!    The netCDF id of the current opened netCDF data file; it is known to all
!    ncdf routines.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  use typeSizes
  use netcdf

  character(len = 1024), save, public :: ncdf_ncname = ''
  integer,               save, public :: ncdf_ncid   = -1
  integer,               save, public :: ncdf_nvars  = 0
  logical, dimension(:), pointer, save, public :: ncdf_read => null() 
  logical,               save, public :: ncdf_delete_on_error = .false.

!------------------------------------------------------------------------------
! 2. Interfaces - Files
!------------------------------------------------------------------------------

!****m* netCDF/Files
!
! DESCRIPTION
!    Routines for handling netCDF data files.
!
! SEE ALSO
!    is_netcdf
!    ncdf_create
!    ncdf_open
!    ncdf_close
!    ncdf_sync
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface is_netcdf
     function is_netcdf(file) result(it_is)
       character(len = *), intent(in) :: file
       logical                        :: it_is
     end function is_netcdf
  end interface

  interface ncdf_create
     subroutine ncdf_create(ncfile, cmode, ncid)
       character (len = *), intent(in) :: ncfile
       integer,             optional   :: cmode
       integer,             optional   :: ncid
     end subroutine ncdf_create
  end interface

  interface ncdf_open
     subroutine ncdf_open(ncfile, ncid, mode, append)
       character (len = *), intent(in) :: ncfile
       integer,             optional   :: ncid
       integer,             optional   :: mode
       logical,             optional   :: append
     end subroutine ncdf_open
  end interface

  interface ncdf_close
     subroutine ncdf_close(ncfile, ncid)
       character (len = *), optional :: ncfile
       integer,             optional :: ncid
     end subroutine ncdf_close
  end interface

  interface ncdf_sync
     subroutine ncdf_sync(ncfile, ncid)
       character (len = *), optional :: ncfile
       integer,             optional :: ncid
     end subroutine ncdf_sync
  end interface

!------------------------------------------------------------------------------
! 3. Interfaces - Query
!------------------------------------------------------------------------------

!****m* netCDF/Query
!
! DESCRIPTION
!    Routines for obtaining information about the contents of a netCDF data
!    file.
!
! SEE ALSO
!    ncdf_getshape
!    ncdf_getsize
!    ncdf_getnrec
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface ncdf_getshape
     subroutine ncdf_getshape(name, shape, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer,                   intent(out) :: shape
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_getshape
  end interface

  interface ncdf_getsize
     subroutine ncdf_sgetsize(name, size, dim, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer,                   intent(out) :: size
       integer,                   optional    :: dim
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_sgetsize
     subroutine ncdf_agetsize(name, size, ncfile, ncid)
       character(len = *),        intent(in)  :: name
       integer, dimension(:),     intent(out) :: size
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_agetsize
  end interface

  interface ncdf_getnrec
     function ncdf_getnrec(ncfile, ncid) result(n_unlimited)
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
       integer                                :: n_unlimited
     end function ncdf_getnrec
  end interface

!------------------------------------------------------------------------------
! 4. Interfaces - attributes
!------------------------------------------------------------------------------

!****m* netCDF/Attributes
!
! DESCRIPTION
!    Routines to deal with attributes, both global and variable specific, in
!    a netCDF data file.
!
! SEE ALSO
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface ncdf_isatt
     function ncdf_isatt(attname, varname, ncfile, ncid, xtype, len, attnum) result(it_is)
       character (len = *), intent(in)            :: attname
       character (len = *), intent(in), optional  :: varname
       character (len = *), intent(in),  optional :: ncfile
       integer,             intent(in),  optional :: ncid
       integer,             intent(out), optional :: xtype, len, attnum
       logical                                    :: it_is
     end function ncdf_isatt
  end interface

  interface ncdf_putatt
PUTGETATT_SCA(OneByteInt,    put)dnl
PUTGETATT_SCA(TwoByteInt,    put)dnl
PUTGETATT_SCA(FourByteInt,   put)dnl
PUTGETATT_SCA(EightByteInt,  put)dnl
PUTGETATT_SCA(FourByteReal,  put)dnl
PUTGETATT_SCA(EightByteReal, put)dnl
PUTGETATT_SCA(text,          put)dnl
PUTGETATT_ARR(1, OneByteInt,    put)dnl
PUTGETATT_ARR(1, TwoByteInt,    put)dnl
PUTGETATT_ARR(1, FourByteInt,   put)dnl
PUTGETATT_ARR(1, EightByteInt,  put)dnl
PUTGETATT_ARR(1, FourByteReal,  put)dnl
PUTGETATT_ARR(1, EightByteReal, put)dnl
  end interface

  interface ncdf_getatt
PUTGETATT_SCA(OneByteInt,    get)dnl
PUTGETATT_SCA(TwoByteInt,    get)dnl
PUTGETATT_SCA(FourByteInt,   get)dnl
PUTGETATT_SCA(EightByteInt,  get)dnl
PUTGETATT_SCA(FourByteReal,  get)dnl
PUTGETATT_SCA(EightByteReal, get)dnl
PUTGETATT_SCA(text,          get)dnl
PUTGETATT_ARR(1, OneByteInt,    get)dnl
PUTGETATT_ARR(1, TwoByteInt,    get)dnl
PUTGETATT_ARR(1, FourByteInt,   get)dnl
PUTGETATT_ARR(1, EightByteInt,  get)dnl
PUTGETATT_ARR(1, FourByteReal,  get)dnl
PUTGETATT_ARR(1, EightByteReal, get)dnl
  end interface

  interface ncdf_getatt_alloc
PUTGETATT_ARR_ALLOC(1, OneByteInt,    get)dnl
PUTGETATT_ARR_ALLOC(1, TwoByteInt,    get)dnl
PUTGETATT_ARR_ALLOC(1, FourByteInt,   get)dnl
PUTGETATT_ARR_ALLOC(1, EightByteInt,  get)dnl
PUTGETATT_ARR_ALLOC(1, FourByteReal,  get)dnl
PUTGETATT_ARR_ALLOC(1, EightByteReal, get)dnl
  end interface

!------------------------------------------------------------------------------
! 5. Interfaces - dimensions
!------------------------------------------------------------------------------

!****m* netCDF/Dimensions
!
! DESCRIPTION
!    Routines to deal with dimensions within a netCDF data file, including
!    defining them.
!
! SEE ALSO
!    ncdf_defdim
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface ncdf_defdim
     function ncdf_defdim(name, n, ncid) result(dimid)
        character(len = *),    intent(in) :: name
        integer,               intent(in) :: n
        integer,               optional   :: ncid
        integer                           :: dimid
     end function ncdf_defdim
  end interface

!------------------------------------------------------------------------------
! 6. Interfaces - variables
!------------------------------------------------------------------------------

!****m* netCDF/Variables
!
! DESCRIPTION
!    Routines to deal with variables within a netCDF data file, including
!    defining, writing and reading them.
!
! SEE ALSO
!    ncdf_isvar
!    ncdf_renvar
!    ncdf_defvar
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface ncdf_isvar
     function ncdf_isvar(name, ncfile, ncid) result(it_is)
       character(len = *), intent(in) :: name
       character(len = *), optional   :: ncfile
       integer,            optional   :: ncid
       logical                        :: it_is
     end function ncdf_isvar
  end interface

  interface ncdf_renvar
     subroutine ncdf_renvar(old_name, new_name, ncfile, ncid)
       character(len = *),        intent(in)  :: old_name
       character(len = *),        intent(in)  :: new_name
       character(len = *),        optional    :: ncfile
       integer,                   optional    :: ncid
     end subroutine ncdf_renvar
  end interface

  interface ncdf_defvar
     function ncdf_defvar_sca(name, long_name, units, type, ncid, &
                              standard_name, positive, formula_terms, &
                              calendar, coordinates) &
                              result(varid)
       character(len = *),    intent(in) :: name, long_name, units
       integer,               optional   :: type
       integer,               optional   :: ncid
       character(len = *),    optional   :: standard_name
       character(len = *),    optional   :: positive
       character(len = *),    optional   :: formula_terms
       character(len = *),    optional   :: calendar
       character(len = *),    optional   :: coordinates
       integer                           :: varid
     end function ncdf_defvar_sca
     function ncdf_defvar_arr(name, long_name, units, dimids, type, ncid, &
                              standard_name, positive, formula_terms, &
                              calendar, coordinates) &
                              result(varid)
       character(len = *),    intent(in) :: name, long_name, units
       integer, dimension(:), intent(in) :: dimids
       integer,               optional   :: type
       integer,               optional   :: ncid
       character(len = *),    optional   :: standard_name
       character(len = *),    optional   :: positive
       character(len = *),    optional   :: formula_terms
       character(len = *),    optional   :: calendar
       character(len = *),    optional   :: coordinates
       integer                           :: varid
     end function ncdf_defvar_arr
  end interface

  interface ncdf_putvar
PUTGETVAR_SCA(OneByteInt,    put)dnl
PUTGETVAR_SCA(TwoByteInt,    put)dnl
PUTGETVAR_SCA(FourByteInt,   put)dnl
PUTGETVAR_SCA(EightByteInt,  put)dnl
PUTGETVAR_SCA(FourByteReal,  put)dnl
PUTGETVAR_SCA(EightByteReal, put)dnl
PUTGETVAR_SCA(text,          put)dnl
PUTGETVAR_ARR(1, OneByteInt,    put)dnl
PUTGETVAR_ARR(1, TwoByteInt,    put)dnl
PUTGETVAR_ARR(1, FourByteInt,   put)dnl
PUTGETVAR_ARR(1, EightByteInt,  put)dnl
PUTGETVAR_ARR(1, FourByteReal,  put)dnl
PUTGETVAR_ARR(1, EightByteReal, put)dnl
PUTGETVAR_ARR(2, OneByteInt,    put)dnl
PUTGETVAR_ARR(2, TwoByteInt,    put)dnl
PUTGETVAR_ARR(2, FourByteInt,   put)dnl
PUTGETVAR_ARR(2, EightByteInt,  put)dnl
PUTGETVAR_ARR(2, FourByteReal,  put)dnl
PUTGETVAR_ARR(2, EightByteReal, put)dnl
PUTGETVAR_ARR(3, OneByteInt,    put)dnl
PUTGETVAR_ARR(3, TwoByteInt,    put)dnl
PUTGETVAR_ARR(3, FourByteInt,   put)dnl
PUTGETVAR_ARR(3, EightByteInt,  put)dnl
PUTGETVAR_ARR(3, FourByteReal,  put)dnl
PUTGETVAR_ARR(3, EightByteReal, put)dnl
PUTGETVAR_ARR(4, OneByteInt,    put)dnl
PUTGETVAR_ARR(4, TwoByteInt,    put)dnl
PUTGETVAR_ARR(4, FourByteInt,   put)dnl
PUTGETVAR_ARR(4, EightByteInt,  put)dnl
PUTGETVAR_ARR(4, FourByteReal,  put)dnl
PUTGETVAR_ARR(4, EightByteReal, put)dnl
PUTGETVAR_ARR(5, OneByteInt,    put)dnl
PUTGETVAR_ARR(5, TwoByteInt,    put)dnl
PUTGETVAR_ARR(5, FourByteInt,   put)dnl
PUTGETVAR_ARR(5, EightByteInt,  put)dnl
PUTGETVAR_ARR(5, FourByteReal,  put)dnl
PUTGETVAR_ARR(5, EightByteReal, put)dnl
PUTGETVAR_ARR(6, OneByteInt,    put)dnl
PUTGETVAR_ARR(6, TwoByteInt,    put)dnl
PUTGETVAR_ARR(6, FourByteInt,   put)dnl
PUTGETVAR_ARR(6, EightByteInt,  put)dnl
PUTGETVAR_ARR(6, FourByteReal,  put)dnl
PUTGETVAR_ARR(6, EightByteReal, put)dnl
PUTGETVAR_ARR(7, OneByteInt,    put)dnl
PUTGETVAR_ARR(7, TwoByteInt,    put)dnl
PUTGETVAR_ARR(7, FourByteInt,   put)dnl
PUTGETVAR_ARR(7, EightByteInt,  put)dnl
PUTGETVAR_ARR(7, FourByteReal,  put)dnl
PUTGETVAR_ARR(7, EightByteReal, put)dnl
  end interface

  interface ncdf_getvar
PUTGETVAR_SCA(OneByteInt,    get)dnl
PUTGETVAR_SCA(TwoByteInt,    get)dnl
PUTGETVAR_SCA(FourByteInt,   get)dnl
PUTGETVAR_SCA(EightByteInt,  get)dnl
PUTGETVAR_SCA(FourByteReal,  get)dnl
PUTGETVAR_SCA(EightByteReal, get)dnl
PUTGETVAR_SCA(text,          get)dnl
PUTGETVAR_ARR(1, OneByteInt,    get)dnl
PUTGETVAR_ARR(1, TwoByteInt,    get)dnl
PUTGETVAR_ARR(1, FourByteInt,   get)dnl
PUTGETVAR_ARR(1, EightByteInt,  get)dnl
PUTGETVAR_ARR(1, FourByteReal,  get)dnl
PUTGETVAR_ARR(1, EightByteReal, get)dnl
PUTGETVAR_ARR(2, OneByteInt,    get)dnl
PUTGETVAR_ARR(2, TwoByteInt,    get)dnl
PUTGETVAR_ARR(2, FourByteInt,   get)dnl
PUTGETVAR_ARR(2, EightByteInt,  get)dnl
PUTGETVAR_ARR(2, FourByteReal,  get)dnl
PUTGETVAR_ARR(2, EightByteReal, get)dnl
PUTGETVAR_ARR(3, OneByteInt,    get)dnl
PUTGETVAR_ARR(3, TwoByteInt,    get)dnl
PUTGETVAR_ARR(3, FourByteInt,   get)dnl
PUTGETVAR_ARR(3, EightByteInt,  get)dnl
PUTGETVAR_ARR(3, FourByteReal,  get)dnl
PUTGETVAR_ARR(3, EightByteReal, get)dnl
PUTGETVAR_ARR(4, OneByteInt,    get)dnl
PUTGETVAR_ARR(4, TwoByteInt,    get)dnl
PUTGETVAR_ARR(4, FourByteInt,   get)dnl
PUTGETVAR_ARR(4, EightByteInt,  get)dnl
PUTGETVAR_ARR(4, FourByteReal,  get)dnl
PUTGETVAR_ARR(4, EightByteReal, get)dnl
PUTGETVAR_ARR(5, OneByteInt,    get)dnl
PUTGETVAR_ARR(5, TwoByteInt,    get)dnl
PUTGETVAR_ARR(5, FourByteInt,   get)dnl
PUTGETVAR_ARR(5, EightByteInt,  get)dnl
PUTGETVAR_ARR(5, FourByteReal,  get)dnl
PUTGETVAR_ARR(5, EightByteReal, get)dnl
PUTGETVAR_ARR(6, OneByteInt,    get)dnl
PUTGETVAR_ARR(6, TwoByteInt,    get)dnl
PUTGETVAR_ARR(6, FourByteInt,   get)dnl
PUTGETVAR_ARR(6, EightByteInt,  get)dnl
PUTGETVAR_ARR(6, FourByteReal,  get)dnl
PUTGETVAR_ARR(6, EightByteReal, get)dnl
PUTGETVAR_ARR(7, OneByteInt,    get)dnl
PUTGETVAR_ARR(7, TwoByteInt,    get)dnl
PUTGETVAR_ARR(7, FourByteInt,   get)dnl
PUTGETVAR_ARR(7, EightByteInt,  get)dnl
PUTGETVAR_ARR(7, FourByteReal,  get)dnl
PUTGETVAR_ARR(7, EightByteReal, get)dnl
  end interface

  interface ncdf_getvar_alloc
PUTGETVAR_ARR_ALLOC(1, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(1, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(1, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(1, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(1, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(1, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(2, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(2, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(2, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(2, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(2, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(2, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(3, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(3, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(3, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(3, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(3, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(3, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(4, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(4, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(4, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(4, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(4, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(4, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(5, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(5, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(5, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(5, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(5, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(5, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(6, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(6, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(6, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(6, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(6, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(6, EightByteReal, get)dnl
PUTGETVAR_ARR_ALLOC(7, OneByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(7, TwoByteInt,    get)dnl
PUTGETVAR_ARR_ALLOC(7, FourByteInt,   get)dnl
PUTGETVAR_ARR_ALLOC(7, EightByteInt,  get)dnl
PUTGETVAR_ARR_ALLOC(7, FourByteReal,  get)dnl
PUTGETVAR_ARR_ALLOC(7, EightByteReal, get)dnl
  end interface

  interface ncdf_save
SAVE_SCA(OneByteInt,    put)
SAVE_SCA(TwoByteInt,    put)
SAVE_SCA(FourByteInt,   put)
SAVE_SCA(EightByteInt,  put)
SAVE_SCA(FourByteReal,  put)
SAVE_SCA(EightByteReal, put)
SAVE_ARR(1, OneByteInt,    put)
SAVE_ARR(1, TwoByteInt,    put)
SAVE_ARR(1, FourByteInt,   put)
SAVE_ARR(1, EightByteInt,  put)
SAVE_ARR(1, FourByteReal,  put)
SAVE_ARR(1, EightByteReal, put)
SAVE_ARR(2, OneByteInt,    put)
SAVE_ARR(2, TwoByteInt,    put)
SAVE_ARR(2, FourByteInt,   put)
SAVE_ARR(2, EightByteInt,  put)
SAVE_ARR(2, FourByteReal,  put)
SAVE_ARR(2, EightByteReal, put)
SAVE_ARR(3, OneByteInt,    put)
SAVE_ARR(3, TwoByteInt,    put)
SAVE_ARR(3, FourByteInt,   put)
SAVE_ARR(3, EightByteInt,  put)
SAVE_ARR(3, FourByteReal,  put)
SAVE_ARR(3, EightByteReal, put)
SAVE_ARR(4, OneByteInt,    put)
SAVE_ARR(4, TwoByteInt,    put)
SAVE_ARR(4, FourByteInt,   put)
SAVE_ARR(4, EightByteInt,  put)
SAVE_ARR(4, FourByteReal,  put)
SAVE_ARR(4, EightByteReal, put)
SAVE_ARR(5, OneByteInt,    put)
SAVE_ARR(5, TwoByteInt,    put)
SAVE_ARR(5, FourByteInt,   put)
SAVE_ARR(5, EightByteInt,  put)
SAVE_ARR(5, FourByteReal,  put)
SAVE_ARR(5, EightByteReal, put)
SAVE_ARR(6, OneByteInt,    put)
SAVE_ARR(6, TwoByteInt,    put)
SAVE_ARR(6, FourByteInt,   put)
SAVE_ARR(6, EightByteInt,  put)
SAVE_ARR(6, FourByteReal,  put)
SAVE_ARR(6, EightByteReal, put)
SAVE_ARR(7, OneByteInt,    put)
SAVE_ARR(7, TwoByteInt,    put)
SAVE_ARR(7, FourByteInt,   put)
SAVE_ARR(7, EightByteInt,  put)
SAVE_ARR(7, FourByteReal,  put)
SAVE_ARR(7, EightByteReal, put)
  end interface

!------------------------------------------------------------------------------
! 5. Interfaces - Misc
!------------------------------------------------------------------------------

!****m* netCDF/Misc
!
! DESCRIPTION
!    Routines for various other useful tasks.
!
! SEE ALSO
!    ncdf_defmode
!    ncdf_datmode
!    ncdf_date_and_time
!    ncdf_error_handler
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
!****

  interface ncdf_defmode
     subroutine ncdf_defmode(ncfile, ncid)
       implicit none
       character(len = *), optional   :: ncfile
       integer,            optional   :: ncid
     end subroutine ncdf_defmode
  end interface

  interface ncdf_datmode
     subroutine ncdf_datmode(ncfile, ncid)
       implicit none
       character(len = *), optional   :: ncfile
       integer,            optional   :: ncid
     end subroutine ncdf_datmode
  end interface

  interface ncdf_date_and_time
     subroutine ncdf_date_and_time(date, time, zone, values, &
                                   year, month, day, hour, min, sec, msec)
       character(len = *),    optional, intent(out) :: date
       character(len = *),    optional, intent(out) :: time
       character(len = *),    optional, intent(out) :: zone
       integer, dimension(:), optional, intent(out) :: values
       integer,               optional, intent(out) :: year
       integer,               optional, intent(out) :: month
       integer,               optional, intent(out) :: day
       integer,               optional, intent(out) :: hour
       integer,               optional, intent(out) :: min
       integer,               optional, intent(out) :: sec
       integer,               optional, intent(out) :: msec
     end subroutine ncdf_date_and_time
  end interface

  interface ncdf_error_handler
     subroutine ncdf_error_handler(status)
       integer, intent(in) :: status
     end subroutine ncdf_error_handler
  end interface

end module ncdf
