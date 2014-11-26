! $Id: ncdf_getgroupid.f90 3424 2012-11-28 10:11:26Z idculv $

FUNCTION ncdf_getgroupid(ncid_local, name, vname, groupid, havegroup) result(nf90_error)

!****f* Query/ncdf_getgroupid
!
! NAME
!    ncdf_getgroupid - Return group ID from a variable name.
!
! SYNOPSIS
!    use ncdf
!      ...
!    status = ncdf_getgroupid(ncid_local, name, vname, groupid, havegroup)
!
! DESCRIPTION
!    This function returns the actual variable name (sans '/'s) and
!    group ID, if applicable.  (Groups are identified like directories, with /.)
!    If no / is present, the input name is copied to vname and havegroup is returned .FALSE.
!
!    Note that, if netCDF-4 is not installed, the ncdf_getgroupid_n3.f90 version
!    of ncdf_getgroupid.f90 will be compiled.  This will result in havegroup being set to .FALSE. 
!    and groupid being set to ncid_local.  Otherwise ncdf_getgroupid_n4.f90 will be
!    compiled, which has the functionality above.
!
! INPUTS
!    INTEGER              :: ncid_local  -  Netcdf file ID
!    CHARACTER (len = *)  :: name        -  Full variable name, including '/'s if applicable
!
! OUTPUT
!    CHARACTER (len = *)  :: vname       -  Actual variable name, stripped of /
!    INTEGER              :: groupid     -  Group ID (if applicable)
!    LOGICAL              :: havegroup   -  Has a group been found?
!
! EXAMPLE
!    To find the netcdf group ID of the variable 
!    '/ropp/is/great' in the file 'yes_it_is.nc':
!    status = nf90_open('yes_it_is.nc', nf90_share, ncid_local)
!    status = ncdf_getgroupid(ncid_local, '/ropp/is/great', vname, groupid, havegroup)
!
!    The variable itself can then be obtained with something like:
!    ncid_local = groupid
!    status = nf90_inq_varid(ncid_local, var_name, var_id)
!    status = nf90_get_var(ncid_local, var_id, var_values)
!
! SEE ALSO
!    ncdf_getsize
!    ncdf_getatt
!    ncdf_getshape
!    ncdf_getvar
!    ncdf_getatt_alloc
!    ncdf_getnrec
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

USE ncdf, not_this => ncdf_getgroupid

IMPLICIT NONE

INTEGER,               INTENT(IN)    :: ncid_local  ! file ID
CHARACTER (len = *),   INTENT(IN)    :: name        ! full variable name, including / if applicable
CHARACTER (len = *),   INTENT(OUT)   :: vname       ! actual variable name, stripped of /
INTEGER,               INTENT(OUT)   :: groupid     ! group ID (if applicable)
LOGICAL,               INTENT(OUT)   :: havegroup   ! found group

! Local variables
INTEGER                              :: nf90_error  ! ncdf 90 exit status of group ID call

! This is a dummy version of ncdf_getgroupid_4.f90, 
! suitable for users who haven't installed netcdf4.

! Return no error, no group and set aname to attname

nf90_error = nf90_noerr

groupid = ncid_local

havegroup  = .FALSE.

vname = TRIM(name)


RETURN

END FUNCTION ncdf_getgroupid
