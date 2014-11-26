! $Id: t_netcdf.f90 3551 2013-02-25 09:51:28Z idculv $

program t_netcdf

!****pi* Tests/t_netcdf *
!
! NAME
!    t_netcdf - Test the user's netCDF installation.
!
! SYNOPSIS
!    t_netcdf
! 
! DESCRIPTION
!    This program creates a sample netCDf data file ('example.nc') by calling
!    netCDF's native Fortran 90 interfaces. The resulting file file should be
!    dumped (using ncdump) and compared to the correct version provided in
!    the data subdirectory of the ropp_io distribution (netcdf_example_good.cdl).
!
!    This is automatically done when running
!
!       make test
!
!    or
!
!       make test_netcdf
!
!    in the test subdirectory of the ropp_io distribution.
!
! INPUTS
!    None.
!
! OUTPUT
!    example.nc   Example file; to be compared with netcdf_example_good.cdl in
!                   the data subdirectory of the ropp_io distribution.
!
! NOTES
!    The program itself provides no user visible output apart from creating
!    (or overwriting) a new file; it is solely intended to be used with the
!    test_netcdf / test targets in the Makefile.
!
! REFERENCES
!    This program has been taken from the netCDF 3.6.1-beta3 distribution and
!    was slightly modified for the use a test program of the netCDF library
!    within ropp_io.
!
! AUTHOR
!    The authors of the original software are:
!       University Corporation for Atmospheric Research/Unidata
!
!    Minor modifications by:
!      Met Office, Exeter, UK.
!      Any comments on this software should be given via the ROM SAF
!      Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!    Copyright 1993-2004 University Corporation for Atmospheric Research/Unidata
!
!    Portions of this software were developed by the Unidata Program at the 
!    University Corporation for Atmospheric Research.
!
!    Access and use of this software shall impose the following obligations
!    and understandings on the user. The user is granted the right, without
!    any fee or cost, to use, copy, modify, alter, enhance and distribute
!    this software, and any derivative works thereof, and its supporting
!    documentation for any purpose whatsoever, provided that this entire
!    notice appears in all copies of the software, derivative works and
!    supporting documentation.  Further, UCAR requests that the user credit
!    UCAR/Unidata in any publications that result from the use of this
!    software or in any product that includes this software, although this
!    is not an obligation. The names UCAR and/or Unidata, however, may not
!    be used in any advertising or publicity to endorse or promote any
!    products or commercial entity unless specific written permission is
!    obtained from UCAR/Unidata. The user also understands that
!    UCAR/Unidata is not obligated to provide the user with any support,
!    consulting, training or assistance of any kind with regard to the use,
!    operation and performance of this software nor to provide the user
!    with any updates, revisions, new versions or "bug fixes."
! 
!    THIS SOFTWARE IS PROVIDED BY UCAR/UNIDATA "AS IS" AND ANY EXPRESS OR
!    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL UCAR/UNIDATA BE LIABLE FOR ANY SPECIAL,
!    INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
!    FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
!    NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
!    WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.
!
!****

! This program provides an elementary check of some of the parts of the 
!   Fortran 90 interface to netCDF 3.5. It is a Fortran 90 implementation
!   of the nctst.cpp program provided with the C++ interface to netcdf
!   (in the src/cxx directory of the netcdf distribution). 

  use typeSizes
  use netcdf
  implicit none
  
  ! netcdf related variables
  integer :: ncFileID,                                   &
             latDimID, lonDimID, frTimeDimID, timeDimID, &
             pressVarID, latVarID, lonVarID, frTimeVarID, refTimeVarID, scalarVarID
             
  ! Local variables
  integer, parameter :: numLats = 4, numLons = 3, &
                        numFrTimes = 2, timeStringLen = 20
  character (len = *), parameter :: fileName = "example.nc"
  integer :: counter                      
  real, dimension(numLons, numLats, numFrTimes) :: pressure
  integer (kind = FourByteInt), dimension(numFrTimes) :: frTimeVals
  real (kind = FourByteReal) fillVal;
  real (kind = FourByteReal), dimension(2) :: validRange;
  
  ! --------------------
  ! Code begins
  ! --------------------
  if(.not. byteSizesOK()) then
    print *, "Compiler does not appear to support required kinds of variables."
    stop
  end if
    
  ! Create the file
  call check(nf90_create(path = trim(fileName), cmode = nf90_clobber, ncid = ncFileID))
  
  ! Define the dimensions
  call check(nf90_def_dim(ncid = ncFileID, name = "lat",     len = numLats,        dimid = latDimID))
  call check(nf90_def_dim(ncid = ncFileID, name = "lon",     len = numLons,        dimid = lonDimID))
  call check(nf90_def_dim(ncid = ncFileID, name = "frtime",  len = nf90_unlimited, dimid = frTimeDimID))
  call check(nf90_def_dim(ncid = ncFileID, name = "timelen", len = timeStringLen,  dimid = timeDimID))

  ! Create variables and attributes
  call check(nf90_def_var(ncid = ncFileID, name = "P", xtype = nf90_float,     &
                     dimids = (/ lonDimID, latDimID, frTimeDimID /), varID = pressVarID) )
  call check(nf90_put_att(ncFileID, pressVarID, "long_name",   "pressure at maximum wind"))
  call check(nf90_put_att(ncFileID, pressVarID, "units",       "hectopascals") )
  ! Use 4-byte reals explicitly, to match 4-byte attribute type in test file
  validRange(1) = 0.
  validRange(2) = 1500
  call check(nf90_put_att(ncFileID, pressVarID, "valid_range", validRange))
  ! Use a 4-byte float constant, to match variable type
  fillVal = -9999.0
  call check(nf90_put_att(ncFileID, pressVarID,  "_FillValue", fillVal ) )
                      
  call check(nf90_def_var(ncFileID, "lat", nf90_float, dimids = latDimID, varID = latVarID) )
  call check(nf90_put_att(ncFileID, latVarID, "long_name", "latitude"))
  call check(nf90_put_att(ncFileID, latVarID, "units", "degrees_north"))

  call check(nf90_def_var(ncFileID, "lon", nf90_float, lonDimID, lonVarID) )
  call check(nf90_put_att(ncFileID, lonVarID, "long_name", "longitude"))
  call check(nf90_put_att(ncFileID, lonVarID, "units",     "degrees_east"))

  call check(nf90_def_var(ncFileID, "frtime", nf90_int, frTimeDimID, frTimeVarID) )
  call check(nf90_put_att(ncFileID, frTimeVarID, "long_name", "forecast time"))
  call check(nf90_put_att(ncFileID, frTimeVarID, "units",     "hours"))

  call check(nf90_def_var(ncFileID, "reftime", nf90_char, timeDimID, refTimeVarID) )
  call check(nf90_put_att(ncFileID, refTimeVarID, "long_name", "reference time"))
  call check(nf90_put_att(ncFileID, refTimeVarID, "units",     "text_time"))
                     
  ! In the C++ interface the define a scalar variable - do we know how to do this? 
  call check(nf90_def_var(ncFileID, "ScalarVariable", nf90_real, scalarVarID))
  
  ! Global attributes
  call check(nf90_put_att(ncFileID, nf90_global, "history", &
                     "created by Unidata LDM from NPS broadcast"))
  call check(nf90_put_att(ncFileID, nf90_global, "title", &
                     "NMC Global Product Set: Pressure at Maximum Wind"))
  
  ! Leave define mode
  call check(nf90_enddef(ncfileID))
  
  ! Write the dimension variables
  call check(nf90_put_var(ncFileID, latVarId,     (/ -90., -87.5, -85., -82.5 /)) )
  call check(nf90_put_var(ncFileID, lonVarId,     (/ -180, -175, -170 /)      ) )
  ! Don't use anonymous array here, in case platform has 8-byte integers
  frTimeVals(1) = 12
  frTimeVals(2) = 18
  call check(nf90_put_var(ncFileID, frTimeVarId,  frTimeVals                  ) )
  call check(nf90_put_var(ncFileID, reftimeVarID, "1992-3-21 12:00"           ) )
  
  ! Write the pressure variable. Write a slab at a time to check incrementing.
  pressure = 949. + real(reshape( (/ (counter, counter = 1, numLats * numLons * numFrTimes) /),  &
                                    (/ numLons, numLats, numFrTimes /) ) )
  call check(nf90_put_var(ncFileID, pressVarID, pressure(:, :, 1:1)) )
  call check(nf90_put_var(ncFileID, pressVarID, pressure(:, :, 2:2), start = (/ 1, 1, 2 /)) )
  
  call check(nf90_put_var(ncFileID, scalarVarID, 10))
  call check(nf90_close(ncFileID))

contains
  ! Internal subroutine - checks error status after each netcdf, prints out text message each time
  !   an error code is returned. 
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
    else
      print *, '*** Netcdf: Success ***'
    end if
  end subroutine check  
end program t_netcdf
