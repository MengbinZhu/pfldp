! $Id: ncdf_date_and_time.f90 1959 2008-11-13 12:15:18Z frhl $

subroutine ncdf_date_and_time(date, time, zone, values, &
                              year, month, day, hour, min, sec, msec)

!****s* Misc/ncdf_date_and_time
!
! NAME
!    ncdf_date_and_time - Get data and time from the operating system.
!
! SYNOPSIS
!    use ncdf
!      ...
!    call ncdf_date_and_time()
!
! DESCRIPTION
!    This routine provides an alternative to Fortran 90's standard 
!    date_and_time() routine.
!
! OUTPUT
!    date
!    time
!    zone
!    values
!    year
!    month
!    day
!    hour
!    min
!    sec
!    msec
!
! NOTES
!    All arguments are optional. The array values is an 8-element integer
!    vector, all following arguments are scalar integers. the arguments
!    date, time and zone are character strings.
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
!***

!------------------------------------------------------------------------------
! 1. Declarations
!------------------------------------------------------------------------------

  implicit none

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

  character(len = 10)                :: my_date
  character(len =  5)                :: my_zone
  character(len = 10)                :: my_time
  integer, dimension(8)              :: my_values

  integer                            :: my_year, my_month, my_day
  integer                            :: my_hour,my_min, my_sec, my_msec

!------------------------------------------------------------------------------
! 2. Call the system routine
!------------------------------------------------------------------------------

  call date_and_time(my_date, my_time, my_zone, my_values)

!------------------------------------------------------------------------------
! 3. Get the individual components
!------------------------------------------------------------------------------

  my_year  = my_values(1)
  my_month = my_values(2)
  my_day   = my_values(3)
  my_hour  = my_values(5)
  my_min   = my_values(6)
  my_sec   = my_values(7)
  my_msec  = my_values(8)

  
!------------------------------------------------------------------------------
! 4. Create reasonably formatted strings
!------------------------------------------------------------------------------

  write (my_date, "(i4.4,'-',i2.2,'-',i2.2)") my_year, my_month, my_day
  write (my_time, "(i2.2,':',i2.2,':',i2.2)") my_hour, my_min, my_sec

!------------------------------------------------------------------------------
! 5. Return variables
!------------------------------------------------------------------------------

  if (present(date))   date   = my_date
  if (present(time))   time   = my_time
  if (present(zone))   zone   = my_zone
  if (present(values)) values = my_values
  if (present(year))   year   = my_year
  if (present(month))  month  = my_month
  if (present(day))    day    = my_day
  if (present(hour))   hour   = my_hour
  if (present(min))    min    = my_min
  if (present(sec))    sec    = my_sec
  if (present(msec))   msec   = my_msec

end subroutine ncdf_date_and_time
