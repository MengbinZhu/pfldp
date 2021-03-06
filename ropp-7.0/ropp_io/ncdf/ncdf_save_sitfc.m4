dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl ncdf.f90.
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
     subroutine NCDF_SSAV (name, values, long_name, units, file, &
                           ncid, overwrite)
       use typeSizes
       character(len = *),        intent(in)  :: name
       TYPE, &
                                  intent(in)  :: values
       character(len = *),        optional    :: long_name
       character(len = *),        optional    :: units
       character(len = *),        optional    :: file
       integer,                   optional    :: ncid
       logical,                   optional    :: overwrite
     end subroutine NCDF_SSAV
