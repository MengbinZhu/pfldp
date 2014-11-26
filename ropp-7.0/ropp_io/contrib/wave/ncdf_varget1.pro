; $Id: ncdf_varget1.pro 1969 2008-11-17 13:08:27Z frhl $

pro ncdf_varget1, ncdf_id, var_id, value, offset = offset

;+
; NAME:
;       NCDF_VARGET1
;
; PURPOSE:
;       This routine mimics IDL's routine of the same name
;
; AUTHOR:
;       Christian Marquardt <christian@marquardt.fsnet.co.uk>
;
; COPYRIGHT:
;       Copyright (c) 2004 Christian Marquardt <christian@marquardt.fsnet.co.uk>
;
;       All rights reserved.
;
;       Permission is hereby granted, free of charge, to any person obtaining
;       a copy of this software and associated documentation files (the
;       "Software"), to deal in the Software without restriction, including
;       without limitation the rights to use, copy, modify, merge, publish,
;       distribute, sublicense, and/or sell copies of the Software, and to
;       permit persons to whom the Software is furnished to do so, subject to
;       the following conditions:
;
;       The above copyright notice and this permission notice shall be
;       included in all copies or substantial portions of the Software as well
;       as in supporting documentation.
;
;       THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;       EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;       MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;       NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;       LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;       OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;       WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
;-

@hdf_common

;  Check arguments
;  ---------------

   if n_params() ne 3 then begin

      message, 'Incorrect number of arguments specified.'

   endif

   sz_varid = size(var_id)

   if sz_varid(sz_varid(0) + 1) eq 7 then begin
      varid = ncvarid(ncdf_id, var_id)
   endif else begin
      varid = var_id
   endelse

   if n_elements(offset) eq 0 then begin
      offset = [0, 0, 0, 0, 0, 0, 0]
   endif

;  Inquire about variable
;  ----------------------

   var_name = '                                                         ' $
      + '                                                               '
   dimids = intarr(100)

   status = ncvarinq(ncdf_id, varid, var_name, var_type, ndims, $
                     dimids, natts)

;  Allocate variable to hold variable value(s)
;  -------------------------------------------

   case var_type of
      NC_BYTE:   value = bytarr(1)
      NC_CHAR:   value = bytarr(var_len+1)
      NC_SHORT:  value = 0
      NC_LONG:   value = 0l
      NC_FLOAT:  value = 0.0
      NC_DOUBLE: value = 0.0d0
      else: message,'Unknown attribute type.'
   endcase

;  Read attribute value(s)
;  -----------------------

   status = ncvarget1(ncdf_id, varid, offset, value)

   if var_type eq NC_CHAR then begin
      value = string(value)
   endif

   return

end
