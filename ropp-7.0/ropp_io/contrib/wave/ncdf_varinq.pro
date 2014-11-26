; $Id: ncdf_varinq.pro 1969 2008-11-17 13:08:27Z frhl $

function ncdf_varinq, ncdf_id, var_id

;+
; NAME:
;       NCDF_VARINQ
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

   if n_params() ne 2 then begin
      message, 'Two arguments are required.'
   endif

;  Get variable id
;  ---------------

   sz_varid = size(var_id)

   if sz_varid(sz_varid(0) + 1) eq 7 then begin
      varid = ncvarid(ncdf_id, var_id)
   endif else begin
      varid = var_id
   endelse

;  Inquire about the variable
;  --------------------------

   status = ncvarinq(ncdf_id, varid, var_name, var_type, var_ndims, var_dims, var_natts)

   case var_type of
      NC_BYTE:   type = 'BYTE'
      NC_CHAR:   type = 'CHAR'
      NC_SHORT:  type = 'INT'
      NC_LONG:   type = 'LONG'
      NC_FLOAT:  type = 'FLOAT'
      NC_DOUBLE: type = 'DOUBLE'
      else: message,'Unknown variable type.'
   endcase

;  Return this as a structure
;  --------------------------

   return, {, NAME:var_name, DATATYPE:type, NDIMS:var_ndims, NATTS:var_natts, DIM:var_dims}

end
