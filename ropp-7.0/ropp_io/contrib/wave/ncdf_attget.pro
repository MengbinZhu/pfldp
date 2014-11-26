; $Id: ncdf_attget.pro 1969 2008-11-17 13:08:27Z frhl $

pro ncdf_attget, ncdf_id, var_id, att_name, att_value, global = global

;+
; NAME:
;       NCDF_ATTGET
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

;  Rearrange arguments for global attributes...
;  --------------------------------------------

   if n_params() eq 3 and keyword_set(global) then begin

      att_name = var_id

      varid = NC_GLOBAL

;  ...or get variable id
;  ---------------------

   endif else if n_params() eq 4 then begin

      sz_varid = size(var_id)

      if sz_varid(sz_varid(0) + 1) eq 7 then begin
         varid = ncvarid(ncdf_id, var_id)
      endif else begin
         varid = var_id
      endelse

   endif else begin

      message, 'Incorrect number of arguments specified.'

   endelse

;  Inquire about attribute
;  -----------------------

   status = ncattinq(ncdf_id, varid, att_name, att_type, att_len)

;  Allocate variable to hold attribute value(s)
;  --------------------------------------------

   case att_type of
      NC_BYTE:   att_value = bytarr(att_len)
      NC_CHAR:   att_value = bytarr(att_len+1)
      NC_SHORT:  att_value = intarr(att_len)
      NC_LONG:   att_value = lonarr(att_len)
      NC_FLOAT:  att_value = fltarr(att_len)
      NC_DOUBLE: att_value = dblarr(att_len)
      else: message,'Unknown attribute type.'
   endcase

;  Read attribute value(s)
;  -----------------------

   status = ncattget(ncdf_id, varid, att_name, att_value)

   if att_type eq NC_CHAR then begin
      att_value = string(att_value)
   endif

;  Rearrange arguments for global attributes
;  -----------------------------------------

   if keyword_set(global) then begin
      att_name = att_value
   endif

   return

end
