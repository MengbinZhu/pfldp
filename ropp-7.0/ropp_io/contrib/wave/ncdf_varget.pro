; $Id: ncdf_varget.pro 1969 2008-11-17 13:08:27Z frhl $

pro ncdf_varget, ncdf_id, var_id, values, count = count, $
                 offset = offset, stride = stride

;+
; NAME:
;       NCDF_VARGET
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

;  Get dimension lengths
;  ---------------------

   if n_elements(count) eq ndims then begin
      icount = long(count)
   endif else begin
      icount = lonarr(ndims)
      for i = 0, ndims - 1 do begin
         ncdf_diminq, ncdf_id, dimids(i), buffer, dlen
         icount(i) = dlen
      endfor
   endelse

;  Start/offset values
;  -------------------

   if n_elements(offset) eq ndims then begin
      istart = long(offset)
   endif else begin
      istart = replicate(0l, ndims)
   endelse

;  Allocate variable to hold variable value(s)
;  -------------------------------------------

   case var_type of
      NC_BYTE:   values = make_array(dimension = icount, /byte)
      NC_CHAR:   values = make_array(dimension = icount, /byte)
      NC_SHORT:  values = make_array(dimension = icount, /int)
      NC_LONG:   values = make_array(dimension = icount, /long)
      NC_FLOAT:  values = make_array(dimension = icount, /float)
      NC_DOUBLE: values = make_array(dimension = icount, /double)
      else: message,'Unknown attribute type, or character (which is not supported).'
   endcase

;  Memory mapping (this is a guess, really...)
;  -------------------------------------------

   case var_type of
      NC_BYTE:   imap = replicate(1l, ndims)
      NC_CHAR:   imap = replicate(1l, ndims)
      NC_SHORT:  imap = replicate(4l, ndims)
      NC_LONG:   imap = replicate(4l, ndims)
      NC_FLOAT:  imap = replicate(4l, ndims)
      NC_DOUBLE: imap = replicate(8l, ndims)
   endcase

;  Read attribute value(s)
;  -----------------------

   if n_elements(stride) eq ndims then begin
      for i = ndims - 1, 1, -1 do begin
         imap(i:*) = imap(i:*) * icount(i-1)
      endfor
      nn = 1
      status = ncvargetg(ncdf_id, varid, istart, icount, stride, imap, values)
   endif else begin
      status = ncvarget(ncdf_id, varid, istart, icount, values)
   endelse

   if var_type eq NC_CHAR then begin
      values = string(values)
   endif else begin
      values = reform(values)
   endelse

   return

end
