; $Id: ncdf_getvar.pro,v 1.10 2002/09/18 18:11:35 frcm Exp $

pro NCDF_GETVAR, FILE, VARNAME, FIELD, NODATA = nodata, LABEL = label,          $
                 UNITS = units, MISSVAL = missval, FILLVAL = fillval,           $
                 START = start, COUNT = count, EDGES = edges, STRIDE = stride,  $
                 DNAMES = dnames, DSIZES = dsizes, TRANSPOSE = transp,          $
                 HELP = hlp, Error=Error
;+
; NAME:
;       NCDF_GETVAR
;
; PURPOSE:
;       This routine gets a variable and its metadata from a NetCDF file.
;
; CATEGORY:
;       I/O.
;
; CALLING SEQUENCE:
;       NCDF_GETVAR, FILE, VARNAME, FIELD, ...
;
; INPUTS:
;       FILE:    netCDF file - either a file name or a valid netCDF
;                ID. If a filename is given, the appropiate netCDF
;                will be opened and closed. If a netCDF ID is
;                specified, the corresponding netCDF will neither be
;                opend or closed; this remains to the caller of the
;                routine.
;       VARNAME: String containing name of variable.
;
; OPTIONAL INPUTS:
;       START:  Array containing start indices for each
;               dimension. Default is to start with the first.
;       COUNT:  Array containing number of values to be read for
;               each dimension. Default is to read all.
;       EDGES:  Array containing end point indices for each
;               dimension. If both COUNT and EDGES are given, EDGES
;               win. 
;       STRIDE: Array containing strides for each dimension; default
;               is to read all data values.
;       
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       FIELD:  Array containing the data.
;
; OPTIONAL OUTPUTS:
;       DNAMES: Array containing the names of each dimension.
;       DSIZES: Array containing the sizes (lengths) of each dimension.
;       Error:  either 1 (error) or 0 (all fine)
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;       Only those attributes of variables are checked that are
;       mentioned in the COARDS convention (available at URL
;       ftp://ftp.unidata.ucar.edu/pub/netcdf/Conventions/COARDS).
;
; PROCEDURE:
;       Based on ncdf_get_1field.pro written 1 April 1993 by William Weibel,
;       UCLA Atmospheric Sciences.
;
; EXAMPLE:
;       To read a variable 'mainvar' from 'test.nc', do
;
;          ncdf_getvar, 'test.nc', 'mainvar', mainvar, ...
;
; AUTHOR:
;       Christian Marquardt <christian.marquardt@metoffice.com>
;
; MODIFICATION HISTORY:
;
;       Aug 14 2006, Axel von Engeln
;               added error keyword
;
;       Sep 18 2002, Christian Marquardt
;       <christian.marquardt@metoffice.com>
;
;		The transpose command under PV Wave does not work like
;		it does under IDL; thus, support for transposing
;		higher dimensional arrays (i.e., higher than rank 2)
;		has been disabled.
;
;       Aug 21 2000, Christian Marquardt
;       <christian.marquardt@metoffice.com>
;
;		Now handles scalar variables as well.
;
;       Wed Sep 8 11:49:20 1999, Christian Marquardt
;       <marq@kg96.gfz-potsdam.de>
;
;		Fixed conversion of numbers for the COARDS
;		convention. 
;
;       Sun Mar 28 23:08:41 1999, Christian Marquardt
;       <marq@medea.met.fu-berlin.de>
;
;		Added support for the /TRANSPOSE keyword; converts any 
;		1 element array into a scalar and rejects arrays.
;
;       Wed Nov 25 13:43:24 1998, Christian Marquardt
;       <marq@strat06.met.fu-berlin.de>
;
;		Added support for numerical filenames - i.e., for
;		specifying a netcdf ID rather than a file name.
;
;       Tue Aug 20 13:58:07 1996, Andreas Braun
;       <braun@strat01.met.fu-berlin.de>
;
;		Fixed a problem for huge data sets by casting all
;               parameters for ncdf_varget to long.
;
;       Fri Aug 16 18:52:34 1996, Christian Marquardt
;       <marq@strat06.met.fu-berlin.de>
;
;               Added documentation, a number of options, support for
;               COARDS attributes.
;
;       Sun Apr 21 15:50:41 1996, Christian Marquardt
;       <marq@strat06.met.fu-berlin.de>
;
;               Created.
;
;-

;  Give some help
;  --------------

   if keyword_set(hlp) then begin
      print, 'Read a variable from a netCDF.'
      print, 'ncdf_getvar, file, varname, field'
      print, '  file    = netCDF file to be opened (string or netCDF-ID).  in'
      print, '  varname = string containing varname.                       in'
      print, '  field   = array containing data.                          out'
      print, 'Keywords:'
      print, '  start   = array containing start indices for each dim.     in'
      print, '  count   = array containing number of values to be read'
      print, '              for each dim.                                  in'
      print, '  edges   = array containing end point indices for each dim. in'
      print, '  stride  = array containing strides for each dim.           in'
      print, '  label   = string containing long_name of the variable.    out'
      print, '  units   = string containing units of the variable.        out'
      print, '  dnames  = array containing names of each dim.             out'
      print, '  dsizes  = array containing sizes of each dim.             out'
      print, 'Notes:'
      print, '  The attributes scale_factor and add_offset are handled'
      print, '  internally as described in the COARDS convention. If file is'
      print, '  given as a numerical netCDF-ID rather than a file name, the'
      print, '  associated netCDF is neither opened nor closed.'
      return
   endif

;  Make sure that file is a scalar
;  -------------------------------

   if n_elements(file) ne 1 then begin
      message, 'Only one netCDF file possible - aborting...'
   endif

   file = file(0)

;  Open netCDF data file
;  ---------------------

   type_file = size(file, /type)

   if type_file eq 7 then begin
      fileid = ncdf_open(file)
   endif else begin
      fileid = file
   endelse

;  check that variable exits, otherwise return with error
;  ------------------------------------------------------
   error = 0
   inq = ncdf_inquire(fileid)
   varnames = strarr(inq.nvars)
   for i=0, inq.nvars-1 do begin
      var = ncdf_varinq(fileid, i)
      varnames[i] = var.name
   endfor
   if max( strupcase(varnames) eq strupcase(varname) ) ne 1 then begin
      print,'IDL netcdf_getvar Error: Variable '+varname+' not found in file. Returning.'
      error = 1
      return
   endif
   
;  Find out how many and which dimensions are used
;  -----------------------------------------------

   var = ncdf_varinq( fileid, varname )
   if (var.ndims gt 0) then begin
      dnames = strarr(var.ndims)
      dsizes = lonarr(var.ndims)
      for ndim = 0,var.ndims-1 do begin
         ncdf_diminq, fileid, var.dim(ndim), buffer, size
         dnames(ndim) = buffer
         dsizes(ndim) = size
      endfor
   endif

;  Read attributes of the variable as required by the COARDS
;  ---------------------------------------------------------
;  convention
;  ----------

   label = varname
   units = ''
   missval = !values.f_nan
   fillval = !values.f_nan

   if (var.natts gt 0) then begin
      for natt = 0l, var.natts - 1l do begin
         attname = ncdf_attname(fileid, varname, natt)
         ncdf_attget, fileid, varname, attname, buffer
         case attname of
            'long_name'     :  label        = string(buffer)
            'units'         :  units        = string(buffer)
            'missing_value' :  missval      = buffer
            '_FillValue'    :  fillval      = buffer
            'scale_factor'  :  scale_factor = buffer
            'add_offset'    :  add_offset   = buffer
            else            :
         endcase
      endfor
   endif

   if not keyword_set(nodata) then begin

;     Handle scalar data...
;     ---------------------

      if (var.ndims eq 0) then begin

         ncdf_varget1, fileid, varname, field

      endif else begin

;     ...or prepare some arrays...
;     ----------------------------

         if n_elements(start) eq 0 then   start  = replicate(0L, var.ndims)
         if n_elements(count) eq 0 then   count  = dsizes   $
         else                             dsizes = count
         if n_elements(edges) ne 0 then   count  = edges - start
         if n_elements(stride) eq 0 then  stride = replicate(1L, var.ndims)

         start  = long(start)
         count  = long(count)
         stride = long(stride)

;     ...and read the variable
;     ------------------------

         ncdf_varget, fileid, varname, field,  $
            offset = start, count = count, stride = stride

      endelse

;     Use scale_factor and add_offset as required by COARDS
;     -----------------------------------------------------

      if n_elements(scale_factor) eq 1 and n_elements(add_offset) eq 1 then begin
         field = field * scale_factor + add_offset
      endif
      if n_elements(scale_factor) eq 1 and n_elements(add_offset) eq 0 then begin
         field = field * scale_factor
      endif
      if n_elements(scale_factor) eq 0 and n_elements(add_offset) eq 1 then begin
         field = field + add_offset
      endif
   endif

;  Close netCDF data file
;  ----------------------

   if type_file eq 7  then begin
      ncdf_close, fileid
   endif

;  Transpose array if required
;  ---------------------------

   if n_elements(transp) eq 1 then begin
      field = transpose(field)
   endif
;   endif else if n_elements(transp) gt 1 then begin
;      field = transpose(field, transp)
;   endif

   return
end
