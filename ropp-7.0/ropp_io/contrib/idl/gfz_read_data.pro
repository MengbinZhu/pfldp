;;
;; CATEGORY:
;;       I/O.
;;
;; CALLING SEQUENCE:
;;       gfz_read_data, file, champ, flcom=flcom
;;
;; INPUTS:
;;       file:    Champ dat file name, can also be an array, 
;;                expecting dsc file in the same path
;;
;; OPTIONAL INPUTS:
;;       flcom:  string, executed to get file list, used instead of file
;;
;; OUTPUT:
;;       champ:  structure holding Champ data, for elements see individual
;;               procedures
;;
;; COPYRIGHT
;;    (C) Copyright 2005, EUMETSAT, All Rights Reserved.
;;
;;    This software was developed within the context of the EUMETSAT Satellite
;;    Application Facility on Radio Occultation Meteorology (ROM SAF), under the Cooperation
;;    Agreement dated 26 August 2003, between EUMETSAT and the Danish
;;    Meteorological Institute (DMI), Denmark, by one or more partners within
;;    the ROM SAF. The partners in the ROM SAF are DMI, Met Office, UK, and
;;    the Institut d'Estudis Espacials de Catalunya (IEEC), Spain.
;;
;; WARRANTY:
;;    No warrantly given whatsoever. Use at own risk.
;;
;; AUTHOR:
;;    Axel von Engeln


PRO counter, i, imin, msgstr

;; outputs a counter i to screen, first time (i = imin) msgstr is printed,
;; afterwards only the number. Output format is hardwired.
;;
;; EXAMPLE:
;;       counter, 0, 0, 'Reading profile: '
;;

if (i eq imin) then print, format='($,%"    %s %8d")',msgstr,i $
	else print,format='($,%"\B\B\B\B\B\B\B\B%8d")',i

end

PRO gfz_create_structure, ndata, champ

;; define the structure, this will hold all values.
;; 
;; The individual structure names and initializations are used
;; for the search. The name is actually also the name found in 
;; the Champ ASCII file for single values, strings. Or at least
;; it is part of the string in the dsc file, where blanks are 
;; removed. Make sure there are not several similar entries that
;; might be picked up over here. Otherwise it should be 
;; sufficient to add a required Champ variable to this structure
;; to get it read.
;;
;; The initialization for single elements is as follows:
;;    -1    entry found in dat file
;;    -2    entry found in dsc file
;;    -3    entry determined here, not read
;;
;; For arrays, the name is also the name found in the dat file
;; description, it is used to find the data in the given file.
;; Missing values in array are set to -1000.0
;;
;; INPUT:
;;     ndata         integer, number of elements in array
;;
;; OUTPUT:
;;     champ         struture, champ data

champ = {$
	occnr                  : -1,                 $ ;; occultation number
	starttime              : '',                 $ ;; occultation start time
	year                   : -3,                 $ ;; year of occultation
	month                  : -3,                 $ ;; month
	day                    : -3,                 $ ;; day
	hour                   : -3,                 $ ;; hour
	minute                 : -3,                 $ ;; minute
	seconds                : -3,                 $ ;; seconds
	occsat                 : -1,                 $ ;; occultation satellite
	viewangle              : -2.0,               $ ;; viewing angle
	antangle               : -2.0,               $ ;; antenna angle
	altitudeearthsurface   : -2.0,               $ ;; altitude earth surface above MSL [km]
	inversionqualitystrato : -2,                 $ ;; inversion quality stratosphere
	inversionqualitytropo  : -2,                 $ ;; inversion quality troposphere
	qualityflag            : -2,                 $ ;; quality flag
	localradiusofcurvature : -2.0,               $ ;; radius of curvature
	geoidundulation        : -2.0,               $ ;; geoid undulation
	numberofflywheeling    : -2,                 $ ;; number of flywheeling activations
	nalt                   : -3,                 $ ;; number of altitudes
	alt_msl                : fltarr(ndata),      $ ;; altitude [km]
	latitude               : fltarr(ndata),      $ ;; latitude
	longitude              : fltarr(ndata),      $ ;; longitude
	refractivity           : fltarr(ndata),      $ ;; refractivity [1]
	density                : fltarr(ndata),      $ ;; density [kg/m3]
	pressure               : fltarr(ndata),      $ ;; pressure [mbar]
	temperature            : fltarr(ndata),      $ ;; temperature [K]
	bendingangle           : fltarr(ndata),      $ ;; bending angle [rad]
	impactparameter        : fltarr(ndata),      $ ;; impact parameter [km]
        geopotentialheight     : fltarr(ndata),      $ ;; geopotential height [m]
	snr                    : fltarr(ndata)       $ ;; SNR, the first entry is taken here!
	}

end


PRO gfz_modify_structure, champ, nfiles

;; modifies champ structure, if nfiles eq 1 removes
;; all empty array entries, if nfiles gt 1 makes an 
;; educated guess on the altitude coverage and fills the
;; arrays accordingly
;;
;; INPUT:
;;     champ            structure, champ data
;;     nfiles           integer, number of files to read
;;
;; OUTPUT:
;;     champ            structure, modified

if nfiles eq 1 then begin

   ndata = champ.nalt
   gfz_create_structure, ndata, champnew

   for i=0,n_elements(tag_names(champ))-1 do begin
      if n_elements(champ.(i)) eq 1 then champnew.(i) = champ.(i) $
      else champnew.(i) = champ.(i)[0:ndata-1]
   endfor
   
endif else begin

   ;; make an educated altitude guess, 0 to 35 km, 0.2 km steps
   ndata = 176
   gfz_create_structure, ndata, champnew
   champnew.alt_msl = indgen(ndata) * 0.2

   elem = tag_names(champ)
   nelem = n_elements(elem)
   
   for i=0,n_elements(elem)-1 do begin
      if n_elements(champ.(i)) eq 1 then champnew.(i) = champ.(i) $
      else begin
         if strpos(strlowcase(elem[i]),'alt_msl') ge 0 then continue
         idx = where(champnew.alt_msl ge champ.alt_msl[0] and $
		 champnew.alt_msl le champ.alt_msl[champ.nalt-1], complement=cidx, ncomplement=ncidx)
	 champnew.(i)[idx]  = champ.(i)[0:champ.nalt-1]
	 if ncidx gt 0 then champnew.(i)[cidx] = -1000.0
      endelse
   endfor

endelse

;; overwrite structure
champ = champnew

end

PRO gfz_read_dscfile, file, champ

;; reads a GFZ dsc file, searching for some input variables
;;
;; INPUT:
;;     file            string, dat file, is modified for dsc
;;
;; OUTPUT:
;; champ           structure, contains read data

;; get the dsc filename
dscfile = strmid(file, 0, strlen(file)-3)+'dsc'

;; check if available
if file_test(dscfile) eq 0 then begin
   print,'Warning: dsc file not found: '+dscfile
   return
endif

;; number of structure elements
elem  = tag_names(champ)
nelem = n_elements(elem)

;; read the file
openr,1,dscfile

while not eof(1) do begin
   str=''
   readf,1,str
   equalpos = strpos(str,'=')
   if equalpos lt 0 then continue
   searchstr = strcompress(strmid(str,0,equalpos), /remove)
   for i=0,nelem-1 do begin
      pos = strpos(searchstr, strlowcase(elem[i]))
      if pos lt 0 then continue
      ;; check if this is a dsc entry
      if n_elements(champ.(i)) gt 1 then continue
      if champ.(i) ne -2 then continue
      ;; deal with int or float
      case size(champ.(i),/type) of
      2: champ.(i) = fix(strmid(str,equalpos+1,strpos(str,';')-equalpos))
      4: champ.(i) = float(strmid(str,equalpos+1,strpos(str,';')-equalpos))
      5: champ.(i) = double(strmid(str,equalpos+1,strpos(str,';')-equalpos))
      else: begin
               print,'Error: Variable type not yet implented.'
	       stop
            end
      endcase
   endfor
endwhile

close,1

end

PRO gfz_read_datfile, file, champ

;; reads a GFZ dat file, searching for some input variables
;;
;; INPUT:
;;     file            string, dat file
;;
;; OUTPUT:
;;     champ           structure, contains read data

;; get the number of data lines (lines with no #), can alternatively
;; been read from the header although in the past some headers gave wrong
;; information
spawn,'more '+file+'|grep -v "#"'+'|wc -l', nlines

;; get the number of comment lines
spawn,'more '+file+'|grep "#"'+'|wc -l', ncomments

;; create the structure
ndata = 500
gfz_create_structure, ndata, champ

;; get the structure elements
elem = tag_names(champ)
nelem = n_elements(elem)

;; search in header if single element, using grep
for i=0, nelem-1 do begin
   if n_elements(champ.(i)) eq 1 then begin
      ;; this is a header or dsc entry
      if champ.(i) le -2 then continue ;; dsc file or processing entry, skip
      spawn,'grep '+strlowcase(elem[i])+' '+file, res, exit_status=estat
      if estat ne 0 then begin
         print,'Error: find data entry: '+elem[i]
	 stop
      endif
      ;; remove the blanks and find the correct entry
      res = strsplit(strcompress(res),' ', /extract)
      for j=0,n_elements(res)-1 do begin
         if strpos(res[j],strlowcase(elem[i])) ge 0 then continue
	 if j eq n_elements(res)-1 then champ.(i)=fix(string(res[j])) $
         else begin
	    for k=j,n_elements(res)-1 do champ.(i) = champ.(i)+' '+res[k]
	    break
	 endelse
      endfor
   endif
endfor

;; find the number of data rows and their position
datarec = intarr(nelem)
datarec = datarec - 1
openr,1,file
str=''
while strpos(str,'|') lt 0 do readf,1,str
str = strcompress(strsplit(str,'|',/extract), /remove)
nrows = n_elements(str)
for i=0,nelem-1 do begin
   if n_elements(champ.(i)) eq 1 then continue
   pos = strpos(strlowcase(str),strlowcase(elem[i]))
   idx = where(pos ne -1, nidx)
   if nidx gt 0 then datarec[i] = idx[0]
endfor

;; rewind file and then go forward to data entries
point_lun,1,0
str = ''
for i=0,ncomments[0]-1 do readf,1,str

;; finally read the data and put into structure
champ.nalt = nlines
data = dblarr(nrows, nlines)
readf,1,data
close,1
for i=0,nelem-1 do begin
   if datarec[i] lt 0 then continue
   champ.(i) = data[datarec[i],*]
endfor

;; get the year, etc from starttime
time = strcompress(champ.starttime, /remove)
champ.year    = strmid(time,0,4)
champ.month   = strmid(time,5,2)
champ.day     = strmid(time,8,2)
champ.hour    = strmid(time,10,2)
champ.minute  = strmid(time,13,2)
champ.seconds = strmid(time,16,2)

end


PRO gfz_read_data, gfzfiles, champ, flcom=flcom

;; reads GFZ Champ data and converts it into a ROPP netCDF file
;;
;; INPUT:
;;      gfzfiles          string array, list of dat files to read. Also
;;                        tries to read a dsc file in the same path
;;
;; OUTPUT:
;;      champ             structure: champ data
;;
;; KEYWORDS:
;;      flcom             string, unix command to generate a GFZ file list,
;;                        e.g. '\ls *.dat'. Make sure no color is set with ls.
;;                        This will overwrite the gfzfiles entry
;;
;; EXAMPLE:
;;      read one file:
;;      gfz_read_data, 'champ.dat', champ
;;
;;      read a list of files, gfzfiles is only a dummystring
;;      gfz_read_data, 'dummystr', champ, flcom='\ls 05.174/*.dat'

;; get the filelist
if n_elements(flcom) ne 0 then begin
   spawn, flcom, gfzfiles, exit_status=estat
   if estat ne 0 then begin
      print,'Error: Could not execute command: '+flcom
      stop
   endif
endif
nfiles = n_elements(gfzfiles)
print,'  Number of GFZ files to read: '+string(nfiles)

;; cycle all input files
for i=long(0),nfiles-1 do begin

   if nfiles gt 1 then counter, i, 0, 'Reading profile: '
	
   ;; read the datafile and return values in structure
   gfz_read_datfile, gfzfiles[i], champr

   ;; read the dsc file, assuming it is in the same path
   gfz_read_dscfile, gfzfiles[i], champr

   ;; modify the structure
   gfz_modify_structure, champr, nfiles

   ;; append
   if i eq 0 then champ = champr else champ = [champ, champr]

endfor

print,''


end
