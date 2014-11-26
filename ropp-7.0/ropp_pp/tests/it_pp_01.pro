PRO it_pp_01, tfile, ofile=ofile

;; does a ropp pp test
;;
;; INPUT:
;;
;; KEYWORDS:
;;
;;; OUTPUT:
;;    error            1 [error] 0 [all fine]
;;

;; file settings
if n_elements(ofile) eq 0 then ofile = '../data/ropp_pp_test.nc' ;; observations
if n_elements(tfile) eq 0 then tfile = 'ropp_pp_test_1m.nc'      ;; output

;; read the original file

print,'Reading original file '+ofile
id = ncdf_open(ofile)

; Level 1b

var = 'impact' & ncdf_getvar, id, var, oimpact, error=error
if error then begin
   print,'IDL it_io_03 Error: (1) Variable not found: '+var
endif

var = 'bangle' & ncdf_getvar, id, var, obangle, error=error
if error then begin
   print,'IDL it_io_03 Error: (2) Variable not found: '+var
endif

; Level 2a

var = 'alt_refrac' & ncdf_getvar, id, var, ogeom, error=error
if error then begin
   print,'IDL it_io_03 Error: (3) Variable not found: '+var
endif

var = 'refrac' & ncdf_getvar, id, var, orefrac, error=error
if error then begin
   print,'IDL it_io_03 Error: (4) Variable not found: '+var
endif

ncdf_close, id

;; read the output file

print,'Reading output file '+tfile
id = ncdf_open(tfile)

; Level 1b

var = 'impact' & ncdf_getvar, id, var, timpact, error=error
if error then begin
   print,'IDL it_io_03 Error: (7) Variable not found: '+var
endif

var = 'bangle' & ncdf_getvar, id, var, tbangle, error=error
if error then begin
   print,'IDL it_io_03 Error: (8) Variable not found: '+var
endif
ii = where ( tbangle lt 0.0, count )
if count gt 0 then tbangle(ii) = 0.0

; Level 2a

var = 'alt_refrac' & ncdf_getvar, id, var, tgeom, error=error
if error then begin
   print,'IDL it_io_03 Error: (9) Variable not found: '+var
endif

var = 'refrac' & ncdf_getvar, id, var, trefrac, error=error
if error then begin
   print,'IDL it_io_03 Error: (10) Variable not found: '+var
endif
ii = where ( trefrac lt 0.0, count )
if count gt 0 then trefrac(ii) = 0.0

ncdf_close, id

ifail=0
no1b=n_elements(oimpact)
no2a=n_elements(ogeom)
nt1b=n_elements(timpact)
nt2a=n_elements(tgeom)

;print,'No. Level 1b levels:',no1b,nt1b
;print,'No. Level 2a levels:',no2a,nt2a

;;; Interpolate data onto common levels
grid1b = min(oimpact)+findgen(1500)*(0.7*(max(oimpact) - min(oimpact)))/1500.0
tbangle = interpol(tbangle, timpact, grid1b)
timpact = interpol(timpact, timpact, grid1b)
obangle = interpol(obangle, oimpact, grid1b)
oimpact = interpol(oimpact, oimpact, grid1b)

grid2a = min(ogeom) + findgen(1500) * (0.7*(max(ogeom) - min(ogeom)))/1500.0
trefrac = interpol(trefrac, tgeom, grid2a)
tgeom = interpol(tgeom, tgeom, grid2a)
orefrac = interpol(orefrac, ogeom, grid2a)
ogeom = interpol(ogeom, ogeom, grid2a)

;;; Compute maximum differences between data

diff = max( (oimpact - timpact)/oimpact)*100.
if ( diff gt 0.05 ) then begin
print, 'Impact parameters differ by ', diff, '% - TEST FAILED'
ifail = 1
endif else begin
print, 'Impact parameters differ by ', diff, '% - TEST PASSED'
endelse

diff = max( (obangle - tbangle))
if ( diff gt 0.05) then begin
print, 'Corrected bending angles differ by ', diff, 'rad - TEST FAILED'
ifail = 1
endif else begin
print, 'Corrected bending angles differ by ', diff, 'rad - TEST PASSED'
endelse  

diff = max(((ogeom - tgeom)/ogeom)*100.)
if ( diff gt 0.01) then begin
print, 'Geometric heights differ by ', diff, '% - TEST FAILED', format='(a,f8.5,a)'
ifail = 1
endif else begin
print, 'Geometric heights differ by ', diff, '% - TEST PASSED', format='(a,f8.5,a)'
endelse  

diff = max(((orefrac - trefrac)/orefrac)*100.)
if ( diff gt 1.0) then begin
print, 'Refractivity differ by ', diff, '% - TEST FAILED', format='(a,f8.5,a)'
ifail = 1
endif else begin
print, 'Refractivity differ by ', diff, '% - TEST PASSED', format='(a,f8.5,a)'
endelse 

jpgfile = 'ropp_pp_comp.jpg'
open_jpgfile, jpgfile, resolution, thisDevice
!P.multi=[0,2,0]
plot, (obangle - tbangle), (grid1b-min(grid1b))/1000.0, xtitle="Bending angle difference (rad)", ytitle="Impact height [x-R] (km)"
plot, ((orefrac - trefrac)/orefrac)*100., grid2a/1000.0, xtitle="Refractivity difference (%)", ytitle="Geopotential height (km)"
close_jpgfile, jpgfile, resolution, thisDevice



if(ifail eq 1)then print, 'At least one test failed.....TEST FAILED!'
if(ifail eq 0)then print, 'All tests successful.....TEST PASSED!'




end   

