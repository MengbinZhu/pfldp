PRO plot_1dvar, file_id, extra_command=extra_command

;; Plot pressure, temperature and humidity increments computed from 1dVar tests

print, 'PLOT_1DVAR: Plotting increments for file_id: ', file_id

if not keyword_set(extra_command) then extra_command=''

;; file settings
yfile = '../data/IT-1DVAR-'+strmid(file_id, 0, 2)+'_y.nc'        ;; observation
bfile = '../data/IT-1DVAR-'+strmid(file_id, 0, 2)+'_b.nc'        ;; background
cfile = '../data/IT-1DVAR-'+strmid(file_id, 0, 2)+'_c.nc'        ;; correlations
ofile = '../data/IT-1DVAR-'+file_id+'.1.nc'                      ;;output

;; get the scenario names
id = ncdf_open(bfile)

var = 'occ_id' & ncdf_getvar, id, var, b_ropp_scenarios, error=error
if error then begin
   print,'IDL plot_1dvar Error: (1) Variable not found: '+var
endif
ropp_scenarios = string(byte(b_ropp_scenarios))

ncdf_close, id

;; read the generated output file, not all variable required here
id = ncdf_open(ofile)
idy = ncdf_open(yfile)

iref = 1
iba = 1

var = 'alt_refrac' & ncdf_getvar, id, var, ropp_alt, error=error, /silent
if error then iref = 0

var = 'impact' & ncdf_getvar, id, var, ropp_impact, error=error, /silent
if error then iba = 0

;; Refractivity 1dVar
if iref eq 1 then begin
var = 'alt_refrac' & ncdf_getvar, id, var, ropp_alt, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var
endif
var = 'refrac' & ncdf_getvar, id, var, ropp_meas, error=error
if error then begin
   print,'IDL plot_1dvar Error: (3) Variable not found: '+var
endif
var = 'alt_refrac' & ncdf_getvar, idy, var, ob_alt, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var
endif
var = 'refrac' & ncdf_getvar, idy, var, ob_meas, error=error
if error then begin
   print,'IDL plot_1dvar Error: (3) Variable not found: '+var
endif
endif

;; Bending angle 1dVar
if iba eq 1 then begin
var = 'impact' & ncdf_getvar, id, var, ropp_alt, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var
endif
var = 'bangle' & ncdf_getvar, id, var, ropp_meas, error=error
if error then begin
   print,'IDL plot_1dvar Error: (3) Variable not found: '+var
endif
var = 'impact' & ncdf_getvar, idy, var, ob_alt, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var
endif
var = 'roc' & ncdf_getvar, idy, var, ob_roc, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var
endif
var = 'bangle' & ncdf_getvar, idy, var, ob_meas, error=error
if error then begin
   print,'IDL plot_1dvar Error: (3) Variable not found: '+var
endif
endif

ncdf_close, idy

;; read meteorological variables

var = 'press' & ncdf_getvar, id, var, ropp_pres, error=error
if error then begin
   print,'IDL plot_1dvar Error: (2) Variable not found: '+var

endif
var = 'geop' & ncdf_getvar, id, var, ropp_geop, error=error
if error then begin
   print,'IDL plot_1dvar Error: (3) Variable not found: '+var

endif
var = 'temp' & ncdf_getvar, id, var, ropp_temp, error=error
if error then begin
   print,'IDL plot_1dvar Error: (4) Variable not found: '+var

endif
var = 'shum' & ncdf_getvar, id, var, ropp_shum, error=error
if error then begin
   print,'IDL plot_1dvar Error: (5) Variable not found: '+var

endif
var = 'lat' & ncdf_getvar, id, var, ropp_lat, error=error
if error then begin
   print,'IDL plot_1dvar Error: (6) Variable not found: '+var

endif
nscenarios = n_elements(ropp_lat)

ncdf_close, id

;; number of scenarios
nsce = n_elements(ropp_pres[0,*])

;; number of observation altitude levels
nobs = n_elements(ob_alt[*,0])

;; read the background file
id = ncdf_open(bfile)

var = 'press' & ncdf_getvar, id, var, bg_pres, error=error
if error then begin
   print,'IDL plot_1dvar Error: (7) Variable not found: '+var

endif
var = 'geop' & ncdf_getvar, id, var, bg_geop, error=error
if error then begin
   print,'IDL plot_1dvar Error: (8) Variable not found: '+var

endif
var = 'temp' & ncdf_getvar, id, var, bg_temp, error=error
if error then begin
   print,'IDL plot_1dvar Error: (9) Variable not found: '+var

endif
var = 'shum' & ncdf_getvar, id, var, bg_shum, error=error
if error then begin
   print,'IDL plot_1dvar Error: (10) Variable not found: '+var

endif
var = 'lat' & ncdf_getvar, id, var, bg_lat, error=error
if error then begin
   print,'IDL plot_1dvar Error: (11) Variable not found: '+var

endif

ncdf_close, id

;; make sure we have the same dimensions in input and output
if nscenarios ne n_elements(bg_geop[0,*]) then begin
   print,'IDL plot_1dvar Error: (12) # of scenarios and # of found ropp output results differ.'

endif 

;; number of retrieval altitudes
nret = n_elements(bg_geop[*,0])

;; holds the differences in absolute values (obs differences in %)
measdiff = dblarr(nsce, nobs)
tempdiff = dblarr(nsce, nret)
shumdiff = dblarr(nsce, nret)
presdiff = dblarr(nsce, nret)

;; run through all scenarios
for i=0, nsce-1 do begin

   ;; some info
   print,'   Processing scenario: '+ropp_scenarios[i]

   ;; interpolate onto common grid to check also geopotential altitudes
   rmeas = interpol(ropp_meas[*,i], ropp_alt[*,i], ob_alt[*,i])
   rtemp = interpol(ropp_temp[*,i], ropp_geop[*,i], bg_geop[*,i])
   rshum = exp(interpol(alog(ropp_shum[*,i]), ropp_geop[*,i], bg_geop[*,i]))
   rpres = exp(interpol(alog(ropp_pres[*,i]), ropp_geop[*,i], bg_geop[*,i]))
   
   ;; calculate the absolute difference 
   measdiff[i,*] = (rmeas - ob_meas[*,i]) / ob_meas[*,i] * 100.0
   tempdiff[i,*] = (rtemp - bg_temp[*,i]) ;;/ bg_temp[*,i] * 100.0
   shumdiff[i,*] = (rshum - bg_shum[*,i]) ;;/ bg_shum[*,i] * 100.0
   presdiff[i,*] = (rpres - bg_pres[*,i]) ;;/ bg_pres[*,i] * 100.0

endfor


;; print some output to screen
print,'   Found max difference [%] in observation scenarios: '+$
      string(max(measdiff,/absolute))
print,'   Found max difference [K] in temperature scenarios: '+$
      string(max(tempdiff,/absolute))
print,'   Found max difference [g/kg] in specific humidity scenarios: '+$
      string(max(shumdiff,/absolute))
print,'   Found max difference [hPa] in pressure scenarios: '+$
      string(max(presdiff,/absolute))
if strmid(file_id, 0, 2) eq '01' and max([max(tempdiff,/absolute), max(shumdiff,/absolute), max(presdiff,/absolute)]) gt 0.01 then begin
   print,'IDL plot_1dvar_01 Error: (32) Deviations in 1DVar between measurement and used background exceeds 0.01%.'
endif

if strmid(file_id, 0, 2) eq '01' then begin   ;; obs=bg test
for i=0, nsce-1 do begin
       tempdiff[i,*] = tempdiff[i,*] + 0.01*i
       shumdiff[i,*] = shumdiff[i,*] + 0.01*i
       presdiff[i,*] = presdiff[i,*] + 0.01*i       
endfor
endif


;; do the plot directly into jpeg impage following Fanning. Resolution
;; is set automatically
jpgfile = 'ropp_1dvar_comp_'+file_id+'.jpg'
if iref eq 1 then jpgfile = 'ropp_1dvar_comp_'+file_id+'_ref.jpg'
if iba eq 1 then jpgfile = 'ropp_1dvar_comp_'+file_id+'_ba.jpg'
print, 'Generating output image file '+jpgfile

open_jpgfile, jpgfile, resolution, thisDevice
!P.multi=[0,3,0]
if iref eq 1 or iba eq 1 then !P.multi=[0,4,0]

;; margin for info at bottom for all following plots
!y.omargin = [7,0]

;; compare output in measurement (ref or ba)
if iref eq 1 then begin

idx = where(abs(ob_meas[*,0]) lt 900.0)
plot, measdiff[0,idx], ob_alt[idx,0]/1000.0, ystyle=1, yrange=[0.,60.], $
	ytitle='Geopotential Height [km]', xtitle='Difference [%]', $
        title='Refractivity difference 1DVar to Ob', charsize=1.6, xrange=[-20.,20.], xstyle=1
for i=1,nsce-1 do begin
    idx = where(abs(ob_meas[*,i]) lt 900.0)
   oplot, measdiff[i,*], ob_alt[*,i]/1000.0, linestyle=i
endfor

endif else if iba eq 1 then begin

idx = where(abs(ob_meas[*,0]) lt 900.0)
plot, measdiff[0,idx], (ob_alt[idx,0]-ob_roc[0])/1000.0, ystyle=1, yrange=[0.,60.], $
	ytitle='Impact Height [km]', xtitle='Difference [%]', $
        title='Bangle difference 1DVar to Ob', charsize=1.6, xrange=[-20.,20.], xstyle=1
for i=1,nsce-1 do begin
    idx = where(abs(ob_meas[*,i]) lt 900.0)
   oplot, measdiff[i,idx], (ob_alt[idx,i]-ob_roc[i])/1000.0, linestyle=i
endfor

endif


;; compare output in temperature
plot, tempdiff[0,*], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.,60.], $
	ytitle='Geopotential Height [km]', xtitle='Difference [K]', $
        title='Temperature Difference 1DVar to Bg', charsize=1.6, xrange=[1.05*min(tempdiff),1.05*max(tempdiff)], xstyle=1
for i=1,nsce-1 do begin
   oplot, tempdiff[i,*], bg_geop[*,i]/1000.0, linestyle=i
endfor

;; compare output in shum
plot, shumdiff[0,*], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.,20], $
	ytitle='Geopotential Height [km]', xtitle='Difference [g/kg]', $
	title='Specific Humidity Difference 1DVar to Bg', charsize=1.6, xrange=[1.05*min(shumdiff),1.05*max(shumdiff)], xstyle=1
for i=1,nsce-1 do begin
   oplot, shumdiff[i,*], bg_geop[*,i]/1000.0, linestyle=i
endfor

;; compare output in pres
plot, presdiff[0,*], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.0,60], $
	ytitle='Geopotential Height [km]', xtitle='Difference [hPa]', $
	title='Pressure Difference 1DVar to Bg', charsize=1.6, xrange=[1.05*min(presdiff),1.05*max(presdiff)], xstyle=1
for i=1,nsce-1 do begin
   oplot, presdiff[i,*], bg_geop[*,i]/1000.0, linestyle=i
endfor

;; put legend
plot,[0,1],[0,1],xstyle=4,ystyle=4,/nodata,/noerase,/noclip
;klegend_d,ropp_scenarios,$
;   /fill,box=0,spacing=2.0,charsize=0.9,$
;   psym=intarr(nsce),colors=intarr(nsce),line=indgen(nsce),$
;   position = [0.1,0.9]

;; info for plot
if(strmid(file_id, 0, 2) eq '01')then begin
pinfo = [ ['0.05','0.10','ROPP comparision of temperatures (left), specific humidity (middle), and pressure (right)'],$
 	  ['0.05','0.07','using 1DVar retrieval on a simulated measurement. Background and simulated measurement'], $
	  ['0.05','0.04','are the same. Profiles from FASCOD and ECMWF model. Note: Profiles (i=0 to n) use an x-offset'], $
	  ['0.05','0.01','of i*0.01% for better readability. Calls ropp_1dvar'+extra_command+'. Produced on: '+systime() ] ]
endif else if(strmid(file_id, 0, 2) eq '04')then begin
pinfo = [ ['0.05','0.10','ROPP comparision of temperatures (left), specific humidity (middle), and pressure (right)'],$
 	  ['0.05','0.07','using 1DVar retrieval using COSMIC measurement. Background profiles from Met Office model.'], $
	  ['0.05','0.01','Calls ropp_1dvar'+extra_command+'. Produced on: '+systime() ] ]
endif else if(strmid(file_id, 0, 2) eq '05')then begin
pinfo = [ ['0.05','0.10','ROPP comparision of temperatures (left), specific humidity (middle), and pressure (right)'],$
 	  ['0.05','0.07','using 1DVar retrieval using GRAS measurement. Background profiles from ECMWF model.'], $
	  ['0.05','0.01','Calls ropp_1dvar'+extra_command+'. Produced on: '+systime() ] ]
endif else begin
pinfo = [ ['0.05','0.10','ROPP comparision of temperatures (left), specific humidity (middle), and pressure (right)'],$
          ['0.05','0.07','Calls ropp_1dvar'+extra_command+'. Produced on: '+systime() ] ]
endelse

for i=0,n_elements(pinfo[0,*])-1 do xyouts, double(pinfo[0,i]), double(pinfo[1,i]), pinfo[2,i], /normal, charsize=1.2

;; close a jpg file
close_jpgfile, jpgfile, resolution, thisDevice

;; reset all to default
!P.multi=0
!y.omargin = [0,0]

end   

