PRO plot_fm_twod, file, extra_command=extra_command

;; Plot refractivity and bending angle profiles computed from FM tests

print, 'PLOT_FM: Plot forward modelled observables for file: ', file

if not keyword_set(extra_command) then extra_command=''

;; read the generated output file
id = ncdf_open(file)

var = 'occ_id' & ncdf_getvar, id, var, b_ropp_scenarios, error=error
if error then begin
   print,'IDL plot_fm Error: (1) Variable not found: '+var
endif
ropp_scenarios = string(byte(b_ropp_scenarios))

;; Refractivity FM
;var = 'alt_refrac' & ncdf_getvar, id, var, ropp_alt, error=error
;if error then begin
;   print,'IDL plot_fm Warning: (2) Variable not found: '+var
;endif
;var = 'refrac' & ncdf_getvar, id, var, ropp_refrac, error=error
;if error then begin
;   print,'IDL plot_fm Warning: (3) Variable not found: '+var
;endif

;; Bending angle FM
var = 'impact' & ncdf_getvar, id, var, ropp_impact, error=error
if error then begin
   print,'IDL plot_fm Warning: (2) Variable not found: '+var
endif
var = 'bangle' & ncdf_getvar, id, var, ropp_bangle, error=error
if error then begin
   print,'IDL plot_fm Warning: (3) Variable not found: '+var
endif
var = 'roc' & ncdf_getvar, id, var, ropp_roc, error=error
if error then begin
   print,'IDL plot_fm Warning: (2) Variable not found: '+var
endif

;; read meteorological variables

var = 'press' & ncdf_getvar, id, var, bg_pres, error=error
if error then begin
   print,'IDL plot_fm Error: (2) Variable not found: '+var

endif
var = 'geop' & ncdf_getvar, id, var, bg_geop, error=error
if error then begin
   print,'IDL plot_fm Error: (3) Variable not found: '+var

endif
var = 'temp' & ncdf_getvar, id, var, bg_temp, error=error
if error then begin
   print,'IDL plot_fm Error: (4) Variable not found: '+var

endif
var = 'shum' & ncdf_getvar, id, var, bg_shum, error=error
if error then begin
   print,'IDL plot_fm Error: (5) Variable not found: '+var

endif
var = 'lat' & ncdf_getvar, id, var, bg_lat, error=error
if error then begin
   print,'IDL plot_fm Error: (6) Variable not found: '+var

endif
nscenarios = n_elements(bg_lat)

ncdf_close, id

;; number of scenarios
nsce = n_elements(ropp_bangle[0,*])

;; number of observation altitude levels
nobs = n_elements(ropp_impact[*,0])

;; number of model vertical levels
nmod = n_elements(bg_geop[*,0,0])

;; number of model horizontal levels
nhor = n_elements(bg_geop[0,*,0])

;; do the plot directly into jpeg impage following Fanning. Resolution
;; is set automatically
jpgfile = file+'.jpg'
print, 'Generating output image file '+jpgfile

open_jpgfile, jpgfile, resolution, thisDevice
!P.multi=[0,5,0]

;; margin for info at bottom for all following plots
!y.omargin = [7,0]


;; Plot output refractivity
if n_elements(ropp_refrac) gt 0 then begin
plot, ropp_refrac[*,0], ropp_alt[*,0]/1000.0, ystyle=1, yrange=[0.1,100.], $
	ytitle='Geopotential Height [km]', xtitle='FM Refractivity [N]', charsize=1.6, /ylog
for isce=1,nsce-1 do $
  oplot, ropp_refrac[*,isce], ropp_alt[*,isce]/1000.0, linestyle=isce
endif

;; Plot output bending angle
plot, ropp_bangle[*,0], (ropp_impact[*,0]-ropp_roc[0])/1000.0, ystyle=1, yrange=[0.1,100.],$
	ytitle='Impact Height [km]', xtitle='FM bending angle [rad]', charsize=1.6, /ylog
for isce=1,nsce-1 do $
  oplot, ropp_bangle[*,isce], (ropp_impact[*,isce]-ropp_roc[isce])/1000.0, linestyle=isce

;; Plot background temperature
plot, bg_temp[*,0], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.1,100.], $
	ytitle='Geopotential Height [km]', xtitle='BG Temperature [K]', $
        charsize=1.6, /ylog
for isce=1,nhor-1 do $
  oplot, bg_temp[*,isce], bg_geop[*,isce]/1000.0, linestyle=isce

twod = fltarr(nhor, nmod)
for i=0,nhor-1 do begin
for j=0,nmod-1 do begin
    twod(i,j) = bg_temp(j,i,0)
endfor
endfor
contour, twod, findgen(nhor),bg_geop(0:nmod-1,0,0)/1000.0, /fill, yrange=[0.1,100.0], ystyle=1, xtitle='Hor', charsize=1.6, levels=160.+findgen(11)*20., /ylog


;; Plot background shum
plot, bg_shum[*,0], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.1,100], $
	ytitle='Geopotential Height [km]', xtitle='BG Specific humidity [g/kg]', $
	charsize=1.6, /ylog
for isce=1,nhor-1 do $
  oplot, bg_shum[*,isce], bg_geop[*,isce]/1000.0, linestyle=isce

;; Plot background pres
;plot, bg_pres[*,0], bg_geop[*,0]/1000.0, ystyle=1, yrange=[0.1,100], $
;	ytitle='Geopotential Height [km]', xtitle='BG Pressure [hPa]', $
;	charsize=1.6, /ylog
;for isce=1,nhor-1 do $
;  oplot, bg_pres[*,isce], bg_geop[*,isce]/1000.0, linestyle=isce

twod = fltarr(nhor, nmod)
for i=0,nhor-1 do begin
for j=0,nmod-1 do begin
    twod(i,j) = bg_shum(j,i,0)
endfor
endfor
contour, twod, findgen(nhor),bg_geop(0:nmod-1,0,0)/1000.0, /fill, yrange=[0.1,100.0], ystyle=1, xtitle='Hor', charsize=1.6, /ylog, levels=findgen(11)*0.2

;; info for plot
pinfo = [ ['0.05','0.10','ROPP forward modelled refractivity and bending angle using ECMWF background profiles'], $
 	  ['0.05','0.07','Data available from http://www.romsaf.org'],$
	  ['0.05','0.01','Calls ropp_fm_bgro_2d'+extra_command+'. Produced on: '+systime() ] ]

for i=0,n_elements(pinfo[0,*])-1 do xyouts, double(pinfo[0,i]), double(pinfo[1,i]), pinfo[2,i], /normal, charsize=1.2

;; close a jpg file
close_jpgfile, jpgfile, resolution, thisDevice

;; reset all to default
!P.multi=0
!y.omargin = [0,0]

end   

