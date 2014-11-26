pro MSIS_ascii2netcdf

;; READ ASCII FORMAT MSIS DATA FILES AND CREATE NETCDF OUTPUT

klim = 31
nlim = 7
nmonth = 12

Ac0 = dblarr(nlim+1,klim+1)
Ac1 = dblarr(nlim,klim+1)
Bc1 = dblarr(nlim,klim+1)
Aa0 = dblarr(nlim+1)
Aa1 = dblarr(nlim)
Ba1 = dblarr(nlim)
Ab0 = dblarr(nlim+1)
Ab1 = dblarr(nlim)
Bb1 = dblarr(nlim)
Ad0 = dblarr(nlim+1)
Ad1 = dblarr(nlim)
Bd1 = dblarr(nlim)
Ae0 = dblarr(nlim+1)
Ae1 = dblarr(nlim)
Be1 = dblarr(nlim)


;; create temporary output netcdf file to be appended with each month
ofile = '../data/MSIS_coeff.nc'
ofiletmp = ofile+'.tmp'
ido = ncdf_create(ofiletmp, /clobber)
ncdf_control, ido, /fill


;; global attributes
print,"Creating attributes..."
;ncdf_attput, ido, 'title', 'Spherical harmonic coefficients for MSIS climatology refractivity data', /Global
ncdf_attput, ido, 'title', 'Spherical harmonic coefficients for MSIS climatology refractivity and bending angle data', /Global
ncdf_attput, ido, 'Conventions', 'CF-1.0', /Global
ncdf_attput, ido, 'processing_date', '17 May 2011', /Global

;; dimensions
dim_unlim = ncdf_dimdef(ido, 'dim_month', /UNLIMITED)
dim_coeff0 = ncdf_dimdef(ido, 'dim_coeff0', 8)
dim_coeff1 = ncdf_dimdef(ido, 'dim_coeff1', 7)
dim_coeff2 = ncdf_dimdef(ido, 'dim_coeff2', 32)

;; variables
v_nAc0 = ncdf_vardef(ido, 'ref_Ac0', [dim_coeff0, dim_coeff2, dim_unlim],/double)
v_nAc1 = ncdf_vardef(ido, 'ref_Ac1', [dim_coeff1, dim_coeff2, dim_unlim], /double)
v_nBc1 = ncdf_vardef(ido, 'ref_Bc1', [dim_coeff1, dim_coeff2, dim_unlim], /double)
v_nAa0 = ncdf_vardef(ido, 'ref_Aa0', [dim_coeff0, dim_unlim], /double)
v_nAa1 = ncdf_vardef(ido, 'ref_Aa1', [dim_coeff1, dim_unlim], /double)
v_nBa1 = ncdf_vardef(ido, 'ref_Ba1', [dim_coeff1, dim_unlim], /double)
v_nAb0 = ncdf_vardef(ido, 'ref_Ab0', [dim_coeff0, dim_unlim], /double)
v_nAb1 = ncdf_vardef(ido, 'ref_Ab1', [dim_coeff1, dim_unlim], /double)
v_nBb1 = ncdf_vardef(ido, 'ref_Bb1', [dim_coeff1, dim_unlim], /double)
v_nAd0 = ncdf_vardef(ido, 'ref_Ad0', [dim_coeff0, dim_unlim], /double)
v_nAd1 = ncdf_vardef(ido, 'ref_Ad1', [dim_coeff1, dim_unlim], /double)
v_nBd1 = ncdf_vardef(ido, 'ref_Bd1', [dim_coeff1, dim_unlim], /double)

v_bAc0 = ncdf_vardef(ido, 'ba_Ac0', [dim_coeff0, dim_coeff2, dim_unlim], /double)
v_bAc1 = ncdf_vardef(ido, 'ba_Ac1', [dim_coeff1, dim_coeff2, dim_unlim], /double)
v_bBc1 = ncdf_vardef(ido, 'ba_Bc1', [dim_coeff1, dim_coeff2, dim_unlim], /double)
v_bAa0 = ncdf_vardef(ido, 'ba_Aa0', [dim_coeff0, dim_unlim], /double)
v_bAa1 = ncdf_vardef(ido, 'ba_Aa1', [dim_coeff1, dim_unlim], /double)
v_bBa1 = ncdf_vardef(ido, 'ba_Ba1', [dim_coeff1, dim_unlim], /double)
v_bAb0 = ncdf_vardef(ido, 'ba_Ab0', [dim_coeff0, dim_unlim], /double)
v_bAb1 = ncdf_vardef(ido, 'ba_Ab1', [dim_coeff1, dim_unlim], /double)
v_bBb1 = ncdf_vardef(ido, 'ba_Bb1', [dim_coeff1, dim_unlim], /double)
v_bAd0 = ncdf_vardef(ido, 'ba_Ad0', [dim_coeff0, dim_unlim], /double)
v_bAd1 = ncdf_vardef(ido, 'ba_Ad1', [dim_coeff1, dim_unlim], /double)
v_bBd1 = ncdf_vardef(ido, 'ba_Bd1', [dim_coeff1, dim_unlim], /double)
v_bAe0 = ncdf_vardef(ido, 'ba_Ae0', [dim_coeff0, dim_unlim], /double)
v_bAe1 = ncdf_vardef(ido, 'ba_Ae1', [dim_coeff1, dim_unlim], /double)
v_bBe1 = ncdf_vardef(ido, 'ba_Be1', [dim_coeff1, dim_unlim], /double)

 ;; Put file back into data mode and close
ncdf_control, ido, /ENDEF
ncdf_close, ido


for imonth=1,12 do begin
month = string(imonth,format='(i2.2)')

;; specify ascii filename
fname_ref='../data/MSIS'+month+'.asc'
fname_ba='../data/dabcs'+month+'.asc'

print, "Month "+month+": Ref: "+fname_ref+"  BA: "+fname_ba

close,11


;; write data to tmp file
ido = ncdf_open(ofiletmp, /write)

;; read refractivity ascii file
openr,11,fname_ref

readf, 11, Ac0, Ac1, Bc1, Aa0, Aa1, Ba1, Ab0, Ab1, Bb1, Ad0, Ad1, Bd1   ;;, Ae0, Ae1, Be1

close,11

ncdf_varput, ido, v_nAc0, Ac0
ncdf_varput, ido, v_nAc1, Ac1
ncdf_varput, ido, v_nBc1, Bc1
ncdf_varput, ido, v_nAa0, Aa0
ncdf_varput, ido, v_nAa1, Aa1
ncdf_varput, ido, v_nBa1, Ba1
ncdf_varput, ido, v_nAb0, Ab0
ncdf_varput, ido, v_nAb1, Ab1
ncdf_varput, ido, v_nBb1, Bb1
ncdf_varput, ido, v_nAd0, Ad0
ncdf_varput, ido, v_nAd1, Ad1
ncdf_varput, ido, v_nBd1, Bd1

;; read bending angle ascii file
openr,11,fname_ba

readf, 11, Ac0, Ac1, Bc1, Aa0, Aa1, Ba1, Ab0, Ab1, Bb1, Ad0, Ad1, Bd1, Ae0, Ae1, Be1

close,11

ncdf_varput, ido, v_bAc0, Ac0
ncdf_varput, ido, v_bAc1, Ac1
ncdf_varput, ido, v_bBc1, Bc1
ncdf_varput, ido, v_bAa0, Aa0
ncdf_varput, ido, v_bAa1, Aa1
ncdf_varput, ido, v_bBa1, Ba1
ncdf_varput, ido, v_bAb0, Ab0
ncdf_varput, ido, v_bAb1, Ab1
ncdf_varput, ido, v_bBb1, Bb1
ncdf_varput, ido, v_bAd0, Ad0
ncdf_varput, ido, v_bAd1, Ad1
ncdf_varput, ido, v_bBd1, Bd1
ncdf_varput, ido, v_bAe0, Ae0
ncdf_varput, ido, v_bAe1, Ae1
ncdf_varput, ido, v_bBe1, Be1

;; close output netCDF file
ncdf_close, ido

;; concatenate to output file
if(imonth eq 1)then begin
    spawn,'cp '+ofiletmp+' '+ofile
endif else begin
    spawn,'ncrcat -h -O '+ofile+' '+ofiletmp+' -o '+ofile
endelse


endfor

print,"Created output file ",ofile

spawn, 'rm -rf '+ofiletmp

end
