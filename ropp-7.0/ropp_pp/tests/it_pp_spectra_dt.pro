;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro it_pp_spectra_dt, op_format=op_format

;; plot localised spatial spectra as function of doppler frequency and
;; time
;;
;; INPUT:
;;
;; KEYWORDS:
;;    op_format - select output image file type ('X', 'EPS', 'PNG', 'GIF', 'JPG')
;;    'EPS' is the default.
;; OUTPUT:
;;
;; SEE ALSO:
;;    it_pp_spectra_ep  (plot spectra impact-bending) 
;;

if (n_elements(op_format) ne 1) then begin
  plotdev = 'EPS'
endif else begin
  plotdev = op_format
endelse

;; Define input filenames

file_RO1 = "ROanalysis_dt_L1.dat"
file_RO2 = "ROanalysis_dt_L2.dat"

;; Read file
line = strarr(1)

CLOSE, 11
OPENR, 11, file_RO1
;; READ HEADER
readf, 11, line, format='(A)'
readf, 11, line, format='(A)'
;print, line

nii = FIX(STRMID(line, 7, 5))
ni = FIX(nii(0))
njj = LONG(STRMID(line, 15, 6))
nj = LONG(njj(0))
ocd = FIX(STRMID(line, 33, 2))
if(ocd gt 0)then occ='rising'
if(ocd lt 0)then occ='setting'
;print, 'File ', file_RO1, ni, nj, ' data points ('+occ+')'

x1 = fltarr(ni)
y1 = fltarr(nj)
U1 = fltarr(ni,nj)

;; READ DATA
for j=0,nj-1 do begin
for i=0,ni-1 do begin
readf, 11, var1, var2, var3
x1(i) = var1
y1(j) = var2
U1(i,j) = var3
endfor
endfor
CLOSE,11

OPENR, 11, file_RO2
;; READ HEADER
readf, 11, line, format='(A)'
readf, 11, line, format='(A)'
;print, line

nii = FIX(STRMID(line, 7, 5))
ni = FIX(nii(0))
njj = LONG(STRMID(line, 15, 6))
nj = LONG(njj(0))
ocd = FIX(STRMID(line, 33, 2))

if(ocd gt 0)then occ='rising'
if(ocd lt 0)then occ='setting'
;print, 'File ', file_RO2, ni, nj, ' data points ('+occ+')'

x2 = fltarr(ni)
y2 = fltarr(nj)
U2 = fltarr(ni,nj)

;; READ DATA
for j=0,nj-1 do begin
for i=0,ni-1 do begin
readf, 11, var1, var2, var3
x2(i) = var1
y2(j) = var2
U2(i,j) = var3
endfor
endfor
CLOSE,11

;; SETUP PLOT
xsiz=700
ysiz=700
CASE STRUPCASE(plotdev) OF
  'PNG': SET_PLOT,'Z',  /Copy
  'GIF': SET_PLOT,'Z',  /Copy
  'JPG': SET_PLOT,'Z',  /Copy
  'EPS': SET_PLOT,'PS', /Copy
  'X'  : SET_PLOT,'X'
ENDCASE

;; 'X' WINDOW OUTPUT
IF !D.Name EQ "X" THEN BEGIN
    WINDOW,0,Xsize=xsiz,Ysize=ysiz,Retain=2
    chrsiz  = 0.70
    xlabsiz = 0.50
ENDIF

;; EPS OUTPUT
IF STRUPCASE(plotdev) EQ "EPS" THEN BEGIN
    opfile = "ropp_pp_spectra_dt.eps"
    PRINT, "Plotting to "+opfile+"..."
    DEVICE, Xoffset=0.0, Yoffset=0.0, Xsize=xsiz/30.,   Ysize=ysiz/30.,  $
            Filename=opfile, /Close_file, /Encapsulated, /Color, /Helvetica
    chrsiz  = 0.75
    xlabsiz = 0.75
ENDIF ELSE IF STRUPCASE(plotdev) EQ "PNG" THEN BEGIN
    opfile = "ropp_pp_spectra_dt.png"
    PRINT, "Plotting to "+opfile+"..."
    dummy         = !P.color
    !P.color      = !P.background
    !P.background = dummy
    DEVICE, Set_Resolution=[xsiz,ysiz], Set_Colors=256, Z_Buffer=0
    chrsiz  = 0.70
    xlabsiz = 0.50
ENDIF ELSE IF STRUPCASE(plotdev) EQ "GIF" THEN BEGIN
    opfile = "ropp_pp_spectra_dt.gif"
    PRINT, "Plotting to "+opfile+"..."
    dummy         = !P.color
    !P.color      = !P.background
    !P.background = dummy
    DEVICE, Set_Resolution=[xsiz,ysiz], Set_Colors=256, Z_Buffer=0
    chrsiz  = 0.70
    xlabsiz = 0.50
ENDIF ELSE IF STRUPCASE(plotdev) EQ "JPG" THEN BEGIN
    opfile = "ropp_pp_spectra_dt.jpg"
    PRINT, "Plotting to "+opfile+"..."
    OPEN_JPGFILE, opfile, [xsiz,ysiz], thisDevice
END

;; PLOT ATTRIBUTES
!P.multi=[0,2,1]
!P.charsize=1.2
!P.charthick=2
!P.thick=2
LOADCT,9

;; Set plot axis ranges
ymin = min(y1)
ymax = max(y1)
xmin = min(x1)
xmax = max(x1)

;; sampling (limit image file size)
if(STRUPCASE(plotdev) EQ "PNG" OR STRUPCASE(plotdev) EQ "EPS")THEN BEGIN
if(ocd gt 0)then ymin = 0.3*ymax
if(ocd lt 0)then ymin = 0.3*ymax
idx = findgen(n_elements(x1)/2)*2
idy = findgen(n_elements(y1)/2)*2
x1 = x1(idx)
y1 = y1(idy)
U1 = U1(idx,*)
U1 = U1(*,idy)
x2 = x2(idx)
y2 = y2(idy)
U2 = U2(idx,*)
U2 = U2(*,idy)
endif

!y.title="Time since start occultation (s)"
!x.title="Frequency offset (Hz)"
!x.range=[xmin,xmax]
!y.range=[ymin,ymax]                     ;; RISING OCC
if(ocd lt 0)then !y.range=[ymax,ymin]    ;; SETTING OCC

;; PLOT DATA
contour, U1, x1, y1, /fill, nlevels=12, xstyle=1, ystyle=1, title="L1"
contour, U2, x2, y2, /fill, nlevels=12, xstyle=1, ystyle=1, title="L2"

;; CLOSE PLOTTING DEVICE
CASE !D.Name OF
    "PS" : BEGIN
             DEVICE, /Close
           END
    "Z"  : BEGIN
             IF STRUPCASE(plotdev) EQ "GIF" THEN  BEGIN
               WRITE_GIF, opfile, TVRD(), R,G,B
             END ELSE IF STRUPCASE(plotdev) EQ "PNG" THEN BEGIN
               WRITE_PNG, opfile, TVRD(), R,G,B
             END ELSE IF STRUPCASE(plotdev) EQ "JPG" THEN BEGIN
               CLOSE_JPGFILE, opfile, [xsiz,ysiz], thisDevice
             ENDIF
             DEVICE, /Close
           END
    "X"  : BEGIN
           END
  ENDCASE

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
