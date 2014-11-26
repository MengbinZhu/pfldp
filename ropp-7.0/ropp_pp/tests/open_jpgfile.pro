PRO open_jpgfile, jpgfile, resolution, thisdevice, standard=standard

;; folling Fanning opens output into jpg file without window 
;; plot. should be used together with close_jpgfile
;;
;; INPUT:
;;      jpgfile        output jpg file name
;;
;; OPTIONAL INPUT:
;;      These parameters are set to default values and must be used in the 
;;      call since they are passed on to close_jpgfile:
;;
;;      resolution     resolution of output, default [1000,500]. 
;;
;; OUTPUT:
;;      thisdevice     current output device, set back in close_jpgfile
;;
;; KEYWORDS:
;;      standard       background color, default is resetting to white. 
;;                     setting /standard will use the default settings.
;;                     Note that the default will also affect later plots.


;; save output device and set to Z
thisDevice = !D.Name
Set_Plot, 'Z', /COPY

;; make background white
dummy         = !P.color
!P.color      = !P.background
!P.background = dummy

;; set resolution and erase any remains in Z buffer
if n_elements(resolution) eq 0 then resolution = [1000,500]
Device, Set_Resolution=resolution, Z_Buffer=0
Erase

end
