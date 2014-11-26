PRO close_jpgfile, jpgfile, resolution, thisDevice

;; closes a jpgfile output without going through a window. Follows
;; Fannings approach. jpgfile is opened with open_jpgfile.
;;
;; INPUT:
;;      jpgfile        output jpg file name
;;      resolution     resolution of output, set in open_jpgfile.
;;      thisdevice     output device before jpg file output. Set back here.

;; catch all output
snapshot = TVRD()
TVLCT, r, g, b, /Get
Device, Z_Buffer=1
Set_Plot, thisDevice

;; put into image
image24 = BytArr(3, resolution[0], resolution[1])
image24[0,*,*] = r[snapshot]
image24[1,*,*] = g[snapshot]
image24[2,*,*] = b[snapshot]

;; write the file
Write_JPEG, jpgfile, image24, True=1, Quality=75

end
