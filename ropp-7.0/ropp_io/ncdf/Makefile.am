## Process the file with automake to create Makefile.in.

SUFFIXES = .m4

# 1. Extra files for the distribution
# -----------------------------------

EXTRA_DIST = ncdf.f90 is_netcdf.f90 ncdf_create.f90        \
	ncdf_open.f90 ncdf_close.f90                       \
	ncdf_getshape.f90 ncdf_getsize.f90 ncdf_renvar.f90 \
	ncdf_getvar.f90 ncdf_getvar_alloc.f90              \
	ncdf_getatt.f90 ncdf_getatt_alloc.f90              \
	ncdf_defvar.f90 ncdf_putvar.f90 ncdf_putatt.f90    \
	ncdf_defdim.f90 ncdf_isatt.f90 ncdf_isvar.f90      \
	ncdf_save.f90 ncdf_getnrec.f90 ncdf_sync.f90       \
	ncdf_error_handler.f90 ncdf_date_and_time.f90      \
	ncdf_defmode.f90 ncdf_datmode.f90                  \
	ncdf_getgroupid_n3.f90 ncdf_getgroupid_n4.f90      \
	ncdf.m4 ncdf_aux.m4 ncdf_save.m4                   \
	ncdf_getatt.m4 ncdf_getatt_alloc.m4 ncdf_putatt.m4 \
	ncdf_putvar.m4 ncdf_getvar.m4 ncdf_getvar_alloc.m4 \
	ncdf_putgetvar_sitfc.m4 ncdf_putgetvar_scode.m4    \
	ncdf_putgetvar_aitfc.m4 ncdf_putgetvar_acode.m4    \
	ncdf_putgetvar_schar.m4                            \
	ncdf_putgetatt_sitfc.m4 ncdf_putgetatt_scode.m4    \
	ncdf_putgetatt_aitfc.m4 ncdf_putgetatt_acode.m4    \
	ncdf_putgetatt_schar.m4                            \
	ncdf_save_sitfc.m4 ncdf_save_scode.m4              \
	ncdf_save_aitfc.m4 ncdf_save_acode.m4

# 2. Files to be cleaned
# ----------------------

# CLEANFILES =

# 3. Rules for compilation
# ------------------------

if HAVE_HDF5
all: ncdf.f90 ncdf_putatt.f90 ncdf_getatt.f90 ncdf_getatt_alloc.f90 \
              ncdf_putvar.f90 ncdf_getvar.f90 ncdf_getvar_alloc.f90 \
              ncdf_save.f90 ncdf_getgroupid_n4.f90
else
all: ncdf.f90 ncdf_putatt.f90 ncdf_getatt.f90 ncdf_getatt_alloc.f90 \
              ncdf_putvar.f90 ncdf_getvar.f90 ncdf_getvar_alloc.f90 \
              ncdf_save.f90 ncdf_getgroupid_n3.f90
endif

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@

# Dependencies
# ------------

ncdf.f90              :: ncdf.m4 ncdf_putgetatt_sitfc.m4 ncdf_putgetatt_aitfc.m4 \
                                 ncdf_putgetvar_sitfc.m4 ncdf_putgetvar_aitfc.m4 \
                                 ncdf_save_sitfc.m4 ncdf_save_aitfc.m4
ncdf_putatt.f90       :: ncdf_putatt.m4 ncdf_aux.m4 \
                                 ncdf_putgetatt_schar.m4 \
                                 ncdf_putgetatt_scode.m4 ncdf_putgetatt_acode.m4
ncdf_getatt.f90       :: ncdf_getatt.m4 ncdf_aux.m4 \
                                 ncdf_putgetatt_schar.m4 \
                                 ncdf_putgetatt_scode.m4 ncdf_putgetatt_acode.m4
ncdf_getatt_alloc.f90 :: ncdf_getatt_alloc.m4 ncdf_aux.m4 \
                                 ncdf_putgetatt_schar.m4 \
                                 ncdf_putgetatt_scode.m4 ncdf_putgetatt_acode.m4
ncdf_putvar.f90       :: ncdf_putvar.m4 ncdf_aux.m4 \
                                 ncdf_putgetvar_schar.m4 \
                                 ncdf_putgetvar_scode.m4 ncdf_putgetvar_acode.m4
ncdf_getvar.f90       :: ncdf_getvar.m4 ncdf_aux.m4 \
                                 ncdf_putgetvar_schar.m4 \
                                 ncdf_putgetvar_scode.m4 ncdf_putgetvar_acode.m4
ncdf_getvar_alloc.f90 :: ncdf_getvar_alloc.m4 ncdf_aux.m4 \
                                 ncdf_putgetvar_schar.m4 \
                                 ncdf_putgetvar_scode.m4 ncdf_putgetvar_acode.m4
ncdf_save.f90         :: ncdf_save.m4 ncdf_aux.m4 \
                                 ncdf_save_scode.m4 ncdf_save_acode.m4
