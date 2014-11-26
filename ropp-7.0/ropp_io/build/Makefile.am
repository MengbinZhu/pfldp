# Makefile.am for ropp_io/src
# ===========================

# 1. The ropp_io library
# ----------------------

# 1.1 The common part
# -------------------

COMMON = ../common/ropp_io_version.f90

# 1.2 The netcdf part
# -------------------------------

NCDF = ../ncdf/ncdf.f90 ../ncdf/is_netcdf.f90             \
  ../ncdf/ncdf_isatt.f90                                                     \
  ../ncdf/ncdf_isvar.f90 ../ncdf/ncdf_defdim.f90                             \
  ../ncdf/ncdf_defmode.f90 ../ncdf/ncdf_datmode.f90                          \
  ../ncdf/ncdf_create.f90 ../ncdf/ncdf_open.f90 ../ncdf/ncdf_close.f90       \
  ../ncdf/ncdf_getshape.f90 ../ncdf/ncdf_getsize.f90 ../ncdf/ncdf_renvar.f90 \
  ../ncdf/ncdf_getnrec.f90 ../ncdf/ncdf_sync.f90                             \
  ../ncdf/ncdf_getvar.f90 ../ncdf/ncdf_getvar_alloc.f90                      \
  ../ncdf/ncdf_getatt.f90 ../ncdf/ncdf_getatt_alloc.f90                      \
  ../ncdf/ncdf_defvar.f90 ../ncdf/ncdf_putatt.f90 ../ncdf/ncdf_putvar.f90    \
  ../ncdf/ncdf_save.f90 ../ncdf/ncdf_error_handler.f90                       \
  ../ncdf/ncdf_date_and_time.f90 

if HAVE_HDF5
  NCDF += ../ncdf/ncdf_getgroupid_n4.f90
else
  NCDF += ../ncdf/ncdf_getgroupid_n3.f90
endif

# 1.3 The ROPP part
# -----------------

ROPP = ../ropp/ropp_io_types.f90          \
       ../ropp/ropp_io.f90                \
       ../ropp/ropp_io_addvar.f90         \
       ../ropp/ropp_io_ascend.f90         \
       ../ropp/ropp_io_assign.f90         \
       ../ropp/ropp_io_init.f90           \
       ../ropp/ropp_io_free.f90           \
       ../ropp/ropp_io_isinrange.f90      \
       ../ropp/ropp_io_nrec.f90           \
       ../ropp/ropp_io_occid.f90          \
       ../ropp/ropp_io_rangecheck.f90     \
       ../ropp/ropp_io_read.f90           \
       ../ropp/ropp_io_read_ncdf_get.f90  \
       ../ropp/ropp_io_shrink.f90         \
       ../ropp/ropp_io_vlist_print.f90    \
       ../ropp/ropp_io_vlist_size.f90     \
       ../ropp/ropp_io_vlist_read.f90     \
       ../ropp/ropp_io_write.f90          \
       ../ropp/ropp_io_write_ncdf_def.f90 \
       ../ropp/ropp_io_write_ncdf_put.f90

# 1.4 The thinner part
# --------------------

THIN = ../thin/ropp_io_thin.f90           \
       ../thin/ropp_io_thin_fixed.f90     \
       ../thin/ropp_io_thin_select.f90    \
       ../thin/ropp_io_thin_sg.f90        \
       ../thin/ropp_io_thin_skip.f90

# 1.5 The BUFR part
# -------------------------------

BUFR = ../bufr/bufrutils.f90     \
       ../bufr/convertcodes.f90  \
       ../bufr/gtshdrs.f90       \
       ../bufr/bufr2ropp_mod.f90 \
       ../bufr/ropp2bufr_mod.f90

lib_LIBRARIES = libropp_io.a

libropp_io_a_SOURCES =  $(COMMON) $(NCDF) $(ROPP) $(THIN) ${BUFR}

# 2. Module files and files to be cleaned
# ---------------------------------------

if CM_FC_UC_MODNAMES
  MODULES = ROPP_IO_TYPES.mod ROPP_IO.mod NCDF.mod \
            BUFRUTILS.mod GTSHDRS.mod \
	    BUFR2ROPP.mod ROPP2BUFR.mod
else
  MODULES = ropp_io_types.mod ropp_io.mod ncdf.mod \
            bufrutils.mod gtshdrs.mod \
            bufr2ropp.mod ropp2bufr.mod
endif

nodist_include_HEADERS = $(MODULES)

CLEANFILES = $(MODULES) *.stb

# 3. Rules for compilation
# ------------------------

# 4. Dependencies
# ---------------

