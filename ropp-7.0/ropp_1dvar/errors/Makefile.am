# $Id: $
#
# Makefile.am for ropp_1dvar/errors
# =================================

# 1. Subdirectories
# -----------------

SUBDIRS = 

# 2. ropp_1dvar error programs
# ----------------------------

bin_PROGRAMS = ropp_1dvar_add_refrac_error \
               ropp_1dvar_add_bangle_error \
               ropp_1dvar_add_bgr_error

if HAVE_ROPP_IO
ropp_1dvar_add_refrac_error_SOURCES = ropp_1dvar_add_refrac_error.f90
ropp_1dvar_add_bangle_error_SOURCES = ropp_1dvar_add_bangle_error.f90
ropp_1dvar_add_bgr_error_SOURCES = ropp_1dvar_add_bgr_error.f90
man_MANS = ropp_1dvar_add_bgr_error.1 ropp_1dvar_add_refrac_error.1 ropp_1dvar_add_bangle_error.1
endif

# 3. Libraries to link in
# -----------------------

LDADD = ../build/libropp_1dvar.a

# 4. Files to be cleaned
# ----------------------

MODULES = 

CLEANFILES     = *.stb $(MODULES)

# 5. Sample background, errcov & observation & data
# --------------------------------------------------

EXTRA_DIST = README                           \
             ropp_bg_ecmwf_error_corr_L60.nc  \
             ropp_bg_ecmwf_error_corr_L91.nc  \
             ropp_bg_meto_error_corr_L50.nc   \
             ropp_bg_meto_error_corr_L70.nc   \
             ropp_ob_bangle_error_corr.nc     \
             ropp_ob_refrac_error_corr.nc     \
             ropp_1dvar_add_refrac_error.f90  \
             ropp_1dvar_add_bangle_error.f90  \
             ropp_1dvar_add_bgr_error.f90     \
             ropp_1dvar_add_bgr_error.1       \
             ropp_1dvar_add_refrac_error.1    \
             ropp_1dvar_add_bangle_error.1


# 6. Dependencies
# ---------------

ropp_1dvar_add_refrac_error.$(OBJEXT) : ropp_1dvar_add_refrac_error.f90
ropp_1dvar_add_bangle_error.$(OBJEXT) : ropp_1dvar_add_bangle_error.f90
ropp_1dvar_add_bgr_error.$(OBJEXT) : ropp_1dvar_add_bgr_error.f90



