# Makefile.am for ropp_fm/build
# =============================

# 1. The ropp_fm library
# ----------------------

# 1.1 The common part

COMMON = ../common/ropp_fm_types.f90             \
         ../common/ropp_fm_constants.f90         \
	 ../common/ropp_fm.f90                   \
	 ../common/ropp_fm_free.f90              \
         ../common/ropp_fm_interpol.f90          \
         ../common/ropp_fm_interpol_ad.f90       \
         ../common/ropp_fm_interpol_log.f90      \
         ../common/ropp_fm_interpol_log_ad.f90   \
         ../common/ropp_fm_interpol_log_tl.f90   \
         ../common/ropp_fm_interpol_tl.f90       \
	 ../common/ropp_fm_compress.f90          \
	 ../common/ropp_fm_compress_ad.f90       \
	 ../common/ropp_fm_compress_tl.f90       \
	 ../common/ropp_fm_compress_2d.f90       \
	 ../common/ropp_fm_compress_2d_ad.f90    \
	 ../common/ropp_fm_compress_2d_tl.f90    \
	 ../common/ropp_fm_version.f90


if HAVE_ROPP_IO
COMMON += ../common/ropp_fm_copy.f90             \
         ../common/ropp_fm_roprof2state.f90      \
         ../common/ropp_fm_state2roprof.f90      \
         ../common/ropp_fm_state2state.f90       \
         ../common/ropp_fm_roprof2obs.f90        \
         ../common/ropp_fm_obs2roprof.f90        \
         ../common/ropp_fm_obs2obs.f90           \
	 ../common/ropp_fm_set_units.f90
endif

# 1.1a The ECMWF-specific part

MODEL_ECMWF = 	../model_ecmwf/ropp_fm_state2state_ecmwf.f90    \
	        ../model_ecmwf/ropp_fm_state2state_ecmwf_tl.f90 \
                ../model_ecmwf/ropp_fm_state2state_ecmwf_ad.f90

# 1.1b The UKMO-specific part

MODEL_METO = 	../model_meto/ropp_fm_state2state_meto.f90     \
	        ../model_meto/ropp_fm_state2state_meto_tl.f90  \
                ../model_meto/ropp_fm_state2state_meto_ad.f90

# 1.2 The 1D refractivity forward model

REFRAC1D = ../refrac_1d/ropp_fm_refrac_1d.f90      \
           ../refrac_1d/ropp_fm_refrac_1d_ad.f90   \
           ../refrac_1d/ropp_fm_refrac_1d_tl.f90   \
	   ../refrac_1d/ropp_fm_refrac_1d_grad.f90

# 1.3 The 1D bending angle forward model

BANGLE1D = ../bangle_1d/ropp_fm_abel.f90         \
           ../bangle_1d/ropp_fm_abel_ad.f90      \
           ../bangle_1d/ropp_fm_abel_tl.f90      \
           ../bangle_1d/ropp_fm_bangle_1d_tl.f90  \
	   ../bangle_1d/ropp_fm_bangle_1d.f90     \
           ../bangle_1d/ropp_fm_bangle_1d_ad.f90  \
           ../bangle_1d/ropp_fm_bangle_1d_grad.f90

# 1.4 The 2D bending angle forward model

BANGLE2D = ../bangle_2d/ropp_fm_alpha2drk.f90     \
           ../bangle_2d/ropp_fm_alpha2drk_tl.f90  \
	   ../bangle_2d/ropp_fm_gpspderivs.f90    \
	   ../bangle_2d/ropp_fm_gpspderivs_tl.f90 \
           ../bangle_2d/ropp_fm_bangle_2d_tl.f90  \
	   ../bangle_2d/ropp_fm_bangle_2d.f90     \
           ../bangle_2d/ropp_fm_alpha2drk_ad.f90  \
	   ../bangle_2d/ropp_fm_gpspderivs_ad.f90 \
           ../bangle_2d/ropp_fm_bangle_2d_ad.f90  \
           ../bangle_2d/ropp_fm_2d_plane.f90

# 1.5 The matrix data types

MATRIX = ../math/matrix/matrix_types.f90

lib_LIBRARIES = libropp_fm.a

libropp_fm_a_SOURCES = $(MATRIX) $(COMMON) ${MODEL_ECMWF} ${MODEL_METO} $(REFRAC1D) $(BANGLE1D) $(BANGLE2D)

# 2. Module files and files to be cleaned
# ---------------------------------------

if CM_FC_UC_MODNAMES
MODULES = ROPP_FM_TYPES.mod ROPP_FM_CONSTANTS.mod ROPP_FM.mod MATRIX_TYPES.mod
if HAVE_ROPP_IO
MODULES += ROPP_FM_COPY.mod
endif
else
MODULES = ropp_fm_types.mod ropp_fm_constants.mod ropp_fm.mod matrix_types.mod
if HAVE_ROPP_IO
MODULES += ropp_fm_copy.mod
endif
endif

nodist_include_HEADERS = $(MODULES)

CLEANFILES      = $(MODULES) *.stb

# 3. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@


# 4. Dependencies
# ---------------
