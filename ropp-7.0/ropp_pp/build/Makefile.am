# Makefile.am for ropp_pp/build
# ================================

# 1. The ropp_pp library
# -------------------------

# 1.1 The model part
# ------------------

MSIS = ../msis_bangle/ropp_pp_msis.f90              \
       ../msis_bangle/ropp_pp_refrac_msis.f90       \
       ../msis_bangle/ropp_pp_bangle_msis.f90
if HAVE_NETCDF
MSIS += ../msis_bangle/ropp_pp_read_msis.f90
endif

# 1.2 The common part
# -------------------

COMMON = ../common/ropp_pp_types.f90                     \
         ../common/ropp_pp_constants.f90                 \
         ../common/ropp_pp_utils.f90	                 \
         ../common/ropp_pp_spline.f90	                 \
	 ../common/ropp_pp.f90 	                         \
         ../common/ropp_pp_interpol.f90                  \
	 ../common/ropp_pp_interpol_log.f90              \
	 ../common/ropp_pp_interpolate_trajectory.f90    \
	 ../common/ropp_pp_monotonous.f90                \
	 ../common/ropp_pp_read_config.f90               \
	 ../common/ropp_pp_satellite_velocities.f90      \
	 ../common/ropp_pp_tdry.f90                      \
	 ../common/ropp_pp_fft.f90                       \
	 ../common/ropp_pp_fourier_filter.f90            \
	 ../common/ropp_pp_filter.f90                    \
	 ../common/ropp_pp_sliding_polynomial.f90        \
	 ../common/ropp_pp_version.f90
if HAVE_NETCDF
COMMON += ../common/ropp_pp_refrac_bg.f90
endif
if HAVE_ROPP_IO
COMMON += ../common/ropp_pp_copy.f90 ../common/ropp_pp_diag2roprof.f90
endif

# 1.3 The bending angle part
# --------------------------

BANGLE = ../bangle/ropp_pp_bending_angle_go.f90  \
	 ../bangle/ropp_pp_geometric_optics.f90  \
	 ../bangle/ropp_pp_geometric_optics_adj.f90  \
	 ../bangle/ropp_pp_bending_angle_wo.f90      \
	 ../bangle/ropp_pp_dct.f90

# 1.4 The preprocessing part
# --------------------------

PREPROC = ../preprocess/ropp_pp_openloop.f90                \
	  ../preprocess/ropp_pp_modelphase.f90              \
	  ../preprocess/ropp_pp_bangle2phase.f90            \
	  ../preprocess/ropp_pp_impact2doppler.f90          \
	  ../preprocess/ropp_pp_amplitude_go.f90            \
	  ../preprocess/ropp_pp_radiooptic_analysis.f90     \
	  ../preprocess/ropp_pp_radioholographic_filter.f90 \
	  ../preprocess/ropp_pp_spectra.f90                 \
	  ../preprocess/ropp_pp_correct_L2.f90

if HAVE_ROPP_IO
PREPROC += ../preprocess/ropp_pp_preproc.f90            \
	   ../preprocess/ropp_pp_preprocess.f90         \
	   ../preprocess/ropp_pp_preprocess_cosmic.f90  \
	   ../preprocess/ropp_pp_preprocess_grasrs.f90  \
           ../preprocess/ropp_pp_cutoff.f90             \
           ../preprocess/ropp_pp_cutoff_amplitude.f90   \
	   ../preprocess/ropp_pp_set_coordinates.f90
endif

# 1.5 The Abel trasform part
# --------------------------

ABEL =   ../abel_invert/ropp_pp_invert_lin.f90      \
	 ../abel_invert/ropp_pp_invert_exp.f90      \
	 ../abel_invert/ropp_pp_abel_lin.f90        \
	 ../abel_invert/ropp_pp_abel_exp.f90

# 1.6 The ionospheric correction part
# -----------------------------------

ICORR =  ../icorr/ropp_pp_linear_combination.f90      \
	 ../icorr/ropp_pp_bg_refraction.f90           \
	 ../icorr/ropp_pp_model_refraction.f90        \
	 ../icorr/ropp_pp_search_model_refraction.f90 \
	 ../icorr/ropp_pp_ionospheric_correction.f90  \
	 ../icorr/ropp_pp_merge_profile.f90           \
         ../icorr/ropp_pp_smooth_profile.f90          \
	 ../icorr/ropp_pp_fit_model_refraction.f90    \
	 ../icorr/ropp_pp_invert_refraction.f90

# 1.7 The tropopause height part
# ------------------------------

TPH =    ../tph/ropp_pp_tph_bangle.f90   \
         ../tph/ropp_pp_tph_refrac.f90   \
         ../tph/ropp_pp_tph_tdry.f90     \
         ../tph/ropp_pp_tph_temp.f90     \
         ../tph/ropp_pp_cov_transform.f90

# 1.8 The pp part
# ------------------

lib_LIBRARIES = libropp_pp.a

libropp_pp_a_SOURCES =  $(MSIS) $(COMMON) $(PREPROC) $(BANGLE) $(ABEL) $(ICORR) $(TPH)


# 2. The PP executables
# ------------------------

LDADD = libropp_pp.a

# 3. Module files and files to be cleaned
# ---------------------------------------

if CM_FC_UC_MODNAMES
MODULES = ROPP_PP_TYPES.mod ROPP_PP_CONSTANTS.mod \
          ROPP_PP_UTILS.mod ROPP_PP_SPLINE.mod ROPP_PP.mod \
          ROPP_PP_MSIS_TYPES.mod ROPP_PP_MSIS.mod
if HAVE_ROPP_IO
MODULES += ROPP_PP_COPY.mod ROPP_PP_PREPROC.mod
endif
else
MODULES = ropp_pp_types.mod ropp_pp_constants.mod \
          ropp_pp_utils.mod ropp_pp_spline.mod ropp_pp.mod \
          ropp_pp_msis_types.mod ropp_pp_msis.mod
if HAVE_ROPP_IO
MODULES += ropp_pp_copy.mod ropp_pp_preproc.mod
endif
endif

nodist_include_HEADERS = $(MODULES)

CLEANFILES      = $(MODULES) *.stb

# 4. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@

# 5. Dependencies
# ---------------

