# Makefile.am for ropp_fm/data
# ============================

# 1. 1D forward model for refractivity
# ------------------------------------

EXTRA_DIST = README                       		\
	bgr20090401_000329_M02_2030337800_N0007_XXXX.nc \
	bgr20090401_000601_M02_1200337800_N0007_XXXX.nc	\
	bgr20090401_000630_M02_2180337800_N0007_XXXX.nc \
	bgr20090401_000758_M02_2210337801_N0007_XXXX.nc \
	bgr20090401_000849_M02_2060337801_N0007_XXXX.nc \
	bgr20090401_multi.nc                            \
	FASCOD_Scenarios.nc                    		\
	FASCOD_non_ideal.nc                    		\
	ecmwf_20020901000213_chmp_u999_meto.nc 		\
	ecmwf_20020901002459_chmp_u999_meto.nc 		\
	ecmwf_20020901003358_chmp_u999_meto.nc		\
	ecmwf_20020901004032_chmp_u999_meto.nc 		\
	ecmwf_20020901004925_chmp_u999_meto.nc 		\
	ecmwf_20020901005119_chmp_u999_meto.nc 		\
	ecmwf_20020901005632_chmp_u999_meto.nc 		\
	ecmwf_multi_20020901_1h_chmp_meto.nc            \
	ECMWF_2D_DATA.nc                                \
	ECMWF_2D_DATA.DAT                               \
	ECMWF_2D_DATA_COMP.DAT                          \
	ECMWF_10_OCCS.nc


# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
