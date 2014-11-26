# Makefile.am for ropp_1dvar/build
# ================================

# 1. The ropp_1dvar library
# -------------------------

# 1.1 The common part
# -------------------

COMMON = ../common/ropp_1dvar_types.f90       \
	 ../common/ropp_1dvar.f90             \
	 ../common/ropp_1dvar_minropp.f90     \
	 ../common/ropp_1dvar_solve.f90       \
	 ../common/ropp_1dvar_read_config.f90 \
	 ../common/ropp_1dvar_cost.f90        \
	 ../common/ropp_1dvar_diagnostics.f90 \
	 ../common/ropp_1dvar_levmarq.f90     \
	 ../common/ropp_1dvar_version.f90

if HAVE_ROPP_IO
COMMON += ../common/ropp_1dvar_copy.f90         \
	  ../common/ropp_1dvar_diag2roprof.f90  \
	  ../common/ropp_1dvar_covar_bg.f90     \
	  ../common/ropp_1dvar_covar_bangle.f90 \
	  ../common/ropp_1dvar_covar_refrac.f90
endif


# 1.2 The mathematical part
# -------------------------

if HAVE_MATRIX_TYPES
MATRIX =
else
MATRIX = ../math/matrix/matrix_types.f90
endif
MATRIX += ../math/matrix/matrix.f90                	\
          ../math/matrix/matrix_full2pp.f90  		\
	  ../math/matrix/matrix_full2pp_alloc.f90  	\
	  ../math/matrix/matrix_pp2full.f90		\
	  ../math/matrix/matrix_pp2full_alloc.f90  	\
	  ../math/matrix/matrix_pp2full_subset.f90  	\
	  ../math/matrix/matrix_full2bm.f90		\
	  ../math/matrix/matrix_full2bm_alloc.f90  	\
	  ../math/matrix/matrix_bm2full.f90      	\
	  ../math/matrix/matrix_bm2full_alloc.f90  	\
	  ../math/matrix/matrix_solve.f90        	\
	  ../math/matrix/matrix_invert.f90         	\
	  ../math/matrix/matrix_assign.f90       	\
	  ../math/matrix/matrix_delete.f90         	\
	  ../math/matrix/matrix_operators.f90    	\
	  ../math/matrix/matrix_sqrt.f90           	\
	  ../math/matrix/matrix_toast.f90        	\
	  ../math/matrix/matrix_svd.f90

PRECON = ../math/precon/ropp_control2state.f90 		\
	 ../math/precon/ropp_state2control.f90 		\
	 ../math/precon/ropp_control2state_ad.f90

# 1.3 The QC part
# ---------------

QC = ../qc/ropp_qc.f90       \
     ../qc/ropp_qc_genqc.f90 \
     ../qc/ropp_qc_omb.f90   \
     ../qc/ropp_qc_bgqc.f90  \
     ../qc/ropp_qc_pge.f90   \
     ../qc/ropp_qc_cutoff.f90

# 1.4 The 1DVar part
# ------------------

lib_LIBRARIES = libropp_1dvar.a

libropp_1dvar_a_SOURCES = $(MATRIX) $(PRECON) $(COMMON) $(QC)

# 2. Module files and files to be cleaned
# ---------------------------------------

if CM_FC_UC_MODNAMES
if HAVE_MATRIX_TYPES
MODULES = ROPP_1DVAR_TYPES.mod ROPP_1DVAR.mod ROPP_QC.mod MATRIX.mod
else
MODULES = ROPP_1DVAR_TYPES.mod ROPP_1DVAR.mod ROPP_QC.mod  \
	 MATRIX.mod MATRIX_TYPES.mod
endif
if HAVE_ROPP_IO
MODULES += ROPP_1DVAR_COPY.mod
endif
else
if HAVE_MATRIX_TYPES
MODULES = ropp_1dvar_types.mod ropp_1dvar.mod ropp_qc.mod matrix.mod
else
MODULES = ropp_1dvar_types.mod ropp_1dvar.mod ropp_qc.mod \
          matrix.mod matrix_types.mod
endif
if HAVE_ROPP_IO
MODULES += ropp_1dvar_copy.mod
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

