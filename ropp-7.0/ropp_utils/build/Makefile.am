# Makefile.am for ropp_utils/build
# ================================

# 1. The ropp_utils library
# -------------------------

# 1.1 Compiler specific components
# --------------------------------

if FC_IS_NAG
COMP = ../compilers/nag_interfaces.f90
endif

# 1.2 Typesizes (this must be built first)
# ----------------------------------------

TPSZ = ../common/typeSizes.f90

# 1.3 Messages
# ------------

MESS = ../ropp_messages/messages.f90 ../ropp_messages/assert.f90                         \
       ../ropp_messages/message_get_addinfo.f90 \
       ../ropp_messages/message_get_program.f90 \
       ../ropp_messages/message_get_routine.f90 \
       ../ropp_messages/message_set_addinfo.f90 \
       ../ropp_messages/message_set_program.f90 \
       ../ropp_messages/message_set_routine.f90

if FC_BACKSLASH_ESCAPE
MESS += ../ropp_messages/message_bs_escape.f90
else
MESS += ../ropp_messages/message.f90
endif

# 1.4 Date and time routines
# --------------------------

DTTM = ../datetime/datetimetypes.f90     ../datetime/datetimeprogs.f90  \
       ../datetime/caltojul.f90          ../datetime/convertdt.f90      \
       ../datetime/date_and_time_utc.f90 ../datetime/datetimeoffset.f90 \
       ../datetime/monthofyear.f90       ../datetime/timesince.f90

# 1.5 Array utilities
# -------------------

ARR = ../arrays/arrays.f90 ../arrays/blend.f90 ../arrays/callocate.f90         \
      ../arrays/copy_alloc.f90 ../arrays/cross_product.f90                     \
      ../arrays/getrun.f90 ../arrays/imaxloc.f90 ../arrays/iminloc.f90         \
      ../arrays/isinrange.f90 ../arrays/l2norm.f90 ../arrays/locate.f90        \
      ../arrays/nruns.f90 ../arrays/outer_and.f90 ../arrays/outer_product.f90  \
      ../arrays/preallocate.f90 ../arrays/reallocate.f90 ../arrays/reverse.f90 \
      ../arrays/setminus.f90 ../arrays/sort.f90 ../arrays/swap.f90             \
      ../arrays/uniq.f90 ../arrays/unit_vector.f90 ../arrays/where.f90

# 1.6 Geodetic routines
# ----------------------

GEOD = ../geodesy/geodesy.f90   ../geodesy/gravity.f90 \
       ../geodesy/r_eff.f90                            \
       ../geodesy/geopotential2geometric.f90           \
       ../geodesy/geometric2geopotential.f90           \
       ../geodesy/great_circle_distance.f90

# 1.7 Geometric routines
# ----------------------

COORD = ../coordinates/coordinates.f90      \
	../coordinates/earth.f90            \
	../coordinates/curvature.f90        \
        ../coordinates/cart2geod.f90        \
        ../coordinates/geod2cart.f90        \
        ../coordinates/ecf2eci.f90          \
        ../coordinates/eci2ecf.f90          \
        ../coordinates/impact_parameter.f90 \
        ../coordinates/occ_point.f90        \
	../coordinates/tangent_point.f90    \
	../coordinates/plane_coordinates.f90 \
        ../coordinates/vectors.f90

# 1.8 Unit conversion routines
# ----------------------------

UNITS = ../unitconvert/unitconvert.f90           \
	../unitconvert/ut_convert.f90            \
	../unitconvert/ropp_unit_conversion.f90


# 1.9 Common routines
# --------------------

COMM = ../common/ropp_utils.f90         \
       ../common/ropp_utils_version.f90 \
       ../common/FileDelete.f90         \
       ../common/GetIOUnit.f90          \
       ../common/ToLower.f90            \
       ../common/ToUpper.f90

# 1.10 The library itself
# -----------------------

lib_LIBRARIES = libropp_utils.a

libropp_utils_a_SOURCES = $(TPSZ) $(COMP) $(ARR)  $(UNITS) \
                          $(MESS) $(DTTM) $(GEOD) $(COORD) $(COMM)

# 2. Module files and files to be cleaned
# ---------------------------------------

# upper case mod files
MODULESU = TYPESIZES.mod DATETIMEPROGS.mod DATETIMETYPES.mod \
           UNITCONVERT.mod ARRAYS.mod MESSAGES.mod EARTHMOD.mod \
           GEODESY.mod COORDINATES.mod ROPP_UTILS.mod

# lower case mod files
MODULESL = typesizes.mod datetimeprogs.mod datetimetypes.mod \
           unitconvert.mod arrays.mod messages.mod earthmod.mod \
           geodesy.mod coordinates.mod ropp_utils.mod

if CM_FC_UC_MODNAMES
MODULES = ${MODULESU}
else
MODULES = ${MODULESL}
endif

nodist_include_HEADERS = $(MODULES)

# always remove everything, including other compilers
CLEANFILES      = $(MODULESU) $(MODULESL) *.stb

# 3. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@

# 4. Dependencies
# ---------------

