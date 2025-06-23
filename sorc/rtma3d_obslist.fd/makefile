#! /bin/sh -v

# Define the fortran compiler and options

#include ./Makefile.conf

FC=ftn
INC=-I/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.8_wmo/include
LIB=-L/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.8_wmo/lib -lwgrib2
FFLAGS0=  -O3 -fp-model strict -convert big_endian -traceback
FFLAGS1= -integer-size 64 -real-size 64
FFLAGS2= -free
LDFLAGS = -m64

LIBS2 = ${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}
LIBS3 = ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
#LIBS4 =    -mkl=sequential -lmpi #-lessl


CMD = rtma3d_obslist
CMAIN = prdgen.f90

OBJ1 = w3fb11_8.o w3fb06_8.o w3fb08_8.o
SORC1 = w3fb11_8.f w3fb06_8.f w3fb08_8.f

${OBJ1}:${SORC1}
	${FC} ${FFLAGS0} ${FFLAGS1} ${FFLAGS2} -c ${SORC1}

OBJ2 = get_ob_lists3d.o gridspecs.o mod_rll_trans.o weather_report_status_list.o
SORC2 =get_ob_lists3d.f90 gridspecs.f90 mod_rll_trans.f90 weather_report_status_list.f90

${OBJ2}:${SORC2}
	${FC} ${FFLAGS0} ${FFLAGS2} -c  ${INC} ${LIB} ${SORC2} ${INC} ${LIB}

OBJ3 = latlon_to_grid.o 

SORC3 = latlon_to_grid.f 

${OBJ3}:${SORC3}
	${FC} ${FFLAGS0} ${FFLAGS2} -c ${SORC3}

LINK-OBJS = $(OBJ1) $(OBJ2) $(OBJ3)

all: $(CMD)

$(CMD): ${LINK-OBJS}
	${FC} ${FFLAGS0} ${LDFLAGS} -o $(CMD) ${CMAIN} ${LINK-OBJS}  ${INC} ${LIB} ${LIBS2} ${LIBS3}

clean:
	/bin/rm -f *.o *.exe *.mod

