#! /bin/sh -v

# Define the fortran compiler and options

#include ./Makefile.conf

FC=ifort
FLAGS=-convert big_endian 

all: kinds rtma3d_read_diag

rtma3d_read_diag: rtma3d_read_diag.o kinds.o
	${FC} -o rtma3d_read_diag.exe ${FLAGS} rtma3d_read_diag.o
	rm rtma3d_read_diag.o

kinds: kinds.F90
	$(FC) $(FLAGS) -c kinds.F90

.SUFFIXES : .f90  .o

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
