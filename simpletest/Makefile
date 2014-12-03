#Makefile for simpletest
FC=ifort
FCFLAGS= -O3 -w
CC=gcc
CXX=g++

LIB = -L/usr/lib/lib

OBJS=progtest.o 

ALL: progtest \
	simexp

progtest: NDGen.F90 sqrtmat.F90 bsdet.F90 sqrtmat2.F90 progtest.F90
	$(FC) $(FCFLAGS) NDGen.F90 sqrtmat.F90 bsdet.F90 sqrtmat2.F90 progtest.F90 -o progtest $(LIB)lapack.a $(LIB)blas.a -llapack -lblas

#random_d.o: random_d.F90
#	$(FC) $(FCFLAGS) random_d.F90 -o random_d.o

simexp: random_d.F90 bsdet.F90 NDGen.F90 simexp.F90 calw.F90 matrixinv.F90 mattrans.F90
	$(FC) $(FCFLAGS) random_d.F90 bsdet.F90 NDGen.F90 calw.F90 simexp.F90 matrixinv.F90 mattrans.F90 -o simexp

.PHONY : clean
clean:
	rm *.o
