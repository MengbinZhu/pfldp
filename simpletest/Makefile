#Makefile for simpletest
FC=ifort
FCFLAGS= -O3 -w
CC=gcc
CXX=g++

OBJS=progtest.o

ALL: progtest \
	simexp

progtest: NDGen.F90 progtest.F90
	$(FC) $(FCFLAGS) NDGen.F90 progtest.F90 -o progtest

simexp: NDGen.F90 simexp.F90 calw.F90 matrixinv.F90
	$(FC) $(FCFLAGS) NDGen.F90 calw.F90 simexp.F90 matrixinv.F90 -o simexp

.PHONY : clean
clean:
	rm *.o
