#Makefile for simpletest
FC=ifort
GFC=gfortran
PGFC=pgf90
FCFLAGS= -O3 -w -g
FCLIBFLAGS = ./hsl_ea20/src/libhsl_ea20.a -llapack -lblas  -L/opt/intel/composer_xe_2011_sp1.7.256/compiler/lib/intel64 -L/opt/intel/composer_xe_2011_sp1.7.256/ipp/../compiler/lib/intel64 -L/opt/intel/composer_xe_2011_sp1.7.256/ipp/lib/intel64 -L/opt/intel/composer_xe_2011_sp1.7.256/mkl/lib/intel64 -L/opt/intel/composer_xe_2011_sp1.7.256/tbb/lib/intel64//cc4.1.0_libc2.4_kernel2.6.16.21 -L/usr/lib/gcc/x86_64-linux-gnu/4.8 -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../../lib64 -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/4.8/../../.. -L/lib64 -L/lib -L/usr/lib64 -L/usr/lib -lifport -lifcore -limf -lsvml -lm -lipgo -lirc -lpthread -lirc_s -ldl
CC=gcc
CXX=g++

LIB = -L/usr/lib/lib
HSLLIB=/home/mengbinzhu/pfldp/simpletest/hsl_ea20/src/
HSLINC=/home/mengbinzhu/pfldp/simpletest/hsl_ea20/src/

OBJS=progtest.o 

ALL: progtest \
	simexp

progtest: NDGen.F90 bsdet.F90 progtest.F90
	$(FC) -I $(HSLINC) $(FCFLAGS) $(FCLIBFLAGS) NDGen.F90 bsdet.F90 progtest.F90 -o progtest $(LIB)lapack.a $(LIB)blas.a -llapack -lblas -L$(HSLLIB) -lhsl_ea20 -I $(HSLINC)

#random_d.o: random_d.F90
#	$(FC) $(FCFLAGS) $(FCLIBFLAGS) random_d.F90 -o random_d.o

simexp: random_d.F90 bsdet.F90 NDGen.F90 simexp.F90 calw.F90 matrixinv.F90 mattrans.F90
	$(FC) -I $(HSLLIB) $(FCFLAGS) $(FCLIBFLAGS) random_d.F90 bsdet.F90 NDGen.F90 calw.F90 simexp.F90 matrixinv.F90 mattrans.F90 -o simexp $(LIB)lapack.a $(LIB)blas.a -llapack -lblas -L$(HSLLIB) -lhsl_ea20 -I $(HSLINC)

.PHONY : clean
clean:
	rm *.o
