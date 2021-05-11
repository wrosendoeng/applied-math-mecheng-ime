FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm
LIBS = -lopenblas -lm

all:ex18.exe

ex18.exe: main.o functions.o fourierseries.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug:main.f90 functions.f90 fourierseries.f90
	$(FC) -g $(FDEBUGFLAGS) -o ex18_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe