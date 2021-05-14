FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all:trab02.exe

trab02.exe: main.o exercise_boyce.o fourierseries.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

debug:main.f90 exercise_boyce.f90 fourierseries.f90
	$(FC) -g $(FDEBUGFLAGS) -o trab02_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe