FC = gfortran
FFLAGS = -march=native -mtune=native -O3
FDEBUGFLAGS = -g -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal
IDIR = -I/usr/local/include/fgsl/
LIBS = -lgsl -lfgsl -lopenblas -lm

all:pdeheatequation.exe

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $@ $(IDIR) $<

pdeheatequation.exe: main.f90 exercise_boyce.o fourierseries.o
	$(FC) $(FFLAGS) -o $@ $(IDIR) $^ $(LIBS)

debug:main.f90 exercise_boyce.f90 fourierseries.f90
	$(FC) -g $(FDEBUGFLAGS) -o pdeheatequation_dbg.exe $(IDIR) $^ $(LIBS)

clean:
	rm -f *.o *.mod *.exe *.pdf *.txt
