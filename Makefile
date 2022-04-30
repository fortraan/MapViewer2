FFLAGS = -I./fortran-paho -I./fortran-SDL2 -ggdb
LDFLAGS = `sdl2-config --cflags`
LDLIBS = fortran-SDL2/libsdl2.a fortran-SDL2/libsdl2_ttf.a -lfortran-paho -lpaho-mqtt3c `sdl2-config --libs` -lSDL2_ttf -L./fortran-paho

%.o: %.f90
	gfortran $(FFLAGS) -c $^

mapviewer2: mqttCredentials.o cFuncs.o grid.o gfx.o strlenIntercept.o mapViewer2.f90
	gfortran $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

clean:
	rm -f ./mapviewer2 *.o *.mod
