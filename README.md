# MapViewer2
Team 17's MQTT map viewer, implemented in Fortran. MQTT connectivity is via [fortran-paho](https://github.com/interkosmos/fortran-paho) and GUI is via [fortran-SDL2](https://github.com/interkosmos/fortran-sdl2).

## Build & Environment
All development was done with gfortran running under WSL2 Ubuntu 20.04 LTS and vcxsrv. It might work on other systems, it might not. Who knows.

## mqttCredentials.f90
`mqttCredentials.f90` holds the username and password for MQTT, and is thus not tracked by git. To compile and run MapViewer2, you'll need to make this file yourself. It should look something like this:
```fortran
module mqttCredentials
	use, intrinsic :: iso_c_binding
	implicit none
	character(len=*), parameter :: USERNAME = "<YOUR USERNAME>"
	character(len=*), parameter :: PASSWORD = "<YOUR PASSWORD>"
	character(len=len(USERNAME) + 1, kind=c_char), target :: USERNAME_C = USERNAME // c_null_char
	character(len=len(PASSWORD) + 1, kind=c_char), target :: PASSWORD_C = PASSWORD // c_null_char
end module mqttCredentials
```
Why the `_C` versions of each? In short, C string/null-terminator shenanigans.

## Running
Install `gfortran` if you don't have it.
Next, install Eclipse Paho. See [fortran-paho](https://github.com/interkosmos/fortran-paho)'s README for instructions.
Then:
```
sudo apt install libsdl2-dev libsdl2-ttf-dev
git clone https://github.com/D22-RBE2002/team-17-MapViewer2.git
cd team-17-MapViewer2
git submodule init
git submodule update
cd fortran-paho
make
cd ..
cd fortran-SDL2
make sdl2 sdl2_ttf
cd ..
make
./mapviewer2
```
