OBJECTS = simple_xy_nc4.nc gen-data.exe gen-version.exe
.PHONY: test clean

MIT_SCHEME_EXE = $(MITSCHEME_ROOT)/bin/mit-scheme

test : simple_xy_nc4.nc gen-version.exe
	cd ../ && echo '(load "test-netcdf")' | $(MIT_SCHEME_EXE) --batch-mode

gen-version.exe : gen-version.c
	gcc -I/usr/include -I/usr/include/hdf5/serial -Wall -o $@ -lnetcdf $<

simple_xy_nc4.nc : gen-data.exe
	./gen-data.exe

gen-data.exe : simple_xy_nc4_wr.c
	gcc -I/usr/include -I/usr/include/hdf5/serial -Wall -o $@ -lnetcdf $<

clean :
	rm -f $(OBJECTS)

