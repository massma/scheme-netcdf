# Copyright (C) 2018 Adam Massmann <massmannak@gmail.com>

# This file is part of scheme-netcdf.

# scheme-netcdf is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.

# scheme-netcdf is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with scheme-netcdf; if not, see <http://www.gnu.org/licenses/>

LDFLAGS=-L$(HOME)/.guix-profile/lib -lnetcdf -lhdf5 -fPIC
TEST_OBJECTS = testing/simple_xy_nc4.nc testing/gen-data.exe \
testing/gen-version.exe testing/gen-xyz-data.exe \
testing/simple_xyz_nc4.nc
.PHONY: build clean

# install: build
# 	echo '(install-shim "$(DESTDIR)" "netcdf")' \
# 	| mit-scheme --batch-mode

clean:
	rm -f netcdf-const* netcdf-types* netcdf-shim* netcdf.com \
	netcdf.bci	netcdf.bin $(TEST_OBJECTS)

check : netcdf.com testing/gen-version.exe testing/simple_xy_nc4.nc \
testing/simple_xyz_nc4.nc test-netcdf.scm 
	echo '(load "test-netcdf.scm")' \
	| mit-scheme --batch-mode --library \
	$(HOME)/.guix-profile/lib/mit-scheme-x86-64:$(PWD) \
	--band all.com

# testing prereqs
testing/gen-version.exe : testing/gen-version.c
	gcc -Wall -o $@ -lnetcdf $<

testing/simple_xy_nc4.nc : testing/gen-data.exe
	cd testing && ./gen-data.exe

testing/simple_xyz_nc4.nc : testing/gen-xyz-data.exe
	cd testing && ./gen-xyz-data.exe

testing/gen-data.exe : testing/simple_xy_nc4_wr.c
	gcc -Wall -o $@ -lnetcdf $<

testing/gen-xyz-data.exe : testing/simple_xyz.c
	gcc -Wall -o $@ -lnetcdf $<

# build stuff
build: netcdf.com

netcdf.com : netcdf-shim.so netcdf-types.bin netcdf-const.bin netcdf.scm
	echo "(load-option 'ffi) (c-include \"netcdf\") (cf \"netcdf\")" \
	| mit-scheme --batch-mode --library \
	$(HOME)/.guix-profile/lib/mit-scheme-x86-64:$(PWD) \
	--band all.com

netcdf-shim.so: netcdf-shim.o
	echo "(link-shim)" \
	| mit-scheme --batch-mode -- -o $@ $^ $(LDFLAGS)

netcdf-shim.o: netcdf-shim.c
	echo '(compile-shim)' \
	| mit-scheme --batch-mode -- -c $<

netcdf-shim.c netcdf-const.c netcdf-types.bin: netcdf.cdecl
	echo '(generate-shim "netcdf" "#include \"netcdf.h\"")' \
	| mit-scheme --batch-mode

netcdf-const.bin: netcdf-const.scm
	echo '(sf "netcdf-const")' | mit-scheme --batch-mode

netcdf-const.scm: netcdf-const
	./netcdf-const

netcdf-const: netcdf-const.o
	gcc -o $@ $^ $(LDFLAGS)

netcdf-const.o: netcdf-const.c
	gcc `pkg-config --cflags netcdf` $(CFLAGS) -o $@ -c $<

