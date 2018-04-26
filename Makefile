# install: build
# 	echo '(install-shim "$(DESTDIR)" "prhello")' \
# 	| mit-scheme --batch-mode

# clean:
# 	rm netcdf-const* netcdf-types* netcdf-shim* 

# build: netcdf-shim.so netcdf-types.bin netcdf-const.bin

# netcdf-shim.so: netcdf-shim.o
# 	echo "(link-shim)" \
# 	| mit-scheme --batch-mode -- -o $@ $^ 

# netcdf-shim.o: netcdf-shim.c
# 	echo '(compile-shim)' \
# 	| mit-scheme --batch-mode -- -c $<

# netcdf-shim.c netcdf-const.c netcdf-types.bin: netcdf.cdecl
# 	echo '(generate-shim "netcdf" "#include <netcdf.h>")' \
# 	| mit-scheme --batch-mode

# netcdf-const.bin: netcdf-const.scm
# 	echo '(sf "netcdf-const")' | mit-scheme --batch-mode

# netcdf-const.scm: netcdf-const
# 	./netcdf-const

# netcdf-const: netcdf-const.o
# 	gcc -o $@ $^ 

# netcdf-const.o: netcdf-const.c
# 	gcc -o $@ -c $<


install: build
	echo '(install-shim "$(DESTDIR)" "netcdf")' \
	| mit-scheme --batch-mode

clean:
	rm netcdf-const* netcdf-types* netcdf-shim* 

build: netcdf-shim.so netcdf-types.bin netcdf-const.bin

netcdf-shim.so: netcdf-shim.o
	echo "(link-shim)" \
	| mit-scheme --batch-mode -- -o $@ $^ -L/home/adam/.guix-profile/lib -lnetcdf -lhdf5 -fPIC

netcdf-shim.o: netcdf-shim.c
	echo '(compile-shim)' \
	| mit-scheme --batch-mode -- -DHAVE_CONFIG_H -I/home/adam/.guix-profile/lib/mit-scheme-x86-64/ -Wold-style-definition -Wextra -Wno-sign-compare -Wno-unused-parameter -Wstrict-prototypes -Wnested-externs -Wredundant-decls -Wall -Wundef -Wpointer-arith -Winline -O3 -fPIC -c $<

netcdf-shim.c netcdf-const.c netcdf-types.bin: netcdf.cdecl
	echo '(generate-shim "netcdf" "#include <netcdf.h>")' \
	| mit-scheme --batch-mode

netcdf-const.bin: netcdf-const.scm
	echo '(sf "netcdf-const")' | mit-scheme --batch-mode

netcdf-const.scm: netcdf-const
	./netcdf-const

netcdf-const: netcdf-const.o
	gcc -o $@ $^ $(LDFLAGS) -L/home/adam/.guix-profile/lib -lnetcdf -lhdf5 -fPIC

netcdf-const.o: netcdf-const.c
	gcc `pkg-config --cflags netcdf` $(CFLAGS) -o $@ -c $<

